# 1. Install pacman if not already installed
if (!require("pacman")) install.packages("pacman")

# 2. Load all packages (installs them automatically if missing)
pacman::p_load(
  shiny,
  dplyr,
  sf,
  igraph,
  tidygraph,
  leaflet,
  ggplot2,
  ggraph,
  tidyr,
  lwgeom,
  shinydashboard
)

shinyServer(function(input, output, session) {
  
  # --- BLOCO DE FUNÇÕES (SOURCE) ---
  { 
    # fix igraph's 'to_subgraph' function
    to_subgraph <- function(graph, ..., subset_by = NULL, delete.vertices = TRUE) {
      if (is.null(subset_by)) {
        subset_by <- active(graph)
      }
      ind <- as_tibble(graph, active = subset_by)
      ind <- mutate(ind, .tidygraph_index = seq_len(n()))
      ind <- filter(ind, ...)
      ind <- ind$.tidygraph_index
      subset <- switch(
        subset_by,
        nodes = induced_subgraph(graph, ind),
        edges = subgraph.edges(graph, ind, delete.vertices = delete.vertices)
      )
      list(subgraph = as_tbl_graph(subset))
    }
    
    SCAN_lite =  function(graph = C, max_Ct = max(graph %>% activate(edges) %>% as_tibble %>% .$Cs),
                          min_Ct = 0.76, Ct_resolution = -0.02, max_diameter = 10, mark_overlap = TRUE, 
                          filter_overlap = FALSE, filter_diameter = FALSE, filter_out_spp = c()    ) {
      
      chorotypes = list()
      g_spp_all = tibble()
      g_summary_all = tibble()
      Ct_resolution = ifelse(Ct_resolution > 0, Ct_resolution * (-1), Ct_resolution)
      if(isTRUE(filter_overlap)) {mark_overlap = TRUE}
      if(isTRUE(mark_overlap)) {graph = graph %>% activate(nodes) %>% mutate(no_overlap = NA)}
      
      # MAIN LOOP --
      for(threshold in seq(max_Ct,min_Ct,Ct_resolution)){
        
        # any species to be filtered out? (1)
        if(length(filter_out_spp) != 0) {
          graph = graph %>% morph(to_subgraph, subset_by = "nodes",  name %in% filter_out_spp,
                                  remove_multiples = TRUE, delete.vertices= TRUE) %>% mutate(filter = 1) %>%
            unmorph() }
        
        # get the communities (components)
        graph = partial_components(graph = graph, threshold = threshold, filter_diameter = filter_diameter,
                                   filter_overlap = filter_overlap)
        
        # get statistics by component (3)
        g = map_by_component(graph = graph, threshold = threshold)
        
        # filter by diameter (4)
        g = g %>% activate(nodes) %>%
          mutate("filter" = ifelse(get(paste0("diameter",threshold)) > max_diameter, threshold, NA))
        
        g = g %>% activate(nodes) %>% mutate("betweenness{threshold}" := round(betweenness(g),1))
        
        # species' list
        g_spp = g %>% activate(nodes) %>% as_tibble() %>%
          group_by(name, get(paste0("components",threshold)), get(paste0("diameter",threshold)),
                   get(paste0("order",threshold)), get(paste0("centrality",threshold))) %>%
          summarize(Ct = threshold, betweenness = get(paste0("betweenness",threshold))) %>%
          select(1,Ct, components = 2, diameter = 3, order = 4, centrality = 5, betweenness) %>%
          arrange(name)
        
        # communities
        g_summary = g %>% activate(nodes) %>% as_tibble %>% group_by(get(paste0("components",threshold)),
                                                                     get(paste0("order",threshold))) %>%
          summarize(Ct = threshold, chorotype_spp = paste(name, collapse = ", "), richness_spp = n(),
                    diameter = max(get(paste0("diameter",threshold))),
                    max_centrality = max(get(paste0("centrality",threshold))),
                    max_betweenness = max(get(paste0("betweenness", threshold)))) %>%
          select(component = 1, Ct, chorotype_spp, richness_spp, diameter, max_centrality, max_betweenness)
        
        # check overlap
        if(isTRUE(mark_overlap)){
          
          g_spp = g_spp %>% mutate(no_overlap = NA)
          g_summary = g_summary %>% mutate(no_overlap = NA)
          
          are_connected = data.frame()
          for(comp in g_summary$component){
            spp = g_summary %>% filter(component == comp) %>% pull(.,"chorotype_spp") %>% strsplit(.,", ") %>% .[[1]]
            
            for (sp1 in spp){
              for(sp2 in spp[which(spp != sp1)]){
                conn = tibble(species1 = sp1, species2 = sp2,
                              connected = igraph::are.connected(graph, sp1,sp2))
                are_connected = rbind(are_connected, conn)
              }        }         }
          
          connected_nodes_in_components =  are_connected %>%
            group_by(species1) %>% summarize(all_connected = ifelse(all(connected), TRUE, FALSE)) %>%
            left_join(g_spp, by = c("species1" = "name")) %>% select(component = 4, name = 1,2) %>%
            arrange(component, name)
          
          all_connected_components = connected_nodes_in_components %>% group_by(component) %>%
            summarize(all_connected = ifelse(all(all_connected), TRUE, FALSE))
          
          not_connected_components = all_connected_components %>% filter(all_connected == FALSE) %>% pull(.,"component")
          spp_in_not_connected_components = g_spp %>% filter(components %in% not_connected_components) %>%
            pull(.,'name')
          
          if(length(not_connected_components) > 0 & isTRUE(mark_overlap)) {
            g_spp = g_spp %>% mutate(no_overlap = replace(NA, name %in% spp_in_not_connected_components, threshold ) )
            g_summary = g_summary %>% mutate(no_overlap = replace(NA, component %in% not_connected_components, threshold))
            
            if(isTRUE(mark_overlap)) {
              graph = graph %>% morph(to_subgraph, subset_by = "nodes",
                                      is.na(no_overlap) & name %in% spp_in_not_connected_components,
                                      remove_multiples = TRUE, delete.vertices= TRUE) %>%
                mutate(no_overlap = threshold) %>%
                unmorph()
            }
          }
        }
        
        g_spp_all = rbind(g_spp_all, g_spp)
        g_summary_all = rbind(g_summary_all, g_summary)
        
      }  # main loop ends
      
      if(isTRUE(mark_overlap)){
        chorotypes[['chorotypes']] = g_summary_all %>% group_by(chorotype_spp, richness_spp, diameter) %>%
          summarise(Ct_max = max(Ct), Ct_min = min(Ct), max_centrality = max(max_centrality),
                    max_betweenness = max(max_betweenness), no_overlap = max(no_overlap)) %>%
          arrange(chorotype_spp, desc(Ct_max))
        
        chorotypes[['all_spp_summary']] = g_spp_all %>% group_by(name, components, order) %>%
          summarise(max_Ct = max(Ct),
                    min_Ct = min(Ct), max_diam = max(diameter), min_diam = min(diameter),
                    max_between = max(betweenness), no_overlap = max(no_overlap))
      } else {
        chorotypes[['chorotypes']] = g_summary_all %>% group_by(chorotype_spp, richness_spp, diameter) %>%
          summarise(Ct_max = max(Ct), Ct_min = min(Ct), max_centrality = max(max_centrality),
                    max_betweenness = max(max_betweenness)) %>%
          arrange(chorotype_spp, desc(Ct_max))
        
        chorotypes[['all_spp_summary']] = g_spp_all %>% group_by(name, components, order) %>% summarise(max_Ct = max(Ct),
                                                                                                        min_Ct = min(Ct), max_diam = max(diameter), min_diam = min(diameter),
                                                                                                        max_between = max(betweenness))
      }
      
      chorotypes[['all_spp']] = g_spp_all
      chorotypes[['graph']] = graph
      
      chorotypes[["parameters"]] = tibble(max_diameter = max_diameter, max_Ct = max_Ct, min_Ct = min_Ct,
                                          Ct_resolution = Ct_resolution, mark_overlap = mark_overlap,
                                          filter_overlap = filter_overlap, filter_diameter = filter_diameter)
      
      return(chorotypes)
    }
    
    partial_components = function (graph = graph, threshold = threshold, filter_diameter = FALSE, filter_depth = FALSE, filter_overlap = FALSE, ...){
      
      print("using 'igraph::group_components' - see more options of community structurig in '?group_components'")
      
      if(isTRUE(filter_overlap) & isTRUE(filter_diameter)) { graph %>% morph(to_subgraph, subset_by = "edges",
                                                                             (Cs >= threshold & is.na(.N()$no_overlap[from]) & is.na(.N()$filter[from])), 
                                                                             remove_multiples = TRUE, delete.vertices= TRUE) %>%
          activate(edges) %>% mutate("Ct{threshold}" := TRUE) %>%
          activate(nodes) %>% mutate("Ct{threshold}" := TRUE) %>%
          mutate("components{threshold}" := group_components("weak")) %>% 
          unmorph()
        
      } else {
        if(isTRUE(filter_overlap)) {
          graph %>% morph(to_subgraph, subset_by = "edges",
                          (Cs >= threshold & is.na(.N()$no_overlap[from])),
                          remove_multiples = TRUE, delete.vertices= TRUE) %>%
            activate(edges) %>% mutate("Ct{threshold}" := TRUE) %>%
            activate(nodes) %>% mutate("Ct{threshold}" := TRUE) %>%
            mutate("components{threshold}" := group_components("weak")) %>%
            unmorph()
        } else {
          
          if(isTRUE(filter_diameter)) {
            graph %>% morph(to_subgraph, subset_by = "edges",
                            (Cs >= threshold & is.na(.N()$filter[from])),
                            remove_multiples = TRUE, delete.vertices= TRUE) %>%
              activate(edges) %>% mutate("Ct{threshold}" := TRUE) %>%
              activate(nodes) %>% mutate("Ct{threshold}" := TRUE) %>%
              mutate("components{threshold}" := group_components("weak")) %>% 
              unmorph()
          } else{
            graph %>% morph(to_subgraph, subset_by = "edges", Cs >= threshold,
                            remove_multiples = TRUE, delete.vertices= TRUE) %>%
              activate(edges) %>% mutate("Ct{threshold}" := TRUE) %>%
              activate(nodes) %>% mutate("Ct{threshold}" := TRUE) %>%
              mutate("components{threshold}" := group_components("weak")) %>%
              unmorph()
          }        }        }        }
    
    map_by_component = function(graph = graph, threshold = threshold){ 
      graph %>% activate(edges) %>% filter(!is.na(get(paste0("Ct",threshold)))) %>%
        activate(nodes) %>% filter(!is.na(get(paste0("Ct",threshold)))) %>%
        morph(to_split, group_by = get(paste0("components",threshold)), subset_by = "nodes") %>%
        mutate("diameter{threshold}" := graph_diameter(unconnected = TRUE),
               "order{threshold}" := graph_order(),
               "centrality{threshold}" := centrality_degree()) %>%
        unmorph()
    }
    
    overlapping_species <- function( map = map(), shrink_factor = 0.01, quartiles_to_buffer = c(3,4)) {
      shrink_factor <- ifelse(shrink_factor < 0, shrink_factor, (-1)*shrink_factor)
      buffer <- ifelse(map$quantile_area %in% quartiles_to_buffer, sqrt(map$area) * shrink_factor, 0)
      map <- st_buffer(map, dist = buffer) #(11s)
      overlapping <- st_intersects(map, map, sparse = F) |> as_tibble() |> 
        setNames(map$sp) |> mutate(sp1 = map$sp) |> select(sp1, everything()) |> 
        pivot_longer(cols = !1, names_to = "sp2") |> filter(value) |> filter(sp1 != sp2) |> 
        filter(!duplicated(paste0(pmax(sp1, sp2), pmin(sp1, sp2))))
      overlapping
    }
    
    overlap_areas_function <- function(map = map, over = overlapping ){
      over <- over |> left_join(map, by = c('sp1' = 'sp')) |> select(sp1,sp2, value, geom_sp1 = geometry)
      over <- over |> left_join(map, by = c('sp2' = 'sp')) |> select(sp1,sp2, value, geom_sp1, geom_sp2 = geometry)
      over <- over |> rowwise() |> mutate(area_overlap = st_area(st_intersection(geom_sp1, geom_sp2)))
      over <- over |> rowwise() |> mutate(area_sp1 = st_area(geom_sp1), area_sp2 = st_area(geom_sp2))
      over <- over |> select(sp1, sp2, area_sp1, area_sp2, area_overlap)
      over 
    }
  }
  
  # --- MAIN SCRIPT server ---
  
  options(shiny.maxRequestSize=1000*1024^2) 
  
  # maps and Cs----
  
  map_upload <- eventReactive(input$get_map,{
    shpdf <- input$filemap
    if(is.null(shpdf)){    return()    }
    previouswd <- getwd()
    uploaddirectory <- dirname(shpdf$datapath[1])
    setwd(uploaddirectory)
    for(i in 1:nrow(shpdf)){   file.rename(shpdf$datapath[i], shpdf$name[i])    }
    setwd(previouswd)
    
    # ATUALIZAÇÃO PARA SF (SUBSTITUINDO readOGR)
    shp_file <- shpdf$name[grep(pattern="\\.shp$", shpdf$name)]
    map1 <- sf::st_read(dsn = paste(uploaddirectory, shp_file, sep="/"), quiet = TRUE)
    map1
  })
  
  map1 <- reactive({
    map_col <- which(map_upload() |> names() == input$colum_sp_map)
    if(is.null(map_col)){ return() }
    map <- map_upload() |> select(sp = map_col, geometry)
    map <- map |> group_by(sp) |> summarise()
    return( map )
  })
  
  map2 <- reactive({
    if (isTRUE(input$modify_crs)) {
      if(st_crs(map1())$epsg != as.numeric(input$map_projection)) { 
        map <- map1() |> st_transform(crs = input$map_projection)
      } else { map <- map1() }
    } else { map <- map1() }
    return(map)
  })
  
  invalid_map <-  reactive ({ 
    invalid <- which( ! st_is_valid( map2() ) ) 
    if( length( invalid ) > 0) {return( invalid )} else { return( c() ) }
  })
  
  output$invalid_map_species <- renderTable( 
    map2()[invalid_map(), ] |>  st_drop_geometry() |> select(sp) |> summarise(invalid_species = paste(sp, collapse = ', '))  
  )
  
  map <- reactive({
    if( isTRUE(input$fix_invalid_shapes) & ( length(invalid_map() ) > 0 ) ) {           
      map <- map2()
      map[invalid_map(),] <- st_make_valid(map[invalid_map(), ]) 
    } else { map <- map2() }
    return(map)
  })
  
  overlapping <- reactive({ 
    map_areas = st_area(map())
    map <- map() |> mutate(area = map_areas)
    map <- map() |> mutate(quantile_area = as.integer(cut(map_areas, quantile(map_areas))))
    map <- map |> mutate(quantile_area = ifelse(is.na(quantile_area), 1, quantile_area))
    
    shrink_factor <- ifelse(input$shrink_factor_buff < 0, input$shrink_factor_buff, (-1) * input$shrink_factor_buff)
    buffer <- ifelse( map$quantile_area %in% input$quantiles_to_buffer, sqrt(map_areas) * shrink_factor, 0 )
    map <- st_buffer(map, dist = buffer)
    
    overlapping <- st_intersects(map, map, sparse = F) |> as_tibble() |> 
      setNames(map$sp) |> mutate(sp1 = map$sp) |> select(sp1, everything()) |> 
      pivot_longer(cols = !1, names_to = "sp2") |> filter(value) |> filter(sp1 != sp2) |> 
      filter(!duplicated(paste0(pmax(sp1, sp2), pmin(sp1, sp2))))
    
    overlapping
  })
  
  Cs_calc <- eventReactive(input$calculate_Cs, {
    req( map() )
    
    # MANTENDO sf_use_s2(FALSE) PARA COMPATIBILIDADE GEOMÉTRICA COM VERSÃO ANTIGA
    sf_use_s2(FALSE)
    
    areas_df <- overlapping() |> left_join( map(), by = c('sp1' = 'sp')) |> select(sp1,sp2, value, geom_sp1 = geometry)
    areas_df <- areas_df |> left_join( map(), by = c('sp2' = 'sp')) |> select(sp1,sp2, value, geom_sp1, geom_sp2 = geometry)
    areas_df <- areas_df |> rowwise() |> mutate(area_overlap = st_area(st_intersection(geom_sp1, geom_sp2)))
    areas_df <- areas_df |> rowwise() |> mutate(area_sp1 = st_area(geom_sp1), area_sp2 = st_area(geom_sp2))
    areas_df <- areas_df |> select(sp1, sp2, area_sp1, area_sp2, area_overlap)
    areas_df <- areas_df |> units::drop_units()
    
    Cs_calc <- areas_df |> mutate( Cs = eval( parse( text = input$cs_similarity_index ) ) ) 
    Cs_calc <- Cs_calc |> filter(Cs >= input$filter_Cs) |>  select(sp1,sp2, Cs) |> arrange(desc(Cs)) |> mutate(Cs = round(Cs, 3))
    Cs_calc
  })
  
  Cs_up <- reactive({
    req(input$Cs_table)
    Cs_file <- input$Cs_table
    Cs_upload <- read.csv(Cs_file$datapath, header = TRUE) |> as_tibble() |> select(sp1,sp2,Cs)
    return(Cs_upload)
  })
  
  Cs <- reactive( {
    if(isTRUE(input$Cs_upload_csv)) {  return(  Cs_up()  |> filter(Cs > input$filter_Cs )  )
    } else {
      return(  Cs_calc() |> filter(Cs > input$filter_Cs )  )  }
  })
  
  graph <- reactive({
    graph <- Cs() |> as_tbl_graph( from = sp1, to = sp2, directed = FALSE )
    graph <- graph %>% igraph::simplify(remove.multiple = TRUE, remove.loops = FALSE, edge.attr.comb="first")
    graph <- graph %>% as_tbl_graph(directed = FALSE)
    return(graph)
  })
  
  SCANlist <- eventReactive( input$run_scan, {
    req(graph())
    thres_max <- input$threshold_max
    while(thres_max >= max_thres_graph()) {  thres_max <- thres_max - input$resolution  }
    
    SCANlist <- SCAN_lite(
      graph = graph(),
      max_Ct =  thres_max,
      min_Ct =  input$threshold_min,
      Ct_resolution =  input$resolution,
      max_diameter = input$max_diameter,
      mark_overlap = input$overlap,
      filter_overlap = input$overlap
    )
    
    SCANlist[['graph_nodes']] <- SCANlist[['graph']] |> activate(nodes) |> as_tibble()
    SCANlist[['graph_edges']] <- SCANlist[['graph']] |> activate(edges) |> as_tibble()
    return(SCANlist)
  })
  
  dataset_SCAN_ouput <- reactive({
    switch(input$scan_data_to_download,
           "chorotypes" = SCANlist()[['chorotypes']],
           "all_spp_summary" = SCANlist()[['all_spp_summary']],
           "all_spp" = SCANlist()[['all_spp']],
           "parameters" = SCANlist()[['parameters']],
           "graph_nodes" = SCANlist()[['graph_nodes']],
           "graph_edges" = SCANlist()[['graph_edges']]
    )
  }) 
  
  max_thres_graph <- reactive({ graph() |> activate(edges) |> as_tibble() |> summarise(max(Cs)) |> unlist() |> round(2)  })
  
  threshold <- reactive({   input$threshold   })
  
  g_full <- reactive({
    if(!isTRUE(input$graph_from_csv)){
      g <- SCANlist()[['graph']]
      g <- g  |>  activate(edges) %>% select(from, to, Cs) %>%  filter(Cs >= threshold()) %>%
        activate(nodes) %>% filter(!is.na(get(paste0("components",threshold())))) %>%
        select( name, comps = paste0('components', threshold() ) ) %>% arrange(comps, name)
      return(g)
    } else {      
      node_file <- input$graph_nodes
      nodes <- read.csv(node_file$datapath, header = TRUE)
      edge_file <- input$graph_edges
      edges <- read.csv(edge_file$datapath, header = TRUE)
      
      g <- tbl_graph(nodes = nodes, edges = edges, directed = F)
      g <- g  |>  activate(edges) %>% select(from, to, Cs) %>%  filter(Cs >= threshold()) %>%
        activate(nodes) %>% filter(!is.na(get(paste0("components",threshold())))) %>%
        select( name, comps = paste0('components', threshold() ) ) %>% arrange(comps, name)
      return(g)
    }
  })
  
  original_components <- reactive({
    g_full() %>% activate(nodes) %>%  select(comps) %>%
      arrange(comps) %>% pull() %>% unique()
  })
  
  g_sub <- reactive({
    g_sub <- g_full() %>% activate(nodes) %>%
      filter(comps %in% input$selected_components)
    g_sub
  })
  
  g_map <- reactive({
    g_spp <- g_sub() |> activate(nodes) |> as_tibble()
    g_map1 <- right_join( map(), g_spp, by = c('sp' = 'name')) %>% select(comps, everything())
  })
  
  # OUTPUTS
  output$map_upload_names <- renderText( paste(map_upload() |> names(), collapse = ', ') )
  
  output$check_Cs_tables <- renderText(
    paste("Cs.csv table with ", ncol(Cs()), "columns:" , names(Cs())[1], ",",  names(Cs())[2], ",", names(Cs())[3], ", with", nrow(Cs()), "rows" )
  )
  
  output$Cs_head <- renderTable(   Cs() |> head()  )
  output$Cs_tail <- renderTable(   Cs() |> tail()  )
  
  output$download_Cs <- downloadHandler(
    filename = function() { paste("Cs_table", ".csv", sep = "")  },
    content = function(file) {  write.csv(Cs(), file, row.names = FALSE)    }
  )
  
  output$map_species <- renderTable(   map() |> st_drop_geometry() |> select(sp) |> summarise(species = paste(sp,collapse = ', '))  )
  output$map_shp_names <- renderText(   paste(map() |> names(), collapse = '-    /    -')  )
  output$map_crs <- renderText(  paste( st_crs(map()) ) )
  
  output$map_shp <- renderPlot(
    map_shp <- if(nrow(map()) > 250) { 
      map()[1:200, "sp" ] |>  plot(col = sf.colors(categorical = TRUE, alpha = 0.5)) 
    } else {  
      map()[, "sp"] |> plot(col = sf.colors(categorical = TRUE, alpha = 0.5)) 
    }  
  )
  
  output$graph_nodes <- renderTable(  graph() |> activate(nodes) |> as_tibble() |> head() )
  output$graph_edges <- renderTable(  graph() |> activate(edges) |> as_tibble() |> head() )
  
  output$test_graph_head <- renderPlot(    (graph() |> activate(edges) |> filter(Cs > 0.5) |> create_layout(layout = "kk") |> ggraph()) + theme_bw() )
  
  output$scan_chorotypes <- renderTable( SCANlist()[['chorotypes']] |> arrange(desc(Ct_max)))
  output$parameters <- renderTable({   SCANlist()[['parameters']]  })
  
  # DOWNLOAD
  output$names_scan_list <- renderUI({   
    names <- names(SCANlist())
    selectInput(inputId = "scan_data_to_download", label = "Choose a SCAN dataset preview (below) or download", choices = names[names !="graph"] )
  }) 
  
  output$downloadData <- downloadHandler(
    filename = function() { paste(input$scan_data_to_download, ".csv", sep = "")  },
    content = function(file) {  write.csv(dataset_SCAN_ouput(), file, row.names = FALSE)    }
  )
  
  output$table_download_preview <- renderDataTable({ 
    if(is.null(SCANlist())) { return()  } else {  return( dataset_SCAN_ouput() )   }
  })
  
  # viewer
  pal <- reactive({ 
    colorBin(palette = input$palette, domain = unique(g_map()$comps), bins = 7)
  })
  
  output$original_components <- renderUI({
    checkboxGroupInput(inputId = "selected_components", 
                       label = paste("Choose the chorotypes at Ct =", threshold() ) , 
                       choices = original_components(), 
                       inline = TRUE, 
                       selected = NULL) 
  })
  
  output$g_sub_table <- renderDataTable({
    g_sub() |> activate(nodes) |> as_tibble() |>
      group_by(comps) |> summarise(n_spp = n(), species = paste0(name, collapse = ', '))
  })
  
  # --- INÍCIO DA PARTE 2 INCORPORADA ---
  
  output$map_plot <- renderLeaflet({  
    g_map() |> leaflet() |> 
      addProviderTiles("Esri.WorldTopoMap") %>%
      addPolygons(  weight = 1,  fillColor = ~ pal()(comps), color = "black", dashArray = "1", fillOpacity = input$map_alpha  ) 
  })
  
  output$ggplot_map <- renderPlot({
    # Ajuste: uso seguro de st_geometry e st_coordinates no pacote sf
    gmap_points <- cbind(g_map() |> st_drop_geometry(), st_coordinates(st_centroid(st_geometry(g_map()))))
    
    ggplot(data = g_map() %>% arrange(comps)|> mutate(comps = as.factor(comps)) )  +
      geom_sf( aes(fill = comps), 
               alpha = input$map_alpha, 
               color = 'black', 
               show.legend = F) +
      scale_fill_manual(values = pal()(g_map()$comps)) +
      ggtitle( paste0(" SCAN - Chorotypes at Ct = ", threshold() ) ) + 
      geom_text(data= gmap_points, aes(x = X, y = Y, label = sp),
                color = "black", size = 4, fontface = "italic", check_overlap = TRUE)  +
      theme_bw()
  })
  
  output$graph_plot <- renderPlot({
    lay <- create_layout(g_sub(), layout = input$layout)
    ggraph(lay) +
      geom_edge_link( aes( alpha = (Cs+0.75)) , width = 1.25 , show.legend = FALSE) +
      geom_node_point( aes( fill = comps),  size = (degree( g_sub(), mode="all") + 15) / 3, shape = 21, show.legend = FALSE) + 
      
      # 💥 LINHA CORRIGIDA 💥
      scale_fill_distiller( direction = 1, palette = input$palette, na.value = "transparent", aesthetics = "fill") +
      
      geom_node_text( aes( label = name), size = 3, col = "black", repel= TRUE) +
      labs( subtitle = paste0("Ct = ", threshold() )) +
      theme_graph()
  })
  
  output$graph_plot2 <- renderPlot({
    lay <- create_layout(g_sub(), layout = input$layout)
    ggraph(lay) +
      geom_edge_link(aes(alpha = (Cs+0.75)) , width = 1.25 , show.legend = FALSE) +
      geom_node_point(aes(fill = comps), 
                      size =  (degree(g_sub(), mode="all") + 20) / 4, shape = 21, show.legend = FALSE) +
      scale_fill_distiller( direction = 1, palette = input$palette, na.value = "transparent", aesthetics = "fill") +
      geom_node_text(aes(label = name), size = 4, col = "black", repel=TRUE) +
      labs( subtitle = paste0("Ct = ", threshold() )) +
      theme_graph()
  })
  
  output$photo <- renderImage({ list( src = file.path("www", "journal.pone.0245818.g004.PNG") ,width = 500, height = 650 
  )}, deleteFile = FALSE)
  
})
