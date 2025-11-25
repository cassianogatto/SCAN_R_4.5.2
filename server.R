library(shiny)
library(shinydashboard)
library(dplyr)
library(igraph)
library(tidygraph)
library(tidyr)
library(ggraph)
library(readr)
library(sf)
library(ggplot2)
library(leaflet)
library(units)
library(lwgeom)      # Para corrigir geometrias (st_make_valid)
library(RColorBrewer)# Para a paleta de cores
library(DT)          # Para as tabelas modernas

# Aumentar limite de upload para 100MB (para shapes grandes)
options(shiny.maxRequestSize=100*1024^2)
# Desativar geometria esférica para evitar erros de cálculo planar em lat/lon
sf::sf_use_s2(FALSE)

shinyServer(function(input, output, session) {
  
  # --- 1. LEITURA E PROCESSAMENTO DO MAPA ---
  
  # Reactive para ler o shapefile
  map_input <- reactive({
    req(input$filemap)
    
    # O input$filemap é um dataframe com 'name' (nome original) e 'datapath' (caminho temporário)
    # Precisamos renomear os arquivos temporários para os nomes originais para o read_sf funcionar
    temp_dir <- tempdir()
    
    for (i in 1:nrow(input$filemap)) {
      file.copy(from = input$filemap$datapath[i], 
                to = file.path(temp_dir, input$filemap$name[i]), 
                overwrite = TRUE)
    }
    
    # Encontrar o arquivo .shp
    shp_file <- input$filemap$name[grep("\\.shp$", input$filemap$name)]
    req(length(shp_file) > 0)
    
    # Ler o shapefile
    st_read(file.path(temp_dir, shp_file), quiet = TRUE)
  })
  
  # Informações sobre as colunas do mapa carregado
  output$map_upload_names <- renderPrint({
    req(map_input())
    names(map_input())
  })
  
  # Reactive principal do Mapa (com correções e renomeação de colunas)
  map <- reactive({
    req(map_input())
    dados <- map_input()
    
    # Renomear coluna de espécie para 'sp' se necessário
    col_name_sp <- input$colum_sp_map
    if(col_name_sp %in% names(dados) && col_name_sp != "sp") {
      names(dados)[names(dados) == col_name_sp] <- "sp"
    }
    
    # Transformar CRS se solicitado
    if(input$modify_crs && !is.na(input$map_projection)) {
      dados <- st_transform(dados, crs = input$map_projection)
    }
    
    # Tentar corrigir geometrias inválidas
    if(input$fix_invalid_shapes) {
      # Requer pacote lwgeom
      dados <- st_make_valid(dados)
    }
    
    return(dados)
  })
  
  # Identificar espécies inválidas
  invalid_map <- reactive({
    req(map())
    !st_isValid(map())
  })
  
  output$invalid_map_species <- renderTable({
    req(map())
    if(any(invalid_map())) {
      map() |> filter(!st_isValid(geometry)) |> select(sp) |> st_drop_geometry()
    } else {
      data.frame(Status = "No invalid species found!")
    }
  })
  
  output$map_crs <- renderText({
    req(map())
    st_crs(map())$input
  })
  
  output$map_species <- renderTable({
    req(map())
    data.frame(Species = unique(map()$sp))
  })
  
  output$map_shp_names <- renderText({
    req(map())
    names(map())
  })
  
  # Plot simples do mapa de entrada
  output$map_shp <- renderPlot({
    req(map())
    ggplot(map()) + geom_sf() + theme_minimal()
  })
  
  
  # --- 2. CÁLCULO DO CS (Spatial Congruence) ---
  
  # Reactive para calcular ou carregar a tabela Cs
  Cs_table_data <- reactive({
    # Se o usuário fez upload de um CSV
    if(input$Cs_upload_csv && !is.null(input$Cs_table)) {
      return(read_csv(input$Cs_table$datapath))
    }
    
    # Se o usuário clicou para calcular (requer mapa)
    req(input$calculate_Cs)
    req(map())
    
    # Isolar execução para não recalcular à toa
    isolate({
      shapes <- map()
      
      # Buffer interno (opcional)
      if(input$use_buffer_map) {
        # Atenção: st_buffer em graus pode ser problemático, idealmente usar projeção métrica
        shapes <- st_buffer(shapes, dist = -input$shrink_factor_buff)
      }
      
      # Cálculo das áreas das espécies
      areas <- shapes |> 
        mutate(area_sp = st_area(geometry)) |>
        st_drop_geometry() |>
        select(sp, area_sp)
      
      # Intersecção (Overlap)
      # Isso pode demorar para muitos polígonos!
      intersections <- st_intersection(shapes, shapes)
      
      # Calcular área de overlap
      # Filtrar onde sp.1 != sp.2 para evitar auto-comparação (embora Cs=1)
      # E evitar duplicatas (A-B e B-A são iguais para Cs simétrico)
      cs_data <- intersections |>
        filter(sp != sp.1) |> # sp vem do x, sp.1 vem do y
        mutate(area_overlap = st_area(geometry)) |>
        st_drop_geometry() |>
        select(sp1 = sp, sp2 = sp.1, area_overlap)
      
      # Juntar com áreas totais
      cs_final <- cs_data |>
        left_join(areas, by = c("sp1" = "sp")) |>
        rename(area_sp1 = area_sp) |>
        left_join(areas, by = c("sp2" = "sp")) |>
        rename(area_sp2 = area_sp)
      
      # Calcular Índice Cs
      # Fórmula padrão (Hargrove): (Overlap/Area1) * (Overlap/Area2)
      if(input$use_alternative_index && nchar(input$cs_similarity_index) > 0) {
        # Avaliar fórmula customizada (perigoso, mas flexível)
        # Requer que a string seja uma expressão válida usando as colunas
        expr <- parse(text = input$cs_similarity_index)
        cs_final <- cs_final |> mutate(Cs = eval(expr))
      } else {
        cs_final <- cs_final |> 
          mutate(Cs = (as.numeric(area_overlap) / as.numeric(area_sp1)) * (as.numeric(area_overlap) / as.numeric(area_sp2)))
      }
      
      return(cs_final |> select(sp1, sp2, Cs))
    })
  })
  
  # Outputs da aba Cs
  output$check_Cs_tables <- renderText({
    req(Cs_table_data())
    paste("Rows:", nrow(Cs_table_data()), " | Columns:", paste(names(Cs_table_data()), collapse=", "))
  })
  
  output$Cs_head <- renderTable({ req(Cs_table_data()); head(Cs_table_data()) })
  output$Cs_tail <- renderTable({ req(Cs_table_data()); tail(Cs_table_data()) })
  
  output$download_Cs <- downloadHandler(
    filename = function() { "Cs_matrix.csv" },
    content = function(file) { write_csv(Cs_table_data(), file) }
  )
  
  
  # --- 3. SCAN ANALYSIS (Network & Chorotypes) ---
  
  SCANlist <- eventReactive(input$run_scan, {
    req(Cs_table_data())
    
    # Filtros iniciais
    cs_filtered <- Cs_table_data() |> 
      filter(Cs >= input$threshold_min)
    
    # Criar grafo completo inicial
    g_full <- as_tbl_graph(cs_filtered, directed = FALSE)
    
    # Lista para guardar resultados
    results <- list()
    chorotypes_df <- data.frame()
    
    # Loop pelos thresholds (Ct)
    thresholds <- seq(input$threshold_min, input$threshold_max, by = input$resolution)
    
    withProgress(message = 'Running SCAN...', value = 0, {
      
      for(ct in thresholds) {
        incProgress(1/length(thresholds), detail = paste("Threshold:", ct))
        
        # 1. Filtrar arestas pelo Ct atual
        # Ativar arestas, filtrar, reativar nós para remover isolados
        g_temp <- g_full |>
          activate(edges) |>
          filter(Cs >= ct) |>
          activate(nodes) |>
          mutate(degree = centrality_degree()) |>
          filter(degree > 0) 
        
        # 2. Identificar componentes (comunidades)
        # Usando 'group_components' do tidygraph (wrapper do igraph::components)
        comps <- g_temp |> 
          activate(nodes) |>
          mutate(component_id = group_components()) |>
          as_tibble()
        
        # Se não houver componentes, pular
        if(nrow(comps) == 0) next
        
        # 3. Validar Chorótipos (Overlap Total / Diâmetro)
        # Agrupar espécies por componente
        comp_list <- split(comps$name, comps$component_id)
        
        for(cid in names(comp_list)) {
          spp_in_group <- comp_list[[cid]]
          
          # Ignorar grupos com apenas 1 espécie (opcional, mas comum em chorótipos)
          if(length(spp_in_group) < 2) next
          
          # Verificar critério de Clique (Overlap total) se solicitado
          is_valid <- TRUE
          if(input$overlap) {
            # Subgrafo do grupo
            g_sub <- g_temp |> filter(name %in% spp_in_group)
            # Densidade = 1 significa clique (todos conectados)
            if(igraph::edge_density(g_sub) < 1) is_valid <- FALSE
          }
          
          # Verificar Diâmetro se solicitado
          if(is_valid && input$filter_diameter) {
            g_sub <- g_temp |> filter(name %in% spp_in_group)
            if(igraph::diameter(g_sub) > input$max_diameter) is_valid <- FALSE
          }
          
          # Se passou nos filtros, salvar
          if(is_valid) {
            chorotypes_df <- rbind(chorotypes_df, data.frame(
              Threshold = ct,
              Chorotype_ID = paste0("Ct", ct, "_G", cid),
              Species = spp_in_group,
              N_Species = length(spp_in_group)
            ))
          }
        }
      }
    }) # fim progress
    
    # Preparar listas finais
    results[['chorotypes']] <- chorotypes_df
    results[['parameters']] <- data.frame(
      Min_Ct = input$threshold_min,
      Max_Ct = input$threshold_max,
      Resolution = input$resolution,
      Overlap_Check = input$overlap,
      Diameter_Check = input$filter_diameter
    )
    results[['graph']] <- g_full # Rede completa para visualização
    results[['all_spp']] <- unique(c(Cs_table_data()$sp1, Cs_table_data()$sp2))
    
    # Nós e Arestas para exportação
    results[['graph_nodes']] <- g_full |> activate(nodes) |> as_tibble()
    results[['graph_edges']] <- g_full |> activate(edges) |> as_tibble()
    
    return(results)
  })
  
  # --- OUTPUTS DE TABELAS (CORRIGIDO COM DT) ---
  
  output$parameters <- renderTable({
    req(SCANlist())
    SCANlist()[['parameters']]
  })
  
  output$scan_chorotypes <- renderTable({
    req(SCANlist())
    head(SCANlist()[['chorotypes']], 20) # Mostrar só as primeiras
  })
  
  # Lista dinâmica de arquivos para download
  output$names_scan_list <- renderUI({
    req(SCANlist())
    selectInput("scan_data_to_download", "Choose table to download/view:", 
                choices = c("Chorotypes" = "chorotypes", 
                            "Graph Nodes" = "graph_nodes", 
                            "Graph Edges" = "graph_edges",
                            "Parameters" = "parameters"))
  })
  
  # Tabela de Preview (CORRIGIDO: renderDT + switch fix)
  dataset_SCAN_ouput <- reactive({
    req(SCANlist(), input$scan_data_to_download)
    
    # Garantir input único (correção do erro 'switch vector length')
    selected <- input$scan_data_to_download[1]
    
    switch(selected,
           "chorotypes" = SCANlist()[['chorotypes']],
           "graph_nodes" = SCANlist()[['graph_nodes']],
           "graph_edges" = SCANlist()[['graph_edges']],
           "parameters" = SCANlist()[['parameters']]
    )
  })
  
  output$table_download_preview <- renderDT({
    req(dataset_SCAN_ouput())
    datatable(dataset_SCAN_ouput(), options = list(pageLength = 5, scrollX = TRUE))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste0(input$scan_data_to_download, ".csv") },
    content = function(file) { write_csv(dataset_SCAN_ouput(), file) }
  )
  
  
  # --- 4. SCAN VIEWER (Visualização Interativa) ---
  
  # Atualizar checkbox de chorótipos baseado no Threshold escolhido
  output$original_components <- renderUI({
    req(SCANlist())
    df <- SCANlist()[['chorotypes']]
    
    # Filtrar pelo Ct escolhido no input 'threshold' da aba Viewer
    # Precisamos arredondar para evitar problemas de float (0.8 vs 0.8000001)
    disp_ct <- input$threshold
    available_groups <- df |> 
      filter(abs(Threshold - disp_ct) < 0.001) |> 
      pull(Chorotype_ID) |> 
      unique()
    
    if(length(available_groups) == 0) return(helpText("No chorotypes at this threshold."))
    
    # Extrair apenas o número do ID (ex: "Ct0.8_G1" -> "1") para ficar mais limpo
    # Assumindo formato gerado acima
    group_nums <- gsub(".*_G", "", available_groups)
    names(available_groups) <- paste("Group", group_nums)
    
    checkboxGroupInput("selected_chorotypes", "Select Chorotypes to View:",
                       choices = available_groups,
                       selected = available_groups[1:min(3, length(available_groups))], # Selecionar os 3 primeiros por padrão
                       inline = TRUE)
  })
  
  # Subconjunto de dados para visualização (Mapa e Grafo)
  g_sub <- reactive({
    req(SCANlist(), input$threshold, input$selected_chorotypes)
    
    # Pegar as espécies dos chorótipos selecionados
    df <- SCANlist()[['chorotypes']]
    selected_spp <- df |> 
      filter(Chorotype_ID %in% input$selected_chorotypes) |>
      pull(Species) |>
      unique()
    
    req(length(selected_spp) > 0)
    
    # Pegar o grafo completo e filtrar
    # Também precisamos filtrar as arestas pelo threshold visualizado!
    g_full <- SCANlist()[['graph']]
    
    g_view <- g_full |>
      activate(edges) |>
      filter(Cs >= input$threshold) |> # Filtra conexões fracas
      activate(nodes) |>
      filter(name %in% selected_spp) |> # Filtra espécies selecionadas
      # Adicionar info de qual grupo pertence (para colorir)
      # Nota: uma espécie pode estar em múltiplos grupos em teoria, mas aqui vamos simplificar
      mutate(comps = group_components()) # Recalcula componentes locais para colorir
    
    return(g_view)
  })
  
  # Subset do Mapa Geográfico
  g_map <- reactive({
    req(g_sub(), map())
    
    # Nomes das espécies no grafo filtrado
    spp_names <- g_sub() |> activate(nodes) |> pull(name)
    
    # Filtrar shapefile
    map_filtered <- map() |> filter(sp %in% spp_names)
    
    # Adicionar info de componentes (para cor)
    # Join com dados do grafo
    node_data <- g_sub() |> activate(nodes) |> as_tibble() |> select(name, comps)
    map_final <- map_filtered |> left_join(node_data, by = c("sp" = "name"))
    
    return(map_final)
  })
  
  # --- PALETA DE CORES PADRONIZADA ---
  # Cria uma paleta nomeada fixa para garantir que cores iguais sejam usadas no Leaflet, ggplot e ggraph
  chorotype_pal <- reactive({
    req(g_sub())
    
    # Identificar todos os componentes únicos (grupos) no subgrafo atual
    comps_presentes <- g_sub() |> activate(nodes) |> as_tibble() |> pull(comps) |> unique() |> sort()
    
    # Gerar cores
    n_cores <- length(comps_presentes)
    if(n_cores < 3) n_cores <- 3 # RColorBrewer precisa de min 3
    
    # Paleta escolhida no UI
    pal_name <- input$palette
    cores <- suppressWarnings(RColorBrewer::brewer.pal(n = n_cores, name = pal_name))
    
    # Se precisar de mais cores do que a paleta tem, interpolar
    if(length(comps_presentes) > length(cores)) {
      cores <- colorRampPalette(cores)(length(comps_presentes))
    } else {
      cores <- cores[1:length(comps_presentes)]
    }
    
    # Nomear o vetor de cores com os IDs dos componentes
    # Isso garante o mapeamento correto: names(vetor) = ID, value = Cor
    names(cores) <- comps_presentes
    
    return(cores)
  })
  
  
  # --- VISUALIZADORES ---
  
  # 1. Mapa Interativo (Leaflet)
  output$map_plot <- renderLeaflet({
    req(g_map(), chorotype_pal())
    
    # Criar função de cor baseada na paleta fixa
    pal_fun <- colorFactor(palette = chorotype_pal(), domain = g_map()$comps)
    
    leaflet(g_map()) |>
      addProviderTiles(providers$CartoDB.Positron) |> # Mapa base clean
      addPolygons(
        fillColor = ~pal_fun(comps),
        fillOpacity = input$map_alpha,
        color = "black", weight = 1,
        popup = ~paste("Species:", sp, "<br>Group:", comps)
      ) |>
      addLegend("bottomright", pal = pal_fun, values = ~comps, title = "Group")
  })
  
  # 2. Mapa Estático (ggplot)
  output$ggplot_map <- renderPlot({
    req(g_map(), chorotype_pal())
    
    ggplot(g_map()) +
      geom_sf(aes(fill = as.factor(comps)), color = "black", size = 0.2, alpha = input$map_alpha) +
      # Usar scale_fill_manual com a paleta fixa
      scale_fill_manual(values = chorotype_pal(), name = "Group") +
      theme_minimal() +
      labs(title = paste("Chorotypes at Ct =", input$threshold))
  })
  
  # 3. Gráfico de Rede (Simples)
  output$graph_plot <- renderPlot({
    req(g_sub(), chorotype_pal())
    
    lay <- create_layout(g_sub(), layout = input$layout)
    
    ggraph(lay) +
      geom_edge_link(aes(alpha = Cs), width = 1, show.legend = FALSE) +
      # Nós coloridos pela paleta fixa
      geom_node_point(aes(fill = as.factor(comps)), size = 5, shape = 21, color = "black") +
      scale_fill_manual(values = chorotype_pal()) + # <--- AQUI A MÁGICA
      geom_node_text(aes(label = name), repel = TRUE, size = 3) +
      theme_graph() +
      theme(legend.position = "none")
  })
  
  # Tabela de composição dos grupos (na aba Viewer)
  output$g_sub_table <- renderDT({
    req(g_sub())
    df <- g_sub() |> activate(nodes) |> as_tibble() |> select(Group = comps, Species = name, Degree = degree) |> arrange(Group)
    datatable(df, options = list(pageLength = 5))
  })
  
})