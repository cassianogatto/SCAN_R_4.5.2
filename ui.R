######################################################
#           THIS IS SCAN V1                          #
######################################################


# Packages ----
#if (!require("pacman")) install.packages("pacman")
#pacman::p_load(shiny, dplyr, sf, igraph, tidygraph, DT, leaflet, ggplot2, ggraph, tidyr, lwgeom, shinydashboard)

#removed pacman and pkg installations
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
library(DT)    

# UI ----
shinyUI(
    
    dashboardPage(skin = 'black',
                  
                  # --- HEADER ---
                  dashboardHeader(
                      titleWidth = 230,
                      title = span(
                          tags$img(src = "SCAN_logo1.png", width = "80px", style = "margin-top: -5px; margin-right: 10px;")
                      )
                  ),
                  
                  # --- SIDEBAR ----
                  
                  dashboardSidebar(
                      width = '230px',
                      sidebarMenu(
                          id = "sidebar_menu", # <--- ADICIONE ESTE ID (CRUCIAL!)
                          menuItem("About SCAN", tabName = "about_SCAN", icon = icon("info-circle")),
                          menuItem("Tutorial", tabName = "tutorial", icon = icon("book")),
                          menuItem("Species Distribution Maps", tabName = "maps", icon = icon("map")),
                          menuItem("Spatial Congruence", tabName = "Cs_tab", icon = icon("calculator")),
                          menuItem("SCAN Analysis", tabName = "scan", icon = icon("project-diagram")),
                          
                          # --- SUBSTITUA O ANTIGO VIEWER POR ESTES DOIS ---
                          menuItem("SCAN Explorer (Map)", tabName = "map_view", icon = icon("globe-americas")),
                          menuItem("SCAN Details (Plots)", tabName = "static_view", icon = icon("chart-pie"))
                      )
                  ),
                  
                  # --- BODY ---
                  dashboardBody(
                      # Custom CSS ---- 
                      tags$head(
                          tags$style(HTML("        /* --- NOVO CSS PARA O LAYOUT GLOBAL 01dez25 --- */
                      .full-screen-map {
                        height: calc(100vh - 50px) !important;
                        position: relative;
                        margin: -15px; /* Remove margens do dashboard */
                        z-index: 0;
                      }
                      .floating-panel { 
                        background-color: rgba(255,255,255,0.95); 
                        padding: 20px; 
                        border-radius: 8px; 
                        box-shadow: 0 4px 15px rgba(0,0,0,0.3);
                        z-index: 1050; /* Garante que fique acima do mapa e do conteúdo */
                        max-height: 90vh;
                        overflow-y: auto;
                      }
                      .content { background-color: white; }
                      .box.box-solid.box-primary>.box-header { background-color: #3c8dbc; color: #fff; }
                      .box.box-solid.box-warning>.box-header { background-color: #f39c12; color: #fff; }
                      .box.box-solid.box-danger>.box-header { background-color: #dd4b39; color: #fff; }
                      .box.box-solid.box-success>.box-header { background-color: #00a65a; color: #fff; }
                      .btn-warning { background-color: #f39c12; border-color: #e08e0b; color: white; }
                      .btn-danger { background-color: #dd4b39; border-color: #d73925; color: white; }
                      /* Alignment fix for buttons next to inputs */
                      .align-btn { margin-top: 25px; } 
                    "))
                      ),
                      
                      # --- 1. PAINEL DE CONTROLE GLOBAL (FLUTUANTE) ---
                      # Aparece apenas nas abas 'map_view' OU 'static_view'
                      conditionalPanel(
                          condition = "input.sidebar_menu == 'map_view' || input.sidebar_menu == 'static_view'",
                          
                          absolutePanel(
                              id = "global_controls", 
                              class = "floating-panel",
                              top = 60, right = 20, width = 350,
                              draggable = TRUE, 
                              
                              tags$h3(icon("cogs"), "SCAN Explorer (floating box)"),
                              
                              # Controle ÚNICO de Threshold
                              sliderInput("threshold_global", "Threshold (Ct):", min=0, max=1, value=0.5, step=0.05),
                              
                              tags$hr(),
                              tags$h5("Select Chorotypes:"),
                              # O conteúdo dos checkboxes será gerado no server
                              uiOutput("chorotype_selector_global"),
                              
                              tags$hr(),
                              tags$h5("Visual Settings:"),
                              
                              checkboxInput("show_minigraph", "Show Network Graph", value = TRUE),# --- NEW CHECKBOX HERE ---
                              sliderInput("alpha_global", "Transparency:", min=0, max=1, value=0.5, step=0.1),
                              selectInput("palette_global", "Palette:", choices = c("Set2", "Set1", "Paired", "Dark2", "RdYlBu")),
                              selectInput("layout_graph", "Graph Layout:", choices = c( "fr", "nicely" #,"circle", "grid"
                              ))
                          )
                      ),
                      
                      ### TAB ITEMS ----
                      
                      tabItems(
                          
                          # 1. ABOUT SCAN ----
                          tabItem("about_SCAN",
                                  fluidPage(
                                      fluidRow(
                                          column(12, align = "center",
                                                 tags$img(src = "SCAN_logo1.png", width = "75%", style = "margin: 20px 0;")
                                          )
                                      ),
                                      fluidRow(
                                          column(12, align = "center",
                                                 tags$h2("Spatial Congruence Analysis", style = "font-weight: bold;"),
                                                 tags$h4("By cassianogatto@gmail.com"),
                                                 tags$h4(icon("github"), tags$a(href="https://github.com/cassianogatto/SCAN_engine_app", "View on GitHub", target="_blank")),
                                                 tags$hr(style = "border-top: 1px solid #ccc; width: 60%;")
                                          )
                                      ),
                                      fluidRow(
                                          column(10, offset = 1,
                                                 tags$div(style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px; border-left: 5px solid #3c8dbc;",
                                                          tags$h3(icon("users"), " What are Chorotypes?", style = "margin-top: 0; color: #3c8dbc;"),
                                                          tags$p(style = "font-size: 1.1em; line-height: 1.5; text-align: justify;",
                                                                 "Chorotypes are unique combinations of species with spatial congruences (Cs) higher between themselves than to any species of other such groups. In SCAN, species groupings are relative to (and determined by) thresholds of spatial congruence (Ct)."
                                                          ),
                                                          tags$p(style = "font-size: 1.1em; line-height: 1.5; text-align: justify;",
                                                                 "Each chorotype corresponds to a 'community' in network terminology. In the graph representation, species are vertices (nodes) and links (edges) are Cs values. The map depicts the actual spatial distribution of each component species of a chorotype."
                                                          ),
                                                          tags$p(style = "font-style: italic; margin-top: 15px;",
                                                                 "SCAN reveals spatial patterns of congruent distributions; interpreting and discussing their biogeographic significance is the biogeographer's task.Please refer to Gatto & Cohn-Haft 2021 for a detailed analysis of these conceptual implications - PlosOne",
                                                                 tags$a(href="https://doi.org/10.1371/journal.pone.0245818", "https://doi.org/10.1371/journal.pone.0245818", target="_blank")
                                                          )
                                                 )
                                          )
                                      ),
                                      tags$br(),
                                      # fluidRow(
                                      #   column(10, offset = 1,
                                      #          box(width = 12, title = "Abstract (Gatto & Cohn-Haft 2021)", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                      #              tags$p(style = "text-align: justify;", "Species with congruent geographical distributions, potentially caused by common historical and ecological spatial processes, constitute biogeographical units called chorotypes...")
                                      #          )
                                      #   )
                                      # )
                                  )
                          ),
                          
                          # 2. TUTORIAL ----
                          tabItem(tabName = "tutorial",
                                  fluidPage(
                                      tags$h2("SCAN Engine Tutorial"),
                                      tags$p("Follow this step-by-step guide to perform a Spatial Congruence Analysis."),
                                      
                                      # Step 1
                                      box(width = 12, title = "Step 1: Loading Species Maps", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                          tags$h4(icon("map"), "Input File Requirements"),
                                          tags$ul(
                                              tags$li("Your map must be a ", tags$strong("Shapefile (.shp)"), "."),
                                              tags$li("The map must contain ", tags$strong("all species' distributions as unique layers"), ", each having unique IDs and geometries."),
                                              tags$li("Ideally, the Shapefile should have at least one ID column named ", tags$code("'sp'"), " and a column named ", tags$code("'geometry'"), ".")
                                          ),
                                          tags$br(),
                                          tags$h4(icon("upload"), "Loading Steps"),
                                          tags$ol(
                                              tags$li(tags$strong("Upload:"), " Click the ", tags$em("Browse..."), " button in the 'Species Distribution Maps' tab. ", 
                                                      tags$strong("Important:"), " You must select the ", tags$code(".shp"), " file along with its associated files (e.g., ", tags$code(".shx, .dbf, .prj"), ") simultaneously."),
                                              tags$li(tags$strong("Verification:"), " Once loaded, the app will display information regarding the number of species, the Geographic Projection (CRS), and column names."),
                                              tags$li(tags$strong("Adjusting ID Column:"), " If your species ID column is not named 'sp', type the ", tags$strong("correct column name"), " in the text box provided and reload the map.")
                                          )
                                      ),
                                      
                                      # Step 2
                                      box(width = 12, title = "Step 2: Spatial Congruence (Cs)", status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                          tags$h4(icon("calculator"), "Calculating the Index"),
                                          tags$p("In the 'Spatial Congruence' tab, you define how similarity between species is quantified."),
                                          tags$ul(
                                              tags$li(tags$strong("Formula:"), " The default is the ", tags$strong("Hargrove Index"), " (overlap area / species area). You can define a custom formula by checking 'Use an alternative Cs Index'."),
                                              tags$li(tags$strong("Filtering:"), " Use the 'Minimum Cs value' input to remove weak connections (e.g., < 0.1) to clean up the network."),
                                              tags$li(tags$strong("Action:"), " Click ", tags$code("Apply Cs index to map"), " to start the calculation.")
                                          ),
                                          tags$br(),
                                          tags$h4(icon("layer-group"), "Optimization (Buffers)"),
                                          tags$p("For large datasets or complex geometries, exact intersection can be slow."),
                                          tags$ul(
                                              tags$li("Check ", tags$strong("Use an internal buffer"), " to slightly shrink polygons before calculation. This avoids marginal/accidental overlaps."),
                                              tags$li(tags$em("Note:"), " This works best with metric CRS (e.g., UTM).")
                                          )
                                      ),
                                      
                                      # Step 3
                                      box(width = 12, title = "Step 3: Running SCAN Analysis", status = "danger", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                          tags$h4(icon("sliders-h"), "Configuration"),
                                          tags$p("Configure how the algorithm scans through the network thresholds (Ct):"),
                                          tags$ul(
                                              tags$li(tags$strong("Range:"), " Set the ", tags$code("Max"), " and ", tags$code("Min"), " threshold values."),
                                              tags$li(tags$strong("Resolution:"), " Define the step size (e.g., 0.01) for the scan. Smaller steps take longer but provide more detail."),
                                              tags$li(tags$strong("Criteria:"), 
                                                      tags$ul(
                                                          tags$li(tags$strong("Overlap:"), " If checked, requires all species in a group to overlap with each other (Clique)."),
                                                          tags$li(tags$strong("Diameter:"), " Limits the maximum 'width' of the network component to avoid sprawling chains of species.")
                                                      )
                                              )
                                          ),
                                          tags$br(),
                                          tags$h4(icon("play-circle"), "Execution"),
                                          tags$p("Click the ", tags$strong("SCAN!"), " button. The result tables (Chorotypes, Parameters) will appear below. You can download them as CSV files.")
                                      ),
                                      
                                      # Step 4
                                      box(width = 12, title = "Step 4: Visualizing Results (Viewer)", status = "success", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                          tags$h4(icon("eye"), "Exploring Chorotypes"),
                                          tags$p("Go to the 'SCAN Viewer' tab to explore the results interactively."),
                                          tags$ol(
                                              tags$li(tags$strong("Select Threshold (Ct):"), " Use the input box to choose a specific cut-off level."),
                                              tags$li(tags$strong("Select Chorotypes:"), " A list of available groups (numbers) for that threshold will appear. Check the boxes to visualize specific groups."),
                                              tags$li(tags$strong("Customize:"), " Adjust transparency and color palettes using the controls on the right.")
                                          ),
                                          tags$br(),
                                          tags$h4(icon("images"), "View Modes"),
                                          tags$ul(
                                              tags$li(tags$strong("Interactive Map:"), " A Leaflet map allowing zooming and panning."),
                                              tags$li(tags$strong("Static Map:"), " A high-quality ggplot map suitable for publication."),
                                              tags$li(tags$strong("Network Graphs:"), " Visualize the topology of the species' relationships (Simple or Detailed views).")
                                          )
                                      )
                                  )
                          ),
                          
                          # 3. MAPS TAB (PRIMARY - BLUE) ----
                          tabItem("maps",
                                  fluidPage(
                                      tags$h2("Map of Species' Distributions"),
                                      
                                      # Help Box
                                      box(width = 12, title = "Instructions", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                          tags$p("Upload the shapefile map containing all species’ distributions. Maps must have an ID column named ‘sp’ and a ‘geometry’ column.")
                                      ),
                                      
                                      fluidRow(
                                          # Input Column
                                          column(width = 5,
                                                 box(width = NULL, title = "1. Input Map", status = "primary", solidHeader = TRUE,
                                                     fileInput("filemap", "Choose shape-files (.shp + .shx + .dbl + .prj)", 
                                                               accept = c('.shp','.dbf','.sbn','.sbx','.shx',".prj"), multiple=TRUE),
                                                     
                                                     fluidRow(
                                                         column(6, checkboxInput("fix_invalid_shapes", "Fix invalid layers?", value = FALSE)),
                                                         column(6, checkboxInput("modify_crs", "Change projection?", value = FALSE))
                                                     ),
                                                     
                                                     conditionalPanel("input.modify_crs == true",
                                                                      numericInput("map_projection", "Enter EPSG code:", value = 4326),
                                                                      helpText("Common: WGS84 (4326), SIRGAS2000 (4674)")
                                                     ),
                                                     
                                                     tags$hr(),
                                                     tags$strong("Column Identification"),
                                                     tags$p("If species ID is not 'sp', specify below:"),
                                                     textInput("colum_sp_map", "Species ID Column:", value = "sp"),
                                                     
                                                     actionButton("get_map", "Load Map", class = "btn-primary", width = "100%", icon = icon("upload"))
                                                 ),
                                                 
                                                 # File Info Box
                                                 box(width = NULL, title = "File Info", status = "primary", collapsible = TRUE,
                                                     verbatimTextOutput("map_upload_names")
                                                 )
                                          ),
                                          
                                          # Output Column
                                          column(width = 7,
                                                 box(width = NULL, title = "2. Map Sample", status = "primary", solidHeader = TRUE,
                                                     plotOutput("map_shp", height = "400px")
                                                 ),
                                                 
                                                 box(width = NULL, title = "3. Diagnostics", status = "primary",
                                                     tags$h5("Map Column Names:"),
                                                     textOutput("map_shp_names"),
                                                     tags$hr(),
                                                     tags$h5("Invalid Geometries?"),
                                                     tableOutput("invalid_map_species"),
                                                     tags$hr(),
                                                     tags$h5("Projection (CRS):"),
                                                     textOutput("map_crs"),
                                                     tags$hr(),
                                                     tags$h5("Species List (First 10):"),
                                                     tableOutput("map_species") 
                                                 )
                                          )
                                      )
                                  )
                          ),
                          
                          # 4. SPATIAL CONGRUENCE TAB (WARNING - ORANGE) ----
                          tabItem("Cs_tab",
                              fluidPage(
                                  tags$h2("Spatial Congruence Index (Cs)"),
                                  
                                  fluidRow(
                                      # Calculation Box
                                      column(width = 4,
                                             box(width = NULL, title = "1. Calculate Index", status = "warning", solidHeader = TRUE,
                                                 tags$p("Calculate the Hargrove Index (default) or a custom formula."),
                                                 
                                                 checkboxInput("use_alternative_index", "Use custom formula?", value = FALSE),
                                                 conditionalPanel("input.use_alternative_index == true",
                                                                  textInput("cs_similarity_index", "Formula:", value = '(area_overlap / area_sp1) * (area_overlap / area_sp2)')
                                                 ),
                                                 
                                                 tags$hr(),
                                                 
                                                 # --- NEW CHUNK PROCESSING OPTION ---
                                                 tags$strong("Processing Settings (Memory Safety):"),
                                                 checkboxInput("use_chunks", "Process in Chunks? (Prevents RAM crash)", value = FALSE),
                                                 
                                                 conditionalPanel(
                                                     condition = "input.use_chunks == true",
                                                     numericInput("chunk_size", "Chunk Size (spp per batch):", value = 20, min = 5, step = 5),
                                                     helpText("Lower values (e.g., 10-20) use less RAM but take longer.")
                                                 ),
                                                 tags$hr(),
                                                 # -----------------------------------
                                                 
                                                 fluidRow(
                                                     column(6, numericInput("filter_Cs", "Minimum Cs value:", value = 0.1, step = 0.05)),
                                                     column(6, actionButton("calculate_Cs", "Apply Cs Index", class = "btn-warning align-btn", width = "100%", icon = icon("play")))
                                                 )
                                             ),
                                             # Upload Cs
                                             box(width = NULL, title = "2. Udload Index", status = "warning", solidHeader = TRUE,
                                                 tags$p("Use a previously calculated Cs Index table."),
                                                 tags$strong("Upload existing Cs table?"),
                                                 checkboxInput("Cs_upload_csv", "Upload .csv instead of calculating"),
                                                 conditionalPanel("input.Cs_upload_csv == true",
                                                                  fileInput("Cs_table", "Select CSV", accept = c(".csv"))
                                                 ),
                                             ),
                                             
                                             # Buffer Box
                                             box(width = NULL, title = "3. Optimization (Buffers)", status = "warning", 
                                                 solidHeader = TRUE,collapsible = TRUE, collapsed = TRUE,
                                                 helpText("Use inner buffers to avoid marginal overlaps in large datasets (metric CRS only)."),
                                                 checkboxInput("use_buffer_map", "Enable Internal Buffer?", value = FALSE),
                                                 
                                                 conditionalPanel("input.use_buffer_map == true",
                                                                  numericInput("shrink_factor_buff", "Buffer Size (metric):", value = 0.01),
                                                                  checkboxGroupInput("quantiles_to_buffer", "Quartiles to buffer:", choices = c(1,2,3,4), selected = c(), inline = TRUE),
                                                                  tags$p(style = "color: red; font-weight: bold;", icon("exclamation-triangle"), " Warning: This alters polygon areas!")
                                                 )
                                             )
                                      ),
                                      
                                      # Data Management Box
                                      column(width = 4,
                                             box(width = NULL, title = "3. Data Management", status = "warning", solidHeader = TRUE,
                                                 
                                                 # PREVIEW IS NOW DEFAULT
                                                 tabsetPanel(
                                                     tabPanel("Preview",
                                                              tags$br(),
                                                              textOutput("check_Cs_tables"),
                                                              tags$h5("Head:"),
                                                              tableOutput("Cs_head"),
                                                              tags$h5("Tail:"),
                                                              tableOutput("Cs_tail")
                                                     ),
                                                     
                                                     tabPanel("Download Cs",
                                                              tags$hr(),
                                                              tags$strong("Download calculated table:"),
                                                              tags$br(),
                                                              downloadButton("download_Cs", "Download Cs.csv", class = "btn-default")
                                                     )
                                                 )
                                             ),
                                             
                                             
                                      ),
                                      
                                      # check Cs network
                                      column( 4, 
                                          box(width = NULL, title = "Graph Check", status = "warning",
                                              tags$h5("Nodes and Edges Preview:"),
                                              fluidRow(
                                                  column(6, tableOutput("graph_nodes")),
                                                  column(6, tableOutput("graph_edges"))
                                              )
                                          )
                                          
                                      ),
                                      
                                  
                                      
                                      
                                  )
                              )
                          ),
                          # 5. SCAN ANALYSIS TAB (DANGER - RED) ----
                          tabItem("scan",
                                  fluidPage(
                                      tags$h2("Spatial Congruence Network Analysis"),
                                      
                                      fluidRow(
                                          column(width = 6,
                                                 # Configuration Box
                                                 box(width = NULL, title = "1. Algorithm Configuration", status = "danger", solidHeader = TRUE,
                                                     fluidRow(
                                                         column(3, numericInput("resolution", "Resolution (Step):", value = 0.1, step = 0.01)),
                                                         column(3, numericInput("threshold_min", "Min Threshold:", value = 0.2, step = 0.05)),
                                                         column(3, numericInput("threshold_max", "Max Threshold:", value = 0.9, step = 0.05)),
                                                         column(3, 
                                                                checkboxInput("filter_diameter", "Limit Diameter?", value = TRUE),
                                                                conditionalPanel("input.filter_diameter == true",
                                                                                 numericInput("max_diameter", "Max Diameter:", value = 15)
                                                                )
                                                         )
                                                     ),
                                                     fluidRow(
                                                         column(3, checkboxInput("overlap", "Require Full Overlap (Clique)?", value = TRUE))
                                                     ),
                                                     tags$hr(),
                                                     actionButton("run_scan", "RUN SCAN ANALYSIS", class = "btn-danger", icon = icon("rocket"), width = "200px")
                                                 ),
                                          ),
                                          column(6,
                                                 
                                                 # Results Box
                                                 box(width = NULL, title = "2. Results & Downloads", status = "danger",
                                                     fluidRow(
                                                         column(8, uiOutput("names_scan_list")),
                                                         column(4, downloadButton("downloadData", "Download Selected", class = "btn-default align-btn"))
                                                     ),
                                                     tags$hr(),
                                                     tags$h4("Preview Data:"),
                                                     DT::DTOutput('table_download_preview'),
                                                     tags$br(),
                                                     tags$h4("Chorotypes List:"),
                                                     tableOutput("scan_chorotypes")
                                                 )
                                          )
                                      )
                                  )
                          ),
                          
                          # 6. MAP VIEWWER 1: TELA CHEIA (MAPA) ----
                          tabItem("map_view",
                                  
                                  div(class = "full-screen-map",
                                      
                                      leafletOutput("map_fullscreen", width = "100%", height = "100%"),
                                      
                                      # --- NEW MINI GRAPH PANEL ---
                                      # Only shows if the checkbox in settings is TRUE
                                      conditionalPanel(
                                          condition = "input.show_minigraph == true",
                                          
                                          absolutePanel(
                                              id = "mini_graph_box",
                                              
                                              # Removemos a classe 'floating-panel' para não pegar o fundo branco padrão
                                              # Definimos estilo manual para ser transparente e compacto
                                              style = "background-color: rgba(255,255,255,0.4); padding: 5px; border-radius: 10px; z-index: 1000;",
                                              bottom = 20, left = 20, 
                                              width = 350, height = 450, # Largura fixa reduzida
                                              draggable = TRUE,
                                              
                                              # Título menor
                                              tags$h3(icon("star"), "Network Topology", style = "margin: 0px 0px 5px 5px; font-weight: bold; opacity: 0.7;"),
                                              
                                              # Plot menor (200px de altura)
                                              plotOutput("mini_graph_plot", height = "330px")
                                          )
                                      )
                                  )
                          ),
                          
                          # 7: MAP + GRAPH VIEWER ----
                          tabItem("static_view",
                                  fluidPage(
                                      fluidRow(
                                          column(5, 
                                                 box(width = NULL, title = "Static Map", status = "primary", solidHeader = TRUE,
                                                     plotOutput("ggplot_map", height = "500px")
                                                 )
                                          ),
                                          column(5,
                                                 box(width = NULL, title = "Network Topology", status = "primary", solidHeader = TRUE,
                                                     plotOutput("graph_plot", height = "500px")
                                                 )
                                          ), 
                                          
                                          column(1,),
                                          
                                      ),
                                      fluidRow(
                                          column(12,
                                                 box(width = NULL, title = "Species List (Selected Groups)", status = "success", solidHeader = TRUE,
                                                     DT::DTOutput("view_species_table")
                                                 )
                                          )
                                      )
                                  )
                          )
                      ) # fim tabItems
                  ) # fim dashboardBody
    ) # dashboardPage
) # shinyUI