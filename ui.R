



#### VERSÃO GEMINI ####



library(shiny)
library(shinydashboard)
library(htmltools)
library(dplyr)
library(igraph)
library(tidygraph)
library(tidyr)
library(ggraph)
library(readr)
library(sf)
library(ggplot2)
library(leaflet)
# library(rgdal) # REMOVIDO: Descontinuado
library(units)
# library(leaflet.esri) # Opcional: Se não usar tiles ESRI específicos, pode remover. Mantido por segurança.

# UI

shinyUI(
  
  dashboardPage( skin = 'black',
                 
                 dashboardHeader(title = "SCAN engine V_0.22"),
                 
                 # menu sidebar ----
                 dashboardSidebar( width = '230px',
                                   
                                   sidebarMenu(
                                     menuItem("about SCAN", tabName = "about_SCAN", icon = icon("info-circle")),
                                     menuItem("Species Distribution Maps ", tabName = "maps", icon = icon("map")),
                                     menuItem("Spatial Congruence", tabName = "Cs_tab", icon = icon("calculator")),
                                     menuItem("SCAN Analysis", tabName = "scan", icon = icon("project-diagram")),
                                     menuItem("SCAN Viewer", tabName = "SCAN_viewer", icon = icon("eye"))
                                     
                                   ) # menu items
                 ),
                 
                 dashboardBody(
                   # Head CSS HTML configs ----
                   tags$head( 
                     tags$style(
                       HTML( 
                         ".content {  
                              background-color:white; 
                              padding-top: 40px; 
                              padding-left: 40px;}
                          
                          .body { 
                              background-color: white;   
                              color: black;   }
                          .sidebar {  
                              position:fixed;  
                              overflow:visible; }             
                          .main-header { 
                              position: fixed; 
                              width = 80%; }"
                       )  )  ),
                   
                   tabItems(
                     
                     # about SCAN ----
                     tabItem("about_SCAN",
                             
                             fluidPage(
                               
                               tags$h3("SCAN"),
                               
                               tags$h4(tags$a(href="https://github.com/cassianogatto/SCAN_engine_app", "https://github.com/cassianogatto/SCAN_engine_app", target="_blank")),
                               
                               fluidRow(
                                 
                                 infoBox(width = 12,
                                         title = "Chorotypes", 
                                         value = " Chorotypes are unique combinations of species with spatial congruences (Cs) higher between themselves than to any species of other such groups.
                                 In SCAN species groupings are relative to (and determined by) thresholds of spatial congruence (Ct).
                                 Each chorotype corresponds to a 'community' in network terminology. In the graph representation, species are vertices (nodes) and links (edges) are Cs values.
                                 The map depicts the actual spatial distribution of each component species of a chorotype.
                                 Chorotypes may 'evolve' grouping more species as thresholds get lower, until any stopping criteria is reached, such as the absence of spatial overlap between all species.
                                 Some chorotypes are ensembled at high Ct values, meaning a very cohesive spatial distribution; others only at low Ct's - it depends on the biogeograpical, ecological and historical attributes of species and environments.
                                 The description, decision, interpretation and discussion about patterns vs. biogeography is the role of the biogeographer. Pls refer to Gatto & Cohn-Haft 2021 to a detailed analysis of these conceptual implications - PlosOne https://doi.org/10.1371/journal.pone.0245818", 
                                         icon = icon("users")),
                                 
                                 imageOutput("photo"), # Corrigido nome do output para bater com server ("photo" vs "photo1")
                                 
                                 box(width = 12,
                                     tags$h4("Abstract (from Gatto & Cohn-Haft 2021)"),
                                     tags$h6("Species with congruent geographical distributions, potentially caused by common historical and 
                                ecological spatial processes, constitute biogeographical units called chorotypes. Nevertheless, the
                                degree of spatial range congruence characterizing these groups of species is rarely used as an explicit 
                                parameter. Methods conceived for the identification of patterns of shared ranges often suffer from scale 
                                bias associated with the use of grids, or the incapacity to describe the full complexity of patterns, 
                                from core areas of high spatial congruence, to long gradients of range distributions expanding from 
                                these core areas. Here, we propose a simple analytical method, Spatial Congruence Analysis (SCAN), 
                                which identifies chorotypes by mapping direct and indirect spatial relationships among species. 
                                Assessments are made under a referential value of congruence as an explicit numerical parameter. 
                                A one-layered network connects species (vertices) using pairwise spatial congruence estimates (edges). 
                                This network is then analyzed for each species, separately, by an algorithm which searches for spatial 
                                relationships to the reference species. The method was applied to two datasets: a simulated gradient of 
                                ranges and real distributions of birds. The simulated dataset showed that SCAN can describe gradients 
                                of distribution with a high level of detail. The bird dataset showed that only a small portion of 
                                range overlaps is biogeographically meaningful, and that there is a large variation in types of patterns 
                                that can be found with real distributions. Species analyzed separately may converge on similar or 
                                identical groups, may be nested in larger chorotypes, or may even generate overlapped patterns with no 
                                species in common. Chorotypes can vary from simple ones, composed by few highly congruent species, to 
                                complex, with numerous alternative component species and spatial configurations, which offer insights 
                                about possible processes driving these patterns in distinct degrees of spatial congruence. Metrics 
                                such as congruence, depth, richness, and ratio between common and total areas can be used to describe 
                                chorotypes in detail, allowing comparisons between patterns across regions and taxa.")
                                 )                     
                               )
                             )
                             
                     ),
                     
                     
                     # maps ----
                     tabItem("maps",
                             
                             fluidPage(
                               
                               tags$h2("Map of Species' Distributions"),
                               
                               # Se o arquivo Rmd não existir, isso vai gerar erro. Comente se necessário.
                               # box(width = 12,
                               #     htmltools::includeMarkdown('www/scan_eng_text1.Rmd')
                               # ),
                               
                               box(width = 7,    
                                   
                                   tags$h3("Input Map"),
                                   
                                   fileInput( inputId = "filemap",  label = "Choose shape-files (.shp + .shx + .dbl + .prj).", 
                                              accept = c('.shp','.dbf','.sbn','.sbx','.shx',".prj"), width = '500px', multiple=TRUE),  
                                   
                                   column(width = 6,
                                          checkboxInput("fix_invalid_shapes", label = "Fix invalid species layers?", value = FALSE)),
                                   
                                   column(width = 6,
                                          checkboxInput("modify_crs", "Change map projection (crs)?", value = FALSE)),
                                   
                                   conditionalPanel(condition = "input.modify_crs == true", 
                                                    
                                                    numericInput(inputId = "map_projection", label = "Choose a crs code to project the map", value = 4326),
                                                    
                                                    tags$h6("WGS84(degree unit: 4326; metric unit: 3857); SIRGAS2000(4674, UTM: 31985, 31977); SAD69(4618)")
                                   ),
                                   
                                   
                                   HTML('<br>'),
                                   
                                   tags$h4("Column names"),
                                   
                                   tags$h5("Shapefile data is encoded in the column 'geometry'.
                                The column identifying species' IDs, if not 'sp' 
                                (default), must be written it in the box below 
                                (currentnames are shown below). 
                                Press 'Load map' again."),
                                   
                                   inputPanel( textInput("colum_sp_map","Which one is the species ID?", value = "sp")),
                                   
                                   # LOAD MAP!!!      
                                   actionButton("get_map", "Load map!",  class = "btn-warning", style = "color: black;"), 
                                   
                                   tags$h4("Map names in the loaded map:"),
                                   
                                   verbatimTextOutput( "map_upload_names" ),
                               ),
                               # map sample ----
                               box( width = 5,
                                    
                                    tags$h3("Map sample"),
                                    
                                    plotOutput("map_shp")
                               ),
                               
                               box(width = 12,
                                   
                                   tags$h3("Check map's column names, species, and projection"),
                                   
                                   tags$h4("Map's column names:"),
                                   
                                   textOutput("map_shp_names"),
                                   
                                   HTML('<br>'), HTML('<br>'),
                                   
                                   tags$h4("Are there invalid species?"),
                                   
                                   tableOutput("invalid_map_species"),
                                   
                                   HTML('<br>'), HTML('<br>'),
                                   
                                   tags$h4("CRS / ESPG specifications"),
                                   
                                   textOutput("map_crs"),
                                   
                                   HTML('<br>'), HTML('<br>'),
                                   
                                   tags$h4("All map species"),
                                   
                                   tableOutput("map_species")
                                   
                               ),
                             )
                     ),
                     
                     tabItem("Cs_tab",      
                             
                             fluidPage(
                               
                               tags$h2("Spatial Congruence Index - Cs"),
                               
                               tags$h3("How much spatially congruent are two species?"),
                               
                               # Cs calculus ----
                               box( width = 6,
                                    
                                    tags$h4("The Spatial Congruence Index (aka Hargrove Index - see Gatto & Cohn-Haft 2021) is calculated for each pair of species."),
                                    
                                    tags$h5("Or you may write an alternative index combining the area variables 'area_overlap', 'area_sp1', 'area_sp2' (see the example in the box)."),
                                    
                                    checkboxInput(inputId = "use_alternative_index", label = "Use an alternative Cs Index", value = FALSE),
                                    
                                    fluidRow(
                                      
                                      conditionalPanel(condition = "input.use_alternative_index == true",
                                                       
                                                       textInput(inputId = "cs_similarity_index", label = "Choose a formula index (default = Hargrove Index)", 
                                                                 
                                                                 value = '(area_overlap / area_sp1) * (area_overlap / area_sp2)') ),
                                      
                                      column( width = 4, 
                                              actionButton("calculate_Cs","Apply Cs index to map", class = "btn-warning") ),
                                      
                                      column( width = 8, 
                                              numericInput( inputId = "filter_Cs", label = "Minimum Cs value", value = 0.1)  ),
                                    ),
                               ), 
                               
                               # buffer ----    
                               box(width = 6,
                                   
                                   tags$h5("For large datasets, the Cs calculus may take a long time. 
                                        The use of inner buffers in selected area-size-classes (quartiles) may save precious time, 
                                        avoiding marginal spatial overlaps."),
                                   
                                   checkboxInput(inputId = "use_buffer_map", label = "Use an internal buffer to avoid marginal overlaps? (metric-based CRS only)", value = FALSE),
                                   
                                   conditionalPanel( condition = "input.use_buffer_map == true",
                                                     
                                                     fluidRow( column(width = 1, ""),
                                                               
                                                               div(style = "color:red",
                                                                   
                                                                   column(width = 2, icon ('skull-crossbones')),    
                                                                   
                                                                   column(width = 6, tags$h4("Shrinking Distribution Buffer!")),
                                                                   
                                                                   column(width = 2, icon ('skull-crossbones'))) ), # caution message
                                                     
                                                     tags$h4("SCAN algorithm uses the original distribution areas (as in the uploaded map) but, depending on the size of the buffer, 
                                                            some overlaps may be overlooked in the Cs table, which prevents SCAN to recognize any relationship between these species. 
                                                                    Be careful to not loose information!"),
                                                     tags$h5("Obs. Buffers are better suited for metric-based CRS projections. The map tab allows crs change (e.g. from WGS84:4326 'degree' unit 
                                                                    to WGS84:3857 Pseudo-Mercator 'metre'."),
                                                     
                                                     numericInput(inputId = "shrink_factor_buff", label = "Choose an internal buffer", value = 0.01),
                                                     
                                                     checkboxGroupInput(inputId = "quantiles_to_buffer", label = "Input quartile areas to buffer (smaller to bigger",
                                                                        
                                                                        choices = c(1,2,3,4), selected = c(), inline = TRUE),
                                   ), # use buffer to polygons
                                   
                                   conditionalPanel( condition = "input.calculate_Cs > 0",  tags$h6("Do NOT change TAB while running ! This may take a long time depending on the number of species ... ")   ), # calculate Cs        
                               ), 
                               
                               box( width = 6,
                                    
                                    tags$h4("Already have a Cs.csv table with 'sp1', 'sp2' and 'Cs' columns? Check the box and upload."),
                                    
                                    checkboxInput("Cs_upload_csv", "Upload a Cs csv file instead"),
                                    
                                    conditionalPanel( condition = "input.Cs_upload_csv == true ",
                                                      
                                                      fileInput( inputId = "Cs_table", label = "Select a Cs table .csv file", accept = c("text/csv","text/comma-separated-values,text/plain",".csv" ))
                                    ), 
                                    
                               ), # # upload Cs
                               
                               box(width = 6,
                                   
                                   tags$h4("You can store your Cs table downloading it from here."),
                                   
                                   downloadButton("download_Cs", "Download Cs.csv"),
                                   
                                   tags$h5("Cs table summary and head & tail"),
                                   
                                   textOutput("check_Cs_tables"),
                                   
                                   fluidRow(
                                     
                                     column( width = 6,  box(  tableOutput("Cs_head") ) ),
                                     
                                     column( width = 6,  box(  tableOutput("Cs_tail") ) )
                                   )
                               ), # download Cs tables
                               
                               box( width = 6,
                                    
                                    tags$h5("<code> Check graph's nodes and edges here!</code>"),    
                                    
                                    fluidRow(    
                                      
                                      column( width = 6, tableOutput("graph_nodes") ),
                                      
                                      column( width = 6, tableOutput("graph_edges") )
                                    )
                               ) # check Cs
                             )
                     ),
                     
                     # SCAN ----
                     tabItem("scan",
                             
                             fluidPage(
                               
                               fluidRow(
                                 
                                 tags$h2("Spatial Congruence Analysis"),
                                 
                                 box( width = 12,
                                      
                                      tags$h4("After calculating or uploading a Cs table of pairwise spatial similarities, you may parameterize and run SCAN!"),
                                      
                                      fluidRow(
                                        
                                        column( width = 3, 
                                                numericInput(inputId = "resolution", label = "Resolution (interval between Ct)", value = 0.1) 
                                        ),
                                        column( width = 3, 
                                                numericInput(inputId = "threshold_max", label = "Max value of threshold Ct", value = 0.9, min = 0.2, max = 1) 
                                        ),
                                        column( width = 3, 
                                                numericInput(inputId = "threshold_min", label = "Min value of threshold Ct", value = 0.2, min = 0.05, max = 0.9) 
                                        ),
                                        column( width = 3, 
                                                conditionalPanel(condition = "input.filter_diameter == true",
                                                                 
                                                                 numericInput(inputId = "max_diameter", label = "Choose the maximum (network) diameter", value = 15)
                                                )       )
                                      ), 
                                      #sliderInput( inputId = "threshold_min_max", label = "Select the threshold range to SCAN", value = c(0.8, 1), min = 0.2, max = 1, step = 0.01 ),
                                      
                                      fluidRow(
                                        
                                        column( width =2, 
                                                actionButton("run_scan", "SCAN!", class = "btn-warning") ),
                                        
                                        column( width = 3, 
                                                checkboxInput(inputId = "overlap", label = "Overlap criterion: Require overlap between all species?", value = TRUE)),
                                        
                                        column( width = 3,  
                                                checkboxInput(inputId = "filter_diameter", label = "Limit the diameter of the chorotypes networks?", value = TRUE)),
                                        
                                      ),
                                      
                                      fluidRow(
                                        
                                        column( width = 8, 
                                                
                                                conditionalPanel( condition = "run_scan > 0",  
                                                                  
                                                                  tags$h6("Do NOT change TAB while running ! This also may last a long time... check R console")  )
                                        )
                                      )
                                 ), # set parameters
                                 
                                 box( width = 12,
                                      
                                      tags$h4("Wait for the results and check the parameters of the SCAN analysis"),
                                      
                                      tableOutput("parameters"),
                                      
                                      fluidRow(
                                        
                                        column( width = 8, uiOutput("names_scan_list") ),
                                        
                                        column( width = 4, downloadButton("downloadData", "Download") )
                                      )
                                 ), # scan results and download
                               ),
                               
                               fluidRow(
                                 
                                 box(width = 12,
                                     
                                     tags$h3('SCAN results - Matrix of range congruences - Cs - data download'),
                                     
                                     dataTableOutput('table_download_preview'),
                                     
                                     tags$h4('All Chorotypes!'),
                                     
                                     tableOutput("scan_chorotypes")
                                     
                                 )
                               )
                             )
                             
                     ),
                     
                     # scan viewer ----
                     tabItem("SCAN_viewer",
                             
                             fluidPage(
                               
                               tags$h2("SCAN Viewer"),
                               
                               fluidRow(
                                 
                                 # Controles de Visualização
                                 box(width = 12, title = "Settings", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                     fluidRow(
                                       column(4, 
                                              numericInput("threshold", "Threshold (Ct):", value = 0.8, step = 0.05, min = 0, max = 1)
                                       ),
                                       column(4,
                                              uiOutput("original_components") # Checkbox group gerado dinamicamente
                                       ),
                                       column(4,
                                              sliderInput("map_alpha", "Map Transparency:", min = 0, max = 1, value = 0.5),
                                              selectInput("palette", "Palette:", choices = c("Set1", "Set2", "Set3", "Paired", "Dark2", "Accent", "Spectral", "RdYlBu"))
                                       )
                                     ),
                                     fluidRow(
                                       column(4,
                                              selectInput("layout", "Graph Layout:", choices = c("nicely", "kk", "fr", "circle", "grid", "random"))
                                       ),
                                       column(4,
                                              checkboxInput("graph_from_csv", "Load Graph from CSV?", value = FALSE)
                                       ),
                                       column(4,
                                              conditionalPanel("input.graph_from_csv == true",
                                                               fileInput("graph_nodes", "Nodes CSV"),
                                                               fileInput("graph_edges", "Edges CSV")
                                              )
                                       )
                                     )
                                 )
                               ),
                               
                               # Visualização Principal (Mapa + Grafos)
                               fluidRow(
                                 tabBox(width = 12,
                                        tabPanel("Interactive Map", 
                                                 leafletOutput("map_plot", height = "600px"),
                                                 tags$hr(),
                                                 tags$h4("Chorotype Composition"),
                                                 dataTableOutput("g_sub_table")
                                        ),
                                        tabPanel("Static Map (ggplot)",
                                                 plotOutput("ggplot_map", height = "600px")
                                        ),
                                        tabPanel("Network Graph (Simple)",
                                                 plotOutput("graph_plot", height = "600px")
                                        ),
                                        tabPanel("Network Graph (Detailed)",
                                                 plotOutput("graph_plot2", height = "600px")
                                        )
                                 )
                               )
                             )
                     )
                   ) # tabItems
                 ) # dashboardBody
  ) # dashboardPage
) # shinyUI