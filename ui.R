# Packages ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, dplyr, sf, igraph, tidygraph, DT, leaflet, ggplot2, ggraph, tidyr, lwgeom, shinydashboard)

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
                
                # --- SIDEBAR ---
                dashboardSidebar(
                  width = '230px',
                  sidebarMenu(
                    menuItem("About SCAN", tabName = "about_SCAN", icon = icon("info-circle")),
                    menuItem("Tutorial", tabName = "tutorial", icon = icon("book")),
                    menuItem("Species Distribution Maps", tabName = "maps", icon = icon("map")),
                    menuItem("Spatial Congruence", tabName = "Cs_tab", icon = icon("calculator")),
                    menuItem("SCAN Analysis", tabName = "scan", icon = icon("project-diagram")),
                    menuItem("SCAN Viewer", tabName = "SCAN_viewer", icon = icon("eye"))
                  )
                ),
                
                # --- BODY ---
                dashboardBody(
                  # Custom CSS
                  tags$head(
                    tags$style(HTML("
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
                                                       "The description, decision, interpretation and discussion about patterns vs. biogeography is the role of the biogeographer. Please refer to Gatto & Cohn-Haft 2021 for a detailed analysis of these conceptual implications - PlosOne",
                                                       tags$a(href="https://doi.org/10.1371/journal.pone.0245818", "https://doi.org/10.1371/journal.pone.0245818", target="_blank")
                                                )
                                       )
                                )
                              ),
                              tags$br(),
                              fluidRow(
                                column(10, offset = 1,
                                       box(width = 12, title = "Abstract (Gatto & Cohn-Haft 2021)", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                           tags$p(style = "text-align: justify;", "Species with congruent geographical distributions, potentially caused by common historical and ecological spatial processes, constitute biogeographical units called chorotypes...")
                                       )
                                )
                              )
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
                                column(width = 6,
                                       box(width = NULL, title = "1. Calculate Index", status = "warning", solidHeader = TRUE,
                                           tags$p("Calculate the Hargrove Index (default) or a custom formula."),
                                           
                                           checkboxInput("use_alternative_index", "Use custom formula?", value = FALSE),
                                           conditionalPanel("input.use_alternative_index == true",
                                                            textInput("cs_similarity_index", "Formula:", value = '(area_overlap / area_sp1) * (area_overlap / area_sp2)')
                                           ),
                                           
                                           fluidRow(
                                             column(6, numericInput("filter_Cs", "Minimum Cs value:", value = 0.1, step = 0.05)),
                                             column(6, actionButton("calculate_Cs", "Apply Cs Index", class = "btn-warning align-btn", width = "100%", icon = icon("play")))
                                           )
                                       ),
                                       
                                       # Buffer Box
                                       box(width = NULL, title = "2. Optimization (Buffers)", status = "warning", collapsible = TRUE, collapsed = TRUE,
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
                                column(width = 6,
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
                                             
                                             tabPanel("Upload/Download",
                                                      tags$br(),
                                                      tags$strong("Upload existing Cs table:"),
                                                      checkboxInput("Cs_upload_csv", "Upload .csv instead of calculating"),
                                                      conditionalPanel("input.Cs_upload_csv == true",
                                                                       fileInput("Cs_table", "Select CSV", accept = c(".csv"))
                                                      ),
                                                      tags$hr(),
                                                      tags$strong("Download calculated table:"),
                                                      tags$br(),
                                                      downloadButton("download_Cs", "Download Cs.csv", class = "btn-default")
                                             )
                                           )
                                       ),
                                       
                                       box(width = NULL, title = "Graph Check", status = "warning",
                                           tags$h5("Nodes and Edges Preview:"),
                                           fluidRow(
                                             column(6, tableOutput("graph_nodes")),
                                             column(6, tableOutput("graph_edges"))
                                           )
                                       )
                                )
                              )
                            )
                    ),
                    
                    # 5. SCAN ANALYSIS TAB (DANGER - RED) ----
                    tabItem("scan",
                            fluidPage(
                              tags$h2("Spatial Congruence Network Analysis"),
                              
                              fluidRow(
                                column(width = 12,
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
                    
                    # 6. SCAN VIEWER (SUCCESS - GREEN) ----
                    tabItem("SCAN_viewer",
                            fluidPage(
                              fluidRow(
                                # Control Column
                                column(width = 2,
                                       tags$h3("SCAN Viewer"),
                                       box(width = NULL, title = "Parameters", status = "success", solidHeader = TRUE,
                                           numericInput("threshold", "Threshold (Ct):", value = 0.5, step = 0.05, min = 0, max = 1),
                                           tags$hr(),
                                           uiOutput("original_components") 
                                       ),
                                       box(width = NULL, title = "Visual Settings", status = "success", collapsible = TRUE,
                                           numericInput("map_alpha", "Transparency:", min = 0, max = 1, value = 0.5, step = 0.1),
                                           selectInput("palette", "Palette:", choices = c("Set1", "Set2", "Set3", "Paired", "Dark2", "RdYlBu")),
                                           selectInput("layout", "Layout:", choices = c("nicely", "kk", "fr", "circle", "grid"))
                                       ),
                                       box(width = NULL, title = "External", status = "success", collapsible = TRUE, collapsed = TRUE,
                                           checkboxInput("graph_from_csv", "Load from CSV?", value = FALSE),
                                           conditionalPanel("input.graph_from_csv == true",
                                                            fileInput("graph_nodes", "Nodes CSV"),
                                                            fileInput("graph_edges", "Edges CSV")
                                           )
                                       )
                                ),
                                
                                # Visualization Column
                                column(width = 10,
                                       fluidRow(
                                         column(6,
                                                box(width = NULL, title = "Static Map", status = "info", solidHeader = TRUE,
                                                    plotOutput("ggplot_map", height = "300px")
                                                )
                                         ),
                                         column(6,
                                                box(width = NULL, title = "Network Topology", status = "info", solidHeader = TRUE,
                                                    plotOutput("graph_plot", height = "300px")
                                                )
                                         )
                                       ),
                                       fluidRow(
                                         box(width = 12, title = "Interactive Map (Leaflet)", status = "success", solidHeader = TRUE,
                                             leafletOutput("map_plot", height = "500px")
                                         )
                                       )
                                )
                              )
                            )
                    )
                  ) # tabItems
                ) # dashboardBody
  ) # dashboardPage
) # shinyUI