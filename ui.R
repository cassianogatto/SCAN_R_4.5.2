



# Packages ----
# 1. Install pacman if not already installed
if (!require("pacman")) install.packages("pacman")

# 2. Load all packages (installs them automatically if missing)
pacman::p_load( shiny, dplyr, sf, igraph, tidygraph, DT, leaflet, ggplot2, ggraph, tidyr, lwgeom, shinydashboard )

# UI ----
shinyUI(
  
  dashboardPage( skin = 'black',
                 
                 # header ----
                 dashboardHeader(         # Set the width to match your sidebar width (230px) so it aligns perfectly # The Image: must be in 'www' folder. # We adjust margin-top to center it vertically and margin-right for spacing
                   
                   titleWidth = 210,        
                   
                   title = span(   tags$img(src = "SCAN_logo1.png", width = "80px", style = "margin-top: -5px; margin-right: 10px;"),
                                   
                                   "SCAN V_1" 
                   )
                 ),
                 
                 # menu sidebar ----
                 dashboardSidebar( width = '210px',
                                   
                                   sidebarMenu(
                                     menuItem("about SCAN", tabName = "about_SCAN", icon = icon("info-circle")),
                                     menuItem("Tutorial", tabName = "tutorial", icon = icon("book")), # <--- NEW ITEM HERE
                                     menuItem("Species Distribution Maps", tabName = "maps", icon = icon("map")),
                                     menuItem("Spatial Congruence", tabName = "Cs_tab", icon = icon("calculator")),
                                     menuItem("SCAN Analysis", tabName = "scan", icon = icon("project-diagram")),
                                     menuItem("SCAN Viewer", tabName = "SCAN_viewer", icon = icon("eye"))
                                     
                                   ) # menu items
                 ),
                 
                 dashboardBody(
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
                     
                     # About Scan ----
                     
                     tabItem("about_SCAN",
                             fluidPage(
                               # --- 1. HERO SECTION: LARGE LOGO ---
                               fluidRow(
                                 column(12, align = "center",
                                        # The image must be in the 'www' folder. Adjust width as needed.
                                        tags$img(src = "SCAN_logo1.png", width = "75%", style = "margin: 20px 0;")
                                 )
                               ),
                               
                               # --- 2. TITLE & LINK ---
                               fluidRow(
                                 column(12, align = "center",
                                        tags$h2("Spatial Congruence Analysis", style = "font-weight: bold;"),
                                        tags$h4("By cassianogatto@gmail.com"),
                                        tags$h4(icon("github"), tags$a(href="https://github.com/cassianogatto/SCAN_engine_app", "View on GitHub", target="_blank")),
                                        tags$hr(style = "border-top: 1px solid #ccc; width: 60%;") # A subtle separator line
                                 )
                               ),
                               
                               # --- 3. INTRODUCTION TEXT ---
                               # Using a centered column for better readability on wide screens
                               fluidRow(
                                 column(10, offset = 1,
                                        
                                        tags$div(style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px; border-left: 5px solid #3c8dbc;",
                                                 tags$h3(icon("users"), " What are Chorotypes?", style = "margin-top: 0; color: #3c8dbc;"),
                                                 tags$p(style = "font-size: 1.1em; line-height: 1.5; text-align: justify;",
                                                        "Chorotypes are unique combinations of species with spatial congruences (Cs) higher between themselves than to any species of other such groups.
                                   In SCAN, species groupings are relative to (and determined by) thresholds of spatial congruence (Ct)."
                                                 ),
                                                 tags$p(style = "font-size: 1.1em; line-height: 1.5; text-align: justify;",
                                                        "Each chorotype corresponds to a 'community' in network terminology. In the graph representation, species are vertices (nodes) and links (edges) are Cs values.
                                   The map depicts the actual spatial distribution of each component species of a chorotype."
                                                 ),
                                                 tags$p(style = "font-size: 1.1em; line-height: 1.5; text-align: justify;",
                                                        "Chorotypes may 'evolve', grouping more species as thresholds get lower, until any stopping criteria is reached. Some chorotypes ensemble at high Ct values (cohesive distribution), others only at low Ct's."
                                                 ),
                                                 tags$p(style = "font-style: italic; margin-top: 15px;",
                                                        "The description, decision, interpretation and discussion about patterns vs. biogeography is the role of the biogeographer. Please refer to Gatto & Cohn-Haft 2021 for a detailed analysis of these conceptual implications - PlosOne",
                                                        tags$a(href="https://doi.org/10.1371/journal.pone.0245818", "https://doi.org/10.1371/journal.pone.0245818", target="_blank")
                                                 )
                                        )
                                 )
                               ),
                               
                               tags$br(),
                               
                               # --- 4. ABSTRACT BOX ---
                               fluidRow(
                                 column(10, offset = 1,
                                        box(width = 12, title = "Abstract (Gatto & Cohn-Haft 2021)", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                                            tags$p(style = "text-align: justify;",
                                                   "Species with congruent geographical distributions, potentially caused by common historical and ecological spatial processes, constitute biogeographical units called chorotypes. Nevertheless, the degree of spatial range congruence characterizing these groups of species is rarely used as an explicit parameter. Methods conceived for the identification of patterns of shared ranges often suffer from scale bias associated with the use of grids, or the incapacity to describe the full complexity of patterns, from core areas of high spatial congruence, to long gradients of range distributions expanding from these core areas. Here, we propose a simple analytical method, Spatial Congruence Analysis (SCAN), which identifies chorotypes by mapping direct and indirect spatial relationships among species. Assessments are made under a referential value of congruence as an explicit numerical parameter. A one-layered network connects species (vertices) using pairwise spatial congruence estimates (edges). This network is then analyzed for each species, separately, by an algorithm which searches for spatial relationships to the reference species. The method was applied to two datasets: a simulated gradient of ranges and real distributions of birds. The simulated dataset showed that SCAN can describe gradients of distribution with a high level of detail. The bird dataset showed that only a small portion of range overlaps is biogeographically meaningful, and that there is a large variation in types of patterns that can be found with real distributions. Species analyzed separately may converge on similar or identical groups, may be nested in larger chorotypes, or may even generate overlapped patterns with no species in common. Chorotypes can vary from simple ones, composed by few highly congruent species, to complex, with numerous alternative component species and spatial configurations, which offer insights about possible processes driving these patterns in distinct degrees of spatial congruence. Metrics such as congruence, depth, richness, and ratio between common and total areas can be used to describe chorotypes in detail, allowing comparisons between patterns across regions and taxa."
                                            )
                                        )
                                 )
                               )
                             )
                     ),
                     
                     
                     
                     # Tutorial----
                     
                     tabItem(tabName = "tutorial",
                             fluidPage(
                               tags$h2("SCAN Engine Tutorial"),
                               tags$p("Follow this step-by-step guide to perform a Spatial Congruence Analysis."),
                               
                               # Step 1: Loading Maps
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
                                   ),
                                   
                                   tags$br(),
                                   tags$h4(icon("exclamation-triangle"), "Troubleshooting & Warnings"),
                                   tags$ul(
                                     tags$li(tags$strong("Invalid Geometries:"), " The status section will indicate if there are 'invalid species' (layers with corrupted geometries). The goal is to see ", tags$code("no invalid species"), "."),
                                     tags$li(tags$strong("Fixing Options:"), " You can attempt to fix geometries by checking the following boxes:",
                                             tags$ul(
                                               tags$li(tags$strong("Projection (CRS):"), " Transform the CRS of the entire shapefile. Check the box and enter a new EPSG number (refer to ", tags$a(href="https://epsg.io", "epsg.io", target="_blank"), ")."),
                                               tags$li(tags$strong("st_make_valid:"), " Apply the ", tags$code("sf::st_make_valid"), " function to attempt to fix individual species layers.")
                                             )
                                     ),
                                     tags$li(tags$strong("Performance Warning:"), " Please be aware that for large datasets, Shiny may take a long time to process both individual steps and the overall analysis.")
                                   )
                               ),
                               
                               # ... (Keep Step 1 Box as it is) ...
                               
                               # Step 2: Spatial Congruence
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
                                   ),
                                   
                                   tags$br(),
                                   tags$h4(icon("file-upload"), "Alternative: Upload Cs"),
                                   tags$p("If you have already calculated the Cs matrix elsewhere, check ", tags$strong("Upload a Cs csv file instead"), " and upload your table containing columns: ", tags$code("sp1, sp2, Cs"), ".")
                               ),
                               
                               # Step 3: Running SCAN
                               box(width = 12, title = "Step 3: Running SCAN Analysis", status = "success", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                   
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
                               
                               # Step 4: Visualization
                               box(width = 12, title = "Step 4: Visualizing Results (Viewer)", status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                   
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
                     
                     
                     # Species Distribution Maps ----
                     tabItem("maps",
                             
                             fluidPage(
                               
                               tags$h2("Map of Species' Distributions"),
                               #   OLD  htmltools::includeMarkdown('www/scan_eng_text1.Rmd')
                               box(width = 12,                                
                                   
                                   tags$div(
                                     # First paragraph: General instructions and derivation of steps
                                     tags$p("Upload the shapefile map containing all species’ distributions as unique layers with unique IDs and geometries. All SCAN steps are derived from these maps: Spatial similarities (Cs), Scanning algorithm, and Plotting results."),
                                     
                                     # Second paragraph: Data requirements (column names)
                                     tags$p("Maps must have, at least, one ID column named ‘sp’, and a ‘geometry’ column (these exact names). If not, chose among the current names (listed after loading the shape for the first time) and write the name of the ID column in the box before loading the the map again. Information about species and geographical projections are also displayed. Check all important details before taking the next steps."),
                                     
                                     # Third paragraph: Troubleshooting and performance warnings
                                     tags$p("Note: Individual species’ layers may be corrupted for many reasons (invalid species are displayed below the no invalid species is the expected info). Sometimes a projection transformation (CRS) of the whole shapefile may fix it; to do so, mark the checkbox (below) and choose another EPSG number (see https://epsg.io)[see https://epsg.io]. SCAN_eng can also apply sf::st_make_valid to try to fix individual species layers (check box). If these procedures are not enough to fix all species you may try to filter out these before loading the map. Be aware that, for large datasets, Shiny may take a long time to run accros each and the whole process as well.")
                                   )
                               ),
                               
                               box(width = 5,    
                                   
                                   tags$h3("Input Map"),
                                   
                                   fileInput( inputId = "filemap",  label = "Choose shape-files (.shp + .shx + .dbl + .prj).", 
                                              accept = c('.shp','.dbf','.sbn','.sbx','.shx',".prj"), width = '500px', multiple=TRUE),  
                                   
                                   column(width = 5,
                                          checkboxInput("fix_invalid_shapes", label = "Fix invalid species layers?", value = FALSE)),
                                   
                                   column(width = 5,
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
                               # check map ----
                               box( width = 7,
                                    
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
                     
                     # Spatial Congruence -----
                     
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
                                      
                                      column( width = 4, 
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
                     
                     # SCAN Analysis ----
                     tabItem("scan",
                             
                             fluidPage(
                               
                               fluidRow(
                                 
                                 tags$h2("Spatial Congruence Network Analysis Algorithm"),
                                 
                                 box( width = 12,
                                      
                                      tags$h4("After calculating or uploading a Cs table of pairwise spatial similarities, you may parameterize and run SCAN!"),
                                      
                                      fluidRow(
                                        
                                        column( width = 2, 
                                                numericInput(inputId = "resolution", label = "Resolution (interval between Ct)", value = 0.1) 
                                        ),
                                        column( width = 2, 
                                                numericInput(inputId = "threshold_max", label = "Max value of threshold Ct", value = 0.9, min = 0.2, max = 1) 
                                        ),
                                        column( width = 2, 
                                                numericInput(inputId = "threshold_min", label = "Min value of threshold Ct", value = 0.2, min = 0.05, max = 0.9) 
                                        ),
                                        column( width = 2, 
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
                                     
                                     DT::DTOutput('table_download_preview'),
                                     
                                     tags$h4('All Chorotypes!'),
                                     
                                     tableOutput("scan_chorotypes")
                                     
                                 )
                               )
                             )
                             
                     ),
                     
                     # SCAN Viewer ----
                     
                     # SCAN Viewer (Remodelado) ----
                     tabItem(tabName = "SCAN_viewer",
                             
                             fluidPage(
                               # Título opcional (pode remover se quiser ganhar espaço vertical)
                               # tags$h2("SCAN Viewer"), 
                               
                               fluidRow(
                                 
                                 # --- 1. PAINEL LATERAL DE CONTROLE (Largura 3/12) ---
                                 # Aqui ficam todos os inputs, organizados verticalmente para limpar a tela
                                 column(width = 3,
                                        
                                        # Box 1: Parâmetros Principais
                                        box(width = NULL, title = "Analysis Parameters", status = "primary", solidHeader = TRUE,
                                            
                                            numericInput("threshold", "Threshold (Ct):", value = 0.8, step = 0.05, min = 0, max = 1),
                                            
                                            tags$hr(), # Linha separadora
                                            
                                            # Renomeado de 'Grupo' para 'Chorotype' conforme sua demanda
                                            helpText("Filter Chorotypes:"),
                                            uiOutput("original_components") 
                                        ),
                                        
                                        # Box 2: Configurações Visuais
                                        box(width = NULL, title = "Visual Settings", status = "warning", solidHeader = TRUE, collapsible = TRUE,
                                            
                                            sliderInput("map_alpha", "Map Transparency:", min = 0, max = 1, value = 0.5),
                                            
                                            selectInput("palette", "Palette:", 
                                                        choices = c("Set1", "Set2", "Set3", "Paired", "Dark2", "Accent", "Spectral", "RdYlBu")),
                                            
                                            selectInput("layout", "Graph Layout:", 
                                                        choices = c("nicely", "kk", "fr", "circle", "grid", "random"))
                                        ),
                                        
                                        # Box 3: Dados Externos
                                        box(width = NULL, title = "External Data", status = "danger", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                            checkboxInput("graph_from_csv", "Load Graph from CSV?", value = FALSE),
                                            
                                            conditionalPanel("input.graph_from_csv == true",
                                                             fileInput("graph_nodes", "Nodes CSV"),
                                                             fileInput("graph_edges", "Edges CSV")
                                            )
                                        )
                                 ), 
                                 
                                 # --- 2. ÁREA PRINCIPAL DE VISUALIZAÇÃO (Largura 9/12) ---
                                 column(width = 9,
                                        
                                        # A. Mapa Interativo (Leaflet) - Ganha destaque total no topo
                                        fluidRow(
                                          box(width = 12, title = "Interactive Map (Leaflet)", status = "success", solidHeader = TRUE,
                                              leafletOutput("map_plot", height = "600px") # Altura aumentada para melhor exploração
                                          )
                                        ),
                                        
                                        # B. Gráficos Secundários (Lado a Lado)
                                        fluidRow(
                                          # Mapa Estático
                                          column(width = 6,
                                                 box(width = NULL, title = "Static Chorotype Map", status = "info", solidHeader = TRUE, collapsible = TRUE,
                                                     plotOutput("ggplot_map", height = "350px")
                                                 )
                                          ),
                                          
                                          # Grafo de Rede
                                          column(width = 6,
                                                 box(width = NULL, title = "Network Topology", status = "info", solidHeader = TRUE, collapsible = TRUE,
                                                     plotOutput("graph_plot", height = "350px")
                                                 )
                                          )
                                        ),
                                        
                                        # C. Info Box (Placeholder para o futuro clique)
                                        fluidRow(
                                          box(width = 12, title = "Selection Details", status = "primary",
                                              verbatimTextOutput("interaction_info"),
                                              helpText("Click on elements in the map to see details (Under Construction).")
                                          )
                                        )
                                 ) 
                               ) # Fim fluidRow
                             ) # Fim fluidPage
                     )
                     
                     # tabItem("SCAN_viewer",
                     #         
                     #         fluidPage(
                     #           tags$h2("SCAN Viewer"),
                     #           
                     #           # 1. CONTROLES
                     #           fluidRow(
                     #             box(width = 12, title = "Settings", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                     #                 fluidRow(
                     #                   column(4, 
                     #                          numericInput("threshold", "Threshold (Ct):", value = 0.8, step = 0.05, min = 0, max = 1)
                     #                   ),
                     #                   column(4,
                     #                          uiOutput("original_components") # Checkbox dinâmico
                     #                   ),
                     #                   column(4,
                     #                          sliderInput("map_alpha", "Map Transparency:", min = 0, max = 1, value = 0.5),
                     #                          selectInput("palette", "Palette:", choices = c("Set1", "Set2", "Set3", "Paired", "Dark2", "Accent", "Spectral", "RdYlBu"))
                     #                   )
                     #                 ),
                     #                 fluidRow(
                     #                   column(4,
                     #                          selectInput("layout", "Graph Layout:", choices = c("nicely", "kk", "fr", "circle", "grid", "random"))
                     #                   ),
                     #                   column(4,
                     #                          checkboxInput("graph_from_csv", "Load Graph from CSV?", value = FALSE)
                     #                   ),
                     #                   column(4,
                     #                          conditionalPanel("input.graph_from_csv == true",
                     #                                           fileInput("graph_nodes", "Nodes CSV"),
                     #                                           fileInput("graph_edges", "Edges CSV")
                     #                          )
                     #                   )
                     #                 )
                     #             )
                     #           ),
                     #           
                     #           # 2. LINHA SUPERIOR: Mapa Estático (Esq) + Rede (Dir)
                     #           fluidRow(
                     #             # Quadrante Superior Esquerdo: Mapa Estático
                     #             column(width = 6,
                     #                    box(width = NULL, title = "Static Map (ggplot)", status = "info", solidHeader = TRUE,
                     #                        plotOutput("ggplot_map", height = "450px") # Altura fixa ajuda no layout
                     #                    )
                     #             ),
                     #             
                     #             # Quadrante Superior Direito: Rede
                     #             column(width = 6,
                     #                    box(width = NULL, title = "Network Graph", status = "warning", solidHeader = TRUE,
                     #                        # Usando 'graph_plot' (Simples). Se preferir o detalhado, mude para 'graph_plot2'
                     #                        plotOutput("graph_plot", height = "450px") 
                     #                    )
                     #             )
                     #           ),
                     #           
                     #           # 3. LINHA INFERIOR: Mapa Interativo (Leaflet) ocupando tudo
                     #           fluidRow(
                     #             column(width = 12,
                     #                    box(width = NULL, title = "Interactive Map (Leaflet)", status = "success", solidHeader = TRUE,
                     #                        leafletOutput("map_plot", height = "600px") # Mais alto para exploração
                     #                    )
                     #             )
                     #           )
                     #         )
                     # )
                   ) # tabItems
                 ) # dashboardBody
  ) # dashboardPage
) # shinyUI