# ui.R

# User Interface (UI)
ui <- dashboardPage(
  skin = "purple",
  
  dashboardHeader(title = "AnaMetabo™"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Network Visualization", tabName = "network_vis", icon = icon("project-diagram")),
      menuItem("Advanced Analysis", tabName = "advanced_analysis", icon = icon("chart-line"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # --- Home Tab ---
      tabItem(tabName = "home",
              h2("Welcome to AnaMetabo!"),
              p("AnaMetabo is a bioinformatics tool designed for the analysis and visualization of metabolic networks from SBML files."),
              
              h3("Features by Tab:"),
              
              # Features Overview by Tab
              h4("1. Network Visualization"),
              tags$ul(
                tags$li("Interactively explore networks"),
                tags$li("Customize the graph (add/remove nodes and edges)"),
                tags$li("Choose from multiple graph layout types")
              ),
              
              h4("2. Advanced Analysis"),
              tags$ul(
                tags$li("Centrality Analysis: Degree, Closeness, and Betweenness calculations"),
                tags$li("Automatic Clustering: Identify related groups"),
                tags$li("Shortest Path Search: Find shortest paths between nodes"),
                tags$li("Community Metrics and Network Resilience calculations")
              ),
              
              h4("Demo File:"),
              p("You can download a sample SBML file to test the available features."),
              downloadButton("download_demo_sbml", "Download SBML File"),
              
              tags$hr(),
              
              h3("About the Project"),
              p("This webserver was developed as part of a Software Engineering project for the Master's program in Bioinformatics (M2) at the University of Bordeaux."),
              p(
                tags$small("Please note that AnaMetabo™ is a fictitious brand name used for educational purposes only.")
              ),
              
              
              tags$footer(
                p("Created by Céline Hosteins, Franck Sanchez and Team."),
                style = "position: fixed; bottom: 0; width: 100%; text-align: center; 
                  background-color: #f8f9fa; padding: 10px; font-size: 12px; color: #555;"
              )
      ),
      
      # --- Network Visualization Tab ---
      tabItem(tabName = "network_vis",
              fluidPage(
                titlePanel("SBML Network Visualization"),
                
                sidebarLayout(
                  sidebarPanel(
                    fileInput("sbml_file", "Import an SBML File (.xml)", accept = c(".xml", ".sbml")),
                    actionButton("generate_graph", "Generate Graph"),
                    tags$hr(),
                    
                    selectInput("layout_choice", "Select a Layout:", 
                                choices = c(
                                  "Force Atlas 2 Based" = "forceAtlas2Based", 
                                  "Barnes Hut" = "barnesHut", 
                                  "Hierarchical" = "hierarchical", 
                                  "Circular" = "circular"
                                ),
                                selected = "forceAtlas2Based" 
                    ),
                    tags$hr(),
                    
                    uiOutput("sidebar_legend"),
                    width = 3
                  ),
                  
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Graph Visualization",
                               div(style = "position: relative;",
                                   downloadButton("download_sbml", label = NULL, 
                                                  icon = icon("download"), 
                                                  style = "position: absolute; top: 10px; right: 10px; z-index: 1000;"),
                                   div(style = "text-align: center; font-size: 16px; margin-top: 10px;", 
                                       textOutput("graph_summary")),
                                   visNetworkOutput("network", height = "580px")
                               )
                      ),
                      
                      tabPanel("Nodes and Edges",
                               h4("Nodes Table"),
                               DTOutput("nodes_table"),
                               h4("Edges Table"),
                               DTOutput("edges_table")
                      )
                    ),
                    width = 9
                  )
                ),
                
                tags$footer(
                  style = "background-color: #f8f9fa; padding: 15px; border-top: 1px solid #dee2e6; text-align: center;",
                  h4("Graph Modification"),
                  div(
                    actionButton("add_node", "Add Node", style = "margin-right: 10px;"),
                    actionButton("delete_node", "Delete Selected Node", style = "margin-right: 10px;"),
                    actionButton("add_edge_button", "Add Edge", style = "margin-right: 10px;"),
                    actionButton("delete_edge", "Delete Edge"),
                    style = "margin-top: 10px;"
                  )
                )
              )
      ),
      
      # --- Advanced Analysis Tab ---
      tabItem(tabName = "advanced_analysis",
              fluidPage(
                titlePanel("Advanced Network Analysis"),
                
                sidebarLayout(
                  sidebarPanel(
                    fileInput("sbml_file_advanced", "Import an SBML File (.xml)", accept = c(".xml", ".sbml")),
                    actionButton("load_graph", "Load Graph"),
                    tags$hr(),
                    
                    # Dropdown menus for functionalities
                    selectInput("menu", "Select Analysis Type:",
                                choices = list(
                                  "Graph Visualization" = "visualization",
                                  "Centrality Analysis" = "centrality",
                                  "Cluster Analysis" = "clusters",
                                  "Shortest Path" = "shortest_path",
                                  "Community Metrics" = "community_metrics",
                                  "Network Resilience" = "resilience"
                                )),
                    
                    conditionalPanel(
                      condition = "input.menu == 'centrality'",
                      selectInput("centrality_metric", "Select Centrality Metric:",
                                  choices = c("Degree" = "degree", "Closeness" = "closeness", "Betweenness" = "betweenness")),
                      actionButton("analyze_centrality", "Analyze Centrality")
                    ),
                    
                    conditionalPanel(
                      condition = "input.menu == 'shortest_path'",
                      selectInput("from_node", "Start Node:", choices = NULL),
                      selectInput("to_node", "End Node:", choices = NULL),
                      actionButton("find_shortest_path", "Find Shortest Path")
                    ),
                    
                    conditionalPanel(
                      condition = "input.menu == 'clusters'",
                      actionButton("analyze_clusters", "Identify Clusters")
                    ),
                    
                    conditionalPanel(
                      condition = "input.menu == 'community_metrics'",
                      actionButton("analyze_community_metrics", "Analyze Community Metrics")
                    ),
                    
                    conditionalPanel(
                      condition = "input.menu == 'resilience'",
                      actionButton("analyze_resilience", "Analyze Network Resilience")
                    ),
                    
                    tags$hr(),
                    textOutput("graph_summary_advanced")
                  ),
                  
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Graph", visNetworkOutput("advanced_network", height = "700px")),
                      tabPanel("Results", DTOutput("analysis_results"))
                    )
                  )
                )
              )
      )
    )
  )
)