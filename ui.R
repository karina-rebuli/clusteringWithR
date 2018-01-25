require( shiny )
require( shinydashboard )
require( shinyjs )
require( markdown )

datasets <- c("Random", "Spherical", "Nonspherical", "Irregular")

ui <- dashboardPage( 
  skin = "black", dashboardHeader( title = "Clustering with R" )
  
  #
  # BEGIN Sidebar ----
  #
  , dashboardSidebar(
   
   sidebarMenu(
     menuItem( 
       "Dataset", icon = icon("list-alt"), startExpanded = TRUE
       # Inputs ----
       , selectInput( "dataset", "Data groups pattern", choices = datasets, selectize = FALSE )
       , numericInput( "ndata", "Number of observations", min = 10, max = 1e3, value = 1e2, step = 10 )
       , radioButtons( "outliers", "Outliers frequence on data",c( "None"="n", "Small"="s", "Median"="m", "High"="h" ) )
       , actionButton( "updateDataButton", "Generate data", icon = icon("refresh"), class = "btn-sm" )
       , br()
       # ----
     )
     , menuItem(
       "Clustering settings", icon = icon("rocket")
       # Inputs ----
       , checkboxInput( "nDiscover", "Discover number of clusters", FALSE )
       , conditionalPanel(
         condition = "input.nDiscover == true"
         , sliderInput( "range_k", "Range for search:", min = 2, max = 30, value = c( 2, 10 ) ) )
       , conditionalPanel(
         condition = "input.nDiscover == false"
         , numericInput( "nk", "Cluster count", 3, min = 1, max = 20, width = 100 ) )
       , selectInput( "algorithm", "Clustering algorithms"
                      , choices = c( "Hierarchical clustering" = "hclust"
                                     , "K-means" = "kmeans"
                                     , "PAM" = "pam"
                                     , "K-means++" = "kpp"
                                     , "SOM" = "som" )
                      , multiple = FALSE, selectize = FALSE )
       , actionButton( "runButton", "Run clustering", icon = icon("play"), class = "btn-sm" )
       , br()
       # ----
      )
   )
  )
  # --- END Sidebar ---
  
  #
  # BEGIN Body ----
  #
  , dashboardBody(
    useShinyjs()
    , fluidRow(
      tags$div(
        style = "padding: 10px"
        , tabsetPanel(
          id = "bodyTabs", type = "tabs", selected = "info"
          , tabPanel(
            #
            # BEGIN TAB Exploratory Analysis ----
            #
            title = "Exploratory analysis"
            , value = "ea"
            , tags$div( 
              style = "padding: 10px"
              , fluidRow(
                box( title = textOutput("datsaset_name"), status = "warning"
                     , width = 12, align = "center"
                     , plotOutput("plot", width = "700px" )
                     )
                )
              , fluidRow(
                box( title = "Data statistics", width = 3, status = "warning"
                     , textOutput("hopkinsStats", inline = TRUE )
                     , br()
                     , textOutput("cor", inline = TRUE )
                     , br()
                     , textOutput("n_outliers_x1")
                     , textOutput("n_outliers_x2")
                     )
                , box( title = "Variables summaries", status = "warning"
                       , width = 9, align = "center"
                       , tableOutput("summaries")
                       )
                )
              )
            )
          # END TAB Exploratory Analysis ---
          
          #
          # BEGIN TAB Clustering results ----
          #
          , tabPanel(
            title = "Clustering results", value = "res"
            , tags$div(
              style = "padding: 10px"
              , fluidRow( 
                box( title = "Clusters on variables", status = "warning"
                     , width = 12, align = "center"
                     , plotOutput( "scatter_plots", width = "500px" )
                     )
                )
              , fluidRow(
                box( title = "Contingency table", status = "warning"
                     , width = 4, tableOutput("contingency")
                     , textOutput("purity")
                     )
                , box( title = "Sihlouette quality", status = "warning"
                       , width = 8
                       , plotOutput("silhouette_plots", width = "400px", height = "300px" )
                       )
                )
              , fluidRow( 
                tags$div(
                  id = "div_dendrogram", class = "hidden"
                  , box( title = "Dendrogram", status = "warning"
                         , width = 12, align = "center"
                         , plotOutput("dendrogram", width = "400px")
                         )
                  )
                )
              , fluidRow( 
                tags$div( id = "div_umatrix", class = "hidden"
                          , box( title = "U-Matrix", status = "warning"
                                 , width = 12, align = "center"
                                 , plotOutput( "umatrix", width = "400px" )
                                 )
                          )
                )
              )
            )
          # END TAB Clustering results ---
                               
          #
          # BEGIN TAB Info
          #
          , tabPanel(
            title = "Info", value = "info"
            , includeHTML("instructions.html")
            )
          )
        # END TAB Info ---
      )
    )
    
  )
  # --- END Body ---
  
)

