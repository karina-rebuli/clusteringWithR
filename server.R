# Packages
require( shiny )
require( shinyjs )
require( markdown )

require( grid )
require( gridExtra )
require( ggplot2 )
require( ggthemes )
require( dendextend )

require( tidyverse )
require( dplyr )
require( plyr )

require( factoextra )
require( cluster )
require( ClusterR )

# Local library
source("localLib.R")

# Server 

shinyServer( function( input, output, session ) {
  
  #
  # Tabs control
  #
  observeEvent( input$updateDataButton, {
    updateTabsetPanel( session, "bodyTabs",
                       selected = "ea"
    )
  })
  
  observeEvent( input$runButton, {
    updateTabsetPanel( session, "bodyTabs",
                       selected = "res"
    )
  })
  
  
  #
  # Reactives
  #
  dt <- eventReactive( input$updateDataButton, {
    
    # Generate data
    res <- dataGeneration( type = tolower( input$dataset )
                           , nDataSim = input$ndata
                           , outliersFreq = input$outliers
                           )
    
    # Return data
    res
    
  })
  
  clust <- eventReactive( input$runButton, {
    
    #
    # Settings
    #
    
    # UI rendering
    if( input$algorithm != "hclust" ){ 
      shinyjs::hide("div_dendrogram")
    }
    if( input$algorithm != "som" ){
      shinyjs::hide("div_umatrix")
    }
    
    # Read data
    dt <- dt()
    
    # Data wrangling for clustering
    dtClust <- dt %>%
      select( -group ) %>%
      scale
    
    # Dissimilarities matrix
    dissMat <- dist( dtClust )
    
    # Number of clusters
    k_min <- input$range_k[1]
    k_max <- input$range_k[2]
    nk <- input$nk
    bestk <- NA
    dend <- NA
    sm <- NA
    
    #
    # Run algorithm
    #
    if( input$algorithm == "hclust" ){
      
      # UI rendering
      shinyjs::show("div_dendrogram")
      shinyjs::removeClass("div_dendrogram", "hidden")
      
      # Clustering
      hc <- hclust( dissMat )
      
      # Best k
      if( input$nDiscover){
        cuts <- lapply( k_min:k_max, cutree, tree = hc )
        bestk <- nk <- seq( k_min, k_max )[ findBestk( clusters = cuts, dissMat) ]
      }
      
      # Dendogram
      dend <- hc %>% 
        as.dendrogram %>% 
        set("branches_lwd", .5) %>%
        set("leaves_pch", 15) %>% 
        set("leaves_cex", 1)
      
      if( nk <= 9 ){
        dend <- dend %>%
          set("branches_k_color", value = colors[ 1:nk ], k = nk )
      }else{
        dend <- dend %>%
          set("branches_k_color", value = colorRamps::matlab.like2( n = nk ), k = nk )
      }
      
      
      # Result
      clusters <- cutree( hc, k = nk )
      
    }
    
    if( input$algorithm == "kmeans" ){
      
      # Best k
      if( input$nDiscover ){
        clusters <- lapply(  k_min:k_max, kmeans, x = dtClust, nstart = 5, iter.max = 10 ) %>%
          map("cluster")
        bestk <- nk <- seq( k_min, k_max )[ findBestk( clusters = clusters, dissMat) ]
      }
      
      # Clustering
      km <- kmeans( dtClust, centers = nk, nstart = 5, iter.max = 10 )
      
      # Result
      clusters <- km$cluster
    }
    
    if( input$algorithm == "pam" ){
      
      # Best k
      if( input$nDiscover ){
        clusters <- lapply(  k_min:k_max, pam, x = dissMat ) %>%
          map("clustering")
        bestk <- nk <- seq( k_min, k_max )[ findBestk( clusters = clusters, dissMat) ]
      }
      
      # Clustering
      pm <- pam( dissMat, k = nk )
      
      # Result
      clusters <- pm$clustering
    }
    
    if( input$algorithm == "kpp" ){
      
      # Best k
      if( input$nDiscover ){
        clusters <- lapply(  k_min:k_max, KMeans_rcpp, data = dtClust, num_init = 5, max_iters = 10, initializer = "kmeans++" ) %>%
          map("clusters")
        bestk <- nk <- seq( k_min, k_max )[ findBestk( clusters = clusters, dissMat) ]
      }
      
      # Clustering
      kp <- KMeans_rcpp( data = dtClust, clusters = nk, num_init = 5, max_iters = 10, initializer = "kmeans++")
      
      # Result
      clusters <- kp$clusters
    }
    
    if( input$algorithm == "som" ){
      
      # UI rendering
      shinyjs::show("div_umatrix")
      shinyjs::removeClass("div_umatrix", "hidden")
      
      # SOM step
      nPointsGrid.x <- nPointsGrid.y <- floor( sqrt( nrow(dtClust) ) * .6 )
      
      # SOM Clustering
      sm <- kohonen::supersom( data = as.matrix( dtClust )
                               , grid = kohonen::somgrid( nPointsGrid.x, nPointsGrid.y, "hexagonal" )
                               , rlen = 1e3, keep.data = TRUE )
      
      # Best k
      if( input$nDiscover ){
        
        # Nodes kmeans clusters
        clustersSOM <- lapply(  k_min:k_max, kmeans, x = sm$codes[[1]], nstart = 5, iter.max = 10 ) %>%
          map("cluster")
        
        # Map grid clusters to data poinst
        clustersDATA <- lapply( clustersSOM, function( x ){
          dt_clusters <- left_join(
            data.frame( 
              sm$data[[1]]
              , unit.classif = sm$unit.classif
            )
            , data.frame(
              unit.classif = 1:( nPointsGrid.x * nPointsGrid.y )
              , cluster = x
            ), by = "unit.classif" )
          dt_clusters$cluster
        })
        
        bestk <- nk <- seq( k_min, k_max )[ findBestk( clusters = clustersDATA, dissMat) ]
        
      }
      
      # Kmeans
      smk <- kmeans( sm$codes[[1]], centers = nk, nstart = 5, iter.max = 10 )
      
      # Map grid clusters to data poinst
      dt_clusters <- left_join(
        data.frame( 
          sm$data[[1]]
          , unit.classif = sm$unit.classif
        )
        , data.frame(
          unit.classif = 1:( nPointsGrid.x * nPointsGrid.y )
          , cluster = smk$cluster
        ), by = "unit.classif" )
      
      # Result
      clusters <- dt_clusters$cluster
    }
    
    #
    # Validations statistics
    #
    sils <- silhouette( x = clusters, dist = dissMat )
    print(class(dend))
    list(
      algorithm = input$algorithm
      , clusters = clusters
      , purity = NMF::purity( table( clusters, dt$group ) )
      , silhouetteObj = sils
      , silhouetteAvg = mean( sils[, "sil_width"] )
      , bestk = bestk
      , dend = dend
      , sm = sm
    )
    
  })
  
  #
  # Renderings
  #
  
  #
  # EA
  #
  output$plot <- renderPlot({
    ggEa( dt() )
  })

  output$datsaset_name <- renderText({
    paste0( "Dataset ", isolate( input$dataset ), " with ", nrow( dt() ), " observations" )
  })

  output$hopkinsStats <- renderText({
    dt_pca <- prcomp( dt()[, 1:2] )
    hopkins <- clustertend::hopkins( dt_pca$x, n = 9 )
    paste0( "Hopkins statistic: ", numFormat( hopkins ) )
  })

  output$cor <- renderText({
    rho <- cor.test( dt()[, 1], dt()[, 2] )
    paste0( "r(", names( dt() )[1], ",", names( dt() )[2], "): "
            , numFormat( rho$est ), " (p-value ", numFormat( rho$p.value, d = 2 ), ")" )
  })
  
  output$n_outliers_x1 <- renderText({
    paste0( "X1 outliers: ", length( boxplot.stats( dt()[, 1] )$out ) )
  })
  
  output$n_outliers_x2 <- renderText({
    paste0( "X2 outliers: ", length( boxplot.stats( dt()[, 2] )$out ) )
  })

  output$summaries_title <- renderText({
    call <- input$dataset
    "Summaries"
  })

  output$summaries <- renderTable(
    hover = TRUE
    , spacing = "xs"
    , bordered = TRUE
    , rownames = TRUE
    , expr = {

      # Eval
      x1.sum <- data.frame(
        Min = min( dt()$x1 )
        , `First Quartil` = quantile( dt()$x1, .25 )
        , Median = quantile( dt()$x1, .5 )
        , `Third Quartil` = quantile( dt()$x1, .75 )
        , Max = max( dt()$x1 )
        , Mean = mean( dt()$x1 )
        , SD = sd( dt()$x1 ) )
      x2.sum <- data.frame(
        Min = min( dt()$x2 )
        , `First Quartil` = quantile( dt()$x2, .25 )
        , Median = quantile( dt()$x2, .5 )
        , `Third Quartil` = quantile( dt()$x2, .75 )
        , Max = max( dt()$x2 )
        , Mean = mean( dt()$x2 )
        , SD = sd( dt()$x2 ) )

      x.sum <- rbind( x1.sum, x2.sum )
      row.names( x.sum ) <- c("x1", "x2")

      # Return
      x.sum

    })
  
  #
  # Analysis
  #
  output$purity <- renderText({ paste0( "Purity: ", numFormat( clust()$purity ) ) })
  output$silhouette <- renderText({ paste0( "Average silhouette: ", numFormat( clust()$silhouetteAvg ) ) })
  output$contingency <- renderTable(
    hover = TRUE
    , render = "xs"
    , bordered = TRUE
    , rownames = TRUE
    , expr = {
      tb <- table( clust()$clusters, dt()$group ) %>%
        as.data.frame.matrix
    })
  output$silhouette_plots <- renderPlot( width = 600, height = 300
                                         , expr = {
                                           factoextra::fviz_silhouette( clust()$silhouetteObj, show_labels = FALSE ) +
                                             scale_color_brewer( palette = "Set1" ) +
                                             scale_fill_brewer( palette = "Set1" ) +
                                             theme_few() +
                                             labs( x = "Observations" )
                                         })
  output$scatter_plots <- renderPlot({
    
    tt <- switch( clust()$algorithm
                  , "hclust" = "Hierarchical Clustering"
                  , "kmeans" = "K-means"
                  , "pam" = "PAM"
                  , "kpp" = "K-means++"
                  , "som" = "SOM and K-means" )
    
    subtt <- if( !is.na( clust()$bestk ) ){
      paste0("Using ", clust()$bestk, " clusters defined between "
             , input$range_k[1], " and ", input$range_k[2], " by maximum silhouette")
    }else{
      paste0("Using ", input$nk, " clusters defined by user")
    }
    
    ggClusScatter( dataset = dt(), clusters = clust()$clusters
                   , x = "x1", y = "x2", dataGroup = "group"
                   , title = paste0( tt," results" ), subtitle = subtt )
  })
  
  output$dendrogram <- renderPlot({
    
    d <- clust()$dend %>%
      as.ggdend
    
    ggplot( d, labels = FALSE ) +
      theme_few() +
      labs( title = "Dendogram clusters" ) +
      theme( title = element_text(size = 8), axis.text = element_text(size = 6) )
    
  })
  
  output$umatrix <- renderPlot({
    plot( clust()$sm, type = "dist", shape= "straight", main = "" )
  })
  
})
