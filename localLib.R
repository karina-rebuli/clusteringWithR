# ---
# Global values
# 
colors <- RColorBrewer::brewer.pal( name = "Set1", n = 9 )
gradColors <- list( low=colors[2], mid = "#FFFFFF", high=colors[1] )
spColors <- seq( 0.5, .9, l=3 ) %>% gray
groupColors <- seq( 0.5, .9, l=5 ) %>% gray

# ----
# Data generation 
#
outliersInput <- function( dt, outliersFreq, nDim = 2 ){
  
  # Set proportion
  outliersFreq <- switch( outliersFreq
                          , n = 0
                          , s = 0.1
                          , m = 0.2
                          , h = 0.4 )
  
  # Generate outliers
  if( outliersFreq > 0 ){
    outliersInd <- sample( 1:nrow(dt), size = nrow(dt)*outliersFreq, replace = FALSE )
    for( i in 1:length( outliersInd ) ){
      out <- dt[ outliersInd[i], 1 ] * runif( 1, min = 1.5, max = 4.5 )
      dt[ outliersInd[i], 1 ] <- out
      out <- dt[ outliersInd[i], 2] * runif( 1, min = 1.5, max = 4.5 )
      dt[ outliersInd[i], 2 ] <- out
    }
  }
  
  # Return dataframe
  dt
  
}

dataGeneration <- function( type = "random"
                            , nDataSim = 1e2
                            , outliersFreq = "n" ){
  switch( type
                , random = dataGenerationRandom( ndata = nDataSim
                                                 , outliersFreq = outliersFreq )
                , spherical = dataGenerationSpher( ndata = nDataSim
                                                   , outliersFreq = outliersFreq )
                , nonspherical = dataGenerationAsymmetric( ndata = nDataSim
                                                           , outliersFreq = outliersFreq )
                , irregular = dataGenerationIrregular( ndata = nDataSim
                                                       , outliersFreq = outliersFreq )
  )
}

dataGenerationRandom <- function( ndata, outliersFreq = "n" ){
  
  # Data generation
  dt <- data.frame(
    x1 = rnorm( ndata )
    , x2 = rnorm( ndata )
    , group = factor("A")
  )
  
  # Outliers inputation and obj return
  outliersInput( dt, outliersFreq )
  
}

dataGenerationSpher <- function( ndata, ncenters = 5, outliersFreq = "n" ){
  
  # Clusters centers
  centersX <- c( rep( -2, ndata/ncenters)
                 , rep( -2, ndata/ncenters)
                 , rep( 0, ndata/ncenters)
                 , rep( 2, ndata/ncenters)
                 , rep( 2, ndata/ncenters) )
  
  centersY <- c( rep( -2, ndata/ncenters)
                 , rep( 2, ndata/ncenters)
                 , rep( 0, ndata/ncenters)
                 , rep(-2, ndata/ncenters)
                 , rep( 2, ndata/ncenters) )
  
  # Data generation
  dt <- data.frame( 
    x1 = rnorm( ndata, m=centersX, sd=.5 )
    , x2 = rnorm( ndata, m=centersY, sd=.5 )
    , group = rep( LETTERS[ 1:ncenters ], each = ndata/ncenters ) )
  
  # Outliers inputation and obj return
  outliersInput( dt, outliersFreq )
  
}

dataGenerationAsymmetric <- function( ndata, ncenters = 5, outliersFreq = "n" ){
  
  # Clusters centers
  centersX <- c( rep( -2, ndata/ncenters)
                 , rep( -1, ndata/ncenters)
                 , rep( 0, ndata/ncenters)
                 , rep( 1, ndata/ncenters)
                 , rep( 2, ndata/ncenters) )
  
  # Data generation
  dt <- data.frame( 
    x1 = rnorm( ndata, centersX, sd=.1 )
    , x2 = rnorm( ndata )
    , group = rep( LETTERS[ 1:ncenters ], ndata/ncenters ) )
  
  # Outliers inputation and obj return
  outliersInput( dt, outliersFreq )
  
}

dataGenerationIrregular <- function( ndata, outliersFreq = "n" ){
  
  # Elipsoid deterministic format
  elip <- function(x){ 1/3 * sqrt( 3^2 - x^2 ) }
  
  # Data generation
  dt <- data.frame(
    x1 = c( seq( -3, 1, l = ndata/2 )
            , seq( -1, 3, l = ndata/2 ) )
    , x2 = c( ( -.25*elip( seq( -3, 1, l = ndata/2 ) ) + 1 )
              , elip( seq( -1, 3, l = ndata/2 ) )+.35 )
    , group = rep( LETTERS[ 1:2 ], each = ndata/2 )
  )
  dt$x1 <- dt$x1 + rnorm( ndata/ndata, m=1:ndata, sd=.5 )
  dt$x2 <- dt$x2 + rnorm( ndata, m=0, sd=.1 )
  
  # Outliers inputation and obj return
  outliersInput( dt, outliersFreq )
  
}


# ----
# Algorithms
#
findBestk <- function( clusters, dissMat ){
  
  sils <- lapply( clusters, silhouette, dist = dissMat )
  sils_avg <- sapply( sils, function( x ){
    x <- x %>% unclass %>% data.frame
    mean( x[, "sil_width"] )
  })
  which.max( sils_avg )
  
}


# ----
# GGplot helpoers
#
ggStyle <- theme_few() +
  theme( title = element_text(size = 7)
         , axis.title = element_text(size = 6)
         , axis.text = element_text(size = 5)
         , legend.title = element_text(size = 7)
         , legend.key.size = unit( 6, "pt")
         , legend.text = element_text(size = 6)
  )

ggBaseHist <- ggplot() +
  ggStyle +
  labs( x = "Dissimilarities", y = "Count" )

ggEa <- function( dataset ){
  
  # Dissimilarities object
  dtDist <- data.frame( dataset %>% select( -group ) %>% scale %>% dist %>% unclass)
  names(dtDist) <- "x"
  
  grid.arrange(
    
    # Row 1
    ggplot( dataset ) +
      ggStyle +
      geom_point( aes(x=x1, y=x2), size = 2, alpha = .5 )
    , ggplot( dataset) +
      ggStyle +
      geom_density( aes(x = x1) ) +
      geom_rug( aes(x=x1), alpha = .75, size = .5 )
    , fviz_dist( dataset %>% select(-group) %>% scale %>% dist, show_labels = FALSE, gradient = gradColors ) +
      labs( title = "Dissimilarities matrix") +
      theme( title = element_text( size = 7) )
    # Row 2
    , ggplot( dataset) +
      ggStyle +
      geom_density( aes(x = x2) ) +
      geom_rug( aes(x=x2), alpha = .75, size = .5 )
    , ggplot( dataset ) +
      ggStyle +
      geom_point( aes(x=x2, y=x1), size = 2, alpha = .5 )
    , ggBaseHist +
      geom_histogram( data = dtDist
                      , aes( x = x )
                      , alpha = .7, fill = colors[2] )
    # Settings
    , ncol = 3
    
  )
  
}

ggClusScatter <- function( dataset, clusters, x, y, dataGroup, title, subtitle = "" ){
  
  # Prepare data
  df <- data.frame( dataset, cluster = clusters ) %>%
    arrange( cluster ) %>%
    mutate( cluster = factor(cluster) )
  
  # Evaluate hulls
  hulls <- df %>%
    dlply( "cluster", find_hull, x=x, y=y ) %>%
    invoke( .f = rbind ) %>%
    arrange( cluster ) %>%
    mutate( cluster = factor(cluster) )
  
  # Make ggplot
  ggplot( df ) +
    geom_point( aes( x = df[, x]
                     , y = df[, y]
                     , color = cluster
                     , shape = factor( df[, dataGroup] )
                     , alpha = factor( df[, dataGroup] ) ) ) +
    geom_polygon( data = hulls
                  , alpha = 0.15
                  , size = .5
                  , aes( x = hulls[, x]
                         , y = hulls[, y]
                         , color = cluster
                         , fill = cluster ) ) +
    theme_few() +
    scale_color_manual( values = colorRamps::matlab.like2( n = length( unique( df$cluster ) ) ), name = "Cluster" ) +
    scale_fill_manual( values = colorRamps::matlab.like2( n = length( unique( df$cluster ) ) ), name = "Cluster" ) +
    scale_alpha_manual( values = seq( .5, 1, l = length( unique( df[, dataGroup] ) ) )
                        , name = "Group" ) +
    scale_shape( name = "Group" ) +
    labs( x = x, y = y, title = title, subtitle = subtitle )
  
}


# ----
# General helpers
#
numFormat <- Vectorize(
  function(x, d=2){
    formatC( x, digits = d, format = "f", big.mark = ",", decimal.mark = ".")
  }
)

find_hull <- function( df, x, y ){
  df[ chull( df[, x], df[, y] ), ]
}
