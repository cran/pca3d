# functions starting with the dot are, by my convention, considered internal
# for this file and the one specific function that is exported from it.
# This is not a formalism, merely a reminder for myself.

.text.frames <- function( coords, text, col= "black", bg= "#66666633" ) {

  sw <- strwidth( text ) * 1.5
  sh <- strheight( text ) * 1.7

  rect( 
        coords[,1] - sw / 2, 
        coords[,2] - sh  / 2,
        coords[,1] + sw / 2, 
        coords[,2] + sh  / 2,
        col= bg,
        border= F
        
        )
  text( coords[,1], coords[,2], text, col= col )

}

.biplot.2D <- function( biplot.coords, biplot.vars ) {

  r <- c( -1.1, 1.1 ) * max( abs( range( biplot.coords[,1:2] ) ) )
  par( usr= rep( r, 2 ) )
  biplot.coords <- biplot.coords[ biplot.vars, ]

  nb <- nrow( biplot.coords )

  arrows( rep( 0, nb ), rep( 0, nb ), biplot.coords[,1], biplot.coords[,2], col= "red", lwd= 2, angle= 20, length= 0.1 )
  labels <- rownames( biplot.coords )

  .text.frames( biplot.coords, labels )

}

.group.labels.2D <- function( coords, group, col= "black", ... ) {
   centr.coords <- calc.centroids( coords, group )
   .text.frames( centr.coords, levels( group ), col= col )
}

.centroids.2D <- function( coords, group, group.shape, group.col, radius, col ) {
  centr.coords <- calc.centroids( coords, group )
  points( centr.coords[,1], centr.coords[,2], col= group.col, pch= group.shape, cex = 2 * radius )
  segments( coords[,1], coords[,2], centr.coords[ group, 1], centr.coords[ group, 2 ], col= col )
}




pca2d <-
function( pca, components= 1:2, col= "grey", title= NULL, new= FALSE,
  axes.color = "black",
  bg= "white",
  radius= NULL,
  group= NULL,
  shape= NULL,
  palette= NULL,
  fancy= FALSE,
  biplot=FALSE,
  biplot.vars= 5,
  show.scale=FALSE,
  show.labels= FALSE,
  labels.col= "black",
  show.axes= TRUE,
  show.axe.titles= TRUE,
  show.plane= TRUE,
  show.shadows= FALSE,
  show.centroids= FALSE,
  show.group.labels= FALSE,
  ...
  ) {

  if( fancy ) {
    show.labels       <- TRUE
    show.shadows      <- TRUE
    show.centroids    <- TRUE
    show.group.labels <- TRUE
  }

  range <- list( x= c( -1, 1 ), y= c( -1, 1 ) )

  if( missing( palette ) ) palette <- palette()

  if( length( components ) > 2 ) {
    cat( "Warning: too many components specified, using only first two" )
    components <- components[1:2]
  }
    
  if( length( components ) != 2 ) {
    stop( "Error: 2 components required" ) 
  }

  pca.coords <- get.pca.coords( pca )

  if( ! missing( biplot ) ) {
    biplot.coords <- get.biplot.coords( pca, biplot )
    biplot.coords <- biplot.coords[ , components]
    biplot <- TRUE
    biplot.vars <- get.biplot.vars( biplot.coords, biplot.vars )
  }

  if( ncol( pca.coords ) < 2 ) 
    stop( sprintf( "Not enough components: found %d, need at least 2", ncol( pca.coords ) ) )

  pca.coords <- pca.coords[ , components ]
  n.p <- nrow( pca.coords )

  if( ! missing( group ) ) {
    group <- factor( group )
    g.n   <- as.numeric( group )
    
    # choose color automatically
    if( missing( col ) ) 
      col <- palette[ g.n %% length( palette ) + 1 ]

    # choose shape automatically
    if( missing( shape ) )
      shape <- c( 15:20 )[ g.n %% 6 + 1 ]

    group.col            <- col[ match( levels( group ), group ) ]
    names( group.col )   <- levels( group )
    group.shape          <- shape[ match( levels( group ), group ) ]
    names( group.shape ) <- levels( group )

    print.legend( group, group.col, group.shape )
  }

  if( length( shape ) > 1 ) {
    if( length( shape ) != n.p ) 
      stop( sprintf( "Incorrect length of the shape vector (should be %d, is %d)", n.p, length( shape ) ) )
  } else {
    shape <- rep( shape, n.p ) 
  }

  #if( ! all( shape %in% all.shapes ) ) 
    #stop( sprintf( "Incorrect shapes (must be one of c(%s))", paste( all.shapes, collapse= ", " ) ) )

  if( length( col ) > 1 ) {
    if( length( col ) != n.p ) 
      stop( sprintf( "Incorrect length of the color vector (should be %d)", n.p ) )
  } else {
    col <- rep( col, n.p )
  }

  if( ! missing( show.centroids ) & missing( group ) ) 
      stop( "show.centroids can only be used if groups parameter is provided" )

  if( missing( radius ) ) radius <- 1


# initialize the rgl window, clear etc.
  pca_plot_init <- function( ) {
  
    # open new window if requested or not opened yet
    if( new ) {
      cat( "Creating new device\n" )
      #open3d( antialias= 3 )
    }

    if( biplot ) {
      range$x <- c( -1, 1 ) * max( abs( range( pca.coords[,1] ) ) )
      range$y <- c( -1, 1 ) * max( abs( range( pca.coords[,2] ) ) )
    } else {
      range$x <- range( pca.coords[,1] )
      range$y <- range( pca.coords[,2] )
    }

    plot( NULL, type= "n", 
      xlim= range$x,
      ylim= range$y,
      xlab= paste( "PC", components[1] ),
      ylab= paste( "PC", components[2] )
       ) 

    usr <- par( "usr" )
    rect( usr[1], usr[3], usr[2], usr[4], col= bg )

    if( show.plane ) {
      abline( h= 0, col= "grey" )
      abline( v= 0, col= "grey" )
    }

  }

  pca_plot_init() 

  # draw shadows ("lollipop" plot)
  if( show.shadows ) 
    segments( pca.coords[,1], pca.coords[,2], pca.coords[,1], 0, col= "grey" )

  points( pca.coords[,1], pca.coords[,2], col= col, pch= shape, cex= radius, ... )

  if( show.centroids )    .centroids.2D( pca.coords, group, group.shape, group.col, 2 * radius, col )
  if( show.group.labels ) .group.labels.2D( pca.coords, group, col= group.col )

  if( ! missing( show.labels ) ) {
    if( class( show.labels ) == "logical" & length( show.labels ) == 1 ) {
      show.labels <- rownames( pca.coords )
    }

    text( pca.coords[,1], pca.coords[,2], show.labels, col= labels.col )
  }

  if( biplot )            .biplot.2D( biplot.coords, biplot.vars )

  return( invisible( NULL ) )
}
