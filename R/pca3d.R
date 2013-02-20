pca3d <-
function( pca, components= 1:3, col= "grey", title= NULL, offset= 50, new= FALSE,
  axes.color = "black",
  bg= "white",
  radius= NULL,
  group= NULL,
  shape= "sphere",

  show.scale=FALSE,
  show.labels= FALSE,
  labels.col= "black",
  show.axes= T,
  show.axe.titles= T,
  show.plane= T,
  show.shades= F,
  show.centroids= F


  ) {

  require( rgl )

  printf <- function( ... ) print( sprintf( ... ) ) 
  catf <- function( ... ) cat( sprintf( ... ) ) 
  all.shapes<- c( "sphere", "tetrahaedron", "cube" )

  if( length( components ) != 3 ) {
    stop( "Error: 3 components required" ) 
  }

  #components <- components[ c( 2, 1, 3 ) ]

  if( class( pca ) == "prcomp" ) pca.coords <- pca$x
  else                           pca.coords <- pca

  #pca.coords <- apply( pca.coords, 2, function( x ) ( x - mean( x ) ) / sd( x ) )
  if( ncol( pca.coords ) < 3 ) 
    stop( sprintf( "Not enough components: found %d, need at least 3", ncol( pca.coords ) ) )

  pca.coords <- pca.coords[ , components ]
  n.p <- nrow( pca.coords )

  if( ! missing( group ) ) {
    if( class( group ) != "factor" ) group <- factor( group )
    g.n <- as.numeric( group )
    
    # choose color automatically
    if( missing( col ) ) {
      lp <- length( palette() )
      col <- palette()[ g.n %% lp + 1 ]
    }

    # choose shape automatically
    if( missing( shape ) ) {
      shape <- all.shapes[ g.n %% 3 + 1 ]
    }

    group.col <- col[ match( levels( group ), group ) ]
    names( group.col ) <- levels( group )
    group.shape <- shape[ match( levels( group ), group ) ]
    names( group.shape ) <- levels( group )

    
    cat( "\nLegend:\n" )
    l.g <- max( nchar( c( levels( group ), unique( group.col ), unique( group.shape ) ) ) )
    fmt <- sprintf( "%% %ds", l.g )
    fmt <- sprintf( "%s: %s, %s\n", fmt, fmt, fmt )
    catf( "%s\n", paste( rep( "-", 3*l.g + 4 ), collapse= "" ) )
    catf( fmt, "group", "color", "shape" )
    catf( "%s\n", paste( rep( "-", 3*l.g + 4 ), collapse= "" ) )
    for( i in 1:length( levels( group ) ) ) {
      catf( fmt, levels( group )[i], group.col[i], group.shape[i])
    }
    cat( "\n" )
  }

  if( length( shape ) > 1 ) {
    if( length( shape ) != n.p ) 
      stop( sprintf( "Incorrect length of the shape vector (should be %d, is %d)", n.p, length( shape ) ) )
  } else {
    shape <- rep( shape, n.p ) 
  }

  if( ! all( shape %in% all.shapes ) ) 
    stop( sprintf( "Incorrect shapes (must be one of c(%s))", paste( all.shapes, collapse= ", " ) ) )

  if( length( col ) > 1 ) {
    if( length( col ) != n.p ) stop( sprintf( "Incorrect length of the color vector (should be %d)", n.p ) )
  } else {
    col <- rep( col, n.p )
  }

  if( ! missing( show.centroids ) ) {
    if( missing( group ) ) {
      stop( "show.centroids can only be used if groups parameter is provided" )
    }

  } 


  if( missing( radius ) ) radius <- 1
  radius <- rep( radius, 3 )

  for( i in 1:3 ) {
    r <- 2*max( abs( pca.coords[ , components[i] ] ) )
    radius[components[i]] <- radius[components[i]] * r / 100
  }

  n <- 10
  t <- seq( 0, 2 * pi, len= n )
  sin.t <- sin( t )
  cos.t <- cos( t )

  centroids <- function( coords, components, group, radius ) {

    segm <- NULL
    centr.coords <- cbind(
      tapply( coords[,components[1]], group, mean ),
      tapply( coords[,components[2]], group, mean ),
      tapply( coords[,components[3]], group, mean )
    )

    rownames( centr.coords ) <- levels( group )

    for( l in levels( group ) ) {
      shape.functions[[ group.shape[l] ]]( centr.coords[l,,drop= F], col= group.col[l], radius= radius, alpha= 0.3 )
      #shape.functions[[ centr.shapes[l] ]]( centr.coords[l,], col= centr.cols[ l ], alpha= 0.3, radius= radius, shininess= 100 ) 
      n.l <- length( which( group == l ) )
      tmp <- coords[ group == l, components ]

      tmp <- rbind( tmp, t( sapply( 1:nrow( tmp ), function( x ) centr.coords[ l, ] ) ) )
      tmp <- tmp[ as.vector( rbind( 1:n.l, (n.l + 1):(2*n.l) ) ), ]
      segments3d( tmp, col= group.col[ l ] )
    }

  }

  # the following three functions are for throwing shades of objects on the
  # XZ plane
  squarexz <- function( x, y, z, radius= c( 1, 1, 1 ), col= "grey", alpha= 0.5 ) {
    tmp <- NULL
    r <- 2*radius/3
    for( i in 1:length( x ) ) {
      tmp <- rbind( tmp,
        c( x[i] - r[1], y[i], z[i] - r[3] ), 
        c( x[i] - r[1], y[i], z[i] + r[3] ), 
        c( x[i] + r[1], y[i], z[i] - r[3] ), 
        c( x[i] + r[1], y[i], z[i] + r[3] ), 
        c( x[i] - r[1], y[i], z[i] + r[3] ), 
        c( x[i] + r[1], y[i], z[i] - r[3] ) )
    }
    triangles3d( tmp, col= col, alpha= alpha )

  }

  trianglexz <- function( x, y, z, radius= c( 1, 1, 1 ), col= "grey", alpha= 0.5 ) {
    tmp <- NULL
    for( i in 1:length( x ) ) {
      tmp <- rbind( tmp, 
        c( x[i]                            , y[i], z[i] + radius[3]     ),
        c( x[i] + sqrt( 3 ) * radius[1] / 2, y[i], z[i] - radius[3] / 2 ),
        c( x[i] - sqrt( 3 ) * radius[1] / 2, y[i], z[i] - radius[3] / 2 )
        )
    }
    triangles3d( tmp, col= col, alpha= alpha )

  }

  circlexz <- function( x, y, z, radius= c( 1, 1, 1), col= "grey", alpha= 0.5 ) {

    xv <- x + radius[1] * sin.t
    yv <- rep( y, n )
    zv <- z + radius[3] * cos.t

    tmp <- NULL
    for( i in 1:(n-1) ) {
      tmp <- rbind( tmp, 
        c( x, y, z ),
        c( xv[i],   yv[i],   zv[i]   ),
        c( xv[i+1], yv[i+1], zv[i+1] ) )
    }

    triangles3d( tmp, col= col, alpha= alpha )
  }

# initialize the rgl window, clear etc.
  pca_plot_init <- function( ) {
  
    # open new window if requested or not opened yet
    if( new | rgl.cur() == 0 ) {
      cat( "Creating new device\n" )
      rgl.open()
      #open3d( antialias= 3 )
      par3d( windowRect= offset + c( 0, 0, 640, 640 ) )
      rgl.viewpoint( theta= 45, phi= 30, fov= 60, zoom= 1 )
      bg3d( bg )
    }

    par3d( skipRedraw= T )


    rgl.clear()

    r <- apply( pca.coords, 2, function( x ) c( -max( abs( range( x ) ) ), max( abs( range( x ) ) ) ) )
    if( show.plane ) {
      quads3d( 
        rbind( 
          c( r[1,1],0,r[1,3] ),
          c( r[1,1],0,r[2,3] ),
          c( r[2,1],0,r[2,3] ),
          c( r[2,1],0,r[1,3] ) 
          ),
        col= "grey",
        alpha= 0.2
      ) 
    }

    r <- r * 1.1 

    if( show.axes ) {
      segments3d( rbind( 
        c( r[1,1], 0, 0 ),
        c( r[2,1], 0, 0 ),
        c( 0, r[1,2], 0 ),
        c( 0, r[2,2], 0 ),
        c( 0, 0, r[1,3] ),
        c( 0, 0, r[2,3] )
        ),
        col= "black" )

      triangles3d( rbind( 
        c( r[2,1], 0, 0 ),
        c( r[2,1] * 0.95, r[1,2] * 0.02, 0  ),
        c( r[2,1] * 0.95, r[2,2] * 0.02, 0  ),
        c( r[2,1], 0, 0 ), 
        c( r[2,1] * 0.95, 0, r[1,3] * 0.02 ),
        c( r[2,1] * 0.95, 0, r[2,3] * 0.02 ),
        c( 0, r[2,2], 0 ),
        c( r[1,1] * 0.02, r[2,2] * 0.95, 0  ),
        c( r[2,1] * 0.02, r[2,2] * 0.95, 0  ),

        c( 0, r[2,2], 0 ),
        c(0, r[2,2] * 0.95,  r[1,1] * 0.02  ),
        c(0, r[2,2] * 0.95,  r[2,1] * 0.02  ),

        c( 0, 0, r[2,3] ),
        c( r[1,1] * 0.02, 0, r[2,3] * 0.95  ),
        c( r[2,1] * 0.02, 0, r[2,3] * 0.95  ),
        c( 0, 0, r[2,3] ),
        c( 0, r[1,2] * 0.02, r[2,3] * 0.95 ),
        c( 0, r[2,2] * 0.02, r[2,3] * 0.95 ) 
      ), col= "black" )
    }

    if( show.axe.titles ) {
      titles = paste( "PC", components )
      
      texts3d( 
        c( 1.1*r[ 2,1 ], 0, 0 ),
        c( 0, 1.1*r[ 2,2 ], 0 ),
        c( 0, 0, 1.1*r[ 2,3 ] ), col= "black", titles ) 


    }
    
    if( show.scale ) {

      axis3d('x',pos=c( NA, 0, 0 ))
      axis3d('y',pos=c( 0, NA, 0 ))
      axis3d('z',pos=c( 0, 0, NA ))


    }



  }

  cube3d <- function( coords, radius= c( 1, 1, 1), col= "grey", ... ) {
    coords.n <- NULL
    r <- 2 * radius / 3 
    for( i in 1:nrow( coords ) ) {
      p <- coords[ i, ]

      for( j in 1:3 ) {
        other <- (1:3)[ -j ]
        oa <- other[1]
        ob <- other[2]
        for( t in c( -r[j], r[j] ) ) {
          for( q in c( -1, 1 ) ) {
            tmp <- matrix( 0, nrow= 3, ncol= 3 )
            tmp[ , oa ] <- c( q * r[oa], q * r[oa], -q * r[oa ] )
            tmp[ , ob ] <- c( q * r[ob], -q * r[ob], q * r[ob ] )
            tmp[ , j  ] <- rep( t, 3 )
            tmp <- t( apply( tmp, 1, function( x ) p + x ) )
            coords.n <- rbind( coords.n, tmp )
          }
        }
      }
    }       
    triangles3d( coords.n, col= col, ... )
  }         

  myspheres3d <- function( coords, radius= c( 1, 1, 1 ), col= "grey", ... ) {
    spheres3d( coords, radius= min( radius ), col= col, ... )
  }

# draw a series of tetrahaedrons
  tetra3d <- function( coords, radius= c( 1, 1, 1 ), col= "grey", ... ) {
    coords.n <- NULL
    
    for( r in 1:nrow( coords ) ) {
      p <- coords[ r, ] 

      # ABC
      coords.n <- rbind( coords.n, p + c( -radius[1], 0, -radius[3]/sqrt(2) ) ) # A
      coords.n <- rbind( coords.n, p + c(  radius[1], 0, -radius[3]/sqrt(2) ) ) # B
      coords.n <- rbind( coords.n, p + c(  0,  radius[2], radius[3]/sqrt(2) ) )  # C

      # ABD
      coords.n <- rbind( coords.n, p + c( -radius[1], 0, -radius[3]/sqrt(2) ) ) # A
      coords.n <- rbind( coords.n, p + c(  radius[1], 0, -radius[3]/sqrt(2) ) ) # B
      coords.n <- rbind( coords.n, p + c(  0, -radius[2], radius[3]/sqrt(2) ) )  # D

      # ACD
      coords.n <- rbind( coords.n, p + c( -radius[1], 0, -radius[3]/sqrt(2) ) ) # A
      coords.n <- rbind( coords.n, p + c(  0,  radius[2], radius[3]/sqrt(2) ) )  # C
      coords.n <- rbind( coords.n, p + c(  0, -radius[2], radius[3]/sqrt(2) ) )  # D

      # BCD
      coords.n <- rbind( coords.n, p + c(  radius[1], 0, -radius[3]/sqrt(2) ) ) # B
      coords.n <- rbind( coords.n, p + c(  0,  radius[2], radius[3]/sqrt(2) ) )  # C
      coords.n <- rbind( coords.n, p + c(  0, -radius[2], radius[3]/sqrt(2) ) )  # D
    }

    triangles3d( coords.n, col= col, ... )

  }

# finalize the plot
  pca_plot_finish <- function( ) {
    aspect3d( 1 )
    par3d( skipRedraw= F )
    rgl.bringtotop()
  }


  shape.functions <- list( sphere= myspheres3d, tetrahaedron= tetra3d, cube= cube3d )
  xz.functions <- list( sphere= circlexz, tetrahaedron= trianglexz, cube= squarexz )

  pca_plot_init() 
  #par3d( scale= c( 100,1.0162013,0.7018172 ) )

  # draw shades ("lollipop" plot)
  if( show.shades ) {

    cat( "Showing shades\n" )
    seg <- NULL
    for( i in 1:nrow( pca.coords ) ) {
      tmp <- pca.coords[ i, ]
      seg <- rbind( seg, tmp )
      tmp[ 2 ] <- 0
      seg <- rbind( seg, tmp )
      #squarexz( tmp[1], tmp[2] + 0.001, tmp[3], radius= radius, col= "grey" )
      xz.functions[[ shape[i] ]]( tmp[1], 0 + 0.001 * radius[2], tmp[3], radius= radius, col= "grey" )
      xz.functions[[ shape[i] ]]( tmp[1], 0 - 0.001 * radius[2], tmp[3], radius= radius, col= "grey" )
    }
    segments3d( seg, col= "grey", alpha= 0.5 )
    seg <- seg[ seq( 2, nrow( seg ), by= 2 ), ]
  }

  if( show.centroids ) centroids( pca.coords, components, group, 2 * radius )

  for( s in unique( shape ) ) {
    for( c in unique( col ) ) {
      sel <- which( shape == s & col == c )
      if( length( sel ) > 0 ) {
        # printf( "Drawing %s %ss", c, s )
        shape.functions[[s]]( pca.coords[ sel, ], col= as.character( c ), alpha= 1, radius= radius, shininess= 100 ) 
      }
    }
  }

  

  if( ! missing( show.labels ) ) {
    print( "showing labels" )
    if( class( show.labels ) == "logical" & length( show.labels ) == 1 ) {
      show.labels <- rownames( pca.coords )
    }

    texts3d( pca.coords[,1], pca.coords[,2], pca.coords[,3], show.labels, col= labels.col )
  }


  #axes3d( col= axes.color )
  #title3d( main= main, sub= sub, col = "black",
  #  xlab= sprintf( "PC%d", components[1] ), 
  #  ylab= sprintf( "PC%d", components[2] ), 
  #  zlab= sprintf( "PC%d", components[3] )  )

  pca_plot_finish() 
}
