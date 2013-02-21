  obsolete_cone3d <- function( base=c(0,0,0),
                      tip=c(0,0,1),
                      rad=1,
                      n=30,
                      draw.base=TRUE,qmesh=FALSE,
                      trans = par3d("userMatrix"), ...) {
   ax <- tip-base
   if (missing(trans) && !rgl.cur()) trans <- diag(4)
   ### is there a better way?
   if (ax[1]!=0) {
     p1 <- c(-ax[2]/ax[1],1,0)
     p1 <- p1/sqrt(sum(p1^2))
     if (p1[1]!=0) {
       p2 <- c(-p1[2]/p1[1],1,0)
       p2[3] <- -sum(p2*ax)
       p2 <- p2/sqrt(sum(p2^2))
     } else {
       p2 <- c(0,0,1)
     }
   } else if (ax[2]!=0) {
     p1 <- c(0,-ax[3]/ax[2],1)
     p1 <- p1/sqrt(sum(p1^2))
     if (p1[1]!=0) {
       p2 <- c(0,-p1[3]/p1[2],1)
       p2[3] <- -sum(p2*ax)
       p2 <- p2/sqrt(sum(p2^2))
     } else {
       p2 <- c(1,0,0)
     }
   } else {
     p1 <- c(0,1,0); p2 <- c(1,0,0)
   }
   degvec <- seq(0,2*pi,length=n+1)[-1]
   ecoord2 <- function(theta) {
     base+rad*(cos(theta)*p1+sin(theta)*p2)
   }
   i <- rbind(1:n,c(2:n,1),rep(n+1,n))
   v <- cbind(sapply(degvec,ecoord2),tip)
   if (qmesh) 
     ## minor kluge for quads -- draw tip twice
     i <- rbind(i,rep(n+1,n))
   if (draw.base) {
     v <- cbind(v,base)
     i.x <- rbind(c(2:n,1),1:n,rep(n+2,n))
     if (qmesh)  ## add base twice
       i.x <-  rbind(i.x,rep(n+2,n))
     i <- cbind(i,i.x)
   }
   if (qmesh) v <- rbind(v,rep(1,ncol(v))) ## homogeneous
   if (!qmesh)
     triangles3d(v[1,i],v[2,i],v[3,i],...)
   else
     return(rotate3d(qmesh3d(v,i,material=...), matrix=trans))
 }      



# functions starting with the dot are, by my convention, considered internal
# for this file and the one specific function that is exported from it.
# This is not a formalism, merely a reminder for myself.

# for drawing circles in 3D, precalculate some values
.sin.t <- sin( seq( 0, 2 * pi, len= 10 ) )
.cos.t <- cos( seq( 0, 2 * pi, len= 10 ) )


# draw labels for each centroid
.group.labels <- function( coords, group, ... ) {
   centr.coords <- calc.centroids( coords, group )
   texts3d( centr.coords[,1], centr.coords[,2], centr.coords[,3], levels( group ), col= "black", cex= 2, ... )
}

.show.axe.titles <- function( r, show.scale, axes.color, components, axe.titles ) {
      if( missing( axe.titles ) | is.null( axe.titles ) ) {
        axe.titles = paste( "PC", components )
      }

      
      texts3d( 
        c( 1.1*r[ 2,1 ], 0, 0 ),
        c( 0, 1.1*r[ 2,2 ], 0 ),
        c( 0, 0, 1.1*r[ 2,3 ] ), col= "black", axe.titles ) 

    
    if( show.scale ) {
      axis3d('x',pos=c( NA, 0, 0 ), col= axes.color)
      axis3d('y',pos=c( 0, NA, 0 ), col= axes.color)
      axis3d('z',pos=c( 0, 0, NA ), col= axes.color)
    }
}

.show.plane <- function( r ) {
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

.show.axes <- function( axes.color, ranges ) {

      axes <- rbind( 
        c( ranges[1,1], 0, 0 ),
        c( ranges[2,1], 0, 0 ),
        c( 0, ranges[1,2], 0 ),
        c( 0, ranges[2,2], 0 ),
        c( 0, 0, ranges[1,3] ),
        c( 0, 0, ranges[2,3] )
        )
      segments3d( axes, col= axes.color )

      radius <- 10

      scale <- c( 1, 1, 1 )
      if( ! missing( ranges ) ) {
        scale <- ranges[2,]
        radius <- max( scale ) / 50
      }

      arrows3d( axes, radius= radius, scale= scale,  col= axes.color )

     #cone3d( base= c( r[2,1] * 0.95, 0, 0 ), tip= c( r[2,1] * 0.05, 0, 0 ), radius= radius, ranges= r[2,] )
     #cone3d( base= c( 0, r[2,2] * 0.95, 0 ), tip= c( 0, r[2,2] * 0.05, 0 ), radius= radius, scale= r[2,] )
     #cone3d( base= c( 0, 0, r[2,3] * 0.95 ), tip= c( 0, 0, r[2,3] * 0.05 ), radius= radius, scale= r[2,] )

# draw arrows
#     triangles3d( rbind( 
#       c( r[2,1], 0, 0 ),
#       c( r[2,1] * 0.95, r[1,2] * 0.02, 0  ),
#       c( r[2,1] * 0.95, r[2,2] * 0.02, 0  ),
#       c( r[2,1], 0, 0 ), 
#       c( r[2,1] * 0.95, 0, r[1,3] * 0.02 ),
#       c( r[2,1] * 0.95, 0, r[2,3] * 0.02 ),
#       c( 0, r[2,2], 0 ),
#       c( r[1,1] * 0.02, r[2,2] * 0.95, 0  ),
#       c( r[2,1] * 0.02, r[2,2] * 0.95, 0  ),
#
#       c( 0, r[2,2], 0 ),
#       c(0, r[2,2] * 0.95,  r[1,1] * 0.02  ),
#       c(0, r[2,2] * 0.95,  r[2,1] * 0.02  ),
#
#       c( 0, 0, r[2,3] ),
#       c( r[1,1] * 0.02, 0, r[2,3] * 0.95  ),
#       c( r[2,1] * 0.02, 0, r[2,3] * 0.95  ),
#       c( 0, 0, r[2,3] ),
#       c( 0, r[1,2] * 0.02, r[2,3] * 0.95 ),
#       c( 0, r[2,2] * 0.02, r[2,3] * 0.95 ) 
#     ), col= axes.color )


}

.centroids.3D <- function( coords, group, radius, group.shape, group.col, shape.functions ) {

  centr.coords <- calc.centroids( coords, group )

  rownames( centr.coords ) <- levels( group )

  for( l in levels( group ) ) {
    shape.functions[[ group.shape[l] ]]( centr.coords[l,,drop= F], col= group.col[l], radius= radius, alpha= 0.3 )
    #shape.functions[[ centr.shapes[l] ]]( centr.coords[l,], col= centr.cols[ l ], alpha= 0.3, radius= radius, shininess= 100 ) 
    n.l <- length( which( group == l ) )
    tmp <- coords[ group == l, ]

    tmp <- rbind( tmp, t( sapply( 1:nrow( tmp ), function( x ) centr.coords[ l, ] ) ) )
    tmp <- tmp[ as.vector( rbind( 1:n.l, (n.l + 1):(2*n.l) ) ), ]
    segments3d( tmp, col= group.col[ l ] )
  }

}

.draw.shadows <- function( pca.coords, xz.functions, shape, radius) {

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
}

.squarexz <- function( x, y, z, radius= c( 1, 1, 1 ), col= "grey", alpha= 0.5 ) {
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

.trianglexz <- function( x, y, z, radius= c( 1, 1, 1 ), col= "grey", alpha= 0.5 ) {
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

.circlexz <- function( x, y, z, radius= c( 1, 1, 1), col= "grey", alpha= 0.5 ) {
  
  r <- radius * 2 / 3 
  n <- length( .sin.t )
  xv <- x + r[1] * .sin.t
  yv <- rep( y, n )
  zv <- z + r[3] * .cos.t

  tmp <- NULL
  for( i in 1:(n-1) ) {
    tmp <- rbind( tmp, 
      c( x, y, z ),
      c( xv[i],   yv[i],   zv[i]   ),
      c( xv[i+1], yv[i+1], zv[i+1] ) )
  }

  triangles3d( tmp, col= col, alpha= alpha )
}

.myspheres3d <- function( coords, radius= c( 1, 1, 1 ), ... ) {
    spheres3d( coords, radius= mean( radius ), ... )
}

.biplot.3D <- function( biplot.coords, biplot.vars, ranges= NULL ) {

  nvar <- length( biplot.vars )

  coords <- matrix( 0, ncol= 3, nrow= nvar * 2 )
  tips <- biplot.coords[ biplot.vars, ]
  coords[ seq( 2, nvar * 2, by= 2 ), ] <- tips

  mx <- max( abs( coords ) )

  coords <- coords / mx 
  scale <- c( 1, 1, 1 )
  if( ! missing( ranges ) ) {
    scale <- ranges[2,]
    coords <- t( apply( coords, 1, function( x ) x * scale ) )
  }

  arrows3d( coords, col= "red", scale= scale )

  if( length( rownames( biplot.coords ) ) > 0 ) 
    names <- rownames( biplot.coords )[biplot.vars]
  else
    names <- biplot.vars

  coords <- coords[ seq( 2, nvar * 2, by = 2 ), ]
  texts3d( coords[,1], coords[,2], coords[,3], names, col= "black" )





}


pca3d <- function( pca, components= 1:3, col= "grey", title= NULL, new= FALSE,
  axes.color = "grey",
  bg= "white",
  radius= NULL,
  group= NULL,
  shape= "sphere",
  palette= NULL,
  fancy= FALSE,
  biplot= FALSE,
  biplot.vars= 5,
  show.scale= FALSE,
  show.labels= FALSE,
  labels.col= "black",
  show.axes= TRUE,
  show.axe.titles= TRUE,
  axe.titles= NULL,
  show.plane= TRUE,
  show.shadows= FALSE,
  show.centroids= FALSE,
  show.group.labels= FALSE,
  show.shapes= TRUE

  ) {

  if( fancy ) {
    show.labels       <- TRUE
    show.shadows      <- TRUE
    show.centroids    <- TRUE
    show.group.labels <- TRUE
  }

  all.shapes<- c( "sphere", "tetrahaedron", "cube" )
  if( missing( palette ) ) palette <- palette()

  if( length( components ) != 3 ) 
    stop( "Error: 3 components required" ) 

  pca.coords <- get.pca.coords( pca )

  if( ncol( pca.coords ) < 3 ) 
    stop( sprintf( "Not enough components: found %d, need at least 3", ncol( pca.coords ) ) )

  if( ! missing( biplot ) ) {
    biplot.coords <- get.biplot.coords( pca, biplot )
    biplot.coords <- biplot.coords[ , components]
    biplot <- TRUE
    biplot.vars <- get.biplot.vars( biplot.coords, biplot.vars )
  }


  pca.coords <- pca.coords[ , components ]
  n.p <- nrow( pca.coords )
  main.ranges <- apply( pca.coords, 2, function( x ) c( -max( abs( range( x ) ) ), max( abs( range( x ) ) ) ) )

  ret <- NULL
  if( ! missing( group ) ) {
    group <- factor( group )
    g.n   <- as.numeric( group )
    
    # choose color automatically
    if( missing( col ) ) 
      col <- palette[ g.n %% length( palette ) + 1 ]

    # choose shape automatically
    if( missing( shape ) ) 
      shape <- all.shapes[ g.n %% 3 + 1 ]

    group.col <- col[ match( levels( group ), group ) ]
    names( group.col ) <- levels( group )
    group.shape <- shape[ match( levels( group ), group ) ]
    names( group.shape ) <- levels( group )

    ret <- print.legend( group, group.col, group.shape )
    
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
    if( length( col ) != n.p ) 
      stop( sprintf( "Incorrect length of the color vector (should be %d)", n.p ) )
  } else {
    col <- rep( col, n.p )
  }

  if( ! missing( show.centroids ) & missing( group ) ) 
      stop( "show.centroids can only be used if groups parameter is provided" )

  if( missing( radius ) ) radius <- 1
  radius <- rep( radius, 3 )

  for( i in 1:3 ) {
    r <- 2*max( abs( pca.coords[ , i ] ) )
    radius[i] <- radius[i] * r / 100
  }


  # the following three functions are for throwing shadows of objects on the
  # XZ plane
# initialize the rgl window, clear etc.
  pca_plot_init <- function( ) {
  
    # open new window if requested or not opened yet
    if( new | rgl.cur() == 0 ) {
      cat( "Creating new device\n" )
      rgl.open()
      #open3d( antialias= 3 )
      par3d( windowRect= 50 + c( 0, 0, 640, 640 ) )
      rgl.viewpoint( theta= 45, phi= 30, fov= 60, zoom= 1 )
      bg3d( bg )
      rgl.pop( "lights" )
      light3d( specular= "black" )
    }

    par3d( skipRedraw= TRUE ) 

    rgl.clear()

    #r <- apply( pca.coords, 2, function( x ) c( -max( abs( range( x ) ) ), max( abs( range( x ) ) ) ) )
    if( show.plane ) .show.plane( main.ranges )

    if( show.axes ) .show.axes( axes.color, main.ranges * 1.1 )

    if( show.axe.titles ) .show.axe.titles( main.ranges, show.scale, axes.color, components, axe.titles )
    
    if( show.scale ) {

      axis3d('x',pos=c( NA, 0, 0 ))
      axis3d('y',pos=c( 0, NA, 0 ))
      axis3d('z',pos=c( 0, 0, NA ))


    }
  }


# finalize the plot
  pca_plot_finish <- function( ) {
    aspect3d( c( 1, 1, 1 ) )
    #aspect3d( "iso" )
    par3d( skipRedraw= F )
    rgl.bringtotop()
  }


  shape.functions <- list( sphere= .myspheres3d, tetrahaedron= tetrahaedrons3d, cube= cubes3d )
  xz.functions <- list( sphere= .circlexz, tetrahaedron= .trianglexz, cube= .squarexz )

  plot.scale <- pca_plot_init() 

  # draw shadows ("lollipop" plot)
  if( show.shadows )   .draw.shadows( pca.coords, xz.functions, shape, radius )
  if( show.centroids ) .centroids.3D( pca.coords, group, 2 * radius, group.shape, group.col, shape.functions )
  if( show.group.labels ) .group.labels( pca.coords, group )

  if( show.shapes ) {
    for( s in unique( shape ) ) {
      for( c in unique( col ) ) {
        sel <- which( shape == s & col == c )
        if( length( sel ) > 0 ) {
          shape.functions[[s]]( pca.coords[ sel,, drop=F ], col= as.character( c ), alpha= 1, radius= radius, shininess= 100 ) 
        }
      }
    }
  }

  if( biplot ) 
    .biplot.3D( biplot.coords, biplot.vars, main.ranges )

  if( ! missing( show.labels ) ) {
    if( class( show.labels ) == "logical" & length( show.labels ) == 1 ) {
      show.labels <- rownames( pca.coords )
    }
    texts3d( pca.coords[,1], pca.coords[,2], pca.coords[,3], show.labels, col= labels.col, adj= c( 1, 1 ) )
  }

  pca_plot_finish() 
  return( invisible( ret ) )
}
