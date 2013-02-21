# draw a series of tetrahaedrons
tetrahaedrons3d <- function( coords, radius= c( 1, 1, 1 ), col= "grey", ... ) {
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



cubes3d <- function( coords, radius= c( 1, 1, 1), col= "grey", ... ) {
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

# return the basic cone mesh
# scale is necessary because of the dependence on the aspect ratio
.getcone <- function( r, h, scale= NULL ) {

  n  <- length( .sin.t )
  xv <- r * .sin.t
  yv <- rep( 0, n )
  zv <- r * .cos.t

  if( missing( scale ) ) scale <- rep( 1, 3 )

  scale <- 1 / scale
  sx <- scale[1]
  sy <- scale[2]
  sz <- scale[3]

  tmp <- NULL
  for( i in 1:(n-1) ) {
    tmp <- rbind( tmp,
      c( 0, 0, 0 ),
      scale3d( c( xv[i],   yv[i],   zv[i]   ), sx, sy, sz ),
      scale3d( c( xv[i+1], yv[i+1], zv[i+1] ), sx, sy, sz ) )
  }
  for( i in 1:(n-1) ) {
    tmp <- rbind( tmp,
      c( 0, h, 0 ),
      scale3d( c( xv[i],   yv[i],   zv[i]   ), sx, sy, sz ),
      scale3d( c( xv[i+1], yv[i+1], zv[i+1] ), sx, sy, sz ) )
  }
  tmp
}

# vector cross product
.cross3 <- function(a,b) {
  c(a[2]*b[3]-a[3]*b[2], -a[1]*b[3]+a[3]*b[1], a[1]*b[2]-a[2]*b[1])
}

# draw a cone (e.g. tip of an arrow)
cone3d <- function( base, tip, radius= 10, col= "grey", scale= NULL, ... ) {
  start <- rep( 0, 3 )

  if( missing( scale ) ) scale= rep( 1, 0 ) 
  else scale <- max( scale ) / scale


  tip  <- as.vector( tip ) * scale
  base <- as.vector( base ) * scale

  v1 <- tip
  v2 <- c( 0, 100, 0 )
  o <- .cross3( v1, v2 )
  theta <- acos( sum( v1 * v2 ) / ( sqrt(sum( v1  *  v1 )) * sqrt(sum( v2  *  v2 )) ) )
  vl <- sqrt( sum( tip^2 ) )

  tmp <- .getcone( radius, vl )
  tmp <- translate3d( rotate3d( tmp, theta, o[1], o[2], o[3] ), base[1], base[2], base[3] )
  scale <- 1 / scale
  tmp <- t( apply( tmp, 1, function( x ) x * scale ) )
  triangles3d( tmp, col= col, ... )
}




arrows3d <- function( coords, headlength= 0.035, head= "end", scale= NULL, radius = NULL, ... ) {

  head <- match.arg( head, c( "start", "end", "both" ) )
  narr <- nrow( coords ) / 2 
  n    <- nrow( coords )

  starts <- coords[ seq( 1, n, by= 2 ), ]
  ends   <- coords[ seq( 2, n, by= 2 ), ]
  if( missing( radius ) ) radius <- ( max( coords ) - min( coords ) ) / 50

  segments3d( coords, ... )
  if( head == "end" | head == "both" ) {
    for( i in 1:narr ) {
      s <- starts[i,]
      e <- ends[i,]
      base <- e - ( e - s ) * headlength 
      tip  <- ( e - s ) * headlength
      cone3d( base, tip, radius= radius, scale= scale, ... )

    }
  }

}
