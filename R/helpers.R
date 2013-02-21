
printf <- function( ... ) print( sprintf( ... ) ) 
catf   <- function( ... ) cat( sprintf( ... ) ) 

calc.centroids <- function( coords, group ) {

  centr.coords <- apply( coords, 2, function( x ) tapply( x, group, mean ) )

  return( centr.coords )
}

get.pca.coords <- function( pca ) {

  if( class( pca ) == "prcomp" )   return( pca$x )
  if( class( pca ) == "princomp" ) return( pca$scores )
  if( class( pca ) != "matrix" ) stop( "pca must be either a matrix or a PCA object (prcomp, princomp etc.)" )
  return( pca )
}

get.biplot.coords <- function( pca, biplot ) {

  if( class( biplot ) == "matrix" ) return( biplot )
  if( class( pca ) == "prcomp" )    return( pca$rotation )
  if( class( pca ) == "princomp" )  return( pca$loadings )

  stop( "For a biplot, another matrix or a prcomp object as pca is needed")

}

get.biplot.vars <- function( biplot.coords, biplot.vars ) {

  if( length( biplot.vars ) > 1 ) 
    return( biplot.vars )

  if( biplot.vars > nrow( biplot.coords ) ) biplot.vars <- nrow( biplot.coords )

  res <- c()
  for( i in 1:ncol( biplot.coords ) ) {
    res <- c( res, head( order( abs( biplot.coords[,i]), decreasing= TRUE ), biplot.vars ) )
  }

  return( unique( res ) )
}

print.legend <- function( group, group.col, group.shape ) {

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

  ret <- data.frame( groups= levels( group ), colors= group.col, shapes= group.shape )

  return( invisible( ret ) )
}
