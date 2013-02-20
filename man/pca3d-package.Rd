\name{pca3d-package}
\alias{pca3d}
\alias{pca3d-package}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Show a three-dimensional plot of a prcomp object
}
\description{
  Show a three-dimensional plot of a prcomp object or a matrix, using
  different symbols and colors for groups of data
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pca3d(pca, components = 1:3, col = "grey", title = NULL, offset = 50, new = FALSE, 
  axes.color = "black", bg = "white", radius = NULL, 
  group = NULL, shape = "sphere", show.scale = FALSE, 
  show.labels = FALSE, labels.col = "black", show.axes = T, 
  show.axe.titles = T, show.plane = T, show.shades = F, show.centroids = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pca}{
    Either a prcomp object or a matrix with at least three columns
}
  \item{components}{
  Vector of length 3 containing the components to be shown
}
  \item{col}{
  Either a single value or a vector of length equal to number of rows,
  containing color definitions for the plot points to be shown
}
  \item{title}{
  Window title
}
  \item{offset}{
  offset
}
  \item{new}{
  Use TRUE to open a new window
}
  \item{axes.color}{
  Axis color
}
  \item{bg}{
  Background color
}
  \item{radius}{
  Scaling item for the size of points to be shown
}
  \item{group}{
  either NULL or a factor of length equal to number of rows. Factor levels
  can be used to automatically generate symbols and colors for the points
  shown
}
  \item{shape}{
  Either a single value or a character vector describing the shapes to be
  used when drawing data points. Allowed shapes are: sphere, tetrahaedron
  and cube, and may be abbreviated.
}
  \item{show.scale}{
  TRUE for showing a numeric scale at the edges of the plot.
}
  \item{show.labels}{
  TRUE for showing labels (taken from the coordinate matrix or the prcomp
  object). Alternatively, a vector with labels to be shown next to the data
  points.
}
  \item{labels.col}{
  Single value or vector describing the colors of the labels.
}
  \item{show.axes}{
  TRUE to show the axes.
}
  \item{show.axe.titles}{
  If TRUE, show axe titles (PC 1, PC 2 etc.)
}
  \item{show.plane}{
  If TRUE, show a grey horizontal plane at y = 0
}
  \item{show.shades}{
  If TRUE, show a "lollipop" representation of the points on the y = 0
  plane: a vertical line joining the data point with the plane and a
  shadow.
}
  \item{show.centroids}{
  If TRUE and the group variable is defined, show cluster centroids (using
  apropriate group symbols) and lines from each data point to the
  corresponding centroid.
}
}
\details{
  pca3d shows a three dimensional representation of a PCA object or any
  other matrix. It uses the rgl package for rendering.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
No value is returned.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
%%  ~~who you are~~
January Weiner <january.weiner@gmail.com>
}
%\note{
%%  ~~further notes~~

%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{

  data( metabo )
  pca <- prcomp( metabo[,-1], scale.= TRUE )

  pca3d( pca, group= metabo[,1] )

}            
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~PCA }
\keyword{ ~prcomp }% __ONLY ONE__ keyword per line
