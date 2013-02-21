\name{pca3d-package}
\alias{pca3d}
\alias{pca2d}
\alias{pca3d-package}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Show a three- or two-dimensional plot of a prcomp object
}
\description{
  Show a three- two-dimensional plot of a prcomp object or a matrix, using
  different symbols and colors for groups of data
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pca3d(pca, components = 1:3, col = "grey", title = NULL, new = FALSE, 
  axes.color = "grey", bg = "white", radius = NULL, 
  group = NULL, shape = "sphere", palette = NULL, fancy= FALSE,
  biplot = FALSE, biplot.vars = 5, 
  show.scale = FALSE, 
  show.labels = FALSE, labels.col = "black", show.axes = TRUE, 
  show.axe.titles = TRUE, axe.titles= NULL, 
  show.plane = TRUE, show.shadows = FALSE, 
  show.centroids = FALSE, show.group.labels = FALSE, show.shapes= TRUE)

pca2d(pca, components = 1:2, col = "grey", title = NULL, new = FALSE, 
  axes.color = "black", bg = "white", radius = NULL, 
  group = NULL, shape = NULL, palette = NULL, fancy= FALSE,
  biplot = FALSE, biplot.vars = 5, 
  show.scale = FALSE, 
  show.labels = FALSE, labels.col = "black", show.axes = TRUE, 
  show.axe.titles = TRUE, axe.titles= NULL,
  show.plane = TRUE, show.shadows = FALSE, 
  show.centroids = FALSE, show.group.labels = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pca}{
    Either a prcomp object or a matrix with at least three columns
}
  \item{components}{
  Vector of length 3 (\code{pca3d}) or 2 (\code{pca2d}) containing the components to be shown
}
  \item{col}{
  Either a single value or a vector of length equal to number of rows,
  containing color definitions for the plot points to be shown
}
  \item{title}{
  Window title
}
  \item{new}{
  Use TRUE to open a new window
}
  \item{axes.color}{
  Axis color

  This option has no effect in pca2d.
}
  \item{bg}{
  Background color
}
  \item{palette}{
    Specifies the color palette when colors are automatically assigned to
    the groups. See Details.
  }
  \item{fancy}{
    set \option{show.labels}, \option{show.shadows},
    \option{show.centroids} and \option{show.group.labels} to TRUE.
  }
  \item{radius}{
  Scaling item for the size of points to be shown.

  In pca2d, this corresponds to the cex parameter.
}
  \item{biplot}{
    Specify whether to show a biplot (see section \sQuote{biplots} below)
  }
  \item{biplot.vars}{
    Specify which loading to show on the biplot (see section
    \sQuote{biplots} below)
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

  In pca2d, the parameter is passed directly on to the pch option of the
  points() function.
}

  \item{show.labels}{
  TRUE for showing labels (taken from the coordinate matrix or the prcomp
  object). Alternatively, a vector with labels to be shown next to the data
  points.
}
  \item{labels.col}{
  Single value or vector describing the colors of the labels.
}
  \item{show.scale}{
  TRUE for showing a numeric scale at the edges of the plot.

  This option has no effect in pca2d.
}
  \item{show.axes}{
  TRUE to show the axes.

  This option has no effect in pca2d.
}
  \item{show.axe.titles}{
  If TRUE, show axe titles (PC 1, PC 2 etc.)

  This option has no effect in pca2d.
}
  \item{axe.titles}{
  A vector with two (pca2d) or three (pca3d) values containing the axe
  titles (corresponds to xlab and ylab in regular plot). If missing, but
  show.axe.titles is TRUE, axe titles will be generated automatically.
}
  \item{show.plane}{
  If TRUE, show a grey horizontal plane at y = 0.

  This option has no effect in pca2d.
}
  \item{show.shadows}{
  If TRUE, show a "lollipop" representation of the points on the y = 0
  plane: a vertical line joining the data point with the plane and a
  shadow.

  In pca2d, for each sample at (x,y), a grey line is drawn from (x,y) to
  (x,0).
}
  \item{show.centroids}{
  If TRUE and the group variable is defined, show cluster centroids (using
  apropriate group symbols) and lines from each data point to the
  corresponding centroid.
}

  \item{show.group.labels}{
    Either TRUTH/FALSE or a vector equal to the number of unique values in
    the \code{group} parameter.  If set, labels for each of the defined
    group will be shown at the group's centroid. If the value of the
    parameter is TRUE, then the group names will be taken from the
    \code{group} parameter. Otherwise, the values from this parameter will
    be used.
  }
  \item{show.shapes}{
      A TRUTH/FALSE value indicating whether the different symbols (shapes)
      for the shown data points should be plotted (default TRUE).


  }
  \item{...}{

    For pca2d, any further argument will be passed on to the points()
    function.
  }
}
\details{
  pca3d shows a three dimensional representation of a PCA object or any
  other matrix. It uses the rgl package for rendering.

  pca2d is the 2D counterpart. It creates a regular, two-dimensional plot
  on the standard graphic device. However, it takes exactly the same
  options as pca3d, such that it is easy to create 2D variants of the 3D
  graph.

  Often, PCA visualisation requires using different symbols and colors for
  different groups of data. pca3d() and pca2d() aim at creating reasonable
  defaults, such that a simple call with two parameters -- the pca object
  and the vector with group assignments of the samples -- is sufficient for
  a basic diagnosis.

  Biplots

}
\section{Biplots}{
  If option \option{biplot} is TRUE, a biplot showing both the PCA results
  (samples) and variables is shown. This corresponds to the
  \code{\link{biplot}} function which works for the \code{\link{prcomp}}
  class objects. However, a biplot showing all variable loadings will be
  unreadable if the data is highly dimensional (for example, gene
  expression data). Therefore, the option \option{biplot.vars} specifies
  which variables are shown on the biplot.

  If \option{biplot.vars} is a vector of length larger than one, it will be
  interpreted as a direct selection of the variables to be shown; for
  example, for a \code{\link{prcomp}} object \var{pca}, the variable selection will
  happen through \code{pca$rotation[biplot.vars,]}.

  If \option{biplot.vars} is a single number, then for each of the
  components shown, a number of variables equal to \option{biplot.vars}
  with the highest absolute loadings will be shown on the biplot.

}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
The functions pca3d and pca2d both return a data frame containing the annotations of the
symbols used. This data frame contains the following columns:
\item{group}{Group names}
\item{colors}{associated colors}
\item{shapes}{associated shapes}
This comes in handy if you want to add a legend to your plot.
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
  pca2d( pca, group= metabo[,1] )

  # a bit more fancy:
  # black background, white axes,
  # centroids
  pca3d( pca, group= metabo[,1], 
    fancy= TRUE, bg= "black", 
    axes.color= "white", new= TRUE )


}            
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~PCA }
\keyword{ ~princomp }
\keyword{ ~biplot }
\keyword{ ~prcomp }% __ONLY ONE__ keyword per line
