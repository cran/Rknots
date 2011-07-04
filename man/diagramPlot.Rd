\name{diagramPlot}
\alias{diagramPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot a knot or link diagram
%%  ~~function to do ... ~~
}
\description{
Plot a knot or link diagram, with under- and overcrossings highlight
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
diagramPlot(points3D, ends, text = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{points3D}{an \eqn{N} x 3 matrix of the \eqn{x}, \eqn{y}, \eqn{z} coordinates of a polygonal link
%%     ~~Describe \code{points3D} here~~
}
  \item{ends}{a vector of positive integers defining the separators of the polygonal link
%%     ~~Describe \code{ends} here~~
}
  \item{text}{logical, if TRUE the numbering of the points is shown 
%%     ~~Describe \code{n} here~~
}
	\item{\dots}{other graphical parameters for \code{plot}
%%     ~~Describe \code{n} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
	\item{colors}{a vector of length \code{nrow(points3D)-1} containing the colors of each edge of the diagram}
	\item{sint}{a list of length \code{nrow(points3D)-1} where the \eqn{i}-th entry contains the parameter(s) \eqn{k} 
		of the negative intersections (intersection sign -1) of the edge \eqn{i}}
	\item{splot}{the same of \code{sint}, where the true value of the parameter has been substituted with the 
		parameter used to represent undercrossings in the diagram}
	\item{ir}{a vector of integers containing the number of the edges that have been plotted}
	\item{points3D}{an \eqn{N} x 3 matrix of the \eqn{x}, \eqn{y}, \eqn{z} coordinates of the polygonal link that has been plotted}
%%  ~Describe the value returned
%%  If it is a LIST, use
%Returns a list of two elements
 % \item{points3D}{an n x 3 matrix of the x, y, z coordinates of the reduced structure}
 % \item{ends}{a vector of positive integers (if a non empty \code{end} 
 % has been provided as an argument defining the separators of the reduced structure}
%% ...
}
\references{
Weisstein, Eric W. "Knot diagram." From MathWorld--A Wolfram Web Resource, \url{http://mathworld.wolfram.com/KnotDiagram.html}
%%
%% ~put references to the literature/web site here ~
}
\author{
Maurizio Rinaldi
%%  ~~who you are~~
}
\note{
%%
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##Diagram plot of the figure-eight knot
data(Rolfsen.table, package = "Rknots")
points3D <- Rolfsen.table.ms$"4.1"

##Points projection (not required)
points3D <- PCAProjection(points3D)

diagramPlot(points3D, ends = c(), lwd = 1.5)
%%
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{graphics}
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
