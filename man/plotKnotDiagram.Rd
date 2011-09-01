\name{plotKnotDiagram}
\alias{plotKnotDiagram}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot a knot or link diagram
%%  ~~function to do ... ~~
}
\description{
Plot a knot or link diagram.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotKnotDiagram(points3D, ends, text = FALSE, verbose = TRUE, ...)
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
  \item{verbose}{logical, indicating whether information about the plot should be reported
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
Called for its effect. If \code{verbose}, then the following list is returned.
	\item{colors}{a vector of length \code{nrow(points3D)-1} containing the colors of each edge of the diagram}
	\item{sint}{a list of length \code{nrow(points3D)-1} where the \eqn{i}-th entry contains the parameter(s) \eqn{k} 
		of the negative intersections (intersection sign -1) of the edge \eqn{i}}
	\item{splot}{the same of \code{sint}, where the true value of the parameter has been substituted with the 
		parameter used to represent undercrossings in the diagram}
	\item{ir}{a vector of integers containing the number of the edges that have been plotted}
	\item{points3D}{an \eqn{N} x 3 matrix of the \eqn{x}, \eqn{y}, \eqn{z} coordinates of the polygonal link that has been plotted}
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

plotKnotDiagram(points3D, ends = c(), lwd = 1.5)

##Plot the diagram of a link
data(link.table, package = "Rknots")
points3D <- link.table[[100]]$points3D
ends <- link.table[[100]]$ends

points3D.reduced <- AlexanderBriggs(points3D, ends = ends)

#show the reduced link diagram
plotKnotDiagram(points3D.reduced$points3D, points3D.reduced$ends) 
%%
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{graphics}
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
