\name{AlexanderBriggs}
\alias{AlexanderBriggs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Alexander-Briggs reduction of a polygonal knot or link
%%  ~~function to do ... ~~
}
\description{
Apply the Alexander-Briggs reduction to a polygonal knot or link. This method is based on 
the concept of elementary deformation, which consists in the replacement of two sides of a 
triangle with the third provided that the triangle is empty. Version 1.1 also provides a fast implementation for links.


%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
AlexanderBriggs(points3D, ends = c())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{points3D}{an \eqn{N} x 3 matrix of the \eqn{x}, \eqn{y}, \eqn{z} coordinates of a polygonal link}
\item{ends}{a vector of positive integers defining the separators of the polygonal link}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{points3D}{an \eqn{M} x 3 matrix of the \eqn{x}, \eqn{y}, \eqn{z} coordinates of the reduced structure}
  \item{ends}{a vector of positive integers (if a non empty \code{ends} 
  has been provided as an argument defining the separators of the reduced structure}
}
\references{
Reidemeister K (1926), Abh Math Sem Univ Hamburg 5: 24-32.

Alexander JW, Briggs GB (1926) On types of knotted curves. Ann of Math 28: 562-586.

%% ~put references to the literature/web site here ~
}
\author{
Federico Comoglio
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{msr}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##On a knot
#Reduce knot 8.1 in the Rolfsen table
data(Rolfsen.table, package = "Rknots")
points3D <- Rolfsen.table$"8.1"

points3D.reduced <- AlexanderBriggs(points3D, ends = c())

#Plot the initial and reduced structure for comparison
plotKnot3D(points3D, ends = c(), text = FALSE, radius = 0.01, lwd = 2)
plotKnot3D(points3D.reduced$points3D, ends = c(), radius = 0.5, lwd = 2)

##On a link
data(link.table, package = "Rknots")
points3D <- link.table[[100]]$points3D
ends <- link.table[[100]]$ends

points3D.reduced <- AlexanderBriggs(points3D, ends = ends)

#show the reduced link diagram
plotKnotDiagram(points3D.reduced$points3D, points3D.reduced$ends) 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{structurereduction}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line