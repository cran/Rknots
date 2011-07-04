\name{AlexanderBriggs}
\alias{AlexanderBriggs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Alexander-Briggs reduction of a polygonal knot
%%  ~~function to do ... ~~
}
\description{
Apply the Alexander-Briggs reduction to a polygonal knot. This method is based on 
the concept of elementary deformation, which consists in the replacement of two sides of a 
triangle with the third provided that the triangle is empty.


%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
AlexanderBriggs(points3D)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{points3D}{an \eqn{N} x 3 matrix of the \eqn{x}, \eqn{y}, \eqn{z} coordinates of a polygonal link}
%%  \item{pdbID}{The PDB file identifier. The extension '.pdb' must be included to 
%%  load the file from the file system. For the online access, the \code{pdbID} is not case sensitive.
%%     ~~Describe \code{pdbID} here~~
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{points3D}{an \eqn{N} x 3 matrix of the \eqn{x}, \eqn{y}, \eqn{z} coordinates of the reduced structure}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
##Reduce knot 8.1 in the Rolfsen table
data(Rolfsen.table, package = "Rknots")
points3D <- Rolfsen.table$"8.1"

points3D.reduced <- AlexanderBriggs(points3D)

##Plot the initial and reduced structure for comparison
msrPlot(points3D, text = FALSE, radius = 0.01, lwd = 2)
msrPlot(points3D.reduced, radius = 0.01, lwd = 2, col = "blue")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{structurereduction}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line