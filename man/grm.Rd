\name{grm}
\alias{grm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generalized Reidemeister Move
%%  ~~function to do ... ~~
}
\description{
Perform a Generalized Reidemeister Move (GRM) on a link diagram. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
grm(points3D, b, ends, inputM)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{points3D}{an \eqn{N} x 3 matrix of the \eqn{x}, \eqn{y}, \eqn{z} coordinates of a polygonal link
%%     ~~Describe \code{points3D} here~~
}
  \item{b}{an integer, the first edge to be considered for the move
%%     ~~Describe \code{b} here~~
}
  \item{ends}{a vector of positive integers defining the separators of the polygonal link
%%     ~~Describe \code{ends} here~~
}
  \item{inputM}{the intersection matrix of \code{points3D}
%%     ~~Describe \code{inputM} here~~
}
}
\details{
This is a lower level function which performs a single Generalized Reidemeister Move. \code{grm} 
is called by \code{MSR} but can be used in a more general context. 
See references for a detailed explanation of the algorithm.

%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
Returns a list of three elements
  \item{points3D}{an \eqn{M} x 3 matrix of the \eqn{x}, \eqn{y}, \eqn{z} coordinates of the reduced polygonal link}
  \item{ends}{the updated vector of separators of the reduced polygonal link}
  \item{M}{the intersection matrix of the reduced polygonal link}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Comoglio F. and Rinaldi M. A Topological Framework for the Computation of the HOMFLY Polynomial and Its Application to Proteins (2011) 
PLoS ONE 6(4): e18693, doi:10.1371/journal.pone.0018693 
ArXiv:1104.3405
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
\code{\link{msr}}, \code{\link{intersectionMatrix}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(Rolfsen.table, package = "Rknots")
trefoil <- Rolfsen.table$"3.1"

##Apply a GRM, b = 1
trefoil.first.reduction <- 
		grm(points3D = trefoil, b = 1, ends = c(), inputM = intersectionMatrix(trefoil))

##Plot the input structure and the result of the reduction step (in blue)
msrPlot(trefoil, radius = 0.01, lwd = 0.5)
msrPlot(trefoil.first.reduction$points3D, text = FALSE, 
	radius = 0.01, lwd = 2, col = "blue")

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{structurereduction}
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line