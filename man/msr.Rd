\name{msr}
\alias{msr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Minimal Structure Reduction
%%  ~~function to do ... ~~
}
\description{
Reduce a polygonal link to its minimal structure by applying Generalized Reidemeister Moves.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
msr(points3D, ends = c(), n = 100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{points3D}{an \eqn{N} x 3 matrix of the \eqn{x}, \eqn{y}, \eqn{z} coordinates of a polygonal link
%%     ~~Describe \code{points3D} here~~
}
  \item{ends}{a vector of positive integers defining the separators of the polygonal link
%%     ~~Describe \code{ends} here~~
}
  \item{n}{the number of iterations
%%     ~~Describe \code{n} here~~
}
}
\details{A minimal structure for a polygonal link \eqn{L} is a nested sequence of subsets of \eqn{L}:
\deqn{L \supset L_1 \supset \ldots \supset L_N} that cannot be extended. 
Each inclusion corresponds to a Generalized Reidemeister Move.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
Returns a list of two elements
  \item{points3D}{an \eqn{M} x 3 matrix of the \eqn{x}, \eqn{y}, \eqn{z} coordinates of the reduced structure}
  \item{ends}{a vector of positive integers (if a non empty \code{end} 
  has been provided as an argument defining the separators of the reduced structure}
  \item{M}{the intersection matrix of the reduced structure}
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
The default number of iterations is 100. This value almost always allows to reach the minimal structure.
If whished, a partial reduction can be achieved with \code{n} between 2 and 5.
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## Load the structure of the trefoil knot (48 points)
data(Rolfsen.table, package = "Rknots")
trefoil <- Rolfsen.table$"3.1"

## Reduce the trefoil via MSR
trefoil.reduced <- msr(trefoil, c())

## 3D plot of both the trefoil and its reduced structure
require(rgl)
msrPlot(trefoil, text = FALSE, 
    radius = 0.01, lwd = 2.5, col = "blue")
msrPlot(trefoil.reduced$points3D, text = TRUE,
    radius = 0.01, lwd = 5, col = "red")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{structurereduction}
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
