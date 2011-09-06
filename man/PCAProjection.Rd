\name{PCAProjection}
\alias{PCAProjection}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Two-dimensional projection by Principal Component Analysis
%%  ~~function to do ... ~~
}
\description{
Two-dimensional projection by Principal Component Analysis
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PCAProjection(points3D)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{points3D}{an \eqn{N} x 3 matrix of the \eqn{x}, \eqn{y}, \eqn{z} coordinates of a polygonal link
%%     ~~Describe \code{points3D} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{points3D.rotated}{an N x 3 matrix containing the x, y, z coordinates of the 
  rotated structure. The first two columns represent the 2D projection of the structure.
  }
%%  ~Describe the value returned
%%  If it is a LIST, use
%Returns a list of two elements
 % \item{points3D}{an n x 3 matrix of the x, y, z coordinates of the reduced structure}
 % \item{ends}{a vector of positive integers (if a non empty \code{end} 
 % has been provided as an argument defining the separators of the reduced structure}
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
\code{\link{plotKnotDiagram}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{
if(require(bio3d)) {
	## Import a PDB file from the web
	protein <- fileImport("3MDZ")
	protein.proj <- PCAProjection(protein)

	##Diagram plot of both the original and rotated structures
	par(mfrow = c(1,2))
	plotKnotDiagram(protein, ends = c(), lwd = 1.5)
 	plotKnotDiagram(protein.proj, ends = c(), lwd = 1.5)
}
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
