\name{centroidClosure}
\alias{centroidClosure}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Structure closure with the centroid method
%%  ~~function to do ... ~~
}
\description{
To be a mathematical knot, the protein backbone has to be closed.
Several methods have been proposed and this function implements
the centroid method (also called CENTER), where the structure ends are extended 
outside the sphere containing the model in the direction of the model centroid.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
centroidClosure(points3D, n.times = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{points3D}{an \eqn{N} x 3 matrix of the \eqn{x}, \eqn{y}, \eqn{z} coordinates of a polygonal link
%%     ~~Describe \code{points3D} here~~
}
  \item{n.times}{given the radius \eqn{r} of the sphere 
  that contains the model, the ends extension can be defined by multiplying 
  \eqn{r} by \code{n.times}.
%%     ~~Describe \code{ends} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
an \eqn{N+2} x 3 matrix of the \eqn{x}, \eqn{y}, \eqn{z} coordinates of a polygonal link. 
The first and the last rows contain the coordinates of the extended ends.
%%  ~Describe the value returned
%%  If it is a LIST, use
%% ...
}
\references{
Lua RC, Grosberg AY (2006) Statistics of Knots, Geometry of Conformations, and Evolution of Proteins. 
PLoS Comput Biol 2(5): e45. doi:10.1371/journal.pcbi.0020045
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## Import a PDB file from the web
if(require(bio3d)) {
	protein <- fileImport("3MDZ")
	closed <- centroidClosure(protein)
	
	##Plot the result
	require(rgl)
	plotKnot3D(closed, ends = c(), text = FALSE, showNC = FALSE, 
    	radius = 0.01, lwd = 5)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
