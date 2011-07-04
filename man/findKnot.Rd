\name{findKnot}
\alias{findKnot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Global wrapper function for finding knots in 3D structures
%%  ~~function to do ... ~~
}
\description{
This function provides a user-friendly wrapper for finding knots in 3D structures.
The Alexander-Briggs reduction algorithm is applied before computing
%% a polynomial invariant. Both 
the HOMFLY polynomial
%% and the Alexander polynomial can be computed, with the latter being
%%slightly faster. The HOMFLY polynomial  is computed 
via the \code{skeinIterator} function (with negative skein sign).
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
%%findKnot(points3D, fast = FALSE)
findKnot(points3D)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{points3D}{an \eqn{N} x 3 matrix of the \eqn{x}, \eqn{y}, \eqn{z} coordinates of a polygonal link
%%     ~~Describe \code{points3D} here~~
}
%%  \item{fast}{logical, if TRUE the Alexander polynomial is computed.}
%%     ~~Describe \code{ends} here~~
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%Either the HOMFLY polynomial or the Alexander polynomial
the HOMFLY polynomial
%%  ~Describe the value returned
%%  If it is a LIST, use
%% ...
}
\references{
	Freyd P, Yetter D, Hoste J, Lickorish WBR, Millett K, et al.
	 (1985) A new polynomial invariant of knots and links. Bull Amer Math Soc (NS) 12: 239-246.
	
	Kauffman, L. Knots and Physics. Teaneck, NJ: World Scientific, p. 19, 1991.
	
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
\code{\link{HOMFLYpolynomial}},
%%\code{\link{mVA}},
\code{\link{msr}},
\code{\link{centroidClosure}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## Import a PDB file from the file system
if(require(bio3d)) {
	rds3p <- fileImport(system.file("extdata/2k0a.pdb", package="Rknots"))

	## The Rds3p protein (2K0A) is a left-handed trefoil knotted protein
	findKnot(rds3p)
}
%%findKnot(rds3p, fast = FALSE)
%%## The Alexander polynomial is faster to compute, but the trefoil handedness is not determined.
%%findKnot(rds3p, fast = TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{invariants}
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line