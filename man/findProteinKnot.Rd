\name{findProteinKnot}
\alias{findProteinKnot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Global wrapper function for finding knots in proteins via the HOMFLY polynomial
%%  ~~function to do ... ~~
}
\description{
Gap finding and chain split, centroid closure and
Alexander-Briggs reduction are performed before computing the HOMFLY polynomial
%%polynomial invariant.
%%Both the HOMFLY polynomial and the Alexander polynomial can be computed, with the latter being
%%slightly faster. 
%%The HOMFLY polynomial  is computed 
via the \code{skeinIterator} function (with negative skein sign)
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
%%findProteinKnot(pdbID, join.gaps = FALSE, fast = FALSE)
findProteinKnot(pdbID, join.gaps = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  	\item{pdbID}{a single element character vector containing the name
  	 of the PDB file to be read, or the four letter PDB identifier for 
  	 online file access. For the latter, the \code{pdbID} is not case sensitive.}
	\item{join.gaps}{logical, if TRUE gaps are joined by a straight line and chains are not split.
	Default is FALSE.}
	 %%\item{fast}{logical, if TRUE the Alexander polynomial is computed.}
%%     ~~Describe \code{ends} here~~
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
return a data frame with 3 columns, PDB name (and part if \code{join.gaps} is FALSE), 
number of residues and %%either 
the HOMFLY polynomial %%or the Alexander polynomial 
(for each part).
%%  ~Describe the value returned
%%  If it is a LIST, use
%% ...
}
\references{
	Freyd P, Yetter D, Hoste J, Lickorish WBR, Millett K, et al.
	 (1985) A new polynomial invariant of knots and links. Bull Amer Math Soc (NS) 12: 239-246.
	
	Kauffman, L. Knots and Physics. Teaneck, NJ: World Scientific, p. 19, 1991.
	
	Comoglio F. and Rinaldi M. A Topological Framework for the Computation of the HOMFLY Polynomial and Its Application to Proteins (2011) 
	PLoS ONE 6(4): e18693, doi:10.1371/journal.pone.0018693 
	ArXiv:1104.3405
%% ~put references to the literature/web site here ~
}
\author{
Federico Comoglio
%%  ~~who you are~~
}
\note{If the protein has been split at position 1, the subsequent chains
are processed only.
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{HOMFLYpolynomial}},
\code{\link{msr}},
\code{\link{centroidClosure}}
\code{\link{findKnot}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## The Rds3p protein (2K0A) is a left-handed trefoil knotted protein
## This protein has no gaps
if(require(bio3d)) {
	findProteinKnot("2K0A", join.gaps = FALSE) 
}
%%, fast = FALSE)
%%findProteinKnot("2K0A", join.gaps = FALSE, fast = TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{invariants}
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line