\name{skeinIterator}
\alias{skeinIterator}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Iterate the skein relation to build a skein tree of a polygonal link
%%  ~~function to do ... ~~
}
\description{
This function is a low-level function required for the computation of the skein tree
of a polygonal link. The tree is built by iterating the skein relation and geometrically
constructing the \eqn{L_0} and \eqn{L_{sw}} configuration (of a Conway skein triple).
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{skeinIterator(points3D, ends, M = c())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{points3D}{an \eqn{N} x 3 matrix of the \eqn{x}, \eqn{y}, \eqn{z} coordinates of a polygonal link
%%     ~~Describe \code{points3D} here~~
}
  \item{ends}{a vector of positive integers defining the separators of the polygonal link
%%     ~~Describe \code{ends} here~~
}
  \item{M}{the intersection matrix of the polygonal link
}
%%     ~~Describe \code{points3D} here~~
%%     ~~Describe \code{ends} here~~
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
	\item{leaves}{a list containing the binary indices of the tree leaves}
	\item{tree}{a list containing the skein tree. Each slot contains the slots
	\code{points3D}, \code{ends}, \code{signs} and \code{M}, which are respectively the 
	coordinates, separators of the current configuration, the skein signs of the
	ancestors (inner vertices) and the intersection matrix of the current configuration.}
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
\note{If you wish to compute the polynomial 
directly from the polygonal link points, the function \code{findKnot} 
is what you are looking for. The same applies for the function \code{findProteinKnot}
in the case you are looking for an automated processing of PDB files.
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{HOMFLYpolynomial}}, \code{\link{findKnot}},
\code{\link{findProteinKnot}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
if(require(bio3d)) {
	## Import a PDB file from the file system
	points3D <- fileImport(system.file("extdata/2k0a.pdb", package="Rknots"))

	## Reduce to minimal structure
	points3D <- msr(points3D, c())$points3D

	## Compute the skein tree
	tree.leaves <- skeinIterator(points3D, c())
	tree.leaves$leaves
	tree.leaves$tree
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{invariants}
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line