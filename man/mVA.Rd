\name{mVA}
\alias{mVA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute the multivariable Alexander polynomial of a polygonal link
%%  ~~function to do ... ~~
}
\description{
Computes the multivariable Alexander polynomial (mVA) of a polygonal link.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mVA(points3D, ends, normalized)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{points3D}{an \eqn{N} x 3 matrix of the \eqn{x}, \eqn{y}, \eqn{z} coordinates of a polygonal link
%%     ~~Describe \code{points3D} here~~
}
  \item{ends}{a vector of positive integers defining the separators of the polygonal link}
%%     ~~Describe \code{ends} here~~
  \item{normalized}{logical, if FALSE (default) the multivariable  non normalized mVA is returned, otherwise the normalized mVA}
}
\details{
The polynomial computation relies on rSymPy. 
Please notice that the first time sympy is invoked is expected to be much slower than subsequent ones.
%%  ~~ If necessary, more details than the description above ~~
}

\value{
the multivariable Alexander polynomial (as string)
%%  ~Describe the value returned
}

\references{
	Alexander J. W. (1928) Topological invariants of knots and links. Trans. Amer. Math. Soc. 30:  275-306.
	
	Conway J. H. (1970) An enumeration of knots and links, and some of their algebraic properties. Computational Problems in Abstract Algebra (Proc. Conf.,Oxford, 1967), Pergamon, Oxford: 329-358.

	Murakami J. (1993) A state model for the multivariable Alexander polynomial. Pacific J. Math. 157, no. 1: 109-135.

	Archibald J, (2008) The weight system of the multivariable Alexander polynomial.  Acta Math. Vietnamica. 33: 459-470.
 
	Archibald  J. (2010) The Multivariable Alexander Polynomial on Tangles. PhD Thesis,Department of Mathematics University of Toronto

	Torres G. (1953) On the Alexander polynomial  Ann. Math. 57:  57-89.

	Comoglio F. and Rinaldi M. (2011) A Topological Framework for the Computation of the HOMFLY Polynomial and Its Application to Proteins, PLoS ONE 6(4): e18693, doi:10.1371/journal.pone.0018693 ArXiv:1104.3405
%% ~put references to the literature/web site here ~
}

\author{
Maurizio Rinaldi
%%  ~~who you are~~
}

\note{
If you wish to make computations faster it is suggested that you first reduce the structure via msr.
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{msr}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\examples{
require(Rknots)
data(link.table)
n <- 6
names(link.table)[n]
points3D <- link.table[[n]]$points3D
ends <- link.table[[n]]$ends
mv <- mVA(points3D, ends, normalized = FALSE)
mv
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{invariants}
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
