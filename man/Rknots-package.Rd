\name{Rknots-package}
\alias{Rknots-package}
\alias{Rknots}
\docType{package}
\title{
Rknots: knots detection with R
%%~~ package title ~~
}
\description{
The Rknots package provides functions for the topological analysis of 
either biological or synthetic knotted polymers, with a major focus on proteins.
%%~~ A concise (1-5 lines) description of the package ~~

}
\details{
\tabular{ll}{
Package: \tab Rknots\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2011-07-04\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}
%%~~ An overview of how to use the package, including the most important ~~
The Rknots package contains functions for the topological analysis of polymeric structures, 
either biological polymers or synthetic structures, with a major focus on proteins.

This release provides a Protein Data Bank (PDB) entries import function, 
two structure reduction algorithms (MSR, Minimal Structure Reduction and Alexander-Briggs reduction), 
polynomials (Alexander, Jones and HOMFLY polynomial via Conway's skein triple)
and linking number computation, graphical functions and a set of utilities.
Credits are due to the bio3d package on which the PDB import function is based, 
to the google code project SymPy that provides a Computer Algebra System for Python and to the Rsympy package. 

Since Rknots would serve as a general purpose framework to identify knots in 3D structures, being 
therefore not limited to proteins, we have tried to carefully describe also part of the low-level functions 
to ensure a possible integration within other frameworks.

Version 1.0 is the first stable version. An update that will include several new features and optimization 
is scheduled before the end of 2011.
 
For any further details or feedback please feel free to contact us. Finally, we encourage external contributions 
to the package.


License: GPL-2

LazyLoad: yes
%%~~ functions ~~
}
\author{
Federico Comoglio and Maurizio Rinaldi

Maintainer: Federico Comoglio \email{federico.comoglio@bsse.ethz.ch}
%%~~ The author and/or maintainer of the package ~~
}
\references{
Comoglio F. and Rinaldi M. A Topological Framework for the Computation of the HOMFLY Polynomial and Its Application to Proteins (2011) 
PLoS ONE 6(4): e18693, doi:10.1371/journal.pone.0018693 
ArXiv:1104.3405
%%~~ Literature or other references for background information ~~
}
%%~~ Optionally other standard keywords, one per line, from file KEYWORDS in ~~
%%~~ the R documentation directory ~~
\keyword{package}
\seealso{
%%~~ Optional links to other man pages, e.g. ~~
%%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
%%\examples{
%%rotationMatrix(pi/3)
%%~~ simple examples of the most important functions ~~
%%}