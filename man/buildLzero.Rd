\name{buildLzero}
\alias{buildLzero}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Build an \eqn{L} zero configuration
%%  ~~function to do ... ~~
}
\description{
This is a low level function that construct an \eqn{L} zero configuration starting from a polygonal link,
 and the indices of the selected crossing.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
buildLzero(points3D, ends, rquad, e1, e2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{points3D}{an \eqn{N} x 3 matrix of the \eqn{x}, \eqn{y}, \eqn{z} coordinates of a polygonal link
%%     ~~Describe \code{points3D} here~~
}
  \item{ends}{a vector of positive integers defining the separators of the polygonal link
%%     ~~Describe \code{ends} here~~
}
  \item{rquad}{a 4 x 3  matrix containing the coordinates of the rotated quadrilateral
%%     ~~Describe \code{n} here~~
}
  \item{e1}{a vector of length 3 containing the \eqn{x}, \eqn{y}, \eqn{z} coordinates of the undercross edge
  %%     ~~Describe \code{n} here~~
}
  \item{e2}{a vector of length 3 containing the \eqn{x}, \eqn{y}, \eqn{z} coordinates of the overcross edge
  %%     ~~Describe \code{n} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
