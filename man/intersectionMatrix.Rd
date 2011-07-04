\name{intersectionMatrix}
\alias{intersectionMatrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute the intersection matrix of a polygonal link
%%  ~~function to do ... ~~
}
\description{
intersectionMatrix
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
intersectionMatrix(points3D, ends = c())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{points3D}{an \eqn{N} x 3 matrix of the \eqn{x}, \eqn{y}, \eqn{z} coordinates of a three-dimensional structure
%%     ~~Describe \code{points3D} here~~
}
  \item{ends}{a vector of positive integers defining the separators of the polygonal link
}
}
\details{
The entries of the intersection matrix are defined as follows.
Given two sets of edges \eqn{A} and \eqn{B} we can compute the intersection matrix \eqn{I=I(A,B)} by setting 
\deqn{
(I(A,B))_{i,j} = 0} 
if \eqn{A_i} and \eqn{B_j} do not intersect transversally.
%%
\deqn{
(I(A,B))_{i,j} = +1} 
if \eqn{A_i} lays over \eqn{B_j}.
%%
\deqn{
(I(A,B))_{i,j} = -1} 
if \eqn{A_i} lays under \eqn{B_j}.
Finally, if \eqn{A=B} we get the skew symmetric square matrix \eqn{I(A)}

%%  ~~ If necessary, more details than the description above ~~
}
\value{
An \eqn{N}-1 x \eqn{N}-1 matrix
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
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
If \code{ends} is not null, the corresponding rows and columns of the intersection matrix are set to zero.
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{singleIntersection}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##Compute the intersection matrix of a random set of 20 points
points <- matrix(runif(60,-5,5), ncol = 3)

intersectionMatrix(points)

##Compute the intersection matrix of the trefoil knot
data(Rolfsen.table, package = "Rknots")
trefoil <- Rolfsen.table$"3.1"

intersectionMatrix(trefoil)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
