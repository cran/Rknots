\name{singleIntersection}
\alias{singleIntersection}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find the intersection of two edges
%%  ~~function to do ... ~~
}
\description{
Given two edges in 3D, find their 2D intersection and computes the 
intersection parameters along with the intersection sign and the crossing point \eqn{Q}.

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
singleIntersection(pointsij, kind = "binary")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pointsij}{a 4 x 3 matrix containing the 3D coordinates of the two edges.
  The first edge is defined by rows 1 and 2.
%%     ~~Describe \code{pointsij} here~~
}
  \item{kind}{if \code{binary} determines if the edges intersect, 
  	if \code{sign} returns the intersection sign,
  	if \code{q} compute the intersection sign and the 3D coordinates of the crossing \eqn{Q}s,
  	if \code{k} compute the intersection sign and the intersection parameter \eqn{k_1},
  	if \code{ks} compute the intersection sign, the intersection parameters \eqn{k_1, k_2}
  	and the 3D coordinates of the crossing \eqn{Q}s,
  	if \code{lk} compute the intersection sign, the intersection parameters \eqn{k_1}
  	and the 2D (\eqn{Q}2D) and 3D \eqn{Q}s coordinates of the crossings.		
  	}	
%%     ~~Describe \code{kind} here~~
}

\details{
\code{pointsij} contains the endpoints \eqn{P_1, P_2, P_3, P_4} 
of the two edges \eqn{e_i=P_1P_2} and \eqn{e_j=P_3P_4}. We consider the lines:
\deqn{P_1 + k_1 (P_2 - P_1)}
\deqn{P_3 + k_2 (P_4 - P_3)}
and we look for their 2D intersection. If the solution (\eqn{\overline{k}_1}, 
\eqn{\overline{k}_2})  
satisfies
\deqn{0 < \overline{k}_1 < 1} and 
\deqn{0 < \overline{k}_2 < 1} 
the edges intersect and we call \eqn{Q}2D the intersection point. 
The lift of \eqn{Q}2D to \eqn{e_1} and \eqn{e_2} are the rows of the matrix \eqn{Q}s.
%%  ~~ If necessary, more details than the description above ~~
}

\value{
	\item{}{a binary variable, 1 if the edges intersect, 0 otherwise}
	\item{sign}{the intersection sign (+1 if the first edge lays over the second, -1 otherwise)}
	\item{Qs}{a 2 x 3 matrix containing the 3D coordinates of the crossing points. 
		The first row belongs to the first edge}
	\item{ks}{if \code{kind = ks} the intersection parameters, the parameter w.r.t the first edge otherwise}
	\item{Q2D}{a vector of length two of the 2D coordinates of the crossing}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Federico Comoglio and Maurizio Rinaldi
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{msr}}
\code{\link{intersectionMatrix}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##Consider the points 
#P1 = (0,0,0)
#P2 = (1,1,0)
#P3 = (1,0,1)
#P4 = (0,1,1)
pointsij <- matrix(c(0,0,0, 
    1,1,0,
    1,0,1,
    0,1,1), ncol = 3, byrow = TRUE)
singleIntersection(pointsij)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
