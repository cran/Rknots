\name{skeinSign}
\alias{skeinSign}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute the skein sign of a crossing
%%  ~~function to do ... ~~
}
\description{Compute the skein sign of a crossing. The skein sign is 1 if 
the configuration we are considering is an \eqn{L_+} and -1 if \eqn{L_-}
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
skeinSign(points3D, edge.indices) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{points3D}{an \eqn{N} x 3 matrix of the \eqn{x}, \eqn{y}, \eqn{z} coordinates of a polygonal link
%%     ~~Describe \code{points3D} here~~
}
  \item{edge.indices}{a vector of length two containing the indices of the crossing edges
  
%%     ~~Describe \code{ends} here~~
}
}
\details{
Notice that the function returns 0 if the two edges does not cross.
%%  ~~ If necessary, more details than the description above ~~
}
\value{The skein sign (+1 or -1)
%%\item{}{the linking number of the polygonal link}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
	Weisstein, Eric W. "Skein Relationship." From MathWorld--A Wolfram Web Resource. \url{http://mathworld.wolfram.com/SkeinRelationship.html}

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
\code{\link{linkingNumber}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##Consider the points
points.cross <- matrix(c(0,0,1,
    1,1,1,
    1,0,0,
    0,1,0), ncol = 3, byrow = TRUE)
##this generates an L+ configuration, that we can check
skeinSign(points.cross, c(1,3))

##by changing the quote of the first two points, one generates an L- configuration
points.cross[1 : 2, 3] <- -1 * points.cross[1 : 2, 3] 
skeinSign(points.cross, c(1,3))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
