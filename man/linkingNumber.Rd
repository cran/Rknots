\name{linkingNumber}
\alias{linkingNumber}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute the linking number of a polygonal link
%%  ~~function to do ... ~~
}
\description{Compute the linking number of a polygonal link
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
linkingNumber(points3D, ends, M = c())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{points3D}{an \eqn{N} x 3 matrix of the \eqn{x}, \eqn{y}, \eqn{z} coordinates of a polygonal link
%%     ~~Describe \code{points3D} here~~
}
  \item{ends}{a vector of positive integers defining the separators of the polygonal link
%%     ~~Describe \code{ends} here~~
}
  \item{M}{the intersection matrix of the polygonal link. If no matrix is provided, the function will
  		compute it (default)
%%     ~~Describe \code{M} here~~
}
}
\details{
The linking number is defined for a two-component oriented link as the sum of +1 crossings and  -1
crossing over all crossings between the two links divided by 2. For components \eqn{\alpha} and \eqn{\beta},
\deqn{
lk(\alpha, \beta) = \frac{1}{2} \sum_{c \in \alpha \cap \beta} \epsilon(c)
} 
where \eqn{\alpha \cap \beta} is the set of crossings of \eqn{\alpha} with \eqn{\beta}, 
and \eqn{\epsilon(c)} is the sign of the crossing.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{lk}{the linking number of the polygonal link}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
	Weisstein, Eric W. "Linking Number." From MathWorld--A Wolfram Web Resource. \url{http://mathworld.wolfram.com/LinkingNumber.html}

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
\code{\link{intersectionMatrix}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##Compute the linking number of the Hopf link
hopf <- matrix(c(0.000084, -1.667027, -0.000300,
    0.179585, 0.249735, 0.991541,
    -0.179478, 0.250355, -0.991211,
    0.000084, -1.667027, -0.000300,
    -0.991561, -0.250457, 0.179413,
    -0.000415, 1.666817, 0.000107,
    0.991784, -0.249435, -0.179541,
    -0.991561, -0.250457, 0.179413),
    ncol = 3, byrow = TRUE)

linkingNumber(points3D = hopf, ends = 4)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{invariants}
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
