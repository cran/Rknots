\name{msrMultiPlot}
\alias{msrMultiPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{3D plot of a polygonal link
%%  ~~function to do ... ~~
}
\description{3D plot of a polygonal link
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
msrMultiPlot(points3D, ends, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{points3D}{an \eqn{N} x 3 matrix of the \eqn{x}, \eqn{y}, \eqn{z} coordinates of a polygonal link
%%     ~~Describe \code{points3D} here~~
}
  \item{ends}{a vector of positive integers defining the separators of the polygonal link
%%     ~~Describe \code{ends} here~~
}
  \item{\dots}{other parameters for \code{lines3d} or \code{spheres3d}
%%     ~~Describe \code{\dots} here~~
}
}
\details{
This function is just a wrapper for \code{msrPlot}.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
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
Federico Comoglio
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{msrPlot}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##3D plot of the Hopf link

hopf <- matrix(c(0.000084, -1.667027, -0.000300,
    0.179585, 0.249735, 0.991541,
    -0.179478, 0.250355, -0.991211,
    0.000084, -1.667027, -0.000300,
    -0.991561, -0.250457, 0.179413,
    -0.000415, 1.666817, 0.000107,
    0.991784, -0.249435, -0.179541,
    -0.991561, -0.250457, 0.179413),
    ncol = 3, byrow = TRUE)
			
msrMultiPlot(hopf, 4, lwd = 5, radius = 0.05)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{graphics}
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line