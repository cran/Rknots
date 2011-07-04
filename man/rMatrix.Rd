\name{rMatrix}
\alias{rMatrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Rotation matrix along the \eqn{z} axis
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Compute the rotation matrix by an angle \eqn{\theta} along the \eqn{z} axis
}
\usage{
rMatrix(theta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{The rotation angle, in radians
%%     ~~Describe \code{theta} here~~
}
}
\details{
The counterclockwise 3 x 3 rotation matrix by an angle \eqn{\theta} along the \eqn{z} axis is defined as:
\deqn{
\left[ \begin{array}{ccc} \cos \theta & -\sin \theta & 0 \\
	 \sin \theta & \cos \theta & 0 \\ 
	 0 & 0 & 1 \\ \end{array} \right]
}
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A 3 x 3 matrix
}
\references{
Weisstein, Eric W. "Rotation Matrix." From MathWorld--A Wolfram Web Resource, \url{http://mathworld.wolfram.com/RotationMatrix.html}
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Federico Comoglio
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##Rotation matrix by theta = 45 degrees
rMatrix(pi/4)

##to rotate clockwise by 45 degrees
rMatrix(-pi/4)
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
}
\keyword{utilities}
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
