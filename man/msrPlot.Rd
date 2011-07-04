\name{msrPlot}
\alias{msrPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
3D plot of a polygonal knot
%%  ~~function to do ... ~~
}
\description{
3D plot of a polygonal knot
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
msrPlot(points3D, text = TRUE, showNC = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{points3D}{an \eqn{N} x 3 matrix of the \eqn{x}, \eqn{y}, \eqn{z} coordinates of a polygonal link
%%     ~~Describe \code{points3D} here~~
}
  \item{text}{logical, if TRUE the numbering of the points is shown 
%%     ~~Describe \code{text} here~~
}
  \item{showNC}{logical, if TRUE the N-terminus and C-terminus of a protein are labeled in red
%%     ~~Describe \code{showNC} here~~
}
  \item{\dots}{other parameters for \code{lines3d} or \code{spheres3d}
%%     ~~Describe \code{\dots} here~~
}
}
\details{
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
For a 3D plot of a polygonal link (more than one component), see \code{msrMultiPlot}
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{msrMultiPlot}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##Plot the stevedore's knot, without points numbering

require(rgl)
data(Rolfsen.table, package = "Rknots")
stevedore <- Rolfsen.table$"6.1"
msrPlot(stevedore, text = FALSE, showNC = FALSE, 
    radius = 0.05, lwd = 5)

##The same plot, without specifying any parameter for the rgl primitive shapes 
## (the default sphere radius is 1).
msrPlot(stevedore, text = FALSE, col = c("steelblue","orange"))
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{graphics}
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
