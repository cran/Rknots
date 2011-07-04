\name{HOMFLY2mirror}
\alias{HOMFLY2mirror}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert the HOMFLY polynomial of a knot into the one of its mirror image
%%  ~~function to do ... ~~
}
\description{
The HOMFLY polynomial of the mirror image of a knot is computed by substituting \deqn{l = -l^-1}
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
HOMFLY2mirror(HOMFLY)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{HOMFLY}{a single element character vector containing the HOMFLY polynomial
%%     ~~Describe \code{points3D} here~~
}
%%     ~~Describe \code{ends} here~~
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
	Freyd P, Yetter D, Hoste J, Lickorish WBR, Millett K, et al.
	 (1985) A new polynomial invariant of knots and links. Bull Amer Math Soc (NS) 12: 239-246.
	
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
\code{\link{HOMFLYpolynomial}},
\code{\link{HOMFLY2Alexander}},
\code{\link{HOMFLY2Jones}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##convert the right-handed trefoil knot polynomial
HOMFLY.trefoil <- "-1/l**4 + 2/l**2 + m**2/l**2"

##into the one of the left-handed trefoil knot
HOMFLY2mirror(HOMFLY.trefoil)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line