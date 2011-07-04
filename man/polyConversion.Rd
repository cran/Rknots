\name{HOMFLY2Jones}
\alias{HOMFLY2Jones}
\alias{HOMFLY2Alexander}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert the HOMFLY polynomial into the Alexander or Jones polynomials
%%  ~~function to do ... ~~
}
\description{
Convert the HOMFLY polynomial into the Alexander or Jones polynomials
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
HOMFLY2Alexander(HOMFLY)
HOMFLY2Jones(HOMFLY)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{HOMFLY}{a single element character vector containing the HOMFLY polynomial
%%     ~~Describe \code{points3D} here~~
}
%%     ~~Describe \code{ends} here~~
}
\details{
The Alexander polynomial is obtained by substituting into the HOMFLY polynomial
\deqn{l=1}
and
\deqn{m=x^{1/2} - x^{-1/2}}
The following substitutions lead instead to the Jones polynomial
\deqn{l=t^{-1}}
and
\deqn{m=t^{1/2} - t^{-1/2}}
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
\code{\link{findKnot}},
\code{\link{findProteinKnot}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
HOMFLY.trefoil <- "-1/l**4 + 2/l**2 + m**2/l**2"
HOMFLY2Alexander(HOMFLY.trefoil)
HOMFLY2Jones(HOMFLY.trefoil)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line