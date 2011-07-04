\name{findGaps}
\alias{findGaps}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find gaps in proteins based on euclidean distance between residues 
%%  ~~function to do ... ~~
}
\description{
Find gaps in proteins based on euclidean distance between residues. 
It provides gap positions and accordingly split points useful when gaps in the structure 
have to be considered as chains terminator before invariant computations.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
findGaps(points3D, cutoff = 7)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{points3D}{an \eqn{N} x 3 matrix of the \eqn{x}, \eqn{y}, \eqn{z} coordinates of a polygonal link
%%     ~~Describe \code{points3D} here~~
}
  \item{cutoff}{the distance cutoff (for proteins, in Angstrom) that defines a chain split.
}
%%     ~~Describe \code{ends} here~~
}
\details{
Default \code{cutoff} is set to 7, approximately twice the average distance 
between two consecutive alpha-Carbon atoms.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
a list of matrix containing the \eqn{x}, \eqn{y}, \eqn{z} coordinates of the split chains. 
%%  ~Describe the value returned
%%  If it is a LIST, use
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
\code{\link{fileImport}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## Import a PDB file from the file system
if(require(bio3d)) {
	protein <- fileImport(system.file("extdata/1AJC_chainA.pdb", package="Rknots"))

	split <- findGaps(protein, cutoff = 7)
	split[[1]]
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line