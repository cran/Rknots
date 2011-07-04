\name{fileImport}
\alias{fileImport}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Import or fetch a PDB file and build the alpha-Carbon trace model
%%  ~~function to do ... ~~
}
\description{
Import a PDB file either by fetching the Protein Data Bank or from the file system
and return the C-alpha trace protein backbone model.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fileImport(pdbID)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pdbID}{a single element character vector containing the name
  	 of the PDB file to be read, or the four letter PDB identifier for 
  	 online file access. For the latter, the \code{pdbID} is not case sensitive.
%%     ~~Describe \code{pdbID} here~~
}
}
\details{
If the selected PDB entry contains more than one chain, a list of available chains along with
their lengths (number of residues) is printed. Users can in this way select one of them.
The PDB atom field is then filtered in order to build the default 
protein C-alpha trace model. However, any other model or any three-dimensional structure can be 
directly loaded in R as an \eqn{N} x 3 matrix, where the columns represents the \eqn{x}, \eqn{y}
, \eqn{z} coordinates. 
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
An \eqn{N} x 3 matrix with the \eqn{x}, \eqn{y}, \eqn{z} coordinates of the C-alpha trace model
}
\references{
Grant, Rodrigues, ElSawy, McCammon, Caves (2006)
\emph{Bio3D: An R package for the comparative analysis of protein structures.},
Bioinformatics 22, 2695-2696 
%% ~put references to the literature/web site here ~
}
\author{
Federico Comoglio
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
This function makes use of the \code{read.pdb} function in the \code{bio3d} package. 
See references for additional details and credits. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
if(require(bio3d)) {
## Import a PDB file from the file system
pdb.model <- fileImport(system.file("extdata/2k0a.pdb", package="Rknots"))

## Import the same file by fetching the PDB
pdb.model <- fileImport("2k0a")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line