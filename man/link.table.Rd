\name{link.table}
\docType{data}
\alias{link.table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Links three dimensional coordinates and separators
%%  ~~function to do ... ~~
}
\description{
	This data set contains the three dimensional coordinates of 
	130 polygonal links (up to 4 components) along with the relevant separators.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
data(link.table)
link.table
}
\format{
A list of lists. Each slot contains a matrix containing the link coordinates and an integer vector of the separators
}
%- maybe also 'usage' for other objects documented here.
\details{
%%  ~~ If necessary, more details than the description above ~~
}

\references{
	The COLAB SFU website, \url{http://www.colab.sfu.ca}
%% ~put references to the literature/web site here ~
}
\note{
%%  ~~further notes~~
	A single entry can be accessed by [[ or using the corresponding link name.
	
	\code{names(link.table)} returns the available link names. 
	For example, the Hopf link can be accessed by \code{link.table$"2.2.1"}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{datasets}