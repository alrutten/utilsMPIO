\name{matchGregexpr}
\alias{matchGregexpr}

\title{does regular expression matching AND RETURNS THE MATCHED STRING.}
\description{for people who always forget exactly how to substring() your matches out of the mess gregexpr() returns. Also for people who do remember.}
\usage{
matchGregexpr(pattern, charv,...)

		}
\arguments{

\item{pattern}{a regular expression}
\item{charv}{a character vector where matches are sought }
}
	
\details{
performs a gregexpr() using pattern & charv (other gregexpr arguments get passed through dot-dot-dot, or so i hope). Not only returns the position and length of the match, but also the string it matched itself.
Probably overlaps with the stringr package, but i never looked into that.

}



\value{
a data.frame with, for each match, the start & end position of the matched string, as well as the matched string itself.
}

\note{} 

\seealso{}
	

\examples{

foo=("het zandkasteel aan zee")
bar=matchGregexpr('e+',foo)


}


\author{AR 2012-Mar-02}