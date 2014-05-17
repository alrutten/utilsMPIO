
\name{is.ascending}
\alias{is.ascending}
\title{check whether a numerical variable is sorted in ascending order}
\description{check whether a numerical variable is sorted in ascending order}
\usage{is.ascending(var)

}
\arguments{
	\item{var}{variable (numeric, does not get tested for numericality)}
	}
\details{
}

\value{
is.ascending returns a boolean (TRUE for ascending)
}
\note{} 
\examples{

foo <- data.frame(a = c(1:4,2,5:10))
is.ascending(foo$a)

bar = c(1:20, rep(20,4))
is.ascending(bar)

is.friday()

}


\author{AR 12.04.2011}