\name{loadDep}
\alias{loadDep}

\title{installs/attaches package dependencies}
\description{installs/attaches package dependencies from cran and the repository at 'contriburl'}
\usage{
loadDep(dep,contriburl="'http://scidb.orn.mpg.de/scidbextras/SOFTWARE/R_repository")

		}
\arguments{

\item{dep}{vector containing package names}
\item{contriburl}{defaults to the scidb repository}
}
	
\details{

}



\value{
should be quiet, but refuses to shut up. Returns a vector of length(dep) indicating whether package requiring was succesful.
}

\note{} 

\seealso{}
	

\examples{

dep=c("snb","utilsMPIO")
loadDep(dep)


}


\author{AR 2012-Mar-02}