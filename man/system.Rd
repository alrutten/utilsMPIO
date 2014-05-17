
\name{changeip}
\alias{runexe}
\alias{changeip}


\title{calling stuff via system}
\description{change IP, run an exe on a stubborn windows machine}

\usage{
changeip(ip, justShow = TRUE)
runexe(exepath)

}

\arguments{
	\item{ip}{the new IP address}
	\item{change = FALSE}{if TRUE }
	\item{mapLst}{ an object of class list and studyArea returned by maps() }

		}
		
\details{
changeip
}	

\value{
\item{changeip}{returns raw netsh results }
}


\note{changeip is mostly usefull in fieldcamps whel a LAN is set through a switch. 
runexe will behave like system() but it is more reliable if security settings are high. 
}
	

\examples{
changeip()
batloc = runexe(paste(system.file("gpsbabel", package="utilsMPIO"), "EXE", "gpsbabel", "gpsbabel.exe", sep=.Platform$file.sep) )
print(batloc)


}


\author{MV 2012-Mar-01 10:53:49}
















