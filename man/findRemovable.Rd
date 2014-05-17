
\name{findRemovable}
\alias{getSDserial}
\alias{findRemovable}
\alias{SDCopy}

\title{functions dealing with removable storage devices}
\description{finds driveletter (findRemovable()) and serial number (getSDserial) for removable devices, and copies data off of removable devices (SDCopy)}
\usage{
findRemovable()
getSDserial(SDdir)
SDCopy(datadir,SDpattern='.')
		}
\arguments{
\item{SDdir}{drive letter for the removable device}
\item{datadir}{directory to copy data to}
\item{SDpattern}{regexp for trawling the removable device (copies all files that have SDpattern)}
}
	
\details{
findRemovable() finds all available drives using wmic, and returnes the ones that are of drivetype 2 (removable storage). Obviously works on windows only.
getSDserial uses dos-command 'dir' to find the not-actually-serial-number of the device attached to SDdir. Actually, if i understand correctly, it's the partition name; if you reformat the device it gets a different number.
SDCopy works to copy data from other removable devices than SD-cards as well.
}



\value{
findRemovable(): a (vector of) drive letter(s) with colon ("A:").
getSDserial: a character string.
}

\note{} 

\seealso{}
	

\examples{

getSDserial(findRemovable()[1])




}


\author{AR 2012-04-20 15:57:18}