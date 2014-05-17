
\name{gis}
\alias{gis}
\alias{maps}
\alias{maps}
\alias{base_map}
\alias{extract.xy}
\alias{zoom.fun}
\alias{reset.zoom}


\title{Study area maps}
\description{Study area maps: Barrow and Westerholz}

\usage{
maps(studyArea)
base_map(mapLst,PDF = FALSE, close.con = FALSE, box_names = TRUE, ...)

}

\arguments{
	\item{studyArea = c("barrow", "westerholz")}{ character vector}
	\item{Proj4string = FALSE}{a valid proj4 string; we do not really have to change it}
	\item{mapLst}{ an object of class list and studyArea returned by maps() }
	\item{PDF = FALSE}{ }
	\item{close.con = FALSE}{ }
	\item{box_names = TRUE}{ only for westerholz}
	\item{\dots}{allows passing arguments to plot(); but only on the 1st map; usefull to set up e..g map limits, see examples. }


		}
		
\details{
no details.
}	

\value{
\item{studyArea}{an object of class list and studyArea. has an extra atribute containing the name of the study area}
}


\note{no notes.
}
	

\examples{
we = maps("westerholz")
ba = maps("barrow")

attributes(we)

plot(we$studyArea) # direct plotting

base_map(we)

# passing arguments to  . . .; using xlim to strech the map
base_map(we, PDF = TRUE, close.con = TRUE, xlim = c(4417191, 4417849) )

base_map(ba)
base_map(ba, PDF = TRUE, close.con = TRUE)

# use close.con = FALSE to put stuff on the base map

mypdf = base_map(ba, PDF = TRUE, close.con = FALSE)
nests = spsample(ba$study_area, 500, "random")
# a very good field season
plot(nests, add = TRUE, col = 2, pch = 20, bg = 2)
dev.off()
shell.exec(mypdf)


extract.xy(we$boxes, ID = 'ID')




}


\author{MV 2012-Feb-29 16:02:06}
















