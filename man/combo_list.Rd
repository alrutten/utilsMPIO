
\name{combo_list}
\alias{combo_list}
\alias{combo_list}
\title{creates a color combination list}
\description{creates a color combination list}

\usage{
combo_list(A, B, C, D)
}

\arguments{
	\item{A}{ character vector, e.g. lower right (LR) color}
	\item{B}{ character vector}
	\item{C}{ character vector}
	\item{D}{ character vector}


		}
		
\details{
only one argument should have length > 1
}	

\value{
\item{data.frame}{a data frame with 4 columns e.g. (UL, LL, UR, LR) }
}

\note{for the moment number of color rings on the variable leg is hadwired to 3
}
	

\examples{
combo_list() #default for Barrow 2011

combo_list(A = c("R", "O", "DB") , B = NA, C = NA, D = "R,GF" )

}


\author{MV 2012-Feb-29 09:08:37}