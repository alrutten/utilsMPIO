
\name{XLSForm}
\alias{XLSForm}


\title{excel form for database data entry}
\description{opens excel to enter data for some database table and returns the result}

\usage{
XLSForm(dfr, tfile)
}
\arguments{
\item{dfr}{dataframe for which to enter data (can be empty, used for number of variables)}
\item{tfile}{excelfile where the data goes, defaults to a tempfile based on usernam() and datetime that i cannot print because it gives the Rd parser a rolberoerte}
}


\value{
a data.frame with the entered data, and an excelfile at tflie
}

	

\examples{
d = XLSForm(data.frame("foo"=NA,"bar"=NA))


}


\author{2012-05-29 11:00:07 AR}

