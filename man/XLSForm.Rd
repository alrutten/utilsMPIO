
\name{XLSForm}
\alias{XLSForm}
\alias{Tedit}


\title{open data in excel}
\description{opens excel to enter data for some database table and returns the result}

\usage{
XLSForm(dfr, tfile)
Tedit(dfr)
}
\arguments{
\item{dfr}{dataframe for which to enter data (can be empty, used for number of variables)}
\item{tfile}{excelfile where the data goes, defaults to a tempfile based on usernam() and datetime that i cannot print because it gives the Rd parser a rolberoerte}
}


\value{
Tedit opens a dataframe in excel. XLSForm opens a dataframe in excel and stores any changes made to the excelfile in a dataframe as well.

}

	

\examples{
d = XLSForm(data.frame("foo"=NA,"bar"=NA))
foo  =data.frame(bar=c(0:10), thing = c(20:30))
Tedit(foo)

}


\author{2012-05-29 11:00:07 AR}

