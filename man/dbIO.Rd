
\name{dbicon}
\alias{sqliQuery}
\alias{sqliSave}
\alias{backup}


\title{IO database functions}
\description{Input/output db functions}

\usage{
dbicon(path, driver = "SQLite")
sqliQuery(query, path)
sqliSave(dat,  tablename, path)
backup(backup_path, path, prefix = "backup")
}

\arguments{
	\item{path}{ character vector, path to db}
	\item{backup_path}{ character vector, backup directory}
	\item{query}{ character vector, sql query}
	\item{dat}{ data.frame}
	\item{tablename}{ character vector}
		}
		
\details{
no details.
}	

\value{
\item{dbicon}{returns a db connection object}
\item{sqliQuery}{a data.frame in the case of select statements}
\item{sqliSave}{TRUE if all is good}



}


\note{no notes.
}
	

\examples{
path = system.file(package="dbiGUI", "testDB", "testDB.sqlite")
con = dbicon(path)
summary(con)

sqliQuery("PRAGMA automatic_index", path) # Luckily automatic indexing is enabled by default. 
sqliQuery("PRAGMA integrity_check ", path) # if not ok then shit happend
sqliQuery("PRAGMA recursive_triggers ", path) # off by default

tabs = sqliQuery("select * from SQLITE_MASTER where type = 'table' limit 2", path)$tbl_name 
lapply(tabs, function(x) { sqliQuery( paste("PRAGMA table_info(", x, ")"), path) } ) 

s = sqliQuery("select * from SQLITE_MASTER", path)
print(sqliSave(s, "temp", path) )

sqliQuery("drop table temp", path)

}


\author{MV 2012-Mar-01 13:08:39}
















