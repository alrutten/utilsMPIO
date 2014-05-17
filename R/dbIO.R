

dbicon <- function(path, driver = "SQLite") {

	if(!file.exists(path)) stop("File does not exist.")

	
	con = dbConnect(dbDriver(driver), dbname = path)
    init_extensions(con)
	return(invisible(con))

}

sqliQuery <- function(path, query) {
	# last change MV 2012-Mar-19 14:34:56
	if(!file.exists(path)) stop("File does not exist.")
	
	con = dbConnect(dbDriver("SQLite"), dbname = path)
		
	init_extensions(con)
	
	return(sqliteQuickSQL(con, query))

	on.exit(expr =sqliteCloseConnection(con), add = TRUE)
	on.exit(rm(con) )
}

sqliSave <- function(dat,  tablename, path)  {
	
	if(!file.exists(path)) stop("File does not exist.")
		
	con = dbConnect(dbDriver("SQLite"), dbname= path)
	init_extensions(con)	
	res = dbWriteTable(con, tablename, dat, append = TRUE, row.names = FALSE) 
	return(res)
	on.exit(expr =sqliteCloseConnection(con), add = TRUE)
	on.exit(rm(con) )
	
}

backup <- function(backup_path, path, prefix = "backup_", suffix = ".sqlite" ) {

	nam = usernam()
	
	fn = paste(prefix, round(as.numeric(Sys.time() )),"_", nam, suffix, sep = "")
	
	bknam = paste(backup_path, fn, sep = .Platform$file.sep)   

	cp = file.copy(from = path, to = bknam)
	
	if(cp) res = bknam else res = ""
	
	return(res)
	
}






















