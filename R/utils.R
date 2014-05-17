
# random stuff 

today.fun <- function(envir)  {   
if (missing(envir)) envir=.GlobalEnv         
assign("today", format(Sys.Date(), "%Y-%m-%d"), env = envir) 
}

tz.fun <- function(tz) {
if (!identical(Sys.timezone(),tz))
tkmessageBox(message=paste("Change your time to",tz),icon="error",type="ok")
}

combo.fun <- function(data) { # data is a df containing ul,ll,ur,lr

	names(data) = toupper(names(data))
	data = subset(data, select = c(UL,LL,UR,LR))
	combo = with(data, paste(paste(UL,LL,sep="-"),paste(UR,LR, sep="-"),sep="|"))
	combo = tolower(gsub('NA','',combo)); combo = gsub('gf','f',combo) ; combo = gsub(',','.',combo)

	combo #returns a vector
	

	
}		


CtrlS <- function(fileName) {
	ahk.exe = system.file("EXE", "AutoHotkey.exe", package = "RSQLiteGUI")
	
	nam = basename(fileName)	
		
	ahk.script = "SetTitleMatchMode, 2
				IfWinNotActive, nam, , WinActivate, nam, 
				Sleep, 100
				Send ^s
				Sleep, 100
				Send, {ALTDOWN}{F4}{ALTUP}
				Sleep, 100
				"
	
	# ahk.script = "MsgBox, test"
	fileName = gsub("\\\\", "/", fileName)
	ahk.script = gsub('fileName',fileName, ahk.script)
	
	ahk.file = gsub("\\\\", "/", tempfile(fileext = ".ahk") )
	cat(ahk.script, file = ahk.file )
	
	system(paste(shQuote(ahk.exe), shQuote(ahk.file) ), wait = FALSE )
	cat("Saving", fileName)
}

dbPathGUI <- function() {
     tkmessageBox(message='select your fielddatabase')
     database_path = tclvalue(tkgetOpenFile(filetypes="{{Field database file} {.sqlite}} {{All files} *}"))                      
     if (database_path=="") tkmessageBox(message = "Nothing selected, Some functions will be not available anymore!") else
      return(database_path)
 }	
 
dbChooseTable <- function(db=dbPathGUI())  {
  
  tbl=sqliQuery(db,"select name from sqlite_master where type='table'")
  tblname=pickOne(tbl$name)
  return(tblname)
  }
 
usernam <- function()  {
shell("echo %USERNAME%", intern = TRUE)
}

sql2Rdate <- function(z) { 
as.POSIXct (strptime(z, "%Y-%m-%d") )
}

string2sql <- function(z) {

paste("julianday(", shQuote(z), ")" )

}

dataGUI <-  function(gui = "sqlitestudio") { 
	message(paste( dQuote(gui), 'will open in a second ...'))
	sf = list.files(system.file(package = "utilsMPIO", "EXE"), full.names = TRUE)
	ss = sf[grep(gui, sf)]
	
	owd = setwd(dirname(ss))
	sr = system(basename(ss) , wait = FALSE)
	if(sr!=0) shell.exec(ss)
	
	Message(paste("If nothing happens during the next few seconds\nopen", dQuote(ss), "yourself!"), F)
	
}  	

SDCopy= function(datadir,SDpattern='.',fname) {
   if (missing(datadir)) stop("specify where to copy data to")
   
   drv=findRemovable()
   
   if (length(drv)>0)  {   # SD card gets detected
     fl=list.files(paste(drv[1],"\\",sep=""),pattern=SDpattern,recursive=TRUE,full.names=TRUE)
	 err=""
     for(i in (1:length(fl))){
	    outfile=paste(datadir,ifelse(missing(fname),basename(fl[i]),fname),sep="/")
	    if (!(file.exists(outfile))){
		 dir.create(dirname(outfile),showWarnings=FALSE,recursive=TRUE)
		 file.copy(paste(drv[1],fl[i],sep="\\"),datadir,overwrite=FALSE,recursive=TRUE)
		 } else err=c(err,i)
	 	}
		if (length(err[-1])>0) tkmessageBox(message=paste(" the following files already existed\nand were not copied:\n",paste(fl[as.numeric(err[-1])],collapse='\n')))
		tkmessageBox(message=paste(i-length(err[-1]),'files copied'))
		}
		}

Message = function(txt, toGUI = TRUE, envir) { 
# if toGUI = TRUE, all the messages are stored in an obj in 'envir' and then displayed in the GUI else a small tk window will display the msg
    if (missing(envir)) envir=.GlobalEnv
	if(!exists("guiMsgs",envir=envir)){
		x = character()
		assign("guiMsgs", x, env = envir)
		}

	if(toGUI == FALSE)
	tkmessageBox(message = txt , icon = "info", type = "ok")

	if(toGUI == TRUE)
	assign("guiMsgs", c(get("guiMsgs",envir=envir), txt),  env = envir)

}
transpcol = function (col = "red", newalpha = 100, mcv = 255) # sets transparency for colour col
{
    mycol = col2rgb(col)
    rgb(mycol[1, ], mycol[2, ], mycol[3, ], alpha = newalpha, 
        maxColorValue = mcv)
}