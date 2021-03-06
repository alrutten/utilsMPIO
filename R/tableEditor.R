


Tedit <- function(dframe, name = "temp", fileType = "xls", tf = tempfile(fileext = paste(".", fileType, sep = ""))) {
     maxwidth = 255*256 #unit  = 1/256 character, max column width is 255 char
  
	writeWorksheetToFile(tf, data=dframe, sheet = name)
	# set widths
	if (nrow(dframe)>0) {
	  widths=256+as.numeric(apply(dframe,2,function(x) max(nchar(x))+3))*256 
	  widths=ifelse(nchar(colnames(dframe))>widths/256,256+(nchar(colnames(dframe))+3)*256,widths)
          widths[widths>maxwidth] = maxwidth
	  foo=loadWorkbook(tf, create = FALSE)
	
	  setColumnWidth(foo,sheet=name,column=c(1:length(dframe)),width=widths) #setColumnWidth(xltmp,...) does not work
	  saveWorkbook(foo)
	 } 
    
    shell.exec(tf)
}
 

XLSForm=function(dfr, tfile=paste(tempdir(),paste(usernam(),format(Sys.time(),"%d%b%y_%H%M%S"),".XLS",sep=""),sep='/')) { 
   
     if (missing(dfr)) stop(error='no variables to enter data for')
     
	 xltmp=tfile
	 wb = loadWorkbook(xltmp,create = TRUE)
	 createSheet(wb, name = "DATA")
	 writeWorksheet(wb, data=dfr,sheet='DATA')
	
	# find column widths if there is data in dfr; width unit= is in 1/256 character widths (alternative: ship a template for each table?)
		
	if (nrow(dfr)>0) {
	  widths=256+as.numeric(apply(dfr,2,function(x) max(nchar(x))+5))*256 
	  widths=ifelse((nchar(colnames(dfr))+5)>widths/256,256+(nchar(colnames(dfr))+5)*256,widths)
      } else widths = (nchar(colnames(dfr))+5)*256
	  
	  setColumnWidth(wb,sheet='DATA',column=c(1:length(dfr)),width=widths)
	  saveWorkbook(wb)
	  
	 # check if excel is already open (and force it closed)
	xlopen = any(grepl("EXCEL.EXE",toupper(system("wmic process list",intern=TRUE))))
	if (xlopen) {
	  f=tkmessageBox(message="if you press 'ok', all your open excel programs will be killed dead.\nSave them now. THEN press OK.")
	  if (exists('f')&any(grepl("EXCEL.EXE",toupper(system("wmic process list",intern=TRUE))))) system("wmic process where name='excel.exe' call terminate") 
	  }
	shell(xltmp,wait=TRUE)

	new=readWorksheet(loadWorkbook(xltmp, create = FALSE),sheet='DATA')
	
	return(new)
		}
 
 
