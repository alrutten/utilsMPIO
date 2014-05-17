
findRemovable = function() { #uses wmic to list all removable drives (fsutil needs admin power)

	drvLetter =paste(unique(gsub("[^A-Z]","",(system('wmic logicaldisk get caption',intern=TRUE)))),":",sep="")
	drvUsed=as.numeric(gsub("\\D","",system('wmic logicaldisk get freespace',intern=TRUE))[-1])
	drvType=as.numeric(gsub("\\D","",system('wmic logicaldisk get drivetype',intern=TRUE))[-1])
	
	drv=as.data.frame(cbind(drvLetter,drvUsed,drvType),stringsAsFactors=FALSE)
    
	return(drv$drvLetter[which(!is.na(drv$drvUsed)&drv$drvType==2)])
	
	
}

  getSDserial = function(SDdir) { 
  foo=shell(paste("dir ",SDdir,sep=""),intern=TRUE)
  foo=gsub(".*is\ ","",foo[2])
  return(foo)
  }