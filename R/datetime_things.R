
wday=function(datetime){
	w=as.numeric(format(datetime,"%w"))
	ifelse(w!='0',w-1,6)
	}

backtoPOSIXct = function(timevar)  structure(timevar, class = c("POSIXt", "POSIXct"))
	