
# TODO : # change to the new package structure
		# document
		
# changes 2012-04-24 11:20:08 AR:

# downloadGPS has a dbpath argument	
# d for upload_nests (nests_data) now needs to be passed to the arguments
	

# ACCESORY FUNCTIONS

findGPS = function(rd=findRemovable()) { # returns c(UNIT ID &  path to GPS);


	garminloc = lapply(rd, function(x) list.files(x ,pattern = "Garmin") )
	garminloc = rd[sapply(garminloc, function(x) length(x) > 0)]
	
	if(length(garminloc) == 0 || length(garminloc) > 1) 
		stop((Message("Either GPS the gps was not plugged in or more than one unit is connected!", FALSE) ))

	
	gpsidPath = list.files(garminloc, full.names = TRUE, recursive = TRUE, pattern = "startup.txt") 
	gpsid = readLines(gpsidPath, warn =  FALSE) 
	gpsid = gpsid[grep("GPS", gpsid)]
	
	if(length(gpsid) == 0 )
		stop(Message("GPS ID is missing!\nGPS unit is not plugged-in or was not configured!", FALSE)) 
	
	gpsid = strsplit(gpsid, "=",  )[[1]][2]
	

	c(id = gpsid, path = garminloc)

}

readGPX = function(gps_path, what = "waypoints", crs ="+proj=longlat +datum=WGS84") { # read GPS (waypoints or tracks) as spdf
  
    if (missing(gps_path)) stop("no gps_path specified")
	
	gpsbabel = paste(system.file("gpsbabel", package="utilsMPIO"), "gpsbabel.exe", sep=.Platform$file.sep)
	out = paste(tempfile(), "csv", sep = ".")

	type =  switch(what,waypoints = "-w", tracks = "-r -t")
    #### TODO	
	system(paste(shQuote(gpsbabel), type, "-i gpx -f", shQuote(gps_path), "-o unicsv -F", out))

	d = read.csv(out, stringsAsFactors = FALSE)
	
	if(nrow(d) > 0 &&  all(c("Latitude", "Longitude", "Altitude","Date", "Time")%in%names(d)) ) {
	d$date_time = gsub("/", "-", paste(d$Date , d$Time))
	if(what == "tracks")
		d$Name = NA
	d = d[, c("Latitude", "Longitude", "Name", "Altitude", "date_time")]
	names(d) =  c("latit", "longit", "point", "altit", "date_time")
	coordinates(d)	 = ~ longit + latit
	proj4string(d) = CRS(crs)
	return(d)
	} 
		else {
		return(NULL)	
		}
	


}

writeGPX =  function (x, filename = "", type = "w")  {
    if (toupper(substr(filename, nchar(filename) - 3, nchar(filename))) != 
        ".GPX" & filename != "") 
        filename <- paste(filename, ".gpx", sep = "")
    options(digits = 7)
    cat("<?xml version=\"1.0\"?>\n<gpx version=\"1.1\" creator=\"pecs\">\n", 
        file = filename, sep = "")
    if (type == "w") {
        for (i in 1:nrow(x)) {
            cat("<wpt lat=\"", x[i, 3], "\" lon=\"", x[i, 2], 
                "\">\n", file = filename, sep = "", append = TRUE)
            cat("<name>", as.character(x[i, 1]), "</name>\n", 
                file = filename, sep = "", append = TRUE)
            if (ncol(x) > 3) 
                cat("<ele>", x[i, 4], "</ele>\n", file = filename, 
                  sep = "", append = TRUE)
            cat("<sym>Flag</sym>\n", file = filename, sep = "", 
                append = TRUE)
            cat("</wpt>\n", file = filename, sep = "", append = TRUE)
        }
    }
    else {
        cat("<trk>\n", file = filename, sep = "", append = TRUE)
        cat("<name>", filename, "</name>\n", file = filename, 
            sep = "", append = TRUE)
        cat("<trkseg>\n", file = filename, sep = "", append = TRUE)
        for (i in 1:nrow(x)) {
            cat("<trkpt lat=\"", x[i, 3], "\" lon=\"", x[i, 2], 
                "\">\n", file = filename, sep = "", append = TRUE)
            if (ncol(x) > 2) 
                cat("<ele>", x[i, 4], "</ele>\n", file = filename, 
                  sep = "", append = TRUE)
            cat("</trkpt>\n", file = filename, sep = "", append = TRUE)
        }
        cat("</trkseg>\n</trk>\n", file = filename, sep = "", 
            append = TRUE)
    }
    cat("</gpx>\n", file = filename, sep = "", append = TRUE)
    options(digits = 3)
}

# main functions
downloadGPS = function(gpsPath, unitID, gpxLoc ="Garmin\\GPX", ignore = "nests.gpx",dbpath) {
    
	if (missing(dbpath)) dbpath=dbPathGUI()

	if(missing(gpsPath)) {
		Message("Searching for USB devices...")
		usddev = findRemovable()
		Message(length(usddev), " found")
	    
		if (length(usddev)>0) {
		  Message("Searching for GPS unit...")
		  x = findGPS(usddev)
		  Message("GPS unit found at ", x["path"])
		  gpsPath = x["path"]
		  unitID = x["id"] 
		} else stop(Message("GPS ID is missing!\nGPS unit is not plugged-in or was not configured!",FALSE) )
       }
	#files
	Message("Searching for GPX files...")
	wl = list.files(paste(gpsPath,gpxLoc, sep = .Platform$file.sep), pattern = "gpx$",  full.names =  TRUE, recursive = TRUE)
	
	#remove ignored files from list
	ign = which(basename(wl) %in% basename(ignore))
		if(length(ign> 0))
			wl = wl[ -ign]	
	
	Message(length(wl), " GPX files found!")
	
	 #w-points	
	Message("Download waypoints",FALSE)
	w = list()
	for(i in 1:length(wl)) {
		w[[i]] = readGPX(wl[i], what = "waypoints")
		}
	
	# DATA IS THERE
		if(length(w) > 0) {
		w = w[sapply(w, function(x)! is.null(x)) ]
		
		w = do.call(rbind, w)
		
		d = data.frame(coordinates(w), cbind(w@data, gps = unitID))
		
		d = d[, c("gps", "point", "latit", "longit","altit", "date_time")]

		last = sqliQuery(dbpath,paste("Select max(date_time) from GPS where gps =", unitID))[1,1]
		
		if(!is.na(last))	
		d = d[as.POSIXct(d$date_time) > as.POSIXct(last),  ]
		
		if( nrow(d) > 0) 
			sqliSave(d, "GPS",dbpath) 
			Message( paste(nrow(d), "waypoints downloaded"), FALSE)
			
	}

	# NO DATA
	if(length(w) == 0 || nrow(d) == 0) {
		Message("No waypoints to download", FALSE)
	}

	 #tracks	
	Message("Downloading tracks...")
	w = list()
	for(i in 1:length(wl)) {
		w[[i]] = readGPX(wl[i], what = "tracks")
		}
	
	
	if(length(w) > 0) {
		
		w = w[sapply(w, function(x)! is.null(x)) ]
		
		w = do.call(rbind, w)
		
		d = data.frame(coordinates(w), cbind(w@data, gps = unitID))
		
		d = d[, c("gps",  "latit", "longit","altit", "date_time")]

		
		last = sqliQuery(dbpath,paste("Select max(date_time) from GPS_tracks where gps =", unitID))[1,1]
		if(!is.na(last))			
		d = d[as.POSIXct(d$date_time) > as.POSIXct(last),  ]

	if( nrow(d) > 0) 
			sqliSave(d, "GPS_tracks",dbpath) 
			Message( paste(nrow(d), "track points downloaded") ,FALSE)

	}
	
	if(length(w) == 0 || nrow(d) == 0) {
		Message("No tracks to download!",FALSE)
	}

	
	Message("GPS download DONE!",FALSE)
	
}

uploadNests = function(gpsPath, unitID, gpxNam ="Garmin\\GPX\\nests",d) {

	if(missing(gpsPath)) {
		Message("Searching for USB devices...")
		usddev = findRemovable()
		Message(length(usddev), " found")
	    
		if (length(usddev)>0) {
		  Message("Searching for GPS unit...")
		  x = findGPS(usddev)
		  message("GPS unit found at ", x["path"])
		  gpsPath = x["path"]
		  unitID = x["id"] 
		} else stop(Message("GPS ID is missing!\nGPS unit is not plugged-in or was not configured!",FALSE) )
		}

	
	d = data.frame(nest = d@data$nest, coordinates(d))
	 
	fn = paste(paste(gpsPath, gpxNam, sep = "\\"), "gpx", sep = ".")
	
	if(file.exists(fn)) file.remove(fn)
	
	writeGPX(x = d, filename = fn,type="w")
	
	Message("Upload done!", FALSE)
	
}

















































