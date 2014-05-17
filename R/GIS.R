

extract.xy = function(spdf, ID = "ID") { # extract  box coordinates from spdf-s
	# ID: the column to associate to coords
	boxes.xy =  data.frame(coordinates(spdf)) 
	names(boxes.xy) = c("x", "y")
	boxes.xy$box = spdf@data[ , ID]
	boxes.xy[order(boxes.xy$box), ]
}

zoom.fun  = function(envir=.GlobalEnv) {
  c1 = locator(1)
  abline(v = c1$x, col = "red")
  abline(h = c1$y, col = "red")
  c2 = locator(1)
  abline(v = c2$x, col = "red")
  abline(h = c2$y, col = "red")
  
  assign("zoom.box", SpatialPoints (rbind(data.frame(c1),  data.frame(c2))), env = envir)
  }

reset.zoom = function(envir=.GlobalEnv) {
   if ( exists("zoom.box",env=envir) ) rm(zoom.box, envir = envir)
   graphics.off()
   }
   

maps <- function(studyArea = c("barrow", "westerholz"), Proj4string = FALSE) {

	fl = list.files(system.file("MAPS", studyArea, package="utilsMPIO"), recursive = TRUE, full.names = TRUE, pattern = ".shp")

	if (!require(rgdal) ) 
		stop ('rgdal package is not available!')

	out = list()
	for(i in 1:length(fl))
		out[[i]] = readOGR(dsn= dirname(fl[i]), layer =  gsub(".shp$", "", basename(fl[i])),verbose = FALSE)
	
	names(out) = gsub(".shp$", "", basename(fl))
	
	if(! class(try(CRS(Proj4string) , silent = TRUE)) ==  "try-error")
		out = lapply(out, function(x) spTransform (x, CRS(Proj4string)) )

	attributes(out)$studyArea = studyArea
	class(out) = c(class(out), "studyArea")
	return(out)

}

base_map <- function(mapLst,PDF = FALSE, close.con = FALSE, box_names = TRUE, ...) { # ... passed to plot e.g. xlim, ylim etc

	if(!inherits(mapLst, 'studyArea')) stop("mapLst is not a studyArea object")
	m = mapLst

	if(!PDF) par(mar = c(0,0,0,0))
	
	w = 8.27
	h = 11.69

	if(PDF) { 
		f = paste(tempfile(), "pdf", sep = ".")
		pdf(f,width = w, height = h,  title = date(),paper = "a4",useDingbats = FALSE )
		par(mar = c(0,0,0,0))
	}	
	
	if(attributes(m)$studyArea == "barrow") {
	
		plot(m$study_area, border = "grey", ... )
		plot(m$NARL, add = TRUE, border = "grey")
		plot(m$new_building, add = TRUE, border = "grey")
		plot(m$buildings, add = TRUE, col = "grey")
		plot(m$power_line, add = TRUE, cex = 0.4)
		plot(m$tripods, add = TRUE, cex = 0.4)
		plot(m$snowFence, add = TRUE, cex = 0.4)
		plot(m$lakes, add = TRUE, border = "blue")
		plot(m$SE_bog_area, add = TRUE, border = "brown", density = 10, col = 'grey90')
	}

	if(attributes(m)$studyArea == "westerholz") {
		#plot(m$boxes, cex = 1, pch=20, col = "grey50",col), ...)
		plot(m$boxes, pch=20, ...)
		plot(m$streets, col= "grey" ,   add = TRUE)
		plot(m$farm, add  = T, col= "grey")
		plot(m$insideRoads, add = T, col= "grey", lwd= 0.8, lty= 2)
			if (box_names == TRUE)
				text(m$boxes, labels = m$boxes$ID, cex = 0.6, pos = 4, offset = 0.3)
		
		arrows(x0 = 4417822, y0 = 5334985, x1 = 4417822, y1 = 5334985+50,angle = 15, length = 0.15,lwd = 2)
		text(x = 4417822, y =  5334985+60, labels = "here be blue tits", cex = 0.8)
	}
		
		
	if(PDF && close.con) { 
		dev.off(); shell.exec(f)
		}
		
	if(PDF) 
		return(f)	

}



















