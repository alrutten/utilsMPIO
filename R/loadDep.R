
 loadDep = function(dep,contriburl="http://scidb.orn.mpg.de/scidbextras/SOFTWARE/R_repository") {
   missingDep = dep[!dep%in%row.names(installed.packages())]
	if(length(missingDep) > 0) {
	    cranDep=missingDep[missingDep%in%available.packages(contrib.url("http://cran.r-project.org"))]
		install.packages(cranDep, repos = "http://cran.r-project.org")
	# check if all dependencies are there now, assuming the missing ones are ours
	    missingDep = dep[!dep%in%row.names(installed.packages())]
	      if(length(missingDep) > 0)
		  install.packages(missingDep, contriburl = contriburl)
	}	

	lapply(dep,function(dep) require(dep,character.only=TRUE,quietly=TRUE))
	}
	