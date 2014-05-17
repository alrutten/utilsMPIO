
#MAKE the binary package file and optionally installs the package locally

MAKE = function(pkgLoc = "utilsMPIO/utilsMPIO" , path = "//ds/kempdba/SCIDB_MANAGEMENT/APPS/R_packages", install = FALSE) {
    pn=basename(pkgLoc)
	
	srcPath   = paste(path, "SRC", pkgLoc,  sep = .Platform$file.sep )
	binPath   = paste(path, "BIN",  sep = .Platform$file.sep )
	reposPath = paste(path, "REPOS", sep = .Platform$file.sep)
	
	td = paste(tempdir(), paste( c(letters[round(runif(8, 1, 26))], rpois(8, 1)) , collapse = ""), sep = .Platform$file.sep )
	dir.create(td)
	oldwd = setwd(td)
	
	file.copy(srcPath, td, recursive = TRUE)
	shell(paste("R CMD INSTALL --build --no-multiarch --byte-compile", basename(pkgLoc) ) )
	shell(paste("R CMD build ", basename(pkgLoc) ))


	zipnam = list.files(pattern = ".zip")
	tarnam = list.files(pattern = ".tar.gz")
	file.copy(zipnam, binPath , overwrite = TRUE)
	file.copy(tarnam, binPath , overwrite = TRUE)
	
	#remove older versions from REPOS
	older  = list.files(reposPath, pattern = paste0(pn,'_'), full.names  =TRUE)
	for (i in 1:length(older)) file.remove(older[i])
	
	file.copy(zipnam, reposPath , overwrite = TRUE)
	file.copy(tarnam, reposPath , overwrite = TRUE)
	if(install) { 
	
	  
	  if(any(grepl(pn,search()))) detach(paste("package:", pn,sep=""),character.only=TRUE,force=TRUE)
	  unlink(system.file( package=pn),  recursive = TRUE)
	  cat("installing", zipnam,"\n")
	  install.packages(zipnam, repos = NULL)
	}
	
	
	setwd(oldwd)
	
	
	
}


# MAKE(install = F)







