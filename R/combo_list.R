

combo_list <- function(A = "GR,GF" , B = "M", C = NA, D = c("R", "O", "Y", "DG", "LB", "DB", "W") ) {

	
	lst = list(A,B,C,D)
	vary = which(sapply( lst, function(x) length(x) > 1))
		if(length(vary) > 1 ) stop ("Only one color is allowed to vary")
	
	V = lst[[vary]]

	setV = expand.grid(V,V,V)

	D = paste(setV[,1], setV[,2], setV[,3], sep = ",")


	COMBO_LIST  = data.frame(A, B, C, D)

	return(COMBO_LIST)

}















 
 