
changeip <- function(ip, change = FALSE) {

	if(missing(ip)) 
	 ip = editString("change IP", "new ip =", "192.168.0.100", "ip")

	o = system("netsh interface ipv4 show address", intern = TRUE)
	 
	if(change) { # do the change
	netsh = paste('netsh interface ip set address name="Local Area Connection" source=static addr=', ip, 'mask=255.255.255.0')
	system(netsh)  } else
	print(o)
	
	n = system("netsh interface ipv4 show address", intern = TRUE)
	 
	if(!identical(o,n) ) 
	  message("iF change = TRUE, you probably managed to change you IP address with one click.", FALSE)
	
	return(invisible(n))
}
 
