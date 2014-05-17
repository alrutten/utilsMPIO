
is.ascending = function(var) {
all(as.numeric(c(var[-1], var[length(as.vector(var))])) >= as.numeric(var), na.rm=TRUE)
}

	
is.friday = function(dt=Sys.Date()) {
	n = c("today != friday\n","no, i don't think today is friday\n"," IT IS FRIDAY! ...no, wait, it isn't.\n"," i don't know if it is friday because of lack of coffee\n","today is a GOOD day, who cares if it is friday?\n","i know if it's friday but i'm not going to tell\n","OUT OF COOKIE ERROR\n","OUT OF FRIDAY ERROR\n","does this LOOK like friday to you??\n","OUT OF COFFEE ERROR\n","who cares about friday, give me cookies\n","what is this 'friday' concept you keep going on about?\n","I don't have a coffee problem. I have a without coffee problem.\n")
	y = c("YES! IT IS FRIDAY! YAY!\n","Houston, we have a Friday\n","RUN! IT'S ROBINSON CRUSOE!\n",
	    "yes, it is friday, happy now?\n","cookies would make this friday SO MUCH BETTER\n",
		"COOKIESCOOKIESCOOKIES, err, FRIDAYFRIDAYFRIDAY\n",
		paste("This friday will self-destruct in ",round(as.POSIXct(Sys.Date()+1)-Sys.time(),2)," hours",sep="" ))
	ifelse(format(dt,"%w")==5,sample(y,size=1),sample(n,size=1))
	}
	
coffee = function(when="NOW", cookies=TRUE) {
    cat("coffee needed",when, if (cookies) ", PLEASE BRING COOKIES\n")
}	

bugs = function() {
 cat("                       ug
                       b
                      g           bug
                      u        bug
      bugbug          b       g
            bug      bugbug bu
               bug  bugbugbugbugbugbug
  bug   bug   bugbugbugbugbugbugbugbugb
     bug   bug bugbugbugbugbugbugbugbugbu
   bugbugbugbu gbugbugbugbugbugbugbugbugbu
  bugbugbugbug
   bugbugbugbu gbugbugbugbugbugbugbugbugbu
     bug   bug bugbugbugbugbugbugbugbugbu
  bug   bug  gbugbugbugbugbugbugbugbugb
               bug  bugbugbugbugbugbug
            bug      bugbug  bu
      bugbug          b        g
                      u         bug
                      g            bug
                       b
                        ug\n")
						}