
\name{tksplash}
\alias{tksplash}
\alias{editString}
\alias{edit2String}
\alias{chooseItems}
\alias{pickOne}
\alias{openForm}
\alias{changeSettings}


\title{tcltk lego bricks}
\description{tcltk lego bricks}

\usage{
tksplash(stuff)
editString(one string to edit)
edit2String(two strings to edit)
pickOne(varnames)
openForm(win=tktoplevel(),varnames=c(1:14),defaults=c(1:14))
changeSettings(envir=.sesa)

}

\arguments{
 See examples
		}
		
\details{
no details
}	

\value{
none or a string
openForm: a list of tcl variables
}


\note{
no notes
}
	

\examples{
tksplash("I am an ugly, useless splash screen\n I'll stay splashed for 5 seconds", secs = 5 , font = "helvetica 40", background = "pink", foreground = "black")

x = editString("please edit me", "here ->", "edit me!", ".I_will_also_be_assgined_to_.GlobalEnv")
print(x)
print(.I_will_also_be_assgined_to_.GlobalEnv)

x = edit2String("please edit me", "here ->", "edit me!", "here too ->", "edit me too!")
print(x)

chooseItems("choose some stuff!", "pick me up ->",  apply(expand.grid(letters[1:3], LETTERS[2:4]),1, paste, collapse = "~~~") )

foo=pickOne(c("pick","one","any","one"))

openForm(varnames=c("i","need","amounts"),defaults=c("really","huge"," of cookies"))

require(sesa)
changeSettings()



}


\author{MV 2012-Mar-01 10:53:49}
















