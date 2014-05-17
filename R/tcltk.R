
tksplash = function(msg = "wait a bit", secs = .5, foreground = "white", background = "black", font = "helvetica 20") {
		require(tcltk)
	win = tktoplevel()
	tkwm.title(win,"")
	tkfocus(win)
	tkwm.resizable(win, 0, 0)
	tkwm.overrideredirect(win, 1)
	tkgrid(tklabel(win,text= msg, font = font ,foreground  = foreground, background = background) )
	Sys.sleep (secs)
	tkdestroy(win)
}

editString = function(title, label, txt, nam) {
	require(tcltk)
	top <- tktoplevel()
	tkwm.minsize(top, 200, 50)
	tktitle(top) <- title

	# date
	string = tclVar(txt)
	lab = tklabel(top,text= label)

		OnOK  =  function() {
			Out <<- tclvalue(string)
			assign(nam, tclvalue(string), envir = .GlobalEnv)
			tkdestroy(top)
		}
		
	tkbind(tkentry(top, width="15", textvariable= string), "<Return>",OnOK)
	tkgrid(lab,                                           row = 1   , column = 0, sticky="e")
	tkgrid(tkbutton(top,text="OK",command=OnOK),          row = 1   , column = 2, sticky="e")
	tkgrid(tkentry(top, width="15", textvariable= string), row = 1   , column = 1, sticky="e")

	tkfocus(top)
	tkwait.window(top)

	return(Out)
}

edit2String = function(title = "",freeTxtLab1 = "", freeTxt1 = 1, freeTxtLab2 = "", freeTxt2 = 2) {
	require(tcltk)
	tclRequire("BWidget")
	
	W = max(c(nchar(freeTxtLab1), nchar(freeTxtLab2)))
	
	top  =  tktoplevel()
	tkwm.title(top, title) 
	
	Frame1   = tkframe(top)
	Frame2   = tkframe(top)
		
	labfreeText1 = tklabel(Frame1,text = freeTxtLab1, width = W)
	freeText1  =  tktext(Frame1)
	freeTxtVal1 = tclVar(freeTxt1)
	freeText1 = tkentry(Frame1 ,textvariable = freeTxtVal1)
	
	labfreeText2 = tklabel(Frame2,text = freeTxtLab2, width = W)
	freeText2  =  tktext(Frame2)
	freeTxtVal2 = tclVar(freeTxt2)
	freeText2 = tkentry(Frame2 ,textvariable = freeTxtVal2)
	

		onOK  =  function() {
			freeTextVal1   =  tclvalue(tcl(freeText1,"get"))
			freeTextVal2   =  tclvalue(tcl(freeText2,"get"))
			
			Out <<- list(freeTextVal2, freeTextVal1)

			tkdestroy(top)
		}

	OK  =  tkbutton(Frame2, text = "OK", width = 6, command = onOK)

	tkpack(Frame1)
	tkgrid(labfreeText1, sticky ="w", column= 0, row = 0)
	tkgrid(freeText1,     sticky="e", column= 1, row = 0)

	tkpack(Frame2)
		tkgrid(labfreeText2, sticky ="w", column= 0, row = 0)
		tkgrid(freeText2,     sticky="e", column= 1, row = 0)
		tkgrid(OK,         sticky="e"  , column= 0, row = 1)
	
	tkfocus(top)
	tkwait.window(top)

	return(Out)

	
}

chooseItems = function(title = 'foo', label ="choose one\nor more", items) {
# sorry about the scrollbar, i promse i'll make it nicer -AR
	require(tcltk)
	top <- tktoplevel()
	tkwm.minsize(top, 200, 50)
	tktitle(top) <- title
	if (length(items)<=10) listBox = tklistbox(top, height = length(items), selectmode = "multiple", 
                                              background = "white") else {
	  scr = tkscrollbar(top, repeatinterval=5,command=function(...)tkyview(listBox,...))
	  listBox = tklistbox(top,height=10,selectmode="multiple",
	                    yscrollcommand=function(...)tkset(scr,...),background="white")
    }
	lab = tklabel(top,text = label)
	nams  =  as.character(items)

	for (i in (1:length(items)) ){
		tkinsert(listBox, "end",nams[i])
	}

	OnOK  =  function() {
		spChoice  <<-  items[as.numeric(tkcurselection(listBox))+1]
		tkdestroy(top)
	}
	
	but  = tkbutton(top ,text="OK" ,command=OnOK)

	tkgrid(lab , row = 1, column = 3)
	tkgrid(but , row = 1, column = 5)
	if (length(items)<=10) tkgrid(listBox, row = 1, column = 4) else tkgrid(listBox,scr)

	tkfocus(top)
	tkwait.window(top)
	return(spChoice)
}


pickOne= function(varnames=c("give","a","vector","of","options")) { #will be incorporated in chooseItems w/ extra argument to set selectmode to 'single' or whatever that should be to select 1 item only
 win=tktoplevel()
 tkwm.title(win, "choose something" )
 onePicked=tclVar(varnames[1])
 tkgrid(tklabel(win,text="pick one, any one",foreground='red'))
 tkgrid(tk2combobox(win,textvariable=onePicked,value=varnames))
  

OnOK <- function()
{
    res<<-tclvalue(onePicked)
    tkdestroy(win)
	
 }
OK.but <- tkbutton(win,text="OK",command=function() OnOK())
tkgrid(OK.but)
tkwait.window(win)
return(res)
}


openForm= function(win=tktoplevel(),varnames=c(1:14),defaults=c(1:14)) { #tcltk window
 maxwidth=max(nchar(defaults))
 vars=list()
 entries=list()
 labels=list()
for (i in 1:length(varnames)){
  vars[[i]]=tclVar(paste(defaults[i],collapse=" "))
  entries[[i]]=tkentry(win)
  labels[[i]]=tklabel(win)
  tkconfigure(entries[[i]],textvariable=vars[[i]],width=maxwidth+4)
  tkconfigure(labels[[i]],text=paste(varnames[i]))
  tkgrid(labels[[i]],entries[[i]])
  
}
tkfocus(win)
return(vars)
}	

changeSettings= function(envir=.sesa) {
 
 st=eapply(env=envir,print)

 
 tt=tktoplevel()
 tkgrid(tklabel(tt,text="variable",font=2),tklabel(tt,text="setting",font=2))
 tkgrid(tklabel(tt,text=""))
 f=openForm(win=tt,varnames=names(st),defaults=st)
 
 onSave= function() {
   
   for (i in 1:length(f)) {
     if (class(try(as.Date(tclvalue(f[[i]])),silent = TRUE)) == 'try-error')
     val = try(eval(try(parse(text=tclvalue(f[[i]])),silent=TRUE)),silent=TRUE)
	 else val=tclvalue(f[[i]])
     if (class(val)=='try-error') val=tclvalue(f[[i]])
	 assign(names(st[i]),val,envir=.sl)
	 }
	 tkdestroy(tt)
	 }
  
	 
 Save.but = tkbutton(tt,text="SAVE",command=function() onSave())
 Exit.but = tkbutton(tt,text="Exit without saving",command=function() tkdestroy(tt))
 
 tkgrid(Save.but,Exit.but)
} 