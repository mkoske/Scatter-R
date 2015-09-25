# 
# GUI for Scatter-R project
# 

require(tcltk)

rm(list=ls())

# Top-level UI window
	scatgui <- tktoplevel()
	tktitle(scatgui) <- "Scatter GUI"

# Functions used in UI main window
#=================================

## Close UI window
	scatgui.close <- function() {tkdestroy(scatgui) }

## Read .csv data file and assign to global environment
	scatgui.readFile <- function(filename) {
		if (filename == "") return;
		scat.data <- read.csv(file=filename, sep=",")
		assign("scat.data",scat.data,envir=.GlobalEnv)
		scatgui.printListbox.lab(colnames(scat.data))
	}

## Show select file dialog for .csv files in working directory
	scatgui.selectFile <- function() { 
		scat.filename <- tclVar(tkgetOpenFile( filetypes = "{{.csv files} {.csv}}"))
		# scat.filename <- tclvalue(tkgetOpenFile( filetypes = "{{.csv files} {.csv}}"))
		if (!nchar(scat.filename))
			tkmessageBox(message="No file selected!")
		else {
			tkconfigure(Filename.label, text=tclvalue(scat.filename))  # Change filename label text
			scatgui.readFile(tclvalue(scat.filename))                  # Read CSV file
			tkgrid(RunScatter.button)
			tkgrid(selectClassVar.label)
			tkgrid(scatgui.listbox.label)
		}
	}

## Run scatter calculation
	scatgui.runScatter <- function() {
		tkmessageBox(title="Scatter-R",message="   Hello world !   ",icon="info",type="ok")
		scatgui.close()
	}

## Produce listbox for label variable
	scatgui.printListbox.lab <- function(itemList) {
		for (i in itemList) {
			tkinsert(scatgui.listbox.label, "end", i)
		}
	}
	
## Produce listbox for nominal variables
	scatgui.printListbox.nom <- function(itemList) {
		for (i in itemList) {
			tkinsert(scatgui.listbox.nominal, "end", i)
		}
	}


# GUI elements
#=============

## Command buttons
	Close.button      <- tkbutton(scatgui, text = "Close window", command = scatgui.close)
	SelectFile.button <- tkbutton(scatgui, text = "Select data file for read", command = scatgui.selectFile)
	RunScatter.button <- tkbutton(scatgui, text = "Run scatter function", command = scatgui.runScatter)

## Labels
	Filename.label <- tklabel(scatgui, text = "No file selected")
	selectClassVar.label <- tklabel(scatgui, text = "Select variable containing class label")

## Other GUI elements
	scatgui.listbox.label <- tklistbox(scatgui, height=10, selectmode="single", background="white")
	scatgui.listbox.nominal <- tklistbox(scatgui, height=10, selectmode="multiple", background="white")
#~ 	scatgui.printListbox.lab(c(1,2,3,4,5,6,7,8,9,10,11,12,13))
	scatgui.printListbox.nom(c(1,2,3,4,5,6,7,8,9,10,11,12,13))


## Place the buttons in the top-level window
	tkgrid(Close.button, sticky="e")
	tkgrid(tklabel(scatgui,text="                                                  "))
	tkgrid(SelectFile.button, Filename.label)
#~ 	tkgrid(scatgui.listbox.label, scatgui.listbox.nominal)
