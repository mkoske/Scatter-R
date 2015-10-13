rm(list=ls())

require(tcltk)
require(gWidgets)
require(gWidgetstcltk)
options("guiToolkit"="tcltk")

sgui <- new.env()
sgui$callingEnvironment <- environment()

# HANDLERS and FUNCTIONS

## TODO Pass data to scatter function 
sgui$hand.calculate <- function(h) {
	str(svalue(sgui$tblNominal))
	str(svalue(sgui$tblClass))
	str(svalue(sgui$cbxOptions))
	str(svalue(sgui$rdoDistance))
	str(sgui$sdata)
}

sgui$hand.fileOpen <- function(h) {
	sgui$sfile <- gfile(
		type="open", 
		filter=list(
			"CSV files"=list(patterns="*.csv"), 
			"TXT files"=list(patterns="*.txt")
	))

	sgui$sdata <- as.data.frame(read.csv(file=sgui$sfile))
	svalue(sgui$lblFilename) <- sgui$sfile
	sgui$func.createBrowser()
	if(!exists("btnCalculate", envir=sgui)) sgui$func.createCalcButtons()
}

## Middle section controls
sgui$func.createBrowser <- function() {
	
	if(!exists("lblNominal", envir=sgui)) {
		sgui$lblNominal <- glabel("Select nominal variables", container=sgui$cont.middle2)
		sgui$lblClass   <- glabel("Select class label variable", container=sgui$cont.middle3)
	}

	if(exists("tblBrowser", envir=sgui)) {
		delete(sgui$cont.middle1, sgui$tblBrowser)
		delete(sgui$cont.middle2, sgui$tblNominal)
		delete(sgui$cont.middle3, sgui$tblClass)
	}
	sgui$tblBrowser <- gtable(sgui$sdata, container=sgui$cont.middle1, height=320, width=550)
	sgui$tblNominal <- gtable(colnames(sgui$sdata), container=sgui$cont.middle2, multiple=TRUE, width=145, height=300)
	sgui$tblClass   <- gtable(colnames(sgui$sdata), container=sgui$cont.middle3, multiple=FALSE, width=145, height=300)
}

## Bottom section controls
sgui$func.createCalcButtons <- function() {
	sgui$lblDistance   <- glabel("Select distance measure", container=sgui$cont.bottom1)
	sgui$rdoDistance   <- gradio(c("Euclidean","Manhattan"), container=sgui$cont.bottom1)
	sgui$lblOptions    <- glabel("Other options", container=sgui$cont.bottom2)
	sgui$cbxOptions    <- gcheckboxgroup(c("Remove missing values"), container=sgui$cont.bottom2)
	sgui$btnCalculate  <- gbutton("Calculate SCATTER", container=sgui$cont.bottom3, handler=sgui$hand.calculate)
}


# CONTAINERS

## Top level
sgui$mainWindow <- gwindow(title="Scatter GUI")
sgui$mainContainer <- ggroup(container=sgui$mainWindow, horizontal=FALSE)
size(sgui$mainWindow) <- c(900, 600)

## Level 1
sgui$cont.top      <- ggroup(container=sgui$mainContainer, horizontal = FALSE)
sgui$cont.middle   <- ggroup(container=sgui$mainContainer, horizontal = TRUE)
sgui$cont.bottom   <- ggroup(container=sgui$mainContainer, horizontal = TRUE)

## Level 2
sgui$cont.top1     <- ggroup(container=sgui$cont.top)
sgui$cont.top2     <- ggroup(container=sgui$cont.top, horizontal=TRUE)

sgui$cont.middle1  <- ggroup(container=sgui$cont.middle, horizontal=TRUE)
sgui$cont.middle2  <- ggroup(container=sgui$cont.middle, horizontal=FALSE)
sgui$cont.middle3  <- ggroup(container=sgui$cont.middle, horizontal=FALSE)

sgui$cont.bottom1  <- ggroup(container=sgui$cont.bottom, horizontal=FALSE)
sgui$cont.bottom2  <- ggroup(container=sgui$cont.bottom, horizontal=FALSE)
sgui$cont.bottom3  <- ggroup(container=sgui$cont.bottom, horizontal=FALSE)



# CONTROLS

## Top section
sgui$lblTopInfo    <- glabel("Select data file for read:", container=sgui$cont.top1)
sgui$btnFileselect <- gbutton("Select file", container=sgui$cont.top2, handler=sgui$hand.fileOpen)
sgui$lblFilename   <- glabel("No file selected", container=sgui$cont.top2)



