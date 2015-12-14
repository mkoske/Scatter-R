# HANDLERS AND FUNCTIONS FOR SCATTER GUI

# GUI Handler: Show file dialog, read data from file, update main window based on data
sgui$hand.fileOpen <- function(h, ...) {
	sgui$sfile <- gfile(
		type="open",
		filter=list(
			"CSV files"=list(patterns="*.csv"),
			"TXT files"=list(patterns="*.txt")
	))

	sgui$sdata <- as.data.frame(read.csv(file=sgui$sfile))
	sgui$sub.updateWithData()
}

sgui$hand.readDatafile <- function(h, ...) {
	datafileWindow <- gwindow(title="Select datafile for read")

	cont.m <- ggroup(cont=datafileWindow, horizontal=FALSE, padding=20)
	cont.a <- ggroup(cont=cont.m, horizontal=TRUE)
	cont.b <- ggroup(cont=cont.m, horizontal=FALSE)
	cont.c <- ggroup(cont=cont.m, horizontal=FALSE)
	cont.d <- ggroup(cont=cont.m, horizontal=FALSE)

	sgui$sfile <- FALSE

	hand.selectfile <- function(h, ...) {
		sgui$sfile <- gfile(
			type="open",
			filter=list(
				"CSV files"=list(patterns="*.csv"),
				"TXT files"=list(patterns="*.txt")
			)
		)
		svalue(lbl_filename) <- sgui$sfile
	}

	hand.readfile <- function(h, ...) {
		if(sgui$sfile==FALSE) return()
		sgui$sdata <- as.data.frame(read.csv(file=sgui$sfile, sep=func.separator_selToArg(svalue(rdo_separator))))
		sgui$sub.updateWithData()
		dispose(datafileWindow)
	}

	func.separator_selToArg <- function(sel) {
		if(sel==";") return (";")
		if(sel==",") return (",")
		if(sel==".") return (".")
		if(sel=="[tab]") return ("\t")
		if(sel=="[space]") return ("")
	}


	btn_selectFile <- gbutton("Select file...", cont=cont.a, handler=hand.selectfile)
	lbl_filename   <- glabel("No file selected", cont=cont.a)
	lbl_separator  <- glabel("Select value separator",cont=cont.b)
	rdo_separator  <- gradio(c(";",",",".","[tab]","[space]"),selected=1,cont=cont.b)
	lbl_decimal    <- glabel("Select decimal separator", cont=cont.c)
	rdo_decimal    <- gradio(c(".",","),selected=1,cont=cont.c)
	btn_readFile   <- gbutton("Read file", cont=cont.d, handler=hand.readfile)

}

# Call this function after changing classvar or included_variables selections
sgui$sub.selectionVectorUpdate_class <- function() {

	sgui$var.classes           <- levels(as.factor(sgui$sdata[[match(sgui$var.classvar, sgui$var.allvariables)]]))
	sgui$var.selected.classes  <- sgui$var.classes
	sgui$var.attributes        <- sgui$var.allvariables[sgui$var.allvariables != sgui$var.classvar]
	sgui$var.selected.attributes <- intersect(sgui$var.attributes, sgui$var.selected.attributes)

	# Attributes vector has changed, do update
	sgui$sub.selectionVectorUpdate_attribute()
}

sgui$sub.selectionVectorUpdate_attribute <- function() {

	sgui$var.attributes_num    <- intersect(sgui$var.selected.attributes, sgui$var.allvariables_num)
	sgui$var.attributes_int    <- intersect(sgui$var.selected.attributes, sgui$var.allvariables_int)
	sgui$var.attributes_fac    <- intersect(sgui$var.selected.attributes, sgui$var.allvariables_fac)
	sgui$var.attributes_flv    <- intersect(sgui$var.selected.attributes, sgui$var.allvariables_flv)

	sgui$var.selected.binarized   <- intersect(sgui$var.attributes_flv, sgui$var.selected.binarized)
	sgui$var.selected.scaled      <- intersect(sgui$var.attributes_num, sgui$var.selected.scaled)
}

sgui$sub.updateWithData <- function() {
	rowCount = nrow(sgui$sdata)
	colCount = ncol(sgui$sdata)
	dataInfo = paste(sgui$sfile, " : ", rowCount, "rows with", colCount, "attributes")
	svalue(sgui$lbl_datainfo) <- dataInfo

	# Initialize parameter selections

	## Invariants
	sgui$var.allvariables     <- colnames(sgui$sdata)
	sgui$var.allvariables_num <- sgui$var.allvariables[sapply(sgui$sdata, is.numeric)]
	sgui$var.allvariables_int <- sgui$var.allvariables[sapply(sgui$sdata, is.integer)]
	sgui$var.allvariables_fac <- sgui$var.allvariables[sapply(sgui$sdata, is.factor) ]
	sgui$var.allvariables_flv <- union(sgui$var.allvariables_fac, sgui$var.allvariables_int)

	## Guess classvar and extract initial classes
	sgui$var.classvar      <- colnames(sgui$sdata[sgui$guessClassvar(sgui$sdata)])
	sgui$var.classes       <- levels(as.factor(sgui$sdata[[match(sgui$var.classvar, sgui$var.allvariables)]]))

	## Do class selection update for selection vectors
	sgui$sub.selectionVectorUpdate_class()

	## Initialize selection vectors for classes and attributes
	sgui$var.selected.classes     <- sgui$var.classes
	sgui$var.selected.attributes  <- sgui$var.attributes

	## Do attribute selection update for selection vectors
	sgui$sub.selectionVectorUpdate_attribute()

	## Initialize selection vectors for binarized and scaled attributes
	sgui$var.selected.binarized   <- sgui$var.attributes_fac
	sgui$var.selected.scaled      <- sgui$var.attributes_num

	## Update view
	sgui$sub.updateSelectionView()
}



sgui$sub.updateSelectionView <- function() {
	svalue(sgui$lbl_selClassVar)  <- sgui$var.classvar
	svalue(sgui$lbl_selVariables) <- paste(sgui$var.selected.attributes, collapse=", ")
	svalue(sgui$lbl_selClasses)   <- paste(sgui$var.selected.classes,    collapse=", ")
	svalue(sgui$lbl_ppScaled)     <- paste(sgui$var.selected.scaled,     collapse=", ")
	svalue(sgui$lbl_ppBinarized)  <- paste(sgui$var.selected.binarized,  collapse=", ")
}

# Guesses what might be the index of variable containing class information in dataframe
sgui$guessClassvar <- function(df) {
	factors  = sapply(df, is.factor )
	integers = sapply(df, is.integer)
	numerics = sapply(df, is.numeric)

	for(i in length(factors ):1) if(factors[i])  return(i)
	for(i in length(integers):1) if(integers[i]) return(i)
	for(i in length(numerics):1) if(numerics[i]) return(i)
}

# GUI handler: Show form for selection of variable containing class label
sgui$hand.select.classvar  <- function(h, ...) {
	if(is.null(sgui$sdata)) return()
	selWindow <- gwindow(title="Select class variable")
	cont.m <- ggroup(cont=selWindow)
	cont.a <- ggroup(cont=cont.m)
	cont.b <- ggroup(cont=cont.m, use.scrollwindow=TRUE, horizontal=FALSE, expand=TRUE)
	okButton <- gbutton("OK", cont=cont.a, hand=function(h,...)
		{
			sgui$var.selected.attributes <- append(sgui$var.selected.attributes, sgui$var.classvar) # Inject old classvar into selected attributes
			sgui$var.classvar <- svalue(radio)
			sgui$sub.selectionVectorUpdate_class()

			sgui$sub.updateSelectionView()
			dispose(selWindow)
		})

	radio <- gradio(sgui$var.allvariables, selected=match(sgui$var.classvar, sgui$var.allvariables), cont=cont.b)
}

# GUI handler: Show form for selection of variables included in scatter calculation
sgui$hand.select.attributes <- function(h, ...) {
	if(is.null(sgui$sdata)) return()
	selWindow <- gwindow(title="Select attributes included in the calculation")
	cont.m <- ggroup(cont=selWindow)
	cont.a <- ggroup(cont=cont.m)
	cont.b <- ggroup(cont=cont.m, use.scrollwindow=TRUE, horizontal=FALSE, expand=TRUE)
	okButton <- gbutton("OK", cont=cont.a, hand=function(h,...) {
		sgui$var.selected.attributes <- svalue(cbx)
		sgui$sub.selectionVectorUpdate_attribute()
		sgui$sub.updateSelectionView()
		dispose(selWindow)
	})
	cbx <- gcheckboxgroup(sgui$var.attributes, checked=(sgui$var.attributes %in% sgui$var.selected.attributes), cont=cont.b)

}

# GUI handler: Show form for selection of classes included in scatter calculation
sgui$hand.select.classes    <- function(h, ...) {
	if(is.null(sgui$sdata)) return()
	selWindow <- gwindow(title="Select classes included in the calculation")
	cont.m <- ggroup(cont=selWindow)
	cont.a <- ggroup(cont=cont.m)
	cont.b <- ggroup(cont=cont.m, use.scrollwindow=TRUE, horizontal=FALSE, expand=TRUE)
	okButton <- gbutton("OK", cont=cont.a, hand=function(h,...) {
		sgui$var.selected.classes <- svalue(cbx)
		sgui$sub.updateSelectionView()
		dispose(selWindow)
	})
	cbx <- gcheckboxgroup(sgui$var.classes, checked=(sgui$var.classes %in% sgui$var.selected.classes), cont=cont.b)
}

# GUI handler: Show form for selection of variables to be binarized in preprocessing
sgui$hand.select.binarized     <- function(h, ...) {
	if(is.null(sgui$sdata)) return()
	selWindow <- gwindow(title="Select attributes to be binarized")
	cont.m <- ggroup(cont=selWindow)
	cont.a <- ggroup(cont=cont.m)
	cont.b <- ggroup(cont=cont.m, use.scrollwindow=TRUE, horizontal=FALSE, expand=TRUE)
	okButton <- gbutton("OK", cont=cont.a, hand=function(h,...) {
		sgui$var.selected.binarized <- svalue(cbx)
		sgui$var.selected.scaled    <- sgui$var.selected.scaled[! sgui$var.selected.scaled %in% intersect(sgui$var.selected.scaled, sgui$var.selected.binarized)]
		sgui$sub.updateSelectionView()
		dispose(selWindow)
	})
	cbx <- gcheckboxgroup(sgui$var.attributes_flv, checked=(sgui$var.attributes_flv %in% sgui$var.selected.binarized), cont=cont.b)
}

# GUI handler: Show form for selection of variables to be scaled to range 0..1 in preprocessing
sgui$hand.select.scaled    <- function(h, ...) {
	if(is.null(sgui$sdata)) return()
	selWindow <- gwindow(title="Select numerical attributes for scaling to range 0..1")
	cont.m <- ggroup(cont=selWindow)
	cont.a <- ggroup(cont=cont.m)
	cont.b <- ggroup(cont=cont.m, use.scrollwindow=TRUE, horizontal=FALSE, expand=TRUE)
	okButton <- gbutton("OK", cont=cont.a, hand=function(h,...) {
		sgui$var.selected.scaled <- svalue(cbx)
		sgui$var.selected.binarized <- sgui$var.selected.binarized[! sgui$var.selected.binarized %in% intersect(sgui$var.selected.scaled, sgui$var.selected.binarized)]
		sgui$sub.updateSelectionView()
		dispose(selWindow)
	})
	cbx <- gcheckboxgroup(sgui$var.attributes_num, checked=(sgui$var.attributes_num %in% sgui$var.selected.scaled), cont=cont.b)

}

# GUI handler: Print GUI selections in R console
sgui$hand.printSelections <- function(h, ...) {
	print("==================================================")
	sgui$func.printVector("Classvar:   ", sgui$var.classvar)
	sgui$func.printVector("Classes:    ", sgui$var.selected.classes)
	sgui$func.printVector("Attributes: ", sgui$var.selected.attributes)
	sgui$func.printVector("Binarized:  ", sgui$var.selected.binarized)
	sgui$func.printVector("Scaled:     ", sgui$var.selected.scaled)
	sgui$func.printVector("NA handing: ", svalue(sgui$rdo_ppMissing, index=TRUE))
	print("--------------------------------------------------")
}

sgui$func.na.action_index_to_name <- function(index) {
	if(index == 1) return("nothing")
	if(index == 2) return("class")
	if(index == 3) return("column")
	if(index == 4) return("rmrows")
	return("class")
}

# GUI handler: Pass data and selections to scatter algorithm; do something with result
sgui$hand.calculate <- function(h, ...) {

	if(!is.data.frame(sgui$sdata)) return()

	sgui$ppdata <- scatter.preprocess (
		df                  = sgui$sdata,
		classvar            = sgui$var.classvar,
		included.attributes = sgui$var.selected.attributes,
		binarized           = sgui$var.selected.binarized,
		scaled              = sgui$var.selected.scaled,
		na.action           = sgui$func.na.action_index_to_name(svalue(sgui$rdo_ppMissing, index=TRUE))
	)

	result <- run (
		data                = sgui$ppdata,
		classlabel          = sgui$var.classvar,
		distanceMethod      = tolower(svalue(sgui$rdo_selectMethod)),
		iterations          = svalue(sgui$spn_selectIterations),
        usecase             = tolower(svalue(sgui$rdo_selectCalculation)),
        baselineIterations  = svalue(sgui$spn_selectBaselineIterations)
	)

	# TODO do something with result
	print(result)

}

# Print a vector with label/explanation in front of it
sgui$func.printVector <- function(heading, vec) {
	print(paste(heading, paste(vec, collapse=", ")))
}
