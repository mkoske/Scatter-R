
scatter.gui <- function() {

	# require(tcltk)
	require(gWidgets2)
	require(gWidgets2RGtk2)
	options(guiToolkit="RGtk2")

	scattergui <- new.env()

	# AVAILABLE OPTIONS
	## Distance measures
	scattergui$opt.distmethod <- c("Euclidean", "Manhattan", "HEOM")
	## Treatment of missing values
	scattergui$opt.missing    <- c("Do nothing", "Replace with class median/mode", "Replace with column median/mode", "Remove rows")
	## Calculation procedures
	scattergui$opt.calcProcedure <- c("Single", "Variables", "Classes", "All")

	# OTHER FILES
	source("scatter_preprocess.R")
	source("algorithm/scatter.R")

# ============ HANDLERS AND FUNCTIONS FOR SCATTER GUI ================ #
	
	# GUI handler: show help text
	scattergui$hand.showHelp <- function(h, ...) {
		helpWindow <- gwindow(title="Help")
		cont.a <- ggroup(cont=helpWindow, horizontal=FALSE, expand=TRUE, fill=TRUE)
		filename <- "helptext.txt"
		helptext <- readChar(filename, file.info(filename)$size)
		lbl_helptext <- glabel(helptext, cont=cont.a, expand=TRUE, fill=TRUE)
		btn_close <- gbutton("Close", cont=cont.a, handler=function(h,...){dispose(helpWindow)})
	}

	# GUI handler:btn_readDatafile; Shows dialog to select a datafile
	#  and options for reading the data from file to dataframe.
	scattergui$hand.readDatafile <- function(h, ...) {

		# Containers
		datafileWindow <- gwindow(title="Select datafile for read")
		cont.m <- ggroup(cont=datafileWindow, horizontal=FALSE, padding=20)
		cont.a <- ggroup(cont=cont.m, horizontal=TRUE)
		cont.b <- ggroup(cont=cont.m, horizontal=FALSE)
		cont.c <- ggroup(cont=cont.m, horizontal=FALSE)
		cont.d <- ggroup(cont=cont.m, horizontal=FALSE)

		scattergui$sfile <- FALSE # Datafile is unselected

		# GUI handler:btn_selectFile; Shows a file selection dialog
		hand.selectfile <- function(h, ...) {
			scattergui$sfile <- gfile(
				type="open",
				filter=list(
					"CSV files"=list(patterns="*.csv"),
					"TXT files"=list(patterns="*.txt")
				)
			)
			svalue(lbl_filename) <- scattergui$sfile
		}

		# GUI handler:btn_readFile; Reads selected file with options and updates GUI
		hand.readfile <- function(h, ...) {
			if(scattergui$sfile==FALSE) return()
			scattergui$sdata <- as.data.frame(read.csv(
				file=scattergui$sfile,
				header=func.header_selToArg(svalue(rdo_headers)),
				sep=func.separator_selToArg(svalue(rdo_separator))
			))
			scattergui$sub.updateWithData()
			dispose(datafileWindow)
		}

		# Convert GUI element value to argument value
		func.separator_selToArg <- function(sel) {
			if(sel==";") return (";")
			if(sel==",") return (",")
			if(sel==".") return (".")
			if(sel=="[tab]") return ("\t")
			if(sel=="[space]") return ("")
		}

		# Convert GUI element value to argument value
		func.header_selToArg <- function(sel) {
			if(sel=="Yes") return(TRUE)
			if(sel=="No")  return(FALSE)
		}

		# Datafile selection dialog GUI elements
		btn_selectFile <- gbutton("Select file...", cont=cont.a, handler=hand.selectfile)
		lbl_filename   <- glabel("No file selected", cont=cont.a)
		lbl_headers    <- glabel("Does the data file have column headers?", cont=cont.b)
		rdo_headers    <- gradio(c("Yes","No"), selected=1, cont=cont.b)
		lbl_separator  <- glabel("Select value separator",cont=cont.c)
		rdo_separator  <- gradio(c(",",";",".","[tab]","[space]"),selected=1,cont=cont.c)
		btn_readFile   <- gbutton("Read file", cont=cont.d, handler=hand.readfile)

	}

	# Does the updates necessary after changing classvar or included_variables selections
	scattergui$sub.selectionVectorUpdate_class <- function() {

		scattergui$var.classes             <- levels(as.factor(scattergui$sdata[[match(scattergui$var.classvar, scattergui$var.allvariables)]]))
		scattergui$var.selected.classes    <- scattergui$var.classes
		scattergui$var.attributes          <- scattergui$var.allvariables[scattergui$var.allvariables != scattergui$var.classvar]
		scattergui$var.selected.attributes <- intersect(scattergui$var.attributes, scattergui$var.selected.attributes)

		# Attributes vector has changed, do update
		scattergui$sub.selectionVectorUpdate_attribute()
	}

	# Does the updates necessary after changing the attribute selection vectors
	scattergui$sub.selectionVectorUpdate_attribute <- function() {

		scattergui$var.attributes_num    <- intersect(scattergui$var.selected.attributes, scattergui$var.allvariables_num)
		scattergui$var.attributes_int    <- intersect(scattergui$var.selected.attributes, scattergui$var.allvariables_int)
		scattergui$var.attributes_fac    <- intersect(scattergui$var.selected.attributes, scattergui$var.allvariables_fac)
		scattergui$var.attributes_flv    <- intersect(scattergui$var.selected.attributes, scattergui$var.allvariables_flv)

		scattergui$var.selected.binarized   <- intersect(scattergui$var.attributes_flv, scattergui$var.selected.binarized)
		scattergui$var.selected.scaled      <- intersect(scattergui$var.attributes_num, scattergui$var.selected.scaled)
	}

	# Updates the GUI logic after reading a data file
	scattergui$sub.updateWithData <- function() {
		rowCount = nrow(scattergui$sdata)
		colCount = ncol(scattergui$sdata)
		dataInfo = paste(scattergui$sfile, " : ", rowCount, "rows with", colCount, "attributes")
		svalue(scattergui$lbl_datainfo) <- dataInfo

		# Initialize parameter selections: make sensible default selections

		## Invariants: these vectors change only when new data file is read in
		scattergui$var.allvariables     <- colnames(scattergui$sdata)
		scattergui$var.allvariables_num <- scattergui$var.allvariables[sapply(scattergui$sdata, is.numeric)]
		scattergui$var.allvariables_int <- scattergui$var.allvariables[sapply(scattergui$sdata, is.integer)]
		scattergui$var.allvariables_fac <- scattergui$var.allvariables[sapply(scattergui$sdata, is.factor) ]
		scattergui$var.allvariables_flv <- union(scattergui$var.allvariables_fac, scattergui$var.allvariables_int)

		## Guess classvar and extract initial classes
		scattergui$var.classvar      <- colnames(scattergui$sdata[scattergui$guessClassvar(scattergui$sdata)])
		scattergui$var.classes       <- levels(as.factor(scattergui$sdata[[match(scattergui$var.classvar, scattergui$var.allvariables)]]))

		## Do class selection update for selection vectors
		scattergui$sub.selectionVectorUpdate_class()

		## Initialize selection vectors for classes and attributes
		scattergui$var.selected.classes     <- scattergui$var.classes
		scattergui$var.selected.attributes  <- scattergui$var.attributes

		## Do attribute selection update for selection vectors
		scattergui$sub.selectionVectorUpdate_attribute()

		## Initialize selection vectors for binarized and scaled attributes
		scattergui$var.selected.binarized   <- scattergui$var.attributes_fac
		scattergui$var.selected.scaled      <- scattergui$var.attributes_num

		## Update view
		scattergui$sub.updateSelectionView()
	}


	# Updates GUI view after selections change
	scattergui$sub.updateSelectionView <- function() {
		svalue(scattergui$lbl_selClassVar)  <- scattergui$var.classvar
		svalue(scattergui$lbl_selVariables) <- paste(scattergui$var.selected.attributes, collapse=", ")
		svalue(scattergui$lbl_selClasses)   <- paste(scattergui$var.selected.classes,    collapse=", ")
		svalue(scattergui$lbl_ppScaled)     <- paste(scattergui$var.selected.scaled,     collapse=", ")
		svalue(scattergui$lbl_ppBinarized)  <- paste(scattergui$var.selected.binarized,  collapse=", ")
	}

	# Guesses what might be the index of variable containing class information in dataframe
	#  Returns the rightmost factor-type column index;
	#  in case there are no factors, the rightmost integer index;
	#  in case there are no integers, the rightmost column
	scattergui$guessClassvar <- function(df) {
		factors  = sapply(df, is.factor )
		integers = sapply(df, is.integer)
		numerics = sapply(df, is.numeric)

		for(i in length(factors ):1) if(factors[i])  return(i)
		for(i in length(integers):1) if(integers[i]) return(i)
		for(i in length(numerics):1) if(numerics[i]) return(i)
	}

	# GUI handler: Show form for selection of variable containing class label
	scattergui$hand.select.classvar  <- function(h, ...) {
		if(is.null(scattergui$sdata)) return()
		selWindow <- gwindow(title="Select class variable")
		cont.m <- ggroup(cont=selWindow)
		cont.a <- ggroup(cont=cont.m)
		cont.b <- ggroup(cont=cont.m, use.scrollwindow=TRUE, horizontal=FALSE, expand=TRUE)
		okButton <- gbutton("OK", cont=cont.a, hand=function(h,...)
			{
				# Inject old classvar into selected attributes
				scattergui$var.selected.attributes <- append(scattergui$var.selected.attributes, scattergui$var.classvar)

				scattergui$var.classvar <- svalue(radio)
				scattergui$sub.selectionVectorUpdate_class()

				scattergui$sub.updateSelectionView()
				dispose(selWindow)
			})

		radio <- gradio(scattergui$var.allvariables, selected=match(scattergui$var.classvar, scattergui$var.allvariables), cont=cont.b)
	}

	# GUI handler: Show form for selection of variables included in scatter calculation
	scattergui$hand.select.attributes <- function(h, ...) {
		if(is.null(scattergui$sdata)) return()
		selWindow <- gwindow(title="Select attributes included in the calculation")
		cont.m <- ggroup(cont=selWindow)
		cont.a <- ggroup(cont=cont.m)
		cont.b <- ggroup(cont=cont.m, use.scrollwindow=TRUE, horizontal=FALSE, expand=TRUE)
		okButton <- gbutton("OK", cont=cont.a, hand=function(h,...) {
			scattergui$var.selected.attributes <- svalue(cbx)
			scattergui$sub.selectionVectorUpdate_attribute()
			scattergui$sub.updateSelectionView()
			dispose(selWindow)
		})
		cbx <- gcheckboxgroup(scattergui$var.attributes, checked=(scattergui$var.attributes %in% scattergui$var.selected.attributes), cont=cont.b)

	}

	# GUI handler: Show form for selection of classes included in scatter calculation
	scattergui$hand.select.classes    <- function(h, ...) {
		if(is.null(scattergui$sdata)) return()
		selWindow <- gwindow(title="Select classes included in the calculation")
		cont.m <- ggroup(cont=selWindow)
		cont.a <- ggroup(cont=cont.m)
		cont.b <- ggroup(cont=cont.m, use.scrollwindow=TRUE, horizontal=FALSE, expand=TRUE)
		okButton <- gbutton("OK", cont=cont.a, hand=function(h,...) {
			scattergui$var.selected.classes <- svalue(cbx)
			scattergui$sub.updateSelectionView()
			dispose(selWindow)
		})
		cbx <- gcheckboxgroup(scattergui$var.classes, checked=(scattergui$var.classes %in% scattergui$var.selected.classes), cont=cont.b)
	}

	# GUI handler: Show form for selection of variables to be binarized in preprocessing
	scattergui$hand.select.binarized     <- function(h, ...) {
		if(is.null(scattergui$sdata)) return()
		selWindow <- gwindow(title="Select attributes to be binarized")
		cont.m <- ggroup(cont=selWindow)
		cont.a <- ggroup(cont=cont.m)
		cont.b <- ggroup(cont=cont.m, use.scrollwindow=TRUE, horizontal=FALSE, expand=TRUE)
		okButton <- gbutton("OK", cont=cont.a, hand=function(h,...) {
			scattergui$var.selected.binarized <- svalue(cbx)
			scattergui$var.selected.scaled    <- scattergui$var.selected.scaled[! scattergui$var.selected.scaled %in% intersect(scattergui$var.selected.scaled, scattergui$var.selected.binarized)]
			scattergui$sub.updateSelectionView()
			dispose(selWindow)
		})
		cbx <- gcheckboxgroup(scattergui$var.attributes_flv, checked=(scattergui$var.attributes_flv %in% scattergui$var.selected.binarized), cont=cont.b)
	}

	# GUI handler: Show form for selection of variables to be scaled to range 0..1 in preprocessing
	scattergui$hand.select.scaled    <- function(h, ...) {
		if(is.null(scattergui$sdata)) return()
		selWindow <- gwindow(title="Select numerical attributes for scaling to range 0..1")
		cont.m <- ggroup(cont=selWindow)
		cont.a <- ggroup(cont=cont.m)
		cont.b <- ggroup(cont=cont.m, use.scrollwindow=TRUE, horizontal=FALSE, expand=TRUE)
		okButton <- gbutton("OK", cont=cont.a, hand=function(h,...) {
			scattergui$var.selected.scaled <- svalue(cbx)
			scattergui$var.selected.binarized <- scattergui$var.selected.binarized[! scattergui$var.selected.binarized %in% intersect(scattergui$var.selected.scaled, scattergui$var.selected.binarized)]
			scattergui$sub.updateSelectionView()
			dispose(selWindow)
		})
		cbx <- gcheckboxgroup(scattergui$var.attributes_num, checked=(scattergui$var.attributes_num %in% scattergui$var.selected.scaled), cont=cont.b)

	}

	# Maps missing value handling selection index
	# to na.action argument name for scatter.preprocess function
	scattergui$func.na.action_index_to_name <- function(index) {
		if(index == 1) return("nothing")
		if(index == 2) return("class")
		if(index == 3) return("column")
		if(index == 4) return("rmrows")
		return("class")
	}

	# GUI handler: Pass data and selections to scatter algorithm; show dialog for saving&plotting results
	scattergui$hand.calculate <- function(h, ...) {

		if(!is.data.frame(scattergui$sdata)) return()
		
		scattergui$var.options.na.action          = scattergui$func.na.action_index_to_name(svalue(scattergui$rdo_ppMissing, index=TRUE))
		scattergui$var.options.distanceMethod     = tolower(svalue(scattergui$rdo_selectMethod))
		scattergui$var.options.iterations         = svalue(scattergui$spn_selectIterations)
		scattergui$var.options.usecase            = tolower(svalue(scattergui$rdo_selectCalculation))
		scattergui$var.options.baselineIterations = svalue(scattergui$spn_selectBaselineIterations)

		scattergui$ppdata <- scatter.preprocess (
			df                  = scattergui$sdata,
			classvar            = scattergui$var.classvar,
			included.attributes = scattergui$var.selected.attributes,
            included.classes    = scattergui$var.selected.classes,
			binarized           = scattergui$var.selected.binarized,
			scaled              = scattergui$var.selected.scaled,
			na.action           = scattergui$func.na.action_index_to_name(svalue(scattergui$rdo_ppMissing, index=TRUE))
		)

		scattergui$result <- run (
			data                = scattergui$ppdata,
			classlabel          = scattergui$var.classvar,
			distanceMethod      = scattergui$var.options.distanceMethod,
			iterations          = scattergui$var.options.iterations,
			usecase             = scattergui$var.options.usecase,
			baselineIterations  = scattergui$var.options.baselineIterations
		)

		# Handle result
		print(scattergui$result)    # Print the result in R console
		scattergui$hand.useResult() # Show saving & plotting options in GUI

	}

	# GUI handler: Show dialog for saving result in 
	#  text/deparsed format, showing different plots of result data
	scattergui$hand.useResult <- function() {
		resultWindow <- gwindow(title="Save or plot results")

		cont.m <- ggroup(cont=resultWindow, horizontal=FALSE, padding=20)
		cont.a <- ggroup(cont=cont.m, horizontal=FALSE)
		cont.b <- ggroup(cont=cont.m, horizontal=FALSE)
		cont.c <- ggroup(cont=cont.m, horizontal=FALSE)
		
		# Saving of results in R console format
		hand.saveText <- function(h, ...) {
			savefile <- FALSE
			savefile <- gfile(type="save",initial.filename="result.txt")
			resultText <- capture.output(scattergui$result)
			if(!identical(savefile, character(0))) write(resultText, file=savefile)
		}

		# Saving of results as text representation of R object;
		#  can be read back to R with dget() function
		hand.saveObject <- function(h, ...) {
			savefile <- gfile(type="save", initial.filename="result.txt")
			if(!identical(savefile, character(0))) dput(scattergui$result, file=savefile)
		}
		
		# Handlers for plotting the results
		
		## Collection vector plotting
		hand.showCollectionVector <- function(h, ...) {
			yvals = c(1,length(unique(scattergui$result$collectionVector)))
			ytx   = 1:length(unique(scattergui$result$collectionVector))
			ylabel = "Class"
			xlabel = "Case"
			plot(scattergui$result$collectionVector, ylim=yvals, ylab=ylabel, yaxt="n", xlab=xlabel)
			axis(2, at=ytx)
		}
		
		## Plotting variable-wise scatter values against baseline values
		hand.showVariableScatter <- function(h, ...) {
			
		}
		
		## Plotting class-wise scatter values against baseline values
		hand.showClassScatter <- function(h, ...) {
		
		}
		
		## Plotting something that is sensible in usecase "all"
		hand.showAllPlot <- function(h, ...) {
			
		}
		
		# GUI elements for saving the calculation result
		btn_saveText     <- gbutton("Save result to TXT file", cont=cont.a, handler=hand.saveText)
		btn_saveObject   <- gbutton("Save result object as text representation", cont=cont.a, handler=hand.saveObject)
		
		# GUI elements for plotting options specific to the use case
		if(scattergui$var.options.usecase=="single") {
			btn_showPlot <- gbutton("Plot collection vector", cont=cont.b, handler=hand.showCollectionVector)
		}
		if(scattergui$var.options.usecase=="variables") {
			btn_showPlot <- gbutton("Plot variable-specific scatter values against baselines", cont=cont.b, handler=hand.showVariableScatter)
		}
		if(scattergui$var.options.usecase=="classes") {
			btn_showPlot <- gbutton("Plot class-specific scatter values", cont=cont.b, handler=hand.showClassScatter) 
		}
		if(scattergui$var.options.usecase=="all") {
			btn_showPlot <- gbutton("Plot something for usecase ALL", cont=cont.b, handler=hand.showAllPlot) 
		}
		
		btn_exit <- gbutton("Close", cont=cont.c, handler=function(h,...) {dispose(resultWindow)})
		
	}

	# Print a vector with label/explanation in front of it
	scattergui$func.printVector <- function(heading, vec) {
		print(paste(heading, paste(vec, collapse=", ")))
	}


# ===================== GUI elements ================================= #

	# MAIN WINDOW AND CONTAINER
	scattergui$winMain <- gwindow(title="Scatter GUI")
	scattergui$cont <- ggroup(container=scattergui$winMain, horizontal=FALSE, spacing=10, use.scrollwindow=TRUE)
	size(scattergui$winMain) <- c(600, 500)


	# CONTAINERS
	scattergui$cont.a   <- ggroup(horizontal=FALSE,  cont=scattergui$cont)
	scattergui$cont.aa  <- ggroup(horizontal=TRUE,   cont=scattergui$cont.a)
	scattergui$cont.aaa <- ggroup(horizontal=TRUE,   cont=scattergui$cont.aa, expand=TRUE, fill=TRUE)
	scattergui$cont.aab <- ggroup(horizontal=TRUE,   cont=scattergui$cont.aa)
	scattergui$cont.ab  <- ggroup(horizontal=FALSE,  cont=scattergui$cont.a)

	scattergui$cont.b   <- ggroup(horizontal=FALSE, cont=scattergui$cont)
	scattergui$lbl_sectSelection  <- glabel("Select included classes and  variables", cont=scattergui$cont.b)

	scattergui$cont.ba  <- ggroup(horizontal=TRUE, cont=scattergui$cont.b)
	scattergui$cont.baa <- ggroup(cont=scattergui$cont.ba)
	scattergui$cont.bab <- ggroup(cont=scattergui$cont.ba, use.scrollwindow=TRUE, fill=TRUE, expand=TRUE)
	scattergui$cont.bb  <- ggroup(horizontal=TRUE, cont=scattergui$cont.b)
	scattergui$cont.bba <- ggroup(cont=scattergui$cont.bb)
	scattergui$cont.bbb <- ggroup(cont=scattergui$cont.bb, use.scrollwindow=TRUE, fill=TRUE, expand=TRUE)
	scattergui$cont.bc  <- ggroup(horizontal=TRUE, cont=scattergui$cont.b)
	scattergui$cont.bca <- ggroup(cont=scattergui$cont.bc)
	scattergui$cont.bcb <- ggroup(cont=scattergui$cont.bc, use.scrollwindow=TRUE, fill=TRUE, expand=TRUE)

	scattergui$cont.c   <- ggroup(horizontal=FALSE, cont=scattergui$cont)
	scattergui$lbl_sectPreprocess <- glabel("Preprocessing options", cont=scattergui$cont.c)

	scattergui$cont.ca  <- ggroup(horizontal=TRUE, cont=scattergui$cont.c)
	scattergui$cont.caa <- ggroup(cont=scattergui$cont.ca)
	scattergui$cont.cab <- ggroup(cont=scattergui$cont.ca, use.scrollwindow=TRUE, fill=TRUE, expand=TRUE)
	scattergui$cont.cb  <- ggroup(horizontal=TRUE, cont=scattergui$cont.c)
	scattergui$cont.cba <- ggroup(cont=scattergui$cont.cb)
	scattergui$cont.cbb <- ggroup(cont=scattergui$cont.cb, use.scrollwindow=TRUE, fill=TRUE, expand=TRUE)
	scattergui$cont.cc   <- ggroup(horizontal=TRUE,  cont=scattergui$cont.c)
	scattergui$cont.cca  <- ggroup(horizontal=FALSE, cont=scattergui$cont.cc)
	scattergui$cont.ccb  <- ggroup(horizontal=FALSE, cont=scattergui$cont.cc)

	scattergui$cont.d   <- ggroup(horizontal=FALSE, cont=scattergui$cont)
	scattergui$lbl_sectOtherOptions <- glabel("Other options", cont=scattergui$cont.d)

	scattergui$cont.da  <- ggroup(horizontal=TRUE, cont=scattergui$cont.d)
	scattergui$cont.db  <- ggroup(horizontal=TRUE,  cont=scattergui$cont.d,  padding=15)
	scattergui$cont.dba <- ggroup(horizontal=FALSE, cont=scattergui$cont.db, padding=10)
	scattergui$cont.dbb <- ggroup(horizontal=FALSE, cont=scattergui$cont.db, padding=10)
	scattergui$cont.dbc <- ggroup(horizontal=FALSE, cont=scattergui$cont.db, padding=10)
	scattergui$cont.dbd <- ggroup(horizontal=FALSE, cont=scattergui$cont.db, padding=10)
	scattergui$cont.dc  <- ggroup(horizontal=TRUE, cont=scattergui$cont.d, fill=TRUE, expand=TRUE)




	# MAIN WINDOW CONTROLS
	scattergui$btn_readDatafile <- gbutton("Read CSV datafile...",cont=scattergui$cont.aaa, handler=scattergui$hand.readDatafile, expand=TRUE)
	scattergui$btn_info         <- gbutton("HELP", cont=scattergui$cont.aab, handler=scattergui$hand.showHelp)
	scattergui$lbl_datainfo     <- glabel("No file selected", cont=scattergui$cont.ab)
	scattergui$btn_selClassVar  <- gbutton("Select class variable...         ", cont=scattergui$cont.baa, handler=scattergui$hand.select.classvar)
	scattergui$lbl_selClassVar  <- glabel("No data", cont=scattergui$cont.bab)
	scattergui$btn_selClasses   <- gbutton("Select included classes...     ", cont=scattergui$cont.bba, handler=scattergui$hand.select.classes)
	scattergui$lbl_selClasses   <- glabel("No data", cont=scattergui$cont.bbb)
	scattergui$btn_selVariables <- gbutton("Select included variables...   ", cont=scattergui$cont.bca, handler=scattergui$hand.select.attributes)
	scattergui$lbl_selVariables <- glabel("No data", cont=scattergui$cont.bcb)
	scattergui$btn_ppScaled     <- gbutton("Select variables to scale...   ",    cont=scattergui$cont.caa, handler=scattergui$hand.select.scaled)
	scattergui$lbl_ppScaled     <- glabel("No data", cont=scattergui$cont.cab)
	scattergui$btn_ppBinarized  <- gbutton("Select variables to binarize...", cont=scattergui$cont.cba, handler=scattergui$hand.select.binarized)
	scattergui$lbl_ppBinarized  <- glabel("No data", cont=scattergui$cont.cbb)
	scattergui$lbl_ppMissing    <- glabel("Handling of missing values", cont=scattergui$cont.cca)
	scattergui$rdo_ppMissing    <- gradio(scattergui$opt.missing, selected=2, cont=scattergui$cont.ccb)
	scattergui$lbl_selectMethod      <- glabel("Select distance measure", cont=scattergui$cont.dba)
	scattergui$rdo_selectMethod      <- gradio(scattergui$opt.distmethod, cont=scattergui$cont.dba)
	scattergui$lbl_selectIterations  <- glabel("Iterations", cont=scattergui$cont.dbb)
	scattergui$spn_selectIterations  <- gspinbutton(from=1, to=500, by=1, value=10, cont=scattergui$cont.dbb)
	scattergui$lbl_selectBaselineIterations <- glabel("Baseline iterations", cont=scattergui$cont.dbc)
	scattergui$spn_selectBaselineIterations <- gspinbutton(from=1, to=500, by=5, value=50, cont=scattergui$cont.dbc)
	scattergui$lbl_selectCalculation <- glabel("Select calculation", cont=scattergui$cont.dbd)
	scattergui$rdo_selectCalculation <- gradio(scattergui$opt.calcProcedure, selected=1, cont=scattergui$cont.dbd)
	scattergui$btn_calculate         <- gbutton("Calculate", cont=scattergui$cont.dc, handler=scattergui$hand.calculate)

}
