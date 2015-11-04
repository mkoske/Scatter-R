# require(tcltk)
require(gWidgets2)
require(gWidgets2RGtk2)
options(guiToolkit="RGtk2")

sgui <- new.env()
sgui$sdata <- NULL

# MAIN WINDOW AND CONTAINER
sgui$winMain <- gwindow(title="Scatter GUI")
sgui$cont <- ggroup(container=sgui$winMain, horizontal=FALSE, spacing=10)
size(sgui$winMain) <- c(600, 600)

# HANDLERS
source("scatter_gui_v3_handlers.R")

# CONTAINERS
sgui$cont.a   <- ggroup(horizontal=FALSE, cont=sgui$cont)


sgui$cont.b   <- ggroup(horizontal=FALSE, cont=sgui$cont)
sgui$lbl_sectSelection  <- glabel("Select included classes and  variables", cont=sgui$cont.b)

sgui$cont.ba  <- ggroup(horizontal=TRUE, cont=sgui$cont.b)
sgui$cont.baa <- ggroup(cont=sgui$cont.ba)
sgui$cont.bab <- ggroup(cont=sgui$cont.ba, use.scrollwindow=TRUE, fill=TRUE, expand=TRUE)

sgui$cont.bb  <- ggroup(horizontal=TRUE, cont=sgui$cont.b)
sgui$cont.bba <- ggroup(cont=sgui$cont.bb)
sgui$cont.bbb <- ggroup(cont=sgui$cont.bb, use.scrollwindow=TRUE, fill=TRUE, expand=TRUE)

sgui$cont.bc  <- ggroup(horizontal=TRUE, cont=sgui$cont.b)
sgui$cont.bca <- ggroup(cont=sgui$cont.bc)
sgui$cont.bcb <- ggroup(cont=sgui$cont.bc, use.scrollwindow=TRUE, fill=TRUE, expand=TRUE)

sgui$cont.c   <- ggroup(horizontal=FALSE, cont=sgui$cont)
sgui$lbl_sectPreprocess <- glabel("Preprocessing options", cont=sgui$cont.c)

sgui$cont.ca  <- ggroup(horizontal=TRUE, cont=sgui$cont.c)
sgui$cont.caa <- ggroup(cont=sgui$cont.ca)
sgui$cont.cab <- ggroup(cont=sgui$cont.ca, use.scrollwindow=TRUE, fill=TRUE, expand=TRUE)

sgui$cont.cb  <- ggroup(horizontal=TRUE, cont=sgui$cont.c)
sgui$cont.cba <- ggroup(cont=sgui$cont.cb)
sgui$cont.cbb <- ggroup(cont=sgui$cont.cb, use.scrollwindow=TRUE, fill=TRUE, expand=TRUE)


sgui$cont.d   <- ggroup(horizontal=FALSE, cont=sgui$cont)
sgui$lbl_sectOtherOptions <- glabel("Other options", cont=sgui$cont.d)

sgui$cont.da  <- ggroup(horizontal=TRUE, cont=sgui$cont.d)




# MAIN WINDOW CONTROLS
sgui$btn_readfile     <- gbutton("Read CSV datafile...",cont=sgui$cont.a, handler=sgui$hand.fileOpen)
sgui$lbl_datainfo     <- glabel("No file selected", cont=sgui$cont.a)

sgui$btn_selClassVar  <- gbutton("Select class variable...         ", cont=sgui$cont.baa, handler=sgui$hand.select.classvar)
sgui$lbl_selClassVar  <- glabel("No data", cont=sgui$cont.bab)

sgui$btn_selClasses   <- gbutton("Select included classes...     ", cont=sgui$cont.bba, handler=sgui$hand.select.classes)
sgui$lbl_selClasses   <- glabel("No data", cont=sgui$cont.bbb)

sgui$btn_selVariables <- gbutton("Select included variables...   ", cont=sgui$cont.bca, handler=sgui$hand.select.attributes)
sgui$lbl_selVariables <- glabel("No data", cont=sgui$cont.bcb)


sgui$btn_ppScaled     <- gbutton("Select variables to scale...   ",    cont=sgui$cont.caa, handler=sgui$hand.select.scaled)
sgui$lbl_ppScaled     <- glabel("No data", cont=sgui$cont.cab)

sgui$btn_ppBinarized  <- gbutton("Select variables to binarize...", cont=sgui$cont.cba, handler=sgui$hand.select.binarized)
sgui$lbl_ppBinarized  <- glabel("No data", cont=sgui$cont.cbb)

sgui$btn_printSelections <- gbutton("Print selections", cont=sgui$cont.da, handler=sgui$hand.printSelections)


