# require(tcltk)
require(gWidgets2)
require(gWidgets2RGtk2)
options(guiToolkit="RGtk2")

sgui       <- new.env()

# AVAILABLE OPTIONS
## Distance measures
sgui$opt.distmethod <- c("Euclidean", "Manhattan", "Heom")
## Treatment of missing values
sgui$opt.missing    <- c("Do nothing", "Replace with class median/mode", "Replace with column median/mode", "Remove rows")


# OTHER FILES
source("scatter_gui_handlers.R")
source("scatter_preprocess.R")
source("algorithm/scatter.R")



# MAIN WINDOW AND CONTAINER
sgui$winMain <- gwindow(title="Scatter GUI")
sgui$cont <- ggroup(container=sgui$winMain, horizontal=FALSE, spacing=10, use.scrollwindow=TRUE)
size(sgui$winMain) <- c(600, 500)


# CONTAINERS
sgui$cont.a   <- ggroup(horizontal=FALSE,  cont=sgui$cont)
sgui$cont.aa  <- ggroup(horizontal=TRUE,   cont=sgui$cont.a)
sgui$cont.aaa <- ggroup(horizontal=TRUE,   cont=sgui$cont.aa, expand=TRUE, fill=TRUE)
sgui$cont.ab  <- ggroup(horizontal=FALSE,  cont=sgui$cont.a)

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

sgui$cont.cc   <- ggroup(horizontal=TRUE,  cont=sgui$cont.c)
sgui$cont.cca  <- ggroup(horizontal=FALSE, cont=sgui$cont.cc)
sgui$cont.ccb  <- ggroup(horizontal=FALSE, cont=sgui$cont.cc)

sgui$cont.d   <- ggroup(horizontal=FALSE, cont=sgui$cont)
sgui$lbl_sectOtherOptions <- glabel("Other options", cont=sgui$cont.d)

sgui$cont.da  <- ggroup(horizontal=TRUE, cont=sgui$cont.d)

sgui$cont.db  <- ggroup(horizontal=TRUE,  cont=sgui$cont.d)
sgui$cont.dba <- ggroup(horizontal=FALSE, cont=sgui$cont.db)
sgui$cont.dbb <- ggroup(horizontal=FALSE, cont=sgui$cont.db)
sgui$cont.dbc <- ggroup(horizontal=FALSE, cont=sgui$cont.db)
sgui$cont.dbd <- ggroup(horizontal=FALSE, cont=sgui$cont.db)

sgui$cont.dc  <- ggroup(horizontal=TRUE, cont=sgui$cont.d)




# MAIN WINDOW CONTROLS
sgui$btn_readfile     <- gbutton("Read CSV datafile...",cont=sgui$cont.aaa, handler=sgui$hand.readDatafile, expand=TRUE)

sgui$lbl_datainfo     <- glabel("No file selected", cont=sgui$cont.ab)

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

sgui$lbl_ppMissing    <- glabel("Handling of missing values", cont=sgui$cont.cca)
sgui$rdo_ppMissing    <- gradio(sgui$opt.missing, cont=sgui$cont.ccb)


sgui$btn_printSelections  <- gbutton("Print selections", cont=sgui$cont.da, handler=sgui$hand.printSelections)

sgui$lbl_selectMethod      <- glabel("Select distance measure", cont=sgui$cont.dba)
sgui$rdo_selectMethod      <- gradio(sgui$opt.distmethod, cont=sgui$cont.dba)

sgui$lbl_selectIterations  <- glabel("Select number of iterations", cont=sgui$cont.dbb)
sgui$spn_selectIterations  <- gspinbutton(from=1, to=500, by=1, value=10, cont=sgui$cont.dbb)


sgui$btn_calculate         <- gbutton("Calculate", cont=sgui$cont.dc, handler=sgui$hand.calculate)
