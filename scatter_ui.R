require(tcltk)
scatter_ui <- tktoplevel()

Close.button <- tkbutton(scatter_ui, text = "Close window", command = function() tkdestroy(scatter_ui))

tkgrid(Close.button)
