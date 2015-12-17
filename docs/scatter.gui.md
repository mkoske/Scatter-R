# ```scatter.gui```

## Purpose and general description

```scatter.gui``` function summons a graphical user interface
to assist in using the **scatter** algorithm
and the associated preprocessing function ```scatter.preprocess```.
```scatter.gui``` provides an easy-to-use graphical interface for
reading data files, 
making selections related to preprocessing
and the **scatter** algorithm calculation,
calling the ```scatter.preprocess``` and ```scatter``` functions 
with the selected arguments
and ultimately saving the results.

## Dependencies

The Scatter GUI requires the ```gWidgets2RGtk2``` package.
It can be installed with command  ```install.packages("gWidgets2RGtk2")```.


## Calling the GUI

```scatter.gui()```  
Opens the GUI main window.

## Using the GUI

In the following, the GUI elements and their functionality 
is described, starting from the top-most elements.

- **Read CSV datafile...** button
- **Select class variable...** button
- **Select included classes...** button
- **Select included variables...** button
- **Select variables to scale...** button
- **Select variables to binarize...** button
- **Handling of missing values** radio button set
	- 


