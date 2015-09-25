#
# Data preprocessing functions for Scatter-R project
#

# Dependency: DiscriMiner package
#~ install.package("DiscriMiner")
#~ require(DiscriMiner)



# Scale data frame to range 0..1
	unit_map <- function(x) { return(as.data.frame(lapply(x, function(x) { (x - min (x, na.rm = TRUE)) / (max(x,na.rm=TRUE) - min(x, na.rm=TRUE))} ))) }

# Binarize variables with Discriminer/Binarize
	
