Scatter-R
=========

Scatter-R is an implementation of Scatter-algorithm in [R](https://r-project.org).

Description
-----------

This is an overview of the project. You can find more information about the algorithm from the papers listed in "References and further reading" -section. Detailed description about this project can be found from the project [wiki](https://github.com/Tommytronic/Scatter-R/wiki).

Scatter is an algorithm useful to determine if the dataset or parts of it has such information that can be successfully used for classification and class prediction. [1] It can also used in dimensionality reduction as shown by Saarikoski et al. in [2].

It works simply by traversing the dataset from a randomly chosen starting case to always closest neighbour recording the class label. In this way a *collection vector* is produced and the *label change count* is calculated from the collection vector. Then, scatter value is calculated as a proportion of the label changes `v` and theoretical maximum number of label changes `w`, thus the equation for Scatter value `S = v / w`. [1]

Scatter value is also used to calculate *separation power*, which is the difference between random situation, i.e. the labels of the current dataset is randomly distributed, and the current situation. The equation for separation power is `F = z - s`, where `z` is the scatter value for random situation.[1]

Further reading and references
------------------------------

[1] Juhola, M., & Siermala, M. (2012). A scatter method for data and variable importance evaluation. Integrated Computer-Aided Engineering, 19 (2), 137–149. http://doi.org/10.3233/ICA-2011-0385

[2] Saarikoski, J., Laurikkala, J., Järvelin, K., Siermala, M., & Juhola, M. (2014). Dimensionality reduction in text classification using scatter method. International Journal of Data Mining, Modelling and Management, 6 (1), 1. http://doi.org/10.1504/IJDMMM.2014.059978
