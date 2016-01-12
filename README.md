# Scatter-R

Scatter-R is an implementation of Scatter-algorithm in [R](https://r-project.org).

## Description

This is an overview of the project. You can find more information about the algorithm from the papers listed in "References and further reading" -section. Detailed description about this project can be found from the project [wiki](https://github.com/Tommytronic/Scatter-R/wiki).

Scatter is an algorithm useful to determine if the dataset or parts of it has such information that can be successfully used for classification and class prediction. [1] It can also used in dimensionality reduction as shown by Saarikoski et al. in [2].

It works simply by traversing the dataset from a randomly chosen starting case to always closest neighbour recording the class label. In this way a *collection vector* is produced and the *label change count* is calculated from the collection vector. Then, scatter value is calculated as a proportion of the label changes `v` and theoretical maximum number of label changes `w`, thus the equation for Scatter value `S = v / w`. [1]

Scatter value is also used to calculate *separation power*, which is the difference between random situation, i.e. the labels of the current dataset is randomly distributed, and the current situation. The equation for separation power is `F = z - s`, where `z` is the scatter value for random situation.[1]

## Installation

### ScatterR-package
 
This package is not currently in CRAN repositories, so it's installation is not as simple as it could be. Follow these steps to install the Scatter-R package, which contains the algorithm itself and command line interface to it. Installation of the GUI package is described.

The name of the package is ScatterR since the R doesn't support dashes in the name of the package.

Go to the [Releases](https://github.com/Tommytronic/Scatter-R/releases) page of the project and find out, which is the most recent release. Under the heading "Downloads" you will find source archives and at least one archive, which name ends with `_release`. Copy it's URL to clipboard since you will need it.
 
Install the package using following commands.

> Note, that the URL may vary depending on the release. In general, the package name ends with `_release`.

#### Install the package.

Install the package by issuing following command in R console.

```
install.packages("https://github.com/Tommytronic/Scatter-R/releases/download/v0.9/ScatterR_0.9_release.tar.gz", repos = NULL, method = "libcurl")
```

#### Test the installation

After the installation has completed without errors, it's time to test the installation.

Issue following commands.

```R
library(ScatterR)
run(iris)
```

The `library`-command loads and attaches the ScatterR package to the search path and it's ready to be used.

The `run`-command runs the usecase `single` for the whole iris dataset. The iris dataset is part of R and you don't need to get and install it.

After the `run` is completed, it prints out some information about the results. More information about these on Usage-section.

### GUI for ScatterR

Not everyone needs or wants GUI. We have separated GUI from this package and those who needs or wants it, can install it as a separate package. The GUI and these same instructions can be found [here](https://github.com/jmpaon/scatterr-gui). 

Our GUI is written using gWidgets2 and GTK, so you need to ensure those are installed too.

## Usage

The main entry point to algorithm is `run()`-function. It aggregates all the steps needed to calculate different values. It's signature is the following.

```R
run <- function(
    data,
    classlabel  = NULL,
    distanceMethod  = "euclidean",
    usecase     = "single",
    iterations  = 10,
    baselineIterations = 50,
    classes     = NULL,
    columns     = NULL,
    nominals    = NULL)
```

**data** is a `dataframe`, from which user wishes to calculate Scatter etc. values from. It must be `dataframe`.

**classlabel** is the index of the column, which contains the class label.

**distanceMethod** is the method to calculate the distance matrix, which is used to traverse the dataset in nearest neighbour -sense. Currently there is euclidean, manhattan and heom available. The heom method is currently very slow, so don't use it if you're in hurry.

**usecase** is parameter, which indicates, if the Scatter value should be calculated for whole dataset, for each class separately or for each variable separately, or all, which means that the classwise scatter is calculated for each variable. Possible values for this parameter is `single`, `classes`, `variables` or `all`. These strings are matched exactly.

**iterations** controls the number of iterationns the Scatter calculation runs. Since the starting point is selected randomly, the algorithm must be run multiple times. This is separate from the baseline iteartions.

**baselineIterations:** This controls the number of baseline iterations.

**classes** and **columns** parameters are used to select which classes and / or variables are used in calculations.

**nominal** is not used at the moment.

In simplest case, using for example popular Iris [3] dataset, the command needed to run the algorithm, is following (assuming the package is already loaded).

```
> run(iris, classlabel = 5)
```

> If the class label column is the last one, classlabel-parameter can be omitted.

This runs the algorithm 10 iterations and 50 iterations for baseline. It outputs the following values (plus the information, that is currently running etc).

- Scatter values for all iterations
- Mean of above values
- Standard deviation of above values
- One collection vector
- Statistical baseline value, which is the mean of all basesline iterations

Please read the articles below to understand how to interpret these values.

Things to note
--------------

There exists also GUI for this projecet. Check out [GUI for Scatter-R](https://github.com/jmpaon/scatterr-gui). If you wish to use the GUI, please install this (Scatter-R) before installing GUI.

Further reading and references
------------------------------

[1] Juhola, M., & Siermala, M. (2012). A scatter method for data and variable importance evaluation. Integrated Computer-Aided Engineering, 19 (2), 137–149. http://doi.org/10.3233/ICA-2011-0385

[2] Saarikoski, J., Laurikkala, J., Järvelin, K., Siermala, M., & Juhola, M. (2014). Dimensionality reduction in text classification using scatter method. International Journal of Data Mining, Modelling and Management, 6 (1), 1. http://doi.org/10.1504/IJDMMM.2014.059978

[3] TBD:

[4] Juhola, M., & Grönfors, T. (2014). Variable Importance Evaluation for Machine Learning Tasks. In Encyclopedia of Information Science and Technology, Third Edition (pp. 306–313). IGI Global. http://doi.org/10.4018/978-1-4666-5888-2.ch029
