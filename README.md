# Scatter-R

Scatter-R is an implementation of Scatter-algorithm in [R](https://r-project.org). It is an algorithm to evaluate variables and dataset learnability for machine learning.

## Description

This is an overview of the software. You can find more information about the algorithm from the papers listed in "References and further reading" -section.

Scatter is an algorithm useful for determining if the dataset or parts of it has such information that can be successfully used for classification. [1] It can also used in dimensionality reduction (See e.g. Saarikoski et al. [2]).

It works simply by traversing the dataset from a randomly chosen starting point to the closest neighbour and recording the class label. In this way a *collection vector* is produced and the *label change count* is calculated from the collection vector. Then, scatter value is calculated as a proportion of the label changes `v` and theoretical maximum number of label changes `w`, thus the equation for Scatter value `S = v / w`. [1]

Scatter value is also used to calculate *separation power*, which is the difference between random situation, i.e. the labels of the current dataset is randomly distributed, and the current situation. The equation for separation power is `F = z - s`, where `z` is the scatter value for random situation.[1]

## Installation

Follow these instructions to install Scatter-project. If you know, what you're doing, you can choose the installation method of your liking.

### ScatterR-package

Follow these steps to install the Scatter-R package. The name of the package is ScatterR even though the repository and project name has dash in it. This is because R doesn't allow dashes in the name of the package.

Go to the "Releases"-section and download latest release. Then, install the package using following command in R-console.

```R
install.packages("https://github.com/Tommytronic/Scatter-R/releases/download/v1.4/ScatterR_1.4_release.tar.gz", repos = NULL, method = "libcurl")
```

#### Test the installation

Test the installation using following commands.

```R
library(ScatterR)
run(iris)
```

The above example runs the usecase `single` for the whole iris dataset. The iris dataset is part of R and you don't need to get it elsewhere.

After the `run` is completed, it prints out some information about the results. More information about these on Usage-section.

### GUI for ScatterR

If you wish to use GUI, see the original project [here](https://github.com/Tommytronic/Scatter-R)

## Usage

The main entry point to algorithm is `run()`-function. It aggregates all the steps needed to calculate various values. It's signature is as follows.

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

**data** is a `data.frame` containing the data.

**classlabel** is the index of the column, which contains the class label. If the class label column is the last one, this can be omitted.

**distanceMethod** is the method to calculate the distance matrix. It is used to traverse the dataset in *nearest neighbor* -sense. Currently, there is Euclidean, Manhattan and HEOM distances available. The HEOM method is currently very slow. Use the names of these distances in lower-case, i.e. "euclidean" for Euclidean distance.

**usecase** is an indicator whether the Scatter value should be calculated for (1) whole dataset, (2) for each class separately or (3) for each variable separately, or (4) all. Value "all" which means that the classwise scatter is calculated for each variable. Possible values for this parameter is `single`, `classes`, `variables` or `all`. These strings are matched exactly.

**iterations** is the number of iterations the Scatter calculation runs. Since the starting point is selected randomly, the algorithm must be run multiple times. This is different from the number of baseline iteartions.

**baselineIterations** is the number of baseline iterations.

**classes** and **columns** are used to select which classes and / or variables are used in calculations.

**nominal** is not used at the moment (to be removed).

In simplest case, using for example popular Iris [3] dataset, the command needed to run the algorithm, is following (assuming the package is already loaded).

```
> run(iris, classlabel = 5)
```

This runs the algorithm 10 iterations and 50 iterations for baseline (see the signature of the function for default values). It outputs the following values (plus the information, about what currently running etc).

- Scatter values for all iterations
- Mean of above values
- Standard deviation of above values
- One collection vector
- Statistical baseline value, which is the mean of all basesline iterations

Please read the articles below to understand how to interpret these values.

The results vary since the randomness of choosing starting points etc., but you should get results, that are between `0.06` and `0.08` for mean and for baseline around `0.65`.

## Further reading and references

[1] Juhola, M., & Siermala, M. (2012). A scatter method for data and variable importance evaluation. Integrated Computer-Aided Engineering, 19 (2), 137–149. http://doi.org/10.3233/ICA-2011-0385

[2] Saarikoski, J., Laurikkala, J., Järvelin, K., Siermala, M., & Juhola, M. (2014). Dimensionality reduction in text classification using scatter method. International Journal of Data Mining, Modelling and Management, 6 (1), 1. http://doi.org/10.1504/IJDMMM.2014.059978

[3] TBD:

[4] Juhola, M., & Grönfors, T. (2014). Variable Importance Evaluation for Machine Learning Tasks. In Encyclopedia of Information Science and Technology, Third Edition (pp. 306–313). IGI Global. http://doi.org/10.4018/978-1-4666-5888-2.ch029

[5] Sebastian Kopf. Github. Retrieved 12.01.2016. Available at https://gist.github.com/sebkopf/9405675
