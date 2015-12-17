Scatter-R
=========

Scatter-R is an implementation of Scatter-algorithm in [R](https://r-project.org).

Description
-----------

This is an overview of the project. You can find more information about the algorithm from the papers listed in "References and further reading" -section. Detailed description about this project can be found from the project [wiki](https://github.com/Tommytronic/Scatter-R/wiki).

Scatter is an algorithm useful to determine if the dataset or parts of it has such information that can be successfully used for classification and class prediction. [1] It can also used in dimensionality reduction as shown by Saarikoski et al. in [2].

It works simply by traversing the dataset from a randomly chosen starting case to always closest neighbour recording the class label. In this way a *collection vector* is produced and the *label change count* is calculated from the collection vector. Then, scatter value is calculated as a proportion of the label changes `v` and theoretical maximum number of label changes `w`, thus the equation for Scatter value `S = v / w`. [1]

Scatter value is also used to calculate *separation power*, which is the difference between random situation, i.e. the labels of the current dataset is randomly distributed, and the current situation. The equation for separation power is `F = z - s`, where `z` is the scatter value for random situation.[1]

Usage
-----

The main entry point to algorithm is `run()`-function. It aggregates all the steps needed to calculate different values. It's signature is the following.

```
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

This runs the algorithm 10 iterations and 50 iterations for baseline. It outputs the following values (plus the information, that is currently running etc).

- Scatter values for all iterations
- Mean of above values
- Standard deviation of above values
- One collection vector
- Statistical baseline value, which is the mean of all basesline iterations

Please read the articles below to understand how to interpret these values.

Further reading and references
------------------------------

[1] Juhola, M., & Siermala, M. (2012). A scatter method for data and variable importance evaluation. Integrated Computer-Aided Engineering, 19 (2), 137–149. http://doi.org/10.3233/ICA-2011-0385

[2] Saarikoski, J., Laurikkala, J., Järvelin, K., Siermala, M., & Juhola, M. (2014). Dimensionality reduction in text classification using scatter method. International Journal of Data Mining, Modelling and Management, 6 (1), 1. http://doi.org/10.1504/IJDMMM.2014.059978

[3] TBD:

[4] Juhola, M., & Grönfors, T. (2014). Variable Importance Evaluation for Machine Learning Tasks. In Encyclopedia of Information Science and Technology, Third Edition (pp. 306–313). IGI Global. http://doi.org/10.4018/978-1-4666-5888-2.ch029
