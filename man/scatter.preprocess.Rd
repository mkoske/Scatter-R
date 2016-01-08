\name{scatter.preprocess}
\alias{scatter.preprocess}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
scatter.preprocess(df, classvar, included.attributes = NULL, included.classes = NULL, binarized = NULL, scaled = NULL, na.action = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
%%     ~~Describe \code{df} here~~
}
  \item{classvar}{
%%     ~~Describe \code{classvar} here~~
}
  \item{included.attributes}{
%%     ~~Describe \code{included.attributes} here~~
}
  \item{included.classes}{
%%     ~~Describe \code{included.classes} here~~
}
  \item{binarized}{
%%     ~~Describe \code{binarized} here~~
}
  \item{scaled}{
%%     ~~Describe \code{scaled} here~~
}
  \item{na.action}{
%%     ~~Describe \code{na.action} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (df, classvar, included.attributes = NULL, included.classes = NULL, 
    binarized = NULL, scaled = NULL, na.action = NULL) 
{
    remove_unselected_attributes_and_classes <- function(df, 
        classvar, included.attributes = NULL, included.classes = NULL) {
        if (!is.null(included.attributes)) {
            df <- df[included.attributes]
        }
        if (!is.null(included.classes)) {
            df <- df[which(df[[classvar]] \%in\% included.classes), 
                ]
        }
        return(df)
    }
    handle_missing_values <- function(df, classvar, action = "class") {
        remove_rows_with_missing_classvar <- function(df, classvar) {
            df[!is.na(df[classvar]), ]
        }
        estimate_by_class <- function(df, classvar) {
            input_class_median_or_mode <- function(vars, classes, 
                replace.with = "median") {
                for (i in 1:length(vars)) {
                  if (is.na(vars[i])) {
                    cls <- classes[i]
                    if (replace.with == "median") 
                      vars[i] <- median(vars[classes == cls], 
                        na.rm = TRUE)
                    if (replace.with == "mode") 
                      vars[i] <- names(sort(-table(vars[classes == 
                        cls])))[1]
                    if (replace.with == "mean") 
                      vars[i] <- mean(vars[classes == cls], na.rm = TRUE)
                  }
                }
                return(vars)
            }
            numericCols <- sapply(df, is.numeric)
            for (i in 1:ncol(df)) {
                if (numericCols[i]) {
                  df[i] <- input_class_median_or_mode(df[[i]], 
                    df[[classvar]], "median")
                }
                else {
                  df[i] <- input_class_median_or_mode(df[[i]], 
                    df[[classvar]], "mode")
                }
            }
            return(estimate_by_column(df))
        }
        estimate_by_column <- function(df) {
            numericCols <- sapply(df, is.numeric)
            for (i in 1:ncol(df)) {
                if (numericCols[i]) {
                  m <- median(df[[i]], na.rm = TRUE)
                }
                else {
                  m <- names(sort(-table(df[[i]])))[1]
                }
                df[[i]][is.na(df[[i]])] <- m
            }
            return(df)
        }
        df <- remove_rows_with_missing_classvar(df, classvar)
        if (is.null(action)) 
            action = "class"
        if (action == "rmrows") 
            df <- na.omit(df)
        if (action == "class") 
            df <- estimate_by_class(df, classvar)
        if (action == "column") 
            df <- estimate_by_column(df)
        return(df)
    }
    binarize_and_scale <- function(df, binarized = NULL, scaled = NULL) {
        my_binarize <- function(df) {
            if (!is.data.frame(df)) 
                stop("df must be a data frame")
            if (!all(sapply(df, is.factor))) 
                stop("df must be a data frame containning only factors")
            if (!all(complete.cases(df))) 
                stop("missing values are not allowed")
            binarizedNames <- function(df) {
                return(unlist(Map(paste, colnames(df), lapply(df, 
                  levels), MoreArgs = list(sep = ":")), use.names = FALSE))
            }
            binNames <- binarizedNames(df)
            nrows <- nrow(df)
            ncols <- ncol(df)
            categories <- sapply(df, nlevels)
            categoriesCount <- sum(categories)
            bdf <- matrix(0, nrows, categoriesCount)
            i1 <- cumsum(categories) - categories + 1
            i2 <- cumsum(categories)
            for (i in 1:ncols) {
                lev <- levels(df[, i])
                m <- matrix(0, nrows, categories[i])
                for (ii in 1:categories[i]) {
                  temp <- df[, i] == lev[ii]
                  m[temp, ii] <- 1
                }
                bdf[, i1[i]:i2[i]] <- m
            }
            colnames(bdf) <- binNames
            return(as.data.frame(bdf))
        }
        appendMerge <- function(df1, df2) {
            if (ncol(df1) == 0 && ncol(df2) == 0) 
                return(df1)
            if (ncol(df1) == 0) 
                return(df2)
            if (ncol(df2) == 0) 
                return(df1)
            df_new <- df1
            for (ci in 1:ncol(df2)) {
                df_new[, ncol(df_new) + 1] <- df2[, ci]
                colnames(df_new)[ncol(df_new)] <- colnames(df2)[ci]
            }
            return(df_new)
        }
        binarize.df <- function(df) {
            if (is.null(df)) 
                return
            if (ncol(df) == 0) 
                return(df)
            fdf <- as.data.frame((sapply(df, as.factor)))
            bdf <- my_binarize(fdf)
            return(bdf)
        }
        unitmap.df <- function(df) {
            if (is.null(df)) 
                return
            if (ncol(df) == 0) 
                return(df)
            return(as.data.frame(lapply(df, function(x) {
                (x - min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - 
                  min(x, na.rm = TRUE))
            })))
        }
        if (is.null(binarized) && is.null(scaled)) 
            return(df)
        bin.df <- df[binarized]
        sca.df <- df[scaled]
        nop.df <- df[setdiff(colnames(df), union(binarized, scaled))]
        bin.df <- binarize.df(bin.df)
        sca.df <- unitmap.df(sca.df)
        mrg.df <- appendMerge(appendMerge(sca.df, bin.df), nop.df)
        return(mrg.df)
    }
    preprocess_selection_vector <- function(v, allowed_values) {
        if (is.null(v)) 
            return(v)
        vname = deparse(substitute(v))
        if (!is.vector(v)) 
            stop(paste(vname, "is not of allowed type: try passing a vector"))
        if (is.numeric(v)) {
            if (!all(v \%in\% 1:length(allowed_values))) 
                stop(paste("non-existing indices selected in", 
                  vname))
            v <- allowed_values[v]
        }
        else {
            if (!all(v \%in\% allowed_values)) 
                stop(paste("non-existing names selected in", 
                  vname))
        }
        return(v)
    }
    if (!is.data.frame(df)) 
        stop("df is not a dataframe")
    if (ncol(df) < 2) 
        stop("df has less than two columns")
    if (nrow(df) < 2) 
        stop("df has less than two rows")
    if (is.numeric(classvar)) {
        if (!classvar \%in\% 1:ncol(df)) 
            stop("non-existing classvar index selected")
        classvar <- colnames(df)[classvar]
    }
    else {
        if (!classvar \%in\% colnames(df)) 
            stop("non-existing classvar name selected")
    }
    if (!hasArg(included.attributes)) 
        included.attributes <- colnames(df)
    if (!hasArg(included.classes)) 
        included.classes <- levels(as.factor(df[[classvar]]))
    if (!hasArg(binarized)) 
        binarized <- included.attributes[sapply(df[included.attributes], 
            is.factor) & included.attributes != classvar]
    if (!hasArg(scaled)) 
        scaled <- included.attributes[sapply(df[included.attributes], 
            is.numeric) & included.attributes != classvar]
    if (!hasArg(na.action)) 
        na.action <- "class"
    if (is.null(included.attributes)) 
        stop("NULL passed for included attributes: no attributes selected")
    if (is.null(included.classes)) 
        stop("NULL passed for included classes: no classes selected")
    if (is.null(na.action)) 
        na.action <- "nothing"
    included.attributes <- preprocess_selection_vector(included.attributes, 
        colnames(df))
    included.classes <- preprocess_selection_vector(included.classes, 
        levels(as.factor(df[[classvar]])))
    scaled <- preprocess_selection_vector(scaled, colnames(df))
    binarized <- preprocess_selection_vector(binarized, colnames(df))
    if (any(binarized \%in\% scaled)) 
        stop("attributes selected for both binarization and scaling")
    if (!is.null(included.attributes)) 
        if (!classvar \%in\% included.attributes) 
            included.attributes <- append(included.attributes, 
                classvar)
    binarized <- binarized[binarized != classvar]
    scaled <- scaled[scaled != classvar]
    if (!(na.action \%in\% c("class", "column", "rmrows", "", "nothing"))) 
        stop("na.action is not supported. Try 'class','column','rmrows','nothing'")
    df <- remove_unselected_attributes_and_classes(df, classvar, 
        included.attributes, included.classes)
    df <- handle_missing_values(df, classvar, action = na.action)
    df <- binarize_and_scale(df, binarized, scaled)
    return(df)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
