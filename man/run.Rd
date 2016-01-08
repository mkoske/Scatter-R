\name{run}
\alias{run}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
run(data, classlabel = NULL, distanceMethod = "euclidean", usecase = "single", iterations = 10, baselineIterations = 50, classes = NULL, columns = NULL, nominals = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{classlabel}{
%%     ~~Describe \code{classlabel} here~~
}
  \item{distanceMethod}{
%%     ~~Describe \code{distanceMethod} here~~
}
  \item{usecase}{
%%     ~~Describe \code{usecase} here~~
}
  \item{iterations}{
%%     ~~Describe \code{iterations} here~~
}
  \item{baselineIterations}{
%%     ~~Describe \code{baselineIterations} here~~
}
  \item{classes}{
%%     ~~Describe \code{classes} here~~
}
  \item{columns}{
%%     ~~Describe \code{columns} here~~
}
  \item{nominals}{
%%     ~~Describe \code{nominals} here~~
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
function (data, classlabel = NULL, distanceMethod = "euclidean", 
    usecase = "single", iterations = 10, baselineIterations = 50, 
    classes = NULL, columns = NULL, nominals = NULL) 
{
    if (!is.data.frame(data)) 
        stop("Input data must be a data frame.")
    if (length(classes) > 0) 
        data <- data[data[, classlabel] \%in\% classes, ]
    class_labels <- data[, classlabel]
    data[, classlabel] <- NULL
    if (length(columns) > 0) 
        data <- data[, columns]
    data$class <- class_labels
    result <- NULL
    if (usecase == "single") {
        result <- usecase.single(data, distanceMethod, iterations, 
            nominal, baselineIterations)
    }
    else if (usecase == "classes") {
        distanceMatrix = distance(data, distanceMethod, nominal)
        result <- usecase.class(data, distanceMatrix, iterations, 
            nominal, baselineIterations)
    }
    else if (usecase == "variables") {
        result <- usecase.variable(data, distanceMethod, iterations, 
            nominal, baselineIterations)
    }
    else if (usecase == "all") {
        result <- usecase.all(data, distanceMethod, iterations, 
            nominal, baselineIterations)
    }
    else {
        stop("Unknown usecase. Must be one of following: single, classes, variables or all.")
    }
    return(result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
