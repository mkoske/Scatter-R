\name{usecase.variable}
\alias{usecase.variable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
usecase.variable(data, distanceMethod = "euclidean", iterations = 10, nominal = c(), baselineIterations = 50)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{distanceMethod}{
%%     ~~Describe \code{distanceMethod} here~~
}
  \item{iterations}{
%%     ~~Describe \code{iterations} here~~
}
  \item{nominal}{
%%     ~~Describe \code{nominal} here~~
}
  \item{baselineIterations}{
%%     ~~Describe \code{baselineIterations} here~~
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
function (data, distanceMethod = "euclidean", iterations = 10, 
    nominal = c(), baselineIterations = 50) 
{
    variables <- ncol(data) - 1
    result <- matrix(nrow = variables, ncol = iterations)
    baselines <- vector(mode = "numeric")
    collectionVector <- vector(mode = "numeric", length = nrow(data))
    for (variable in 1:variables) {
        distanceMatrix <- distance(as.data.frame(data[, variable]), 
            distanceMethod, nominal)
        for (i in 1:iterations) {
            print(sprintf("Running iteration \%s for variable \%s...", 
                i, variable))
            collectionVector <- traverse(data, distanceMatrix)
            result[variable, i] <- scatter(collectionVector)
        }
        baselines <- c(baselines, baseline(data$class, baselineIterations))
    }
    means <- apply(result, 1, mean)
    return(list(values = result, means = means, baselines = baselines))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
