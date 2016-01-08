\name{usecase.single}
\alias{usecase.single}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
usecase.single(data, distanceMethod = "euclidean", iterations = 10, nominal = c(), baselineIterations = 50)
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
    ncols <- ncol(data) - 1
    collectionVector <- vector(length = nrow(data))
    distanceMatrix <- distance(data[, 1:ncols], distanceMethod, 
        nominal)
    values <- vector(length = iterations)
    for (i in 1:iterations) {
        print(sprintf("Running iteration \%s...", i))
        collectionVector <- traverse(data, distanceMatrix)
        values[i] <- scatter(collectionVector)
    }
    baseline <- baseline(collectionVector, baselineIterations)
    return(list(values = values, mean = (sum(values)/iterations), 
        sd = sd(values), collectionVector = collectionVector, 
        baseline = baseline))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
