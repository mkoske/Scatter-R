\name{heom}
\alias{heom}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
heom(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
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
function (data) 
{
    ncols <- ncol(data) - 1
    data <- data[, 1:ncols]
    factors <- sapply(data, is.factor)
    numerics <- sapply(data, is.numeric)
    ranges <- apply(data[, numerics], 2, max, na.rm = T) - apply(data[, 
        numerics], 2, min, na.rm = T)
    data <- sapply(data, as.numeric)
    dlen <- nrow(data)
    result <- matrix(nrow = nrow(data), ncol = nrow(data))
    row <- vector(mode = "numeric", length = ncol(data))
    for (i in 1:dlen) {
        for (j in 1:dlen) {
            if (!is.na(result[i, j])) 
                next
            row[factors] <- !(data[i, factors] == data[j, factors])
            row[numerics] <- (data[i, numerics] - data[j, numerics])/ranges
            row[is.na(row)] <- 1
            ssum <- sum(row^2)
            result[i, j] <- ssum
            result[j, i] <- ssum
        }
    }
    return(sqrt(result))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
