\name{distance}
\alias{distance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
distance(data, distmethod = "euclidean", nominals = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{distmethod}{
%%     ~~Describe \code{distmethod} here~~
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
function (data, distmethod = "euclidean", nominals = NULL) 
{
    if (!is.data.frame(data)) 
        stop("Data must be a data frame type.")
    if (distmethod == "heom") 
        source("algorithm/heom.R")
    result <- switch(distmethod, euclidean = as.matrix(dist(data, 
        method = "euclidean")), manhattan = as.matrix(dist(data, 
        method = "manhattan")), heom = as.matrix(heom(data)), 
        c())
    if (is.null(result)) 
        stop("Invalid distmethod. Must be 'euclidean', 'manhattan' or 'heom'.")
    return(result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
