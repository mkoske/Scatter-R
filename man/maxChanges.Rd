\name{maxChanges}
\alias{maxChanges}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
maxChanges(labels, current = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{labels}{
%%     ~~Describe \code{labels} here~~
}
  \item{current}{
%%     ~~Describe \code{current} here~~
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
function (labels, current = NULL) 
{
    nmax <- NULL
    max <- NULL
    n <- length(labels)
    sizes <- table(labels)
    if (is.null(current)) {
        max <- max(sizes)
        nmax <- length(as.vector(which(sizes == max)))
    }
    else {
        max <- sizes[[current]]
    }
    if ((nmax == 1) && (max > (n - max))) {
        thmax <- (2 * (n - max))
    }
    else {
        thmax <- (n - 1)
    }
    return(thmax)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
