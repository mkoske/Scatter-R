\name{baseline}
\alias{baseline}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
baseline(labels, iterations = 50)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{labels}{
%%     ~~Describe \code{labels} here~~
}
  \item{iterations}{
%%     ~~Describe \code{iterations} here~~
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
function (labels, iterations = 50) 
{
    print("Calculating baseline...")
    n <- length(labels)
    values <- vector(mode = "numeric", length = iterations)
    for (i in 1:iterations) {
        sample <- sample(labels, size = n)
        values[i] <- scatter(sample)
    }
    return(sum(values)/iterations)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
