\name{traverse}
\alias{traverse}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
traverse(df, distm)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
%%     ~~Describe \code{df} here~~
}
  \item{distm}{
%%     ~~Describe \code{distm} here~~
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
function (df, distm) 
{
    ncols <- ncol(df) - 1
    nrows <- nrow(df)
    lbls <- vector(mode = "character")
    df$Visited = F
    diag(distm) <- NA
    currentIdx <- sample(1:nrows, size = 1)
    while (nrow(df[df$Visited == F, ]) > 0) {
        if (all(is.na(distm))) {
            lbls <- c(lbls, df[currentIdx, (ncols + 1)])
            break
        }
        lbls <- c(lbls, df[currentIdx, (ncols + 1)])
        df[currentIdx, "Visited"] <- T
        minima <- min(distm[currentIdx, ], na.rm = T)
        minimas <- which(distm[currentIdx, ] == minima)
        distm[currentIdx, ] <- NA
        distm[, currentIdx] <- NA
        if (length(minimas) > 1) {
            nearest <- sample(minimas, size = 1)
        }
        else {
            nearest <- minimas[1]
        }
        currentIdx <- nearest
    }
    return(lbls)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
