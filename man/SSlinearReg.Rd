\name{SSlinearReg}
\alias{SSlinearReg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sample size calculation for linear regression
}
\description{
Compute the necessary sample size for a linear regression analysis. This is based on, and improves the \code{pwr.f2.test} function from the \code{pwr} package.
}
\usage{
  SSlinearReg(R, df, mod = NULL,
             alpha = 0.05, power = 0.8)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{
Squared root of the estimate coefficient of determination for the model. 
}
  \item{df}{
Number of degrees of freedom for the model, ie. total number of variables minus one.
}
  \item{mod}{
An \code{lm} object that can be used instead of \code{R} and \code{df}. 
This is unusual as one must most of the time estimate \code{R} and \code{df} from previous studies and do not have direct access to a model.
}
  \item{alpha}{
    Type I Error. Default is 0.05.
}
  \item{power}{
    1 - Type II Error. Default is 0.80.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Sample size recquired as an integer. The true number is rounded up.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
  Hugo Marthinet
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{pwr.f2.test}}
}
\examples{
X <- c(46.8, 48.7, 48.4, 53.7, 56.7)
Y <- c(14.6, 19.6, 18.6, 25.5, 20.4)

mod <- lm(Y ~ X)

SSlinearReg(df = summary(mod)$fstatistic[2],
           R = sqrt(summary(mod)$adj.r.squared))

SSlinearReg(mod)
}
