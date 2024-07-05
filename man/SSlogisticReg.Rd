\name{SSlogisticReg}
\alias{SSlogisticReg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Sample size determination for logistic regression
}
\description{
Compute the necessary sample size for a logistic regression analysis. This is based on, and improves the \code{wp.logistic} function from the \code{WebPower} package.
}
\usage{
  SSlogisticReg(p0, p1,
             alpha = 0.05, power = 0.80,
             alternative = c("two.sided", "less", "greater"),
             family = c("Bernoulli", "exponential", "lognormal", "normal", "Poisson", "uniform"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p0}{
Estimated probability of Y=1 when X=0.
}
  \item{p1}{
Estimated probability of Y=1 when X=1.
}
  \item{alpha}{
    Significance level : Type I Error. Default is 0.05.
}
  \item{power}{
    Statistical power : 1 - Type II Error. Default is 0.80.
}
  \item{alternative}{
The alternative hypothesis. Default is \code{alternative = "two.sided"}. Other options are \code{alternative = "less"} are \code{alternative = "greater"}.
}
  \item{family}{
    Distribution of the predictor ("Bernoulli","exponential", "lognormal", "normal", "Poisson", "uniform"). 
Default is "Bernoulli".
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
  \code{\link{wp.logistic}}
}
\examples{
SSlogisticReg(0.75, 0.85)
SSlogisticReg(0.75, 0.85, family = "normal")
}
