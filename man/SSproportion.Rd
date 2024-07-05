\name{SSproportion}
\alias{SSproportion}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Sample size determination to underline a difference in proportions
}
\description{
Compute the necessary sample size to underline a difference in proportions. This is based on, and improves the \code{pwr.p.test} and \code{ES.h} functions from the \code{pwr} package.
}
\usage{
  SSproportion(p1, p2,
                 alpha = 0.05, power = 0.8,
                 alternative =  = c("two.sided","less","greater"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{
Arguments \code{p1} et \code{p2} correspond to the estimated prevalence in the reference and the studied populations, or in the two studied populations.
Those can be use interchangeably.
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
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
        Sample size recquired as an integer. The true number is rounded up.
The specified value is the recquirement PER GROUP.
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
  \code{\link{pwr.p.test}}, \code{\link{ES.h}}
}
\examples{
SSproportion(0.20, 0.22)
}
