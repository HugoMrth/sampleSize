\name{SSmean}
\alias{SSmean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sample size determination to underline a difference in means
}
\description{
Compute the necessary sample size to underline a difference in means. This is based on, and improves the \code{pwr.t.test} function from the \code{pwr} package.
}
\usage{
  SSmean(m1, m2,
              sd1, sd2,
              sd.global = NULL,
              alpha = 0.05, power = 0.8,
              type = c("two.sample", "one.sample", "paired"),
              alternative = c("two.sided", "less", "greater"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{m}{
    Arguments \code{m1} et \code{m2} correspond to the estimated means in the reference and the studied populations, or in the two studied populations.
Those can be use interchangeably, but must be consistent with \code{sd1} and \code{sd2}. 

}
  \item{sd}{
Arguments \code{sd1} et \code{sd2} correspond to the estimated standard deviation in the reference and the studied populations, or in the two studied populations.
Those can be use interchangeably, but must be consistent with \code{m1} and \code{m2}. 
}
  \item{alpha}{
    Significance level : Type I Error. Default is 0.05.
}
  \item{power}{
    Statistical power : 1 - Type II Error. Default is 0.80.
}
  \item{type}{
    Type of Student test to use, default is \code{type = "two.sample"}.
    Other options are \code{type = "less"} and \code{type = "greater"}.
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
  \code{\link{pwr.t.test}}
}
\examples{
SSmean(m1 = 2350, m2 = 1850,
            sd1 = 250, sd2 = 400)
}
