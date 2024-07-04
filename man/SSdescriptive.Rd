\name{SSdescriptive}
\alias{SSdescriptive}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Sample size calculation for a descriptive study.
}
\description{
  Compute the necessary sample size for a descriptive study. This must only be used if no further analysis is planned. 
In most cases, one must assume that every answers to a dichotomous question throughout the survey will be split perfectly between the two possibilities.
}
\usage{
  SSdescriptive(N.tot, e = 0.05, p = 0.5, alpha = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{N.tot}{
    Estimation de la taille de la population ciblee.
}
  \item{e}{
    Error margin. Default is \code{e = 0.05}.
}
  \item{p}{
    Prevalence in the average questions of the survey, often 0.5 if no other hypothesis can be imposed. Default is \code{p = 0.5} as we assume that the survey will contains several questions.
}
  \item{alpha}{
Type I error. Corresponds to the size of the confindence interval. Default is \code{alpha = 0.05}.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Sample size recquired as an integer. The true number is rounded p.
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
  \code{\link{SScohort}}
}
\examples{
SSdescriptive(3000)
SSdescriptive(10000)
}
