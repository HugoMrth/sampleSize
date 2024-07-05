\name{SSsensibility}
\alias{SSsensibility}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Sample size determination for a sensibility or specificity study.
}
\description{
  Compute the necessary sample size for a sensibility or specificity study.
}
\usage{
  SSsensibility(sensi = NULL, speci = NULL, prevalence = NULL,
                       dropout = 0.1,
                       alpha = 0.05, precis = 0.1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sensi}{
    Estimated sensibility
}
  \item{speci}{
    Estimated specificity
}
  \item{prevalence}{
    Estimated prevalence
}
  \item{dropout}{
    Expected dropout rate. Default is \code{dropout = 0.1}.
}
  \item{alpha}{
    Significance level : Type I Error. Default is 0.05.
}
  \item{precis}{
    The error margin, the precision of the confidence intervals. Default is \code{precis = 0.1}.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A data frame containing the original parameters along with the sample sizes recquired for a sensibility study, 
a specificity study, and the maximum of the two. Dropout rates are taken into account. Every estimate are rounded up.
}
\references{
Buderer NM. Statistical methodology: I. Incorporating the prevalence of
disease into sample size calculation for sensitivity and specificity. Acad
Emerg Med 1996;3:895–900.
}
\author{
  Hugo Marthinet
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
}
\examples{
SSsensibility(sensi = 0.9, speci = rep(c(0.7, 0.9), each = 4), prevalence = rep(rep(c(0.1, 0.588), each = 2), 2))
}
