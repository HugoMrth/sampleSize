\name{SScohort}
\alias{SScohort}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Cohort study sample size calculation
}
\description{
Compute the recquired person-year for a cohort study along with the total number of subjects (exposed and unexposed) over the whole period.
}
\usage{
  SScohort(incidence.exp, incidence.nexp = NULL,
              prop.treatment, risk.factor = NULL,
              time,
              alpha = 0.05, power = 0.80)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{incidence.exp}{
    Expected incidence over the exposed group.
}
  \item{incidence.nexp}{
Expected incidence over the unexposed group. May be ignored if \code{risk.factor} is filled in.
}
  \item{prop.treatment}{
    Proportion of treated patients.
}
  \item{fact.risque}{
Risk factor associated to unexposed patients. Used only if \code{incidence..exp = NULL}
}
  \item{time}{
    Study time, no specific unit
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
A list containing the total number of subject over the study period (\code{N.time}), the number of person-year (\code{N}),
the total number of exposed and unexposed patients (\code{N.exp} and \code{N.nexp}). 
Every number is rounded up.
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
  \code{\link{SSdescriptive}}
}
\examples{
SScohort(incidence.exp = 2/1000, risk.factor = 3,
            prop.treatment = 0.3, time = 5)
}
