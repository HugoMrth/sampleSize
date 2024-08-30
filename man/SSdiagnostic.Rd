\name{SSdiagnostic}
\alias{SSdiagnostic}
\alias{printDiagnosticTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Sample size for  accuracy in diagnostic 
}
\description{
Compute the necessary sample size for design accuracy in diagnostic test studies.
}
\usage{
  printDiagnosticTable(prevalence)
  
  SSdiagnostic(prevalence, sens_speci, min_ci,
              alpha = 0.05, power = 0.8,
              n_cases = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{prevalence}{
  Estimated prevalence
}
  \item{sens_speci}{
    Estimated sensibility or specificity
}
  \item{min_ci}{
Minimal acceptable lower confidence limit
}
  \item{alpha}{
    Type I Error. Default is 0.05.
}
  \item{power}{
    1 - Type II Error. Default is 0.80.
}
  \item{n_cases}{
Number of cases estimated from \code{printDiagnosticTable}.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list containing the total number of subject along with the number of cases and number of control.
}
\references{
Flahault A, Cadilhac M, Thomas G. Sample size calculation should be performed for design accuracy in diagnostic test studies. Journal of Clinical Epidemiology. 1 août 2005;58(8):859‑62. 
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
printDiagnosticTable(-1)
printDiagnosticTable(0.8)
printDiagnosticTable(0.2)

  
SSdiagnostic(prevalence = 0.2)
SSdiagnostic(prevalence = 0.1, n_cases = 70)
SSdiagnostic(prevalence = 0.2, n_cases = 235)
}
