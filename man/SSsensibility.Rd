\name{SSsensibility}
\alias{SSsensibility}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Sample size determination for a sensibility or sensitivity study.
}
\description{
  Compute the necessary sample size for a sensibility or sensitivity study.
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
  Un data frame reprenant les parametres d'entree et incluant en plus les nombres de sujets necessaires (NSN)
  pour une etude de sensibilite, de specificite, et le NSN total correspondant au maximum des deux premiers
  après prise en compte du taux de perdus de vue. Tous les nombres de sujets sont arrondis a l'entier superieur.
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
