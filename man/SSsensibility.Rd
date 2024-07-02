\name{SSsensibility}
\alias{SSsensibility}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Calcul le nombre de sujets necessaires pour des etudes de sensibilite/specificite
}
\description{
  Retourne le nombre de sujets necessaire pour une etude de sensibilite/specificite.
}
\usage{
  SSsensibility(sensi = NULL, speci = NULL, prevalence = NULL,
                       dropout = 0.1,
                       alpha = 0.05, precis = 0.1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sensi}{
    Estimation de la sensibilite.
}
  \item{speci}{
    Estimation de la specificite.
}
  \item{prevalence}{
    Estimation de la prevalence.
}
  \item{dropout}{
    Taux de perdu de vue. Par defaut/convention \code{dropout = 0.1}.
}
  \item{alpha}{
    Risque de premiere espece. Par defaut/convention \code{alpha = 0.05}.
}
  \item{precis}{
    Précision de l'intervalle de confiance. Par defaut/convention \code{precis = 0.1}.
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
