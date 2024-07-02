\name{SScohort}
\alias{SScohort}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Calcul le nombre de sujets necessaires pour une etude de cohorte.
}
\description{
  Retourne le nombre de patients-annees necessaires pour une etude de cohorte,
  ainsi que le nombre total de patients (exposes et non-exposes) sur la periode de l'etude.
}
\usage{
  SScohort(incidence.exp, incidence.non.exp = NULL,
              prop.traitement, fact.risque = NULL,
              temps.etude,
              alpha = 0.05, power = 0.80)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{incidence.exp}{
    Incidence attendue chez les patients exposes.
}
  \item{incidence.non.exp}{
    Incidence attendue chez les patients non-exposes. Peut-etre ignore au profit de \code{fact.risque}.
}
  \item{prop.traitement}{
    Proportion des patients recevant le traitement.
}
  \item{fact.risque}{
    Facteur de risque associe aux patient non-exposes, utilise si \code{incidence.non.exp = NULL} (defaut).
}
  \item{temps.etude}{
    Temps de l'etude (sans unite).
}
  \item{alpha}{
    Risque de premiere espece. Par defaut 0.05.
}
  \item{power}{
    Puissance desiree. Par defaut 0.80.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
    Une liste de nombre de sujets. \code{N.temps} le nombre de patients sur la duree de l'etude, \code{N} le nombre de patients annees.
    \code{N.exp} et \code{N.non.exp} le nombre total de patients exposes et non-exposes (\code{N.exp + N.non.exp = N.temps}).
    Toutes ces valeurs sont des entier arrondis au superieur depuis le NSN decimal calcule.
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
SScohort(incidence.exp = 2/1000, fact.risque = 3,
            prop.traitement = 0.3, temps.etude = 5)
}
