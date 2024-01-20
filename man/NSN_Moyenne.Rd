\name{NSN_Moyenne}
\alias{NSN_Moyenne}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Calcul le nombre de sujets necessaires dans un contexte de test de moyenne
}
\description{
  Retourne le nombre de sujets necessaire dans un contexte de test de moyenne. Fait appel a la fonction du package \code{pwr} :
  \code{pwr.t.test}.
}
\usage{
  NSN_Moyenne(m1, m2,
              sd1, sd2,
              sd.global = NULL,
              alpha = 0.05, power = 0.8,
              type = c("two.sample", "one.sample", "paired"),
              alternative = c("two.sided", "less", "greater"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{m}{
    Les parametres \code{m1} et \code{m2} sont interchangeables, et correspondent aux moyennes estimees dans
    la population de reference et la population d'etude, ou dans les deux populations d'etude.
}
  \item{sd}{
    Les parametres \code{sd1} et \code{sd2} sont interchangeables, et correspondent aux ecart-types estimes dans
    la population de reference et la population d'etude, ou dans les deux populations d'etude. Si l'ecart-type global
    \code{sd.global} sur l'ensemble de la population est connu, il peut etre renseigne a la place.
}
  \item{alpha}{
    Risque de premiere espece. Par defaut/convention \code{alpha = 0.05}.
}
  \item{power}{
    Puissance du test minimum souhaitee dans le calcul du NSN. Par defaut/convention \code{power = 0.8}.
}
  \item{type}{
    Type de test de student effectue, par defaut \code{type = "two.sample"}.
    Les autres possiblites sont \code{type = "less"} et \code{type = "greater"}.
}
  \item{alternative}{
    Hypothese alternative du test, par defaut \code{alternative = "two.sided"} et l'egalite des deux proportions sera testee.
    Les autres possiblites sont \code{alternative = "less"} et \code{alternative = "greater"}.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
    Le nombre de sujets nécessaire, un entier arrondi au superieur depuis la valeur decimale calculee. Le N retourne correspond aux nombre de sujets PAR GROUPE.
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
NSN_Moyenne(m1 = 2350, m2 = 1850,
            sd1 = 250, sd2 = 400)
}
