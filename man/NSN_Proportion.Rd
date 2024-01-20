\name{NSN_Proportion}
\alias{NSN_Proportion}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Calcul le nombre de sujets necessaires dans un contexte de test de proportions.
}
\description{
  Retourne le nombre de sujets necessaire dans un contexte de test de proportions. Fait appel aux fonctions du package \code{pwr} :
  \code{pwr.p.test} et \code{ES.h}.
}
\usage{
  NSN_Proportion(p1, p2,
                 alpha = 0.05, power = 0.8,
                 alternative =  = c("two.sided","less","greater"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{
    Les parametres \code{p1} et \code{p2} sont interchangeables, et correspondent aux proportions/prevalences estimees dans
    la population de reference et la population d'etude.
}
  \item{alpha}{
    Risque de premiere espece. Par defaut/convention \code{alpha = 0.05}.
}
  \item{power}{
    Puissance du test minimum souhaitee dans le calcul du NSN. Par defaut/convention \code{power = 0.8}.
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
    Le nombre de sujets nécessaire, un entier arrondi au superieur depuis la valeur decimale calculee.
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
NSN_Proportion(0.20, 0.22)
}
