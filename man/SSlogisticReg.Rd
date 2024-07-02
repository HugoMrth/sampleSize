\name{SSlogisticReg}
\alias{SSlogisticReg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Calcul le nombre de sujets necessaires dans un contexte de regression logistique
}
\description{
  Retourne le nombre de sujets necessaire dans un contexte de regression logistique. Fait appel a la fonction du package \code{WebPower} :
  \code{wp.logistic}.
}
\usage{
  SSlogisticReg(p0, p1,
             alpha = 0.05, power = 0.80,
             alternative = c("two.sided", "less", "greater"),
             family = c("Bernoulli", "exponential", "lognormal", "normal", "Poisson", "uniform"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p0}{
    Probabilite que Y soit egal a un quand X est egal a 0.
}
  \item{R}{
    Probabilite que Y soit egal a un quand X est egal a 1.
}
  \item{alpha}{
    Risque de premiere espece. Par defaut/convention \code{alpha = 0.05}.
}
  \item{power}{
    Puissance du test minimum souhaitee dans le calcul du NSN. Par defaut/convention \code{power = 0.8}.
}
  \item{alternative}{
    Direction de l'hypothese alternative. Par defaut \code{alternative = "two.sided"}. Les autres options sont
    \code{alternative = "less"} et \code{alternative = "greater"}.
}
  \item{family}{
    Famille de distribution du predicteur. Par defaut \code{family = "Bernoulli"}. Les autres options sont
    \code{"exponential", "lognormal", "normal", "Poisson", "uniform"}.
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
  \code{\link{wp.logistic}}
}
\examples{
SSlogisticReg(0.75, 0.85)
SSlogisticReg(0.75, 0.85, family = "normal")
}
