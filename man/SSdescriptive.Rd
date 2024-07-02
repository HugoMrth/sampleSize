\name{SSdescriptive}
\alias{SSdescriptive}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Calcul le nombre de sujets nécessaires pour une étude descritptive.
}
\description{
  Retourne le nombre de sujets nécessaire pour une étude descritptive.
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
    Marge d'erreure souhaitee. Par défaut/convention \code{e = 0.05}.
}
  \item{p}{
    Prévalence estimee de l'etude. Par défaut, et si plusieurs questions, on assume \code{p = 0.5}.
}
  \item{alpha}{
    Risque de premiere espece correspondant a la taille de l'intervalle de confiance. Par défaut/convention \code{alpha = 0.05}.
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
  \code{\link{SScohort}}
}
\examples{
SSdescriptive(3000)
SSdescriptive(10000)
}
