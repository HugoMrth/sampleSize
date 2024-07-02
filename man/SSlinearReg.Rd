\name{SSlinearReg}
\alias{SSlinearReg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Calcul le nombre de sujets necessaires dans un contexte de regression lineaire
}
\description{
  Retourne le nombre de sujets necessaire dans un contexte de regression lineaire. Fait appel a la fonction du package \code{pwr} :
  \code{pwr.f2.test}.
}
\usage{
  SSlinearReg(R, df, mod = NULL,
             alpha = 0.05, power = 0.8)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{
    L'estimation du coefficient de correlation du modele, ou de la racine carree du coefficient de determination.
    Soit un chiffre comprit entre 0 et 1.
}
  \item{df}{
    Le nombre de degre de liberte du modele, soit le nombre de variables (Y inclus) moins 1.
}
  \item{mod}{
    Un objet de classe \code{lm}. Si \code{mod} n'existe pas, il faut renseigner \code{R} et \code{df}.
}
  \item{alpha}{
    Risque de premiere espece. Par defaut/convention \code{alpha = 0.05}.
}
  \item{power}{
    Puissance du test minimum souhaitee dans le calcul du NSN. Par defaut/convention \code{power = 0.8}.
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
  \code{\link{pwr.f2.test}}
}
\examples{
X <- c(46.8, 48.7, 48.4, 53.7, 56.7)
Y <- c(14.6, 19.6, 18.6, 25.5, 20.4)

mod <- lm(Y ~ X)

SSlinearReg(df = summary(mod)$fstatistic[2],
           R = sqrt(summary(mod)$adj.r.squared))

SSlinearReg(mod)
}
