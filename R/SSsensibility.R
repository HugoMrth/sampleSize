SSsensibility <- function(sensi = NULL, speci = NULL, prevalence = NULL,
                       dropout = 0.1,
                       alpha = 0.05, precis = 0.1) {
  N <- data.frame(
    sensi = sensi,
    speci = speci,
    prevalence = prevalence,
    alpha = alpha,
    precis = precis,
    dropout = dropout
  )

  N$SS_sensi <- ceiling((qnorm(N$alpha/2)^2*(N$sensi*(1-N$sensi)/N$precis^2))/N$prevalence)
  N$SS_speci <- ceiling((qnorm(N$alpha/2)^2*(N$speci*(1-N$speci)/N$precis^2))/(1-N$prevalence))
  N$SS_total <- ceiling(apply(N, 1, function(x) {max(x["SS_sensi"], x["SS_speci"]) / (1-x["dropout"])}))
  N
}
