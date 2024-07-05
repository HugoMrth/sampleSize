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
  N$SS_total <- ceiling(apply(N, 1, function(x) {max(x["NSN_sensi"], x["NSN_speci"]) / (1-x["dropout"])}))

  N
}


# SSsensibility(sensi = 0.9, speci = rep(c(0.7, 0.9), each = 4), prevalence = rep(rep(c(0.1, 0.588), each = 2), 2))
