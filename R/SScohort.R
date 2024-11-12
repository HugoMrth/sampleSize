SScohort <- function(incidence.exp, incidence.nexp = NULL,
                        prop.treatment, risk.factor = NULL,
                        time,
                        alpha = 0.05, power = 0.80) {

  #### Check Params ####
  if(is.null(incidence.exp)) stop("incidence.exp missing")
  if(is.null(prop.treatment)) stop("prop.treatment missing")

  #### Code Fonction ####
  #Gestion cas incidence non exp vs facteur de risque
  if (is.null(incidence.nexp)) incidence.nexp <- incidence.exp * risk.factor

  #Calcul des paramètres
  d <- abs(incidence.exp - incidence.nexp)
  gamma <- (1 - prop.treatment)/prop.treatment
  mu <- (incidence.exp + gamma * incidence.nexp) / (1 + gamma)
  z_alpha <- abs(qnorm(p = alpha/2))
  z_beta <- abs(qnorm(p = power))

  #Calcul du NSN
  N.exp <- (z_alpha * sqrt((1 + gamma) * mu * (1 - mu)) +
              z_beta * sqrt((1 + incidence.exp) * incidence.nexp * (1 - incidence.nexp)))^2 /
    (gamma * d^2)

  N.non.exp <- gamma * N.exp
  N.temps <- N.exp + N.non.exp
  N <- N.temps / time

  return(list(N.exp = ceiling(N.exp),
              N.nexp = ceiling(N.non.exp),
              N.time = ceiling(N.temps),
              N = ceiling(N)))
}
