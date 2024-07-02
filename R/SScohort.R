SScohort <- function(incidence.exp, incidence.non.exp = NULL,
                        prop.traitement, fact.risque = NULL,
                        temps.etude,
                        alpha = 0.05, power = 0.80) {

  #### Check Params ####
  if(is.null(incidence.exp)){
    stop("incidence.exp manquant")
  }
  if(is.null(prop.traitement)){
    stop("prop.traitement manquant")
  }

  #### Code Fonction ####

  #Gestion cas incidence non exp vs facteur de risque
  if (is.null(incidence.non.exp)) {
    incidence.non.exp <- incidence.exp * fact.risque
  }

  #Calcul des paramètres
  d <- abs(incidence.exp - incidence.non.exp)
  gamma <- (1 - prop.traitement)/prop.traitement
  mu <- (incidence.exp + gamma * incidence.non.exp) / (1 + gamma)
  z_alpha <- abs(qnorm(p = alpha/2))
  z_beta <- abs(qnorm(p = power))

  #Calcul du NSN
  N.exp <- (z_alpha * sqrt((1 + gamma) * mu * (1 - mu)) +
              z_beta * sqrt((1 + incidence.exp) * incidence.non.exp * (1 - incidence.non.exp)))^2 /
    (gamma * d^2)

  N.non.exp <- gamma * N.exp
  N.temps <- N.exp + N.non.exp
  N <- N.temps / temps.etude

  return(list(N.exp = ceiling(N.exp),
              N.nexp = ceiling(N.non.exp),
              N.time = ceiling(N.temps),
              N = ceiling(N)))
}

# SScohort(incidence.exp = 2/1000, fact.risque = 3,
#             prop.traitement = 0.3, temps.etude = 5)
