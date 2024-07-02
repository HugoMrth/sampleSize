SSdescriptive <- function(N.tot,
                           e = 0.05, p = 0.5, alpha = 0.05) {

  #### Check Params ####
  if(is.null(N.tot)){
    stop("N.tot manquant")
  }

  #### Code Fonction ####

  #Calcul des paramètres
  z <- abs(qnorm(p = alpha/2))

  N <- ((z^2 * p * (1 - p)) /
          (e^2)) /
    (1 + ((z^2 * p * (1 - p)) /
       (e^2 * N.tot)))
  return(N = ceiling(N))
}


# SSdescriptive(3000)
# SSdescriptive(10000)
