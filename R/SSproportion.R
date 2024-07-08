SSproportion <- function(p1 = NULL, p2 = NULL,
                           alpha = 0.05, power = 0.8,
                           alternative = "two.sided") {

  #### Check Params ####
  if(is.null(p1) | is.null(p2)){
    stop("p1 and p2 must be provided")
  }

  #### Code Fonction ####
  return(N = ceiling(pwr::pwr.p.test(ES.h(p1, p2),
                                     sig.level = alpha, power = power,
                                     alternative = alternative)$n))
}


# SSproportion(0.20, 0.22)
