NSN_RegLog <- function(p0, p1,
                       alpha = 0.05, power = 0.80,
                       alternative = "two.sided",
                       family = "Bernoulli") {

  #### Check Params ####
  if(is.null(p0) | is.null(p1)){
    stop("Vous devez renseigner au minimum p0 et p1")
  }

  #### Code Fonction ####
  return(N = ceiling(WebPower::wp.logistic(p0 = 0.75, p1 = 0.85,
                                           alpha = alpha, power = power,
                                           alternative = alternative,
                                           family = family)$n))
}


# NSN_RegLog(0.75, 0.85)
# NSN_RegLog(0.75, 0.85, family = "normal")

