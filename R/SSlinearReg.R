SSlinearReg <- function(R = NULL, df = NULL,
                       mod = NULL,
                       alpha = 0.05, power = 0.8) {

  #### Check Params ####
  if(is.null(mod) & (is.null(R) | is.null(df))){
    stop("Vous devez renseigner soit le modele, soit le R du modele et le nombre de degre de liberte")
  }

  #### Code Fonction ####
  if (!is.null(mod)) {
    R <- sqrt(summary(mod)$adj.r.squared)
    df <- summary(mod)$fstatistic[2]
  }


  return(N = ceiling(pwr::pwr.f2.test(u = df, f2 = R,  sig.level = alpha, power = power)$v + as.numeric(df)))
}


# X <- c(46.8, 48.7, 48.4, 53.7, 56.7)
# Y <- c(14.6, 19.6, 18.6, 25.5, 20.4)
#
# mod <- lm(Y ~ X)
#
# SSlinearReg(df = summary(mod)$fstatistic[2],
#            R = sqrt(summary(mod)$adj.r.squared))
#
# SSlinearReg(mod)

