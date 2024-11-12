SSlinearReg <- function(R = NULL, df = NULL,
                       mod = NULL,
                       alpha = 0.05, power = 0.8) {
  #### Check Params ####
  if(is.null(mod) & (is.null(R) | is.null(df))) stop("If mod is missing, both R and df must be provided")

  #### Code Fonction ####
  if (!is.null(mod)) {
    R <- sqrt(summary(mod)$adj.r.squared)
    df <- summary(mod)$fstatistic[2]
  }

  return(N = ceiling(pwr::pwr.f2.test(u = df, f2 = R,  sig.level = alpha, power = power)$v + as.numeric(df)))
}

