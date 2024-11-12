SSdiagnostic <- function(prevalence, sens_speci, min_ci,
                         alpha = 0.05, power = 0.8,
                         n_cases = NULL) {

  if (prevalence > 0.5) {
    delta <- sens_speci - min_ci
    n_cases <- (qnorm(power)*sqrt(sens_speci*(1-sens_speci)) +
       qnorm(1-alpha)*sqrt(min_ci*(1-min_ci)))/(delta*delta)
  } else {
    if (is.null(n_cases)) {
      stop("When prevalence is below 50% one must first estimate the number of cases using the printDiagnosticTable() function and use this number as an input for the call.")
    }
  }

  n_control <- n_cases * ((1-prevalence)/prevalence)

  return(list(
    n_cases = n_cases,
    n_control = n_control,
    n_total = n_cases + n_control
  ))
}
