printDiagnosticTable <- function(prevalence) {
  if (prevalence < 0 | prevalence > 1) {
    stop("prevalence is a proportion and must be between 0 and 1")
  }
  
  if (prevalence > 0.5) {
    stop("If the expected prevalence is above 50%, one must direclty use the SSdiagnostic() function")
  }
  
  if (prevalence <= 0.5) {
    message("If the expected prevalence is below 50%, one must determine the number of cases using the tables betlow, 
            then use the SSdiagnostic() function to estimate the total sample size.")
    
    tab_nsn1 <- data.frame(
      m50 = c(268, 119, 67, 42, 28, 18, 13, 11),
      m55 = c(1058, 262, 114, 62, 40, 26, 18, 12),
      m60 = c("", 1018, 248, 107, 60, 33, 24, 14),
      m65 = c("", "", 960, 230, 98, 52, 31, 16),
      m70 = c("", "", "", 869, 204, 85, 41, 24),
      m75 = c("", "", "", "", 756, 176, 70, 34),
      m80 = c("", "", "", "", "", 624, 235, 50),
      m85 = c("", "", "", "", "", "", 474, 93),
      m90 = c("", "", "", "", "", "", "", 298)
    )
    colnames(tab_nsn1) <- c("0.50", "0.55", "0.60", "0.65", "0.70", 
                            "0.75", "0.80", "0.85", "0.90")
    rownames(tab_nsn1) <- c("0.60", "0.65", "0.70", "0.75", "0.80", "0.85", "0.90", "0.95")
    
    
    tab_nsn2 <- data.frame(
      m85 = c(319, 220, 166, 126, 93, 76, 59, 50, 50),
      m86 = c(438, 294, 203, 153, 109, 82, 63, 53, 50),
      m87 = c(666, 403, 273, 183, 137, 98, 79, 58, 50),
      m88 = c(1127, 613, 372, 248, 169, 117, 85, 63, 51),
      m89 = c(2443, 1035, 549, 334, 217, 151, 105, 69, 56),
      m90 = c(9309, 2215, 934, 493, 298, 191, 129, 89, 61),
      m91 = c("", 8428, 1992, 832, 434, 253, 158, 115, 68),
      m92 = c("", "", 7512, 1763, 729, 374, 224, 129, 77),
      m93 = c("", "", "", 6576, 1524, 625, 309, 185, 109),
      m94 = c("", "", "", "", 5626, 1288, 519, 259, 127),
      m95 = c("", "", "", "", "", 4654, 1036, 386, 181),
      m96 = c("", "", "", "", "", "", 3643, 78, 261),
      m97 = c("", "", "", "", "", "", "", 2620, 521),
      m98 = c("", "", "", "", "", "", "", "", 1567)
    )
    colnames(tab_nsn2) <- c("0.85", "0.86", "0.87", "0.88", "0.89"
                            , "0.90", "0.91", "0.92", "0.93", "0.94"
                            , "0.95", "0.96", "0.97", "0.98")
    rownames(tab_nsn2) <- c("0.91", "0.92", "0.93", 
                            "0.94", "0.95", "0.96", 
                            "0.97", "0.98", "0.99")
    
    # print(tab_nsn1)
    # print(tab_nsn2)
    return(list(
      low_minimal_CI = tab_nsn1,
      high_minimal_CI = tab_nsn2
    ))
  }
}

# printDiagnosticTable(-1)
# printDiagnosticTable(0.8)
# printDiagnosticTable(0.2)

