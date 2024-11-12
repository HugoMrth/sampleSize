test_that("Args are checked properly :", {
  expect_error(SScohort(risk.factor = 3, prop.treatment = 0.3, time = 5))
  expect_error(SScohort(incidence.exp = 2/1000, risk.factor = 3, time = 5))

  expect_error(SSdescriptive())
  expect_error(SSdescriptive(-3000))

  expect_error(SSdiagnostic(prevalence = 0.2))

  expect_error(SSlinearReg())

  expect_error(SSlogisticReg(0.75))
  expect_error(SSlogisticReg(p1 = 0.75))

  expect_error(SSmean(m1 = 2350, sd1 = 250, sd2 = 400))
  expect_error(SSmean(m2 = 2350, sd1 = 250, sd2 = 400))
  expect_error(SSmean(m1 = 2350, m2 = 1850, sd1 = 250))
  expect_error(SSmean(m1 = 2350, m2 = 1850, sd2 = 400))

  expect_error(SSproportion(0.20))
  expect_error(SSproportion(p2 = 0.20))
})


test_that("Output type are right :", {
  expect_type(SScohort(incidence.exp = 2/1000, risk.factor = 3, prop.treatment = 0.3, time = 5), "list")
  expect_equal(length(SScohort(incidence.exp = 2/1000, risk.factor = 3, prop.treatment = 0.3, time = 5)), 4)

  expect_type(SSdescriptive(3000), "double")
  expect_equal(length(SSdescriptive(3000)), 1)

  expect_type(SSdiagnostic(prevalence = 0.1, n_cases = 70), "list")
  expect_equal(length(SSdiagnostic(prevalence = 0.1, n_cases = 70)), 3)

  expect_type(SSlinearReg(
    df = summary(mod)$fstatistic[2],
    R = sqrt(summary(mod)$adj.r.squared)
  ), "double")
  expect_type(SSlinearReg(mod = mod), "double")
  expect_equal(length(SSlinearReg(
    df = summary(mod)$fstatistic[2],
    R = sqrt(summary(mod)$adj.r.squared)
  )), 1)
  expect_equal(length(SSlinearReg(mod = mod)), 1)

  expect_type(SSlogisticReg(0.75, 0.85), "double")
  expect_equal(length(SSlogisticReg(0.75, 0.85)), 1)

  expect_type(SSmean(m1 = 2350, m2 = 1850, sd1 = 250, sd2 = 400), "double")
  expect_equal(length(SSmean(m1 = 2350, m2 = 1850, sd1 = 250, sd2 = 400)), 1)

  expect_type(SSproportion(0.20, 0.22), "double")
  expect_equal(length(SSproportion(0.20, 0.22)), 1)

  expect_equal(dim(SSsensibility(sensi = 0.9, speci = rep(c(0.7, 0.9), each = 4), prevalence = rep(rep(c(0.1, 0.588), each = 2), 2))), c(8, 9))
})


