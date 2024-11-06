test_that("can reproduce lm fits", {
  ## Annette Dobson (1990)
  ## "An Introduction to Generalized Linear Models".
  ## Page 9: Plant Weight Data.
  ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
  trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
  dobson <- data.frame(
    weight = c(ctl, trt),
    group = gl(2, 10, 20, labels = c("Ctl","Trt")))
  lm_fit <- lm(weight ~ 1 + group, data = dobson)
  rbc_fit <- rbc(weight ~ 1 | 1 + group,
    init = Normal(0, 1),
    flows = list(Scale, Translate),
    data = dobson)
  expect_equal(as.vector(coef(rbc_fit)[-1]),
    as.vector(coef(lm_fit)), tolerance = 0.0001)
  expect_equal(as.vector(exp(coef(rbc_fit)[1]) * sqrt(20 / 18)),
    summary(lm_fit)$sigma, tolerance = 0.0001)
  expect_equal(as.vector(fitted(rbc_fit)), as.vector(fitted(lm_fit)))
})

test_that("can reproduce log-linear model fits", {
  lm_fit <- lm(log(mpg) ~ hp + wt + factor(cyl), data = mtcars)
  rbc_fit <- rbc(mpg ~ 1 | 1 + hp + wt + factor(cyl),
    init = LogNormal(0, 1),
    flows = list(Power, Scale),
    data = mtcars)
  expect_equal(as.vector(coef(rbc_fit)[-1]),
    as.vector(coef(lm_fit)), tolerance = 0.0001)
})

test_that("can reproduce logistic regression", {
  glm_fit <- glm(case ~ age + parity,
    data = infert, family = binomial())
  rbc_fit <- rbc(case ~ 1 + age + parity,
    init = Bernoulli(0.5),
    flows = list(ScaleOdds),
    data = infert)
  expect_equal(as.vector(coef(rbc_fit)),
    as.vector(coef(glm_fit)), tolerance = 0.0001)
  expect_equal(as.vector(fitted(rbc_fit)), as.vector(fitted(glm_fit)), tolerance = 0.0001)
})

test_that("can reproduce linear-in-probability model", {
  glm_fit <- glm(case ~ age + parity,
    data = infert, family = binomial(link = "identity"))
  suppressWarnings(
    rbc_fit <- rbc(case ~ 1 + age + parity,
      init = Bernoulli(0),
      flows = list(TranslateRisk1),
      data = infert,
      par = c(0.5, 0, 0)
    )
  )
  expect_equal(as.vector(coef(rbc_fit)), 
    as.vector(coef(glm_fit)), tolerance = 0.0001)
  expect_equal(as.vector(fitted(rbc_fit)), as.vector(fitted(glm_fit)), tolerance = 0.0001)
})
