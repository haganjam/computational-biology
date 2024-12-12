
# boostrap p-values
# https://stats.stackexchange.com/questions/272417/get-p-value-of-coefficients-in-regression-models-using-bootstrap

model <- function(data) {
  lm(divorce ~ ., data = data)
}

simulator <- function(data) {
  rows <- sample(nrow(data), nrow(data), replace = TRUE)
  data[rows, ]
}

estimator <- function(data) {
  coefficients(model(data))
}

test <- function(data, b.test) {
  fit <- model(data)
  b <- coefficients(fit)
  var <- diag(vcov(fit))
  t <- (b - b.test) / sqrt(var)
  t
}

pvalue <- function(t.star, t.hat, alternative = c("two.sided", "less", "greater")) {
  alternative <- match.arg(alternative)
  
  p.upper <- (sum(t.star >= t.hat) + 1) / (length(t.star) + 1)
  p.lower <- (sum(t.star <= t.hat) + 1) / (length(t.star) + 1)
  
  if (alternative == "greater") {
    p.upper
  } else if (alternative == "less") {
    p.lower
  } else {
    # The two-tailed p-value is twice the smaller of the two one-tailed p-values.
    2 * min(p.upper, p.lower)
  }
}

bootstrap.summary <- function(b, t, p) {
  tibble(
    `Name` = names(b),
    `Estimate` = b,
    `t value` = t,
    `Pr(>|t|)` = p
  )
}