
# two-sample t-test

# sample size
n <- 30

# treatment1

# mu
mu_t1 <- 120
sd_t1 <- 20

# treatment 2

# mu
mu_t2 <- 100
sd_t2 <- 20

# loop 1000 times
n_rep <- 1000
sev <- vector("list", length = n_rep)
for (i in seq_len(n_rep)) {
  
  # t1
  t1_dat <- rnorm(n = n, mean = mu_t1, sd = sd_t1)
  
  # t2
  t2_dat <- rnorm(n = n, mean = mu_t2, sd = sd_t2)
  
  # loop over different hypothesised mean differences
  mu_h <- seq(0, 20, 0.5)
  
  p_val <- vector(length = length(mu_h))
  t_val <- vector(length = length(mu_h))
  for (k in seq_along(mu_h)) {
    # run the test
    x <- t.test(x = t1_dat, y = t2_dat, 
                 alternative = "greater", mu = mu_h[k],
                 var.equal = TRUE)
    # add to list
    p_val[k] <- x$p.value
    t_val[k] <- x$statistic
  }
  
  # pull this into a data.frame
  sev[[i]] <- dplyr::tibble(rep = i,
                            mu = mu_h,
                            p = p_val,
                            t = t_val)
  
}

# bind into a data.frame
sev <- dplyr::bind_rows(sev)

# arrange by mu difference
sev <-
  sev |>
  dplyr::arrange(mu, rep)

# check the data
head(sev)

# summarise the data
sev_sum <-
  sev |>
  dplyr::mutate(severity = (1 - p)) |>
  dplyr::group_by(mu) |>
  dplyr::summarise(severity_mean = mean(severity),
                   severity_sd = sd(severity))

# plot the data
library(ggplot2)

ggplot(data = sev_sum,
       mapping = aes(x = mu, y = severity_mean)) +
  geom_line() +
  geom_errorbar(mapping = aes(x = mu, 
                              ymin = severity_mean - severity_sd, 
                              ymax = severity_mean + severity_sd),
                width = 0) +
  geom_hline(yintercept = 0.8, linetype = "dashed", colour = "red") +
  ylab("Severity") +
  xlab("Mean difference") +
  theme_classic()


# randomised block-design

# load relevant libraries
library(ggplot2)
library(lme4)

# do classical ecological experiments pass with severity?

# simulate some ecological experiments

# randomised block design (with replication within block-treatments)

# set-up the two means
a_pop <- 100
b_pop <- 5

# set-up the two sds
a_pop_sd <- 10
b_pop_sd <- 5

# number of locations
n_loc <- 5

# run the simulation
sim_a <- rnorm(n = n_loc, mean = a_pop, sd = a_pop_sd)
sim_b <- rnorm(n = n_loc, mean = b_pop, sd = b_pop_sd)

# within-block sd
block_sd <- 10

# within block_n
block_n <- 6

# make a list with the data
dat <- vector("list", length = (n_loc))
for (i in seq_len((n_loc))) {
  
  # sample from these group means
  b <- rnorm(n = block_n, sim_a[i], sd = block_sd)
  d <- rnorm(n = block_n, sim_a[i] + sim_b[i], sd = block_sd)
  
  # pull into a data.frame
  dat[[i]] <- dplyr::tibble(block = LETTERS[i],
                            treatment = (rep(c(0, 1), each = block_n)),
                            value = c(b, d))
  
}

# bind into a data.frame
dat <- dplyr::bind_rows(dat)
head(dat)

## linear mixed-model

# simple block random intercept
lmm1 <- lme4::lmer(value ~ treatment + (1 | block), data = dat)
lmm1
summary(lmm1)
VarCorr(lmm1)

ranef(lmm1)

equatiomatic::extract_eq(lmm1)

# hypothesis test using Wald's t-tests
lmm1_par <- summary(lmm1)

# set the hypothesis
beta_h0 <- 0

# extract the treatment effect t-value
t_val <- (lmm1_par$coefficients[2, 1] - beta_h0)/lmm1_par$coefficients[2, 2]
print(t_val)

# estimate the degrees of freedom (put a random value for now)
df <- 30

# compare to the t-distribution
1 - pt(t_val, df = df)

# uncorrelated random intercept and random slope within group
lmm2 <- lme4::lmer(value ~ treatment + (1 | block) + (0 + treatment|block), data = dat)
lmm2
summary(lmm2)
VarCorr(lmm2)

equatiomatic::extract_eq(lmm2)

# testing models
lmm3 <- lme4::lmer(value ~ treatment + block + (1 | block:treatment), data = dat)
lmm3
summary(lmm3)
VarCorr(lmm3)

equatiomatic::extract_eq(lmm3)

# testing other models
dat$plot <- with(dat, paste0(block, treatment))
lmm4 <- lme4::lmer(value ~ treatment + (1 | block/plot), data = dat)
lmm4
summary(lmm3)
VarCorr(lmm3)

equatiomatic::extract_eq(lmm4)







# hypothesis test using Wald's t-tests
lmm2_par <- summary(lmm2)

# set the hypothesis
beta_h0 <- 5

# extract the treatment effect t-value
t_val <- (lmm2_par$coefficients[2, 1] - beta_h0)/lmm2_par$coefficients[2, 2]
print(t_val)

# estimate the degrees of freedom (put a random value for now)
df <- 30

# compare to the t-distribution
1 - pt(t_val, df = df)




# check whether this matches with the simulation
lm1_tidy <- broom::tidy(lm1)
head(lm1_tidy)

# extract the relevant global mean parameters

# extract mean and se of a
a_est <- with(lm1_tidy, estimate[term == "(Intercept)"])
a_est_se <- with(lm1_tidy, std.error[term == "(Intercept)"])

# extract mean and se of b
b_est <- with(lm1_tidy, estimate[term == "treatment2"])
b_est_se <- with(lm1_tidy, std.error[term == "treatment2"])

# compare predicted to true means
df_pred <- dplyr::tibble(parameter = rep(c("alpha", "beta"), each = 2),
                         true_pred = rep(c("simulated", "model"), 2),
                         value = c(a_pop, a_est, b_pop, b_est),
                         value_se = c(NA, a_est_se, NA, b_est_se))

# plot the results
library(ggplot2)
ggplot(data = df_pred,
       mapping = aes(x = value, y = parameter, colour = true_pred)) +
  geom_point(position = position_dodge(width = 0.25)) +
  geom_errorbarh(mapping = aes(xmin = value - value_se, xmax = value + value_se, 
                               y = parameter, colour = true_pred),
                 position = position_dodge(width = 0.25), height = 0) +
  ylab(NULL) +
  xlab("Estimate value") +
  theme_minimal() +
  theme(legend.title = element_blank())

# did we correctly estimate the location means

# location means
loc_means <- with(lm1_tidy, 
                  estimate[term == "(Intercept)"] + estimate[(!(term %in% c("(Intercept)", "treatment2")))])

# add the intercept
loc_means <- c(with(lm1_tidy, estimate[term == "(Intercept)"]), loc_means)

# compare with a correlation
plot(loc_means, sim_mv[,1])
abline(a = 0, b = 1)










# check whether this matches with the simulation
lm1_tidy <- broom::tidy(lm1)
head(lm1_tidy)

# extract the relevant global mean parameters

# extract mean and se of a
a_est <- with(lm1_tidy, estimate[term == "(Intercept)"])
a_est_se <- with(lm1_tidy, std.error[term == "(Intercept)"])

# extract mean and se of b
b_est <- with(lm1_tidy, estimate[term == "treatment2"])
b_est_se <- with(lm1_tidy, std.error[term == "treatment2"])

# compare predicted to true means
df_pred <- dplyr::tibble(parameter = rep(c("alpha", "beta"), each = 2),
                         true_pred = rep(c("simulated", "model"), 2),
                         value = c(a_pop, a_est, b_pop, b_est),
                         value_se = c(NA, a_est_se, NA, b_est_se))

# plot the results
library(ggplot2)
ggplot(data = df_pred,
       mapping = aes(x = value, y = parameter, colour = true_pred)) +
  geom_point(position = position_dodge(width = 0.25)) +
  geom_errorbarh(mapping = aes(xmin = value - value_se, xmax = value + value_se, 
                               y = parameter, colour = true_pred),
                 position = position_dodge(width = 0.25), height = 0) +
  ylab(NULL) +
  xlab("Estimate value") +
  theme_minimal() +
  theme(legend.title = element_blank())

# did we correctly estimate the location means

# location means
loc_means <- with(lm1_tidy, 
                  estimate[term == "(Intercept)"] + estimate[(!(term %in% c("(Intercept)", "treatment2")))])

# add the intercept
loc_means <- c(with(lm1_tidy, estimate[term == "(Intercept)"]), loc_means)

# compare with a correlation
plot(loc_means, sim_mv[,1])
abline(a = 0, b = 1)


# severity testing using bootstrapping

# https://stats.stackexchange.com/questions/272417/get-p-value-of-coefficients-in-regression-models-using-bootstrap

# using boostrapped samples from a model to test null hypotheses
# severity
# bootstrapping

boot_func <- function(.) {
  c(beta = fixef(.)[2])
}

# extract the bootstrapped samples
boo_samp <- bootMer(lmm1, boot_func, nsim = 10)
boo_samp$t
boo_samp$t

as.data.frame(boo_samp)







