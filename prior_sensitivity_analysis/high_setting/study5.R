# Packages ----------------------------------------------------------------

library(tidyverse)
library(haven)
library(rstan)
library(msm)
library(bridgesampling)
library(httr)

results <- read.csv("bf_sensitivity2.csv", header = TRUE, row.names = 1)

# Stan Models -------------------------------------------------------------

### Compile Models

# Unconstrained
u_mod <- stan_model(
  "prior_sensitivity_analysis/high_setting/u_mod_sensi2.stan",
  model_name = "unconstrained"
)

# Common-Effect
c_mod <- stan_model(
  "one_model.stan",
  model_name = "common_effect"
)


# Data --------------------------------------------------------------------


##Study 5 - Ingendahl et al., 2021 Experiment 1

GET("https://osf.io/download/mcak9/", 
    write_disk(tf <- tempfile(fileext = ".xlsx")))
df <- readxl::read_xlsx(tf) |> 
  filter(!(CASE %in% c(150, 662))) |> 
  pivot_longer(
    cols = contains("LI"),
    names_to = "trial",
    values_to = "Y"
  ) |> 
  mutate(
    phase = rep(rep(c("training", "same", "new"), each = 24), length(unique(CASE))),
    direction = rep(rep(c(.5, -.5), 3 * 12), length(unique(CASE))),
    id = dense_rank(CASE),
    liking_norm = (Y - 6) / 5
  ) |> 
  filter(phase == "new") |>
  select(id, direction, liking_norm)
unlink(tf)

data = list(
  N = nrow(df),
  k = length(unique(df$id)),
  id = df$id,
  cond = df$direction,
  y = df$liking_norm
)



### Sample

mod_unconstrained <- sampling(
  u_mod,
  data = data,
  chains = 8,
  cores = 8,
  iter = 5500,
  warmup = 1500
)

mod_common <- sampling(
  c_mod,
  data = data,
  chains = 8,
  cores = 8,
  iter = 5500,
  warmup = 1500
)


# Model Comparison --------------------------------------------------------

### Unconstrained vs. Common-Effect

# Calculate Marginal Likelihoods
ml_common <- bridge_sampler(mod_common)
ml_unconstrained <- bridge_sampler(mod_unconstrained)

# Calculate Log Bayes Factor
results[1,5] <- bridgesampling::bayes_factor(ml_unconstrained, ml_common, log = T)$bf


### Unconstrained vs. Positive-Effects

# Calculate Prior Probability of Constraint
k <- 50000
mu <- rnorm(k, 0, .1)
sd <- rtnorm(k, 0, .3, lower = 0)
beta_prior <- rnorm(
  k * data$k,
  mean = mu,
  sd = sd
) |> 
  matrix(
    nrow = k
  )
prior_prob <- apply(beta_prior, MARGIN = 1, FUN = \(x) all(x > 0)) |> mean()

# Posterior Probabiliy of Constraint
q <- rstan::extract(mod_unconstrained, c("beta"))
post_prob <- apply(q$beta, MARGIN = 1, FUN = \(x) all(x > 0)) |> mean()
post_prob <- max(post_prob, 1 / k)

# Calculate Log Bayes Factor
results[2,5] <- (log(prior_prob) - log(post_prob))



# Visualization -----------------------------------------------------------

beta <- q$beta |> colMeans()
ll <- q$beta |> apply(2, quantile, probs = c(.025))
ul <- q$beta |> apply(2, quantile, probs = c(.975))



agg <- df |> 
  summarise(
    y = mean(liking_norm),
    .by = c(id, direction)
  ) |> 
  pivot_wider(
    id_cols = id,
    values_from = y,
    names_from = direction
  ) |> 
  mutate(
    eff = `0.5` - `-0.5`,
    beta = beta,
    ul = ul,
    ll = ll
  ) |> 
  arrange(eff) |> 
  mutate(index = 1:n())

ggplot(agg, aes(index)) +
  geom_hline(yintercept = 0, lwd = 1) +
  geom_hline(yintercept = mean(agg$eff), lwd = 1, linetype = "dashed") +
  geom_line(aes(y = eff), lwd = 1, color = "darkslategrey",
            alpha = .25) +
  geom_point(aes(y = eff), size = 3, shape = 21,
             fill = "darkslategrey", alpha = .7) +
  geom_line(aes(y = beta), lwd = 1, color = "firebrick") +
  geom_point(aes(y = beta), size = 3, shape = 21,
             fill = "firebrick") +
  geom_ribbon(aes(ymin = ll, ymax = ul), fill = "firebrick", alpha = 0.2) + 
  xlab("Participants") +
  ylab("Effect") + 
  theme_classic()

write.csv(results, "bf_sensitivity2.csv", row.names = TRUE)