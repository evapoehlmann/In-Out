# Packages ----------------------------------------------------------------

library(tidyverse)
library(haven)
library(rstan)
library(msm)
library(bridgesampling)
library(httr)

results <- read.csv("bf_sensitivity1.csv", header = TRUE, row.names = 1)

# Stan Models -------------------------------------------------------------

### Compile Models

# Unconstrained
u_mod <- stan_model(
  "prior_sensitivity_analysis/low_setting/u_mod_sensi1.stan",
  model_name = "unconstrained"
)

# Common-Effect
c_mod <- stan_model(
  "one_model.stan",
  model_name = "common_effect"
)



# Data --------------------------------------------------------------------

#Study 9, Topolinski et al., 2024, Experiment 3
url <- "https://osf.io/download/g6cpj/?view_only=6be49bfc6a3d40beada864f74208b6b0"
filename <- "ds3raw.xlsx"
GET(url, write_disk(filename, overwrite = TRUE))
df <- readxl::read_xlsx(filename)

df<- cbind(df, do.call(rbind, apply(df[c("LI01_01", "LI02_01", "LI03_01", "LI04_01", "LI05_01", "LI06_01", "LI07_01", "LI08_01", "LI09_01", "LI10_01", 
                                                      "LI11_01", "LI12_01", "LI13_01", "LI14_01", "LI15_01", "LI16_01", "LI17_01", "LI18_01", "LI19_01", "LI20_01",
                                                      "LI21_01", "LI22_01", "LI23_01", "LI24_01", "LI25_01", "LI26_01", "LI27_01", "LI28_01", "LI29_01", "LI30_01", 
                                                      "LI31_01", "LI32_01", "LI33_01", "LI34_01", "LI35_01", "LI36_01", "LI37_01", "LI38_01", "LI39_01", "LI40_01", "LI41_01", "LI42_01", "LI43_01", "LI44_01", "LI45_01", "LI46_01", "LI47_01", "LI48_01", "LI49_01", "LI50_01", 
                                                      "LI51_01", "LI52_01", "LI53_01", "LI54_01")], 1, function(x) {
                                                        x1 <- table(x)
                                                        data.frame(var.judgments = max(x1), mode=names(x1)[which.max(x1)])})))


unlink(filename)
df <- df |>
  dplyr::select(starts_with("LI")) |> 
  mutate(CASE = 1:n()) |> 
  pivot_longer(cols = contains("LI"),
               names_prefix = "LI",
               names_to = "item",
               values_to = "Y"
  ) |>  
  mutate(
    direction = rep(
      c(
        rep("lc", times = 9), 
        rep("cl", times = 9), 
        rep("ld", times = 9), 
        rep("dl", times = 9),
        rep("cd", times = 9),
        rep("dc", times = 9)
      ),
      401
    )
  ) |>
  filter(direction %in% c("ld", "dl")) |>
  mutate(
    direction = ifelse(direction == "ld", 0.5, -0.5),
    id = dense_rank(CASE),
    liking_norm = (Y - 6) / 5
  ) |> 
  dplyr::select(id, direction, liking_norm)


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
results[1,9] <- bridgesampling::bayes_factor(ml_unconstrained, ml_common, log = T)$bf


### Unconstrained vs. Positive-Effects

# Calculate Prior Probability of Constraint
k <- 50000
mu <- rnorm(k, 0, .1)
sd <- rtnorm(k, 0, .03, lower = 0)
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
b_samps <- (q$beta)
post_prob <- apply(b_samps, MARGIN = 1, FUN = \(x) all(x > 0)) |> mean()
post_prob <- max(post_prob, 1 / k)

# Calculate Log Bayes Factor
results[2,9] <- (log(prior_prob) - log(post_prob))



# Visualization -----------------------------------------------------------

beta <- b_samps |> colMeans()
ll <- b_samps |> apply(2, quantile, probs = c(.025))
ul <- b_samps |> apply(2, quantile, probs = c(.975))


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
    ll = ll,
    ul = ul
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


write.csv(results, "bf_sensitivity1.csv", row.names = TRUE)