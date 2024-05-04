# Packages ----------------------------------------------------------------

library(tidyverse)
library(haven)
library(rstan)
library(msm)
library(bridgesampling)
library(httr)


# Stan Models -------------------------------------------------------------

### Compile Models

# Unconstrained
u_mod <- stan_model(
  "u_model.stan",
  model_name = "unconstrained"
)

# Common-Effect
c_mod <- stan_model(
  "one_model.stan",
  model_name = "common_effect"
)



# Data --------------------------------------------------------------------



#Study 13, Ingendahl et al., 2023, Experiment 4

url <- "https://osf.io/download/nfwjv/"
filename <- "study4rawOS.xlsx"
GET(url, write_disk(filename, overwrite = TRUE))


df <- readxl::read_xlsx(filename) |>
  mutate(
    wordtype = case_when(
      origin == "samec" ~ "Same Consonants",
      origin == "newc"  ~ "New Consonants",
      TRUE              ~ "Conditioned"  
    )
  ) |>
  filter(!OSid == "276164680") |>
  filter(current == "rating") |>
  mutate(
    direction = ifelse(direction == "in", 0.5, -0.5),
    liking_norm = ((as.numeric(response))-5)/4,
    id = dense_rank(OSid)
  ) |>
  filter(wordtype == "New Consonants") |>
  dplyr::select(c("direction", "liking_norm", "id")) |>
  arrange(id)

unlink(filename)

data = list(
  N = nrow(df),
  k = length(unique(df$id)),
  id = df$id,
  cond = df$direction,
  y = df$liking_norm
)

##Study 14, Ingendahl et al., 2023, Experiment 5
url <- "https://osf.io/download/qs2kr/"
filename <- "study5rawOS.xlsx"
GET(url, write_disk(filename, overwrite = TRUE))

df1 <- readxl::read_xlsx(filename) |>
  mutate(
    wordtype = case_when(
      origin == "samec" ~ "Same Consonants",
      origin == "newc"  ~ "New Consonants",
      TRUE              ~ "Conditioned"  
    )
  ) |>
  filter(!OSid %in% c(737495106, 817895977)) |>
  filter(current == "rating") |>
  mutate(
    direction = ifelse(direction == "in", 0.5, -0.5),
    liking_norm = ((as.numeric(response))-5)/4,
    id = dense_rank(OSid)
  ) |>
  filter(wordtype == "New Consonants") |>
  dplyr::select(c("direction", "liking_norm", "id")) |>
  arrange(id)

unlink(filename)

data = list(
  N = nrow(df),
  k = length(unique(df$id)),
  id = df$id,
  cond = df$direction,
  y = df$liking_norm
)

df$id <- paste0("A_", df$id)

df1$id <- paste0("B_", df1$id)

df2 <-rbind(df, df1)
df2$id <- rep(1:293, each = 54)

data = list(
  N = nrow(df2),
  k = length(unique(df2$id)),
  id = df2$id,
  cond = df2$direction,
  y = df2$liking_norm
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


bridgesampling::bayes_factor(ml_unconstrained, ml_common, log = T)$bf #19.24419

### Unconstrained vs. Positive-Effects

# Calculate Prior Probability of Constraint
k <- 50000
mu <- rnorm(k, 0, .1)
sd <- rtnorm(k, 0, .1, lower = 0)
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
(log(prior_prob) - log(post_prob)) #8.60813



# Visualization -----------------------------------------------------------

beta <- b_samps |> colMeans()
ll <- b_samps |> apply(2, quantile, probs = c(.025))
ul <- b_samps |> apply(2, quantile, probs = c(.975))



agg <- df2 |> 
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



