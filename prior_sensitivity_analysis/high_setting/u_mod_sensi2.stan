
data {
  int<lower=0> N;
  int<lower=0> k;
  int<lower=0> id[N];
  vector[N] cond;
  vector[N] y;
}

parameters {
  
  real mu;
  vector[k] z_alpha;
  real mu_b;
  vector[k] z_beta;
  real<lower=0> sigma_a;
  real<lower=0> sigma_b;
  real<lower=0> sigma;
  
}

transformed parameters {

  vector[k] alpha;
  vector[k] beta;
  alpha = mu + z_alpha * sigma_a;
  beta = mu_b + z_beta * sigma_b;

}

model {
  
  // Prior
  target += normal_lpdf(mu | 0, .5);
  target += normal_lpdf(mu_b | 0, .1);
  target += normal_lpdf(z_alpha | 0, 1);
  target += normal_lpdf(z_beta | 0, 1);
  target += normal_lpdf(sigma_a | 0, .1) - normal_lccdf(0 | 0, .1);
  target += normal_lpdf(sigma_b | 0, .3) - normal_lccdf(0 | 0, .3);
  target += normal_lpdf(sigma | 0, .3) - normal_lccdf(0 | 0, .3);
  
  // Likelihood
  for (i in 1:N) {
    target += normal_lpdf(y[i] | alpha[id[i]] + cond[i] * beta[id[i]], sigma);
  } 
  
}
