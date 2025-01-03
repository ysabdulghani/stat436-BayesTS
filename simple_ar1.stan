data {
  int<lower=0> N;
  vector[N] rate;
  vector[N] hours;
}
parameters {
  real alpha;
  real beta;
  real<lower=0> sigma;
}
model {
  sigma ~ InvGamma()
  for (n in 2:N) {
    rate[n] ~ normal(alpha + beta * rate[n-1], sigma);
  }
}

generated quantities {
  vector<lower=0>[N] counts;
  for (n in 1:N) {
    counts[n] = poisson_rng(rate[n] * hours[n] + 0.000001);
  }
}
