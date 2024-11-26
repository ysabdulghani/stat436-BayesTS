data {
  int<lower=0> N;
  vector[N] rate;
}
parameters {
  real alpha;
  real beta;
  real<lower=0> sigma;
}
model {
  for (n in 2:N)
    rate[n] ~ normal(alpha + beta * rate[n-1], sigma);
}
