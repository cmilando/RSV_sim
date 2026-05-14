data {
  int<lower=0> N;
  //array[N] int<lower=0> y;
}
parameters {
  real<lower=0> lambda;
}
model {
  lambda ~ gamma(1, 1);
  //y ~ poisson(lambda);
}
generated quantities {
  int<lower=0> y_tilde = poisson_rng(lambda);
}
