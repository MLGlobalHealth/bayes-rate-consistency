data {
  int A; // Number of one year age groups
  int C; // Number of age groups
  int DF;
  array[C] int Y_M;
  array[C] int Y_F;
  matrix[A,DF] B;
  matrix[A,C] age_strata_map;
}

parameters {
  // Baseline
  real beta0_M;
  real beta0_F;
  real<lower=0> inv_scale;

  // Spline parameters
  vector[DF] beta_M;
  vector[DF] beta_F;
}

transformed parameters {
  vector[A] lambda_M;
  vector[A] lambda_F;
  { // local scope
    lambda_M = exp(beta0_M + B*beta_M);
    lambda_F = exp(beta0_F + B*beta_F);
  }
}

model {
  target += normal_lpdf(beta0_M | 0, 5);
  target += normal_lpdf(beta0_F | 0, 5);
  target += normal_lpdf(beta_M | 0, 0.2);
  target += normal_lpdf(beta_F | 0, 0.2);
  target += exponential_lpdf(inv_scale | 0.1);

  {
    target += neg_binomial_lpmf(Y_M | (age_strata_map' * lambda_M) / inv_scale, inv(inv_scale));
    target += neg_binomial_lpmf(Y_F | (age_strata_map' * lambda_F) / inv_scale, inv(inv_scale));
  }
}

generated quantities {
  array[C] int Yhat_M = neg_binomial_rng((age_strata_map' * lambda_M) / inv_scale, inv(inv_scale));
  array[C] int Yhat_F = neg_binomial_rng((age_strata_map' * lambda_F) / inv_scale, inv(inv_scale));
}
