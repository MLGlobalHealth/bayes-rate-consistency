data {
  int A; // Number of one year age groups
  int C; // Number of age groups
  array[C] int Y_M;
  array[C] int Y_F;
  array[A] int x;

  matrix[A,C] age_strata_map;
}

parameters {
  // Baseline
  real beta0;

  // GP parameters
  real<lower=0> alpha_M; // scale parameter
  real<lower=0> alpha_F; // scale parameter
  real<lower=0> rho_M;   // lengthscale parameter
  real<lower=0> rho_F;   // lengthscale parameter
  vector[A] f_M;
  vector[A] f_F;
}

transformed parameters {
  vector[A] lambda_M;
  vector[A] lambda_F;
  { // local scope
    matrix[A, A] L_M = cholesky_decompose(gp_exp_quad_cov(x, alpha_M, rho_M) + diag_matrix(rep_vector(1e-10, A)));
    matrix[A, A] L_F = cholesky_decompose(gp_exp_quad_cov(x, alpha_F, rho_F) + diag_matrix(rep_vector(1e-10, A)));

    lambda_M = exp(beta0 + L_M * f_M);
    lambda_F = exp(beta0 + L_F * f_F);
  }
}

model {
  target += normal_lpdf(beta0 | 0, 10);

  target += inv_gamma_lpdf(alpha_M | 5, 5);
  target += inv_gamma_lpdf(alpha_F | 5, 5);
  target += inv_gamma_lpdf(rho_M | 5, 5);
  target += inv_gamma_lpdf(rho_F | 5, 5);
  target += normal_lpdf(f_M | 0, 1);
  target += normal_lpdf(f_F | 0, 1);

  {
    target += poisson_lpmf(Y_M | age_strata_map' * lambda_M);
    target += poisson_lpmf(Y_F | age_strata_map' * lambda_F);
  }
}

generated quantities {
  array[C] int Yhat_M = poisson_rng(age_strata_map' * lambda_M);
  array[C] int Yhat_F = poisson_rng(age_strata_map' * lambda_F);
}
