functions
{
#include ../gp-functions.stan
}

data
{
  int<lower=1> N_U, N_R; // Number of observations for Urban or Rural

  int<lower=1> A;       // Number of age inputs
  int<lower=1> C;       // Number of age strata

  array[N_U] int Y_U; // Contacts for age i to ageband b
  array[N_R] int Y_R;

  array[N_U] int ROW_MAJOR_IDX_U;
  array[N_R] int ROW_MAJOR_IDX_R;

  vector[A] log_N_U, log_N_R; // Participant size offsets
  vector[A] log_S_U, log_S_R; // Group contact offsets
  row_vector[A] log_P; // Population size offsets

  vector[A] age_idx_std;          // Standardized age index
  vector[2*A-1] diff_idx_std;     // Standardized age difference index
  matrix[A,C] map_age_to_strata;  // Indicator Matrix that maps age to age strata
  array[A*A] int NN_IDX;          // Index indicating the locations of the non-nuisance parameters in the resturctured HSGP matrix

  // HSGP parameters
  real<lower=0> C1; // Factor to determine the boundary value L (cohort age dimension)
  int<lower=1> M1;  // Number of basis functions (cohort age dimension)
  real<lower=0> C2; // Factor to determine the boundary value L for age of contacted individuals (age difference dimension)
  int<lower=1> M2;  // Number of basis functions (age difference dimension)
}

transformed data
{
  int N = N_U + N_R;  // Total number of observations
  int U = 1, R = 2;
  real gp_delta = 1e-9;               // GP nugget
  real epsilon = 1e-13;               // Prevent shape parameter to be 0

  // Precompute offset terms
  array[R] matrix[A,A] log_offset;
  log_offset[U] = rep_matrix(log_N_U + log_S_U, A) + rep_matrix(log_P, A);
  log_offset[R] = rep_matrix(log_N_R + log_S_R, A) + rep_matrix(log_P, A);

  real L1, L2;
  matrix[2*A-1, M1] PHI1;
  matrix[A, M2] PHI2;

  // Precompute HSGP basis functions
  L1 = C1 * max(diff_idx_std);
  L2 = C2 * max(age_idx_std);
  PHI1 = PHI(2*A-1, M1, L1, diff_idx_std);
  PHI2 = PHI(A, M2, L2, age_idx_std);

  // append data
  array[N] int Y = append_array(Y_U, Y_R);
}

parameters
{
  real beta_0;
  real<lower=0> nu;

  vector<lower=0>[R] gp_rho_1;
  vector<lower=0>[R] gp_rho_2;
  vector<lower=0, upper=pi()/2 >[R] gp_alpha_unif;

  matrix[R*M1, M2] z; // HSGP basis function coefficients
}

transformed parameters
{
  // Reparametrize Half-Cauchy for optimization in HMC
  vector<lower=0>[R] gp_alpha = tan(gp_alpha_unif);
  array[R] matrix[A, A] log_cnt_rate;

  { // Local scope
    matrix[A, A] f_U, f_R;
    f_U = hsgp_matern52_restruct(A, gp_alpha[U], gp_rho_1[U], gp_rho_2[U],
                                 L1, L2, M1, M2, PHI1, PHI2,
                                 z[1:M1,], NN_IDX);
    f_R = hsgp_matern52_restruct(A, gp_alpha[R], gp_rho_1[R], gp_rho_2[R],
                                 L1, L2, M1, M2, PHI1, PHI2,
                                 z[(M1+1):2*M1,], NN_IDX);
    log_cnt_rate[U] = beta_0 + symmetrize_from_lower_tri(f_U);
    log_cnt_rate[R] = beta_0 + symmetrize_from_lower_tri(f_R);
  }
}

model
{
  // GP priors
  target += inv_gamma_lpdf(gp_rho_1 | 5, 5);
  target += inv_gamma_lpdf(gp_rho_2 | 5, 5);
  target += cauchy_lpdf(gp_alpha | 0, 1);
  target += std_normal_lpdf( to_vector(z) );

  // Overdispersion
  target += exponential_lpdf(nu | 1);

  // baseline
  target += normal_lpdf(beta_0 | 0, 10);

  { // Local scpoe
    array[R] matrix[A, C] alpha_strata;

    alpha_strata[U] = exp(log_cnt_rate[U] + log_offset[U]) * map_age_to_strata * nu + epsilon;
    alpha_strata[R] = exp(log_cnt_rate[R] + log_offset[R]) * map_age_to_strata * nu + epsilon;

    vector[N] alpha_strata_flat =
    append_row(
      to_vector(alpha_strata[U]')[ROW_MAJOR_IDX_U],
      to_vector(alpha_strata[R]')[ROW_MAJOR_IDX_R]
    );
    target += neg_binomial_lpmf( Y | alpha_strata_flat, inv(nu) );
  }
}

generated quantities
{
  array[N] real log_lik;
  array[R,A,C] int yhat_strata;

  { // Local scope
    // The code is a repeat of the model block but it significantly
    // reduces fitted model size
    array[R] matrix[A, C] alpha_strata;

    alpha_strata[U] = exp(log_cnt_rate[U] + log_offset[U]) * map_age_to_strata * nu + epsilon;
    alpha_strata[R] = exp(log_cnt_rate[R] + log_offset[R]) * map_age_to_strata * nu + epsilon;

    vector[N] alpha_strata_flat =
    append_row(
      to_vector(alpha_strata[U]')[ROW_MAJOR_IDX_U],
      to_vector(alpha_strata[R]')[ROW_MAJOR_IDX_R]
    );

    // Generate predictions
    for(r in 1:R){
      for(i in 1:A){
        yhat_strata[r,i,:] = neg_binomial_rng( alpha_strata[r,i,:], inv(nu) );
      }
    }

    // Calculate log-likelihood
    for(i in 1:N) {
      log_lik[i] = neg_binomial_lpmf( Y[i] | alpha_strata_flat[i], inv(nu) );
    }
  }
}

