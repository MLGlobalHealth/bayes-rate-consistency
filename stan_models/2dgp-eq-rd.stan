functions
{
#include gp-functions.stan
}

data
{
  int<lower=1> N_MM, N_FF, N_MF, N_FM; // Number of observations for each gender pair

  int<lower=1> A;       // Number of age inputs
  int<lower=1> C;       // Number of age strata

  array[N_MM] int Y_MM; // Contacts for age i to ageband b
  array[N_FF] int Y_FF;
  array[N_MF] int Y_MF;
  array[N_FM] int Y_FM;

  array[N_MM] int ROW_MAJOR_IDX_MM;
  array[N_FF] int ROW_MAJOR_IDX_FF;
  array[N_MF] int ROW_MAJOR_IDX_MF;
  array[N_FM] int ROW_MAJOR_IDX_FM;

  vector[A] log_N_M, log_N_F; // Participant size offsets
  vector[A] log_S_M, log_S_F; // Group contact offsets
  row_vector[A] log_P_M, log_P_F; // Population size offsets

  array[A] real age_idx_std;          // Standardized age index
  array[2*A-1] real diff_idx_std;     // Standardized age difference index
  matrix[A,C] map_age_to_strata; // Indicator Matrix that maps age to age strata
  array[A*A] int NN_IDX;         // Index indicating the locations of the non-nuisance parameters in the resturctured HSGP matrix
}

transformed data
{
  int N = N_MM + N_FF + N_MF + N_FM;  // Total number of observations
  int MM = 1, FF = 2, MF = 3, FM = 4; // gender indexes
  int G = 4;                          // gender combinations
  int D = 2*A-1;                       // age difference dimension size
  real gp_delta = 1e-9;               // GP nugget
  real epsilon = 1e-13;               // Prevent shape parameter to be 0

  // Precompute offset terms
  array[G] matrix[A,A] log_offset;
  log_offset[MM] = rep_matrix(log_N_M + log_S_M, A) + rep_matrix(log_P_M, A);
  log_offset[FF] = rep_matrix(log_N_F + log_S_F, A) + rep_matrix(log_P_F, A);
  log_offset[MF] = rep_matrix(log_N_M + log_S_M, A) + rep_matrix(log_P_F, A);
  log_offset[FM] = rep_matrix(log_N_F + log_S_F, A) + rep_matrix(log_P_M, A);

  // append data
  array[N] int Y = append_array( append_array( append_array(Y_MM, Y_FF), Y_MF), Y_FM);
}

parameters
{
  vector[G] beta_0;
  real<lower=0> nu;

  vector<lower=0>[G-1] gp_rho_1;
  vector<lower=0>[G-1] gp_rho_2;
  vector<lower=0, upper=pi()/2 >[G-1] gp_alpha_unif;

  matrix[(G-1)*D, A] z; // HSGP basis function coefficients
}

transformed parameters
{
  vector<lower=0>[G-1] gp_alpha = tan(gp_alpha_unif); // Reparametrize Half-Cauchy for optimization in HMC

  matrix[A,A] f_MM, f_FF, f_MF;
  array[G] matrix[A,A] log_cnt_rate;
  array[G] matrix<lower=0>[A,C] alpha_strata;

  f_MM = gp2d_restruct(diff_idx_std, age_idx_std, gp_alpha[MM], gp_rho_1[MM], gp_rho_2[MM], z[1:D,], NN_IDX);
  f_FF = gp2d_restruct(diff_idx_std, age_idx_std, gp_alpha[FF], gp_rho_1[FF], gp_rho_2[FF], z[D+1:2*D,], NN_IDX);
  f_MF = gp2d_restruct(diff_idx_std, age_idx_std, gp_alpha[MF], gp_rho_1[MF], gp_rho_2[MF], z[2*D+1:3*D,], NN_IDX);

  log_cnt_rate[MM] = beta_0[MM] + symmetrize_from_lower_tri(f_MM);
  log_cnt_rate[FF] = beta_0[FF] + symmetrize_from_lower_tri(f_FF);
  log_cnt_rate[MF] = beta_0[MF] + f_MF;
  log_cnt_rate[FM] = beta_0[FM] + f_MF';

  alpha_strata[MM] = exp(log_cnt_rate[MM] + log_offset[MM]) * map_age_to_strata / nu + epsilon;
  alpha_strata[FF] = exp(log_cnt_rate[FF] + log_offset[FF]) * map_age_to_strata / nu + epsilon;
  alpha_strata[MF] = exp(log_cnt_rate[MF] + log_offset[MF]) * map_age_to_strata / nu + epsilon;
  alpha_strata[FM] = exp(log_cnt_rate[FM] + log_offset[FM]) * map_age_to_strata / nu + epsilon;
}

model
{
  // GP priors
  target += inv_gamma_lpdf(gp_rho_1 | 5, 5);
  target += inv_gamma_lpdf(gp_rho_2 | 5, 10);
  target += cauchy_lpdf(gp_alpha | 0, 1);
  target += std_normal_lpdf( to_vector(z) );

  // Overdispersion
  target += exponential_lpdf(nu | 1);

  // baseline
  target += normal_lpdf(beta_0 | 0, 10);

  {
    vector[N] alpha_strata_flat =
    append_row(
        append_row(
          append_row(
            to_vector(alpha_strata[MM]')[ROW_MAJOR_IDX_MM],
            to_vector(alpha_strata[FF]')[ROW_MAJOR_IDX_FF]
          ),
          to_vector(alpha_strata[MF]')[ROW_MAJOR_IDX_MF]
        ),
      to_vector(alpha_strata[FM]')[ROW_MAJOR_IDX_FM]
    );
    target += neg_binomial_lpmf( Y | alpha_strata_flat, inv(nu));
  }
}

generated quantities
{
  array[N] real log_lik;
  array[G,A,C] int yhat_strata;

  for(g in 1:G){
    for(i in 1:A){
      yhat_strata[g,i,:] = neg_binomial_rng( alpha_strata[g,i,:], inv(nu) );
    }
  }

  {
    vector[N] alpha_strata_flat =
    append_row(
        append_row(
          append_row(
            to_vector(alpha_strata[MM]')[ROW_MAJOR_IDX_MM],
            to_vector(alpha_strata[FF]')[ROW_MAJOR_IDX_FF]
          ),
          to_vector(alpha_strata[MF]')[ROW_MAJOR_IDX_MF]
        ),
      to_vector(alpha_strata[FM]')[ROW_MAJOR_IDX_FM]
    );

    for(i in 1:N) {
      log_lik[i] = neg_binomial_lpmf( Y[i] | alpha_strata_flat[i], inv(nu));
    }
  }
}
