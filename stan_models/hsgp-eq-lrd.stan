functions
{
#include gp-functions.stan
}

data
{
  int<lower=1> U;       // Survey wave x repeated response
  int<lower=1> T;       // Survey wave
  int<lower=1> R;       // Repeated reports
  int<lower=1> A;       // Number of age inputs
  int<lower=1> C;       // Number of age strata
  int<lower=0> N_M, N_F;

  // Contacts for age i to ageband b
  array[N_M] int<lower=0> Y_MM;
  array[N_F] int<lower=0> Y_FF;
  array[N_M] int<lower=0> Y_MF;
  array[N_F] int<lower=0> Y_FM;

  array[N_M] int<lower=1>  ROW_MAJOR_IDX_M;
  array[N_F] int<lower=1>  ROW_MAJOR_IDX_F;

  // Indexes denoting the maximum index positions of ROW_MAJOR_IDXs
  array[U] int START_IDX_M, START_IDX_F;
  array[U] int END_IDX_M, END_IDX_F;

  // Pariticant size offsets
  vector[N_M] part_M;
  vector[N_F] part_F;

  // Ambigous contact offests
  vector[N_M] S_M;
  vector[N_F] S_F;

  // Population size offsets
  row_vector[A] pop_M, pop_F;

  array[T, R] int map_tr_to_u;
  matrix[A, C] map_age_to_strata; // Indicator Matrix that maps age to age strata
  array[A*A] int NN_IDX; // Index indicating the locations of the non-nuisance parameters in the resturctured HSGP matrix

  // Standardized age and age difference index
  vector[A] age_idx_std;
  vector[2*A-1] diff_idx_std;

  // HSGP arguments
  real<lower=0> C1;    // determines the boundary value L for age of participants (age_idx)
  int<lower=1> M1;     // number of basis functions
  real<lower=0> C2;    // determines the boundary value L for age of contacted individuals (strata_idx)
  int<lower=1> M2;     // number of basis functions
}

transformed data
{
  int N = N_M + N_F + N_M + N_F;
  int MM = 1, FF = 2, MF = 3, FM = 4; // gender indexes
  int G = 4;                          // gender combinations
  real gp_delta = 1e-9;               // GP nugget

  // Pre-compute offset vector
  vector[N_M] log_O_M = log(part_M) + log(S_M);
  vector[N_F] log_O_F = log(part_F) + log(S_F);

  // Pre-compute population size matrix
  matrix[A, A] log_P_M = rep_matrix(log(pop_M), A);
  matrix[A, A] log_P_F = rep_matrix(log(pop_F), A);

  // HSGP basis functions in both age dimensions
  real L1 = C1 * max(diff_idx_std);
  real L2 = C2 * max(age_idx_std);
  matrix[2*A-1, M1] PHI1 = PHI(2*A-1, M1, L1, diff_idx_std);
  matrix[A, M2] PHI2 = PHI(A, M2, L2, age_idx_std);

  // Vectorize Y
  array[N] int<lower=0> Y = append_array(
    Y_MM,
    append_array(
      Y_FF,
      append_array(
        Y_MF,
        Y_FM
      )
    )
  );
}

parameters
{
  vector[G] beta_0; // contact rate baseline
  vector[T-1] tau; // time effect
  vector[R-1] rho; // repeated response effect

  real<lower=0> nu; // over disperison

  matrix<lower=0>[T, G-1] gp_rho_1; // length-scale
  matrix<lower=0>[T, G-1] gp_rho_2; // length-scale
  matrix<lower=0, upper=pi()/2 >[T, G-1] gp_sigma_unif; // magnitude

  array[T] matrix[(G-1)*M2, M1] z; // HSGP basis function coefficients
}

transformed parameters
{
  matrix<lower=0>[T, G-1] gp_sigma = tan(gp_sigma_unif); // Reparametrize Half-Cauchy for stability

  array[T, G] matrix[A, A] log_cnt_rate; // Expose for easy access
  array[T, G-1] matrix[A, A] f;

  vector[N_M] mu_flat_MM;
  vector[N_F] mu_flat_FF;
  vector[N_M] mu_flat_MF;
  vector[N_F] mu_flat_FM;

  for (t in 1:T){
    f[t, MM] = hsgp_restruct(A, gp_sigma[t,MM], gp_rho_1[t,MM], gp_rho_2[t,MM],
                             L1, L2, M1, M2, PHI1, PHI2, z[t, 1:M2,], NN_IDX);
    f[t, FF] = hsgp_restruct(A, gp_sigma[t,FF], gp_rho_1[t,FF], gp_rho_2[t,FF],
                             L1, L2, M1, M2, PHI1, PHI2, z[t, (M2+1):2*M2,], NN_IDX);
    f[t, MF] = hsgp_restruct(A, gp_sigma[t,MF], gp_rho_1[t,MF], gp_rho_2[t,MF],
                             L1, L2, M1, M2, PHI1, PHI2, z[t, (2*M2+1):3*M2,], NN_IDX);

    if(t == 1){
      log_cnt_rate[t, MM] = beta_0[MM] + symmetrize_from_lower_tri(f[t, MM]);
      log_cnt_rate[t, FF] = beta_0[FF] + symmetrize_from_lower_tri(f[t, FF]);
      log_cnt_rate[t, MF] = beta_0[MF] + f[t, MF];
      log_cnt_rate[t, FM] = beta_0[FM] + f[t, MF]';
    } else { // Add time effects
      log_cnt_rate[t, MM] = beta_0[MM] + symmetrize_from_lower_tri(f[t, MM]) + tau[t-1];
      log_cnt_rate[t, FF] = beta_0[FF] + symmetrize_from_lower_tri(f[t, FF]) + tau[t-1];
      log_cnt_rate[t, MF] = beta_0[MF] + f[t, MF] + tau[t-1];
      log_cnt_rate[t, FM] = beta_0[FM] + f[t, MF]' + tau[t-1];
    }

    { // Local scope
      // Stratified contact intensities
      vector[A*C] m_flat_MM = to_vector( (exp(log_cnt_rate[t, MM] + log_P_M) * map_age_to_strata)' );
      vector[A*C] m_flat_FF = to_vector( (exp(log_cnt_rate[t, FF] + log_P_F) * map_age_to_strata)' );
      vector[A*C] m_flat_MF = to_vector( (exp(log_cnt_rate[t, MF] + log_P_F) * map_age_to_strata)' );
      vector[A*C] m_flat_FM = to_vector( (exp(log_cnt_rate[t, FM] + log_P_M) * map_age_to_strata)' );


      for(r in 1:t){ // Add repeat effects
        int u = map_tr_to_u[t, r];

        if (START_IDX_M[u] == 0) continue; // Skip missing

        int sm = START_IDX_M[u];
        int sf = START_IDX_F[u];
        int em = END_IDX_M[u];
        int ef = END_IDX_F[u];

        // stratified expected contact counts
        if(r > 1){
          mu_flat_MM[sm:em] = m_flat_MM[ROW_MAJOR_IDX_M[sm:em]] .* exp(rho[r-1] + log_O_M[sm:em]);
          mu_flat_FF[sf:ef] = m_flat_FF[ROW_MAJOR_IDX_F[sf:ef]] .* exp(rho[r-1] + log_O_F[sf:ef]);
          mu_flat_MF[sm:em] = m_flat_MF[ROW_MAJOR_IDX_M[sm:em]] .* exp(rho[r-1] + log_O_M[sm:em]);
          mu_flat_FM[sf:ef] = m_flat_FM[ROW_MAJOR_IDX_F[sf:ef]] .* exp(rho[r-1] + log_O_F[sf:ef]);
        } else {
          mu_flat_MM[sm:em] = m_flat_MM[ROW_MAJOR_IDX_M[sm:em]] .* exp(log_O_M[sm:em]);
          mu_flat_FF[sf:ef] = m_flat_FF[ROW_MAJOR_IDX_F[sf:ef]] .* exp(log_O_F[sf:ef]);
          mu_flat_MF[sm:em] = m_flat_MF[ROW_MAJOR_IDX_M[sm:em]] .* exp(log_O_M[sm:em]);
          mu_flat_FM[sf:ef] = m_flat_FM[ROW_MAJOR_IDX_F[sf:ef]] .* exp(log_O_F[sf:ef]);
        }
      }
    }
  }
}

model
{
  // Additive linear parameters
  target += normal_lpdf(beta_0 | 0, 10); // Baseline contact rate
  target += normal_lpdf(tau | 0, 1);  // Survey wave
  target += normal_lpdf(rho | 0, 1);  // Repeated response
  target += normal_lpdf(zeta | 0, 1);

  // GP parameters
  target += inv_gamma_lpdf( to_vector(gp_rho_1) | 10, 10);
  target += inv_gamma_lpdf( to_vector(gp_rho_2) | 10, 10);
  target += cauchy_lpdf( to_vector(gp_sigma) | 0, 1);
  for (t in 1:T){ target += std_normal_lpdf( to_vector(z[t]) ); }

  // Negative binomial parameters
  target += exponential_lpdf(nu | 1);    // Overdispersion

  // Update the log-likelihood
  {
    vector[N] mu_flat = append_row(
      mu_flat_MM,
      append_row(
        mu_flat_FF,
        append_row(
          mu_flat_MF,
          mu_flat_FM
        )
      )
    );

    target += neg_binomial_lpmf(Y | mu_flat / nu + 1e-13, inv(nu));
  }
}

generated quantities
{
  // Predicted coarse-strata contacts
  array[N] int yhat_strata;

  {
    vector[N] mu_flat = append_row(
      mu_flat_MM,
      append_row(
        mu_flat_FF,
        append_row(
          mu_flat_MF,
          mu_flat_FM
        )
      )
    );

    yhat_strata = neg_binomial_rng( mu_flat / nu + 1e-13, inv(nu) );
  }
}
