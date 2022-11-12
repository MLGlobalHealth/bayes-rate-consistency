vector diagSPD_EQ(real alpha, real rho, real L, int M)
  {
    return alpha * sqrt(sqrt(2*pi()) * rho) * exp(-0.25*(rho*pi()/2/L)^2 * linspaced_vector(M, 1, M)^2);
  }

vector diagSPD_Matern32(real alpha, real rho, real L, int M)
{
  return 2*alpha * (sqrt(3)/rho)^1.5 * inv((sqrt(3)/rho)^2 + ((pi()/2/L) * linspaced_vector(M, 1, M))^2);
}

vector diagSPD_Matern52(real alpha, real rho, real L, int M)
{
  return 2*alpha * sqrt(4.0/3) * (sqrt(5)/rho)^2.5 * inv((sqrt(5)/rho)^2 + ((pi()/2/L) * linspaced_vector(M, 1, M))^2)^1.5;
}

matrix PHI(int N, int M, real L, vector x)
{
  return sin(diag_post_multiply(rep_matrix(pi()/(2*L) * (x+L), M), linspaced_vector(M, 1, M)))/sqrt(L);
}

/** Kronecker multivariate product
  *
  * Enables efficient sampling from a 2D multivariate normal distribution.
  *
  * @param A
  * @param B
  * @param V A matrix of N(0,1) distributed random variables
  */
matrix kron_mvprod(matrix A, matrix B, matrix V)
{
  return (B*V) * transpose(A);
}

// Transform restructured matrix back to a squared matrix
matrix inverse_restruct(matrix B, array[] int nn_idx)
{
  return to_matrix(to_vector(B)[nn_idx], cols(B), cols(B), 0);
}

/** Kronecker Decomposed 2D Gaussian process
  *
  * @param x1, x2: participant and contact age
  * @param delta: GP nugget
  * @param alpha: GP scaling parameter
  * @param rho1, rho2: GP length-scale parameter on the participants and contacts dimensions
  * @param z:
  * @return A two dimensional Gaussian process function
  */
matrix gp2d(array[] real x1, array[] real x2,
            real alpha, real rho1, real rho2, matrix z)
{
  int A = size(x1);
  int B = size(x2);

  // Compute the exponentiated quadratic covariance function:
  matrix[A,A] K1 = gp_exp_quad_cov(x1, alpha, rho1) + diag_matrix(rep_vector(1e-9, A));
  matrix[B,B] K2 = gp_exp_quad_cov(x2, alpha, rho2) + diag_matrix(rep_vector(1e-9, B));

  // Cholesky Decomposition K1 = L_K1 L_K1^T
  matrix[A,A] L_K1 = cholesky_decompose(K1);
  matrix[B,B] L_K2 = cholesky_decompose(K2);

  matrix[A,B] f = kron_mvprod(L_K2, L_K1, z);

  return(f);
}

/** Kronecker Decomposed 2D Gaussian process (Matern 3/2)
  *
  * @param x1, x2: participant and contact age
  * @param delta: GP nugget
  * @param alpha: GP scaling parameter
  * @param rho1, rho2: GP length-scale parameter on the participants and contacts dimensions
  * @param z:
  * @return A two dimensional Gaussian process function
  */
matrix gp2d_matern32(array[] real x1, array[] real x2,
                     real alpha, real rho1, real rho2, matrix z)
{
  int A = size(x1);
  int B = size(x2);

  // Compute the exponentiated quadratic covariance function:
  matrix[A,A] K1 = gp_matern32_cov(x1, alpha, rho1) + diag_matrix(rep_vector(1e-9, A));
  matrix[B,B] K2 = gp_matern32_cov(x2, alpha, rho2) + diag_matrix(rep_vector(1e-9, B));

  // Cholesky Decomposition K1 = L_K1 L_K1^T
  matrix[A,A] L_K1 = cholesky_decompose(K1);
  matrix[B,B] L_K2 = cholesky_decompose(K2);

  matrix[A,B] f = kron_mvprod(L_K2, L_K1, z);

  return(f);
}

/** Kronecker Decomposed 2D Gaussian process (Matern 5/2)
  *
  * @param x1, x2: participant and contact age
  * @param delta: GP nugget
  * @param alpha: GP scaling parameter
  * @param rho1, rho2: GP length-scale parameter on the participants and contacts dimensions
  * @param z:
  * @return A two dimensional Gaussian process function
  */
matrix gp2d_matern52(array[] real x1, array[] real x2,
                     real alpha, real rho1, real rho2, matrix z)
{
  int A = size(x1);
  int B = size(x2);

  // Compute the exponentiated quadratic covariance function:
  matrix[A,A] K1 = gp_matern52_cov(x1, alpha, rho1) + diag_matrix(rep_vector(1e-9, A));
  matrix[B,B] K2 = gp_matern52_cov(x2, alpha, rho2) + diag_matrix(rep_vector(1e-9, B));

  // Cholesky Decomposition K1 = L_K1 L_K1^T
  matrix[A,A] L_K1 = cholesky_decompose(K1);
  matrix[B,B] L_K2 = cholesky_decompose(K2);

  matrix[A,B] f = kron_mvprod(L_K2, L_K1, z);

  return(f);
}

/** Restructured Kronecker Decomposed 2D Gaussian process
  *
  * @param x1, x2: Age difference and cohort age
  * @param delta GP nugget
  * @param alpha: GP scaling parameter
  * @param rho1, rho2: GP length-scale parameter on age difference and cohort age dimensions
  * @param z:
  * @param nn_idx: Index of non-nuiance parameters
  * @return A two dimensional Gaussian process fuction
  */
matrix gp2d_restruct(array[] real x1, array[] real x2,
                     real alpha, real rho1, real rho2, matrix z,
                     array[] int nn_idx)
{
  int A = size(x1);
  int B = size(x2);
  matrix[A,B] f_restruct = gp2d(x1, x2, alpha, rho1, rho2, z);

  return(inverse_restruct(f_restruct, nn_idx));
}

/** Restructured Kronecker Decomposed 2D Gaussian process (Matern 3/2)
  *
  * @param x1, x2: Age difference and cohort age
  * @param delta GP nugget
  * @param alpha: GP scaling parameter
  * @param rho1, rho2: GP length-scale parameter on age difference and cohort age dimensions
  * @param z:
  * @param nn_idx: Index of non-nuiance parameters
  * @return A two dimensional Gaussian process fuction
  */
matrix gp2d_matern32_restruct(array[] real x1, array[] real x2,
                              real alpha, real rho1, real rho2, matrix z,
                              array[] int nn_idx)
{
  int A = size(x1);
  int B = size(x2);
  matrix[A,B] f_restruct = gp2d_matern32(x1, x2, alpha, rho1, rho2, z);

  return(inverse_restruct(f_restruct, nn_idx));
}

/** Restructured Kronecker Decomposed 2D Gaussian process (Matern 5/2)
  *
  * @param x1, x2: Age difference and cohort age
  * @param delta GP nugget
  * @param alpha: GP scaling parameter
  * @param rho1, rho2: GP length-scale parameter on age difference and cohort age dimensions
  * @param z:
  * @param nn_idx: Index of non-nuiance parameters
  * @return A two dimensional Gaussian process fuction
  */
matrix gp2d_matern52_restruct(array[] real x1, array[] real x2,
                              real alpha, real rho1, real rho2, matrix z,
                              array[] int nn_idx)
{
  int A = size(x1);
  int B = size(x2);
  matrix[A,B] f_restruct = gp2d_matern52(x1, x2, alpha, rho1, rho2, z);

  return(inverse_restruct(f_restruct, nn_idx));
}

/** Hilbert Space approximate 2D Gaussian process
  *
  * @param A: Number of ages for particiapnts
  * @param alpha: GP scaling parameter
  * @param rho1, rho2: GP length-scale parameter on participants and contacts age dimensions
  * @param L1, L2: HSGP parameters
  * @param M1, M2: HSGP parameters
  * @param z:
  * @return A two dimensional Gaussian process fuction
  */
matrix hsgp(int A, real alpha, real rho1, real rho2, real L1, real L2, int M1, int M2,
            matrix PHI1, matrix PHI2, matrix z)
{
  vector[M1] sqrt_spd_1 = diagSPD_EQ(alpha, rho1, L1, M1);
  vector[M2] sqrt_spd_2 = diagSPD_EQ(alpha, rho2, L2, M2);

  matrix[A,A] f = kron_mvprod(
    diag_post_multiply( PHI1, sqrt_spd_1 ),
    diag_post_multiply( PHI2, sqrt_spd_2 ),
    z
  );

  return(f);
}

/** Hilbert Space approximate 2D Gaussian process (Matern 3/2)
  *
  * @param A: Number of ages for particiapnts
  * @param alpha: GP scaling parameter
  * @param rho1, rho2: GP length-scale parameter on participants and contacts age dimensions
  * @param L1, L2: HSGP parameters
  * @param M1, M2: HSGP parameters
  * @param z:
  * @return A two dimensional Gaussian process fuction
  */
matrix hsgp_matern32(int A, real alpha, real rho1, real rho2, real L1, real L2, int M1, int M2,
            matrix PHI1, matrix PHI2, matrix z)
{
  vector[M1] sqrt_spd_1 = diagSPD_Matern32(alpha, rho1, L1, M1);
  vector[M2] sqrt_spd_2 = diagSPD_Matern32(alpha, rho2, L2, M2);

  matrix[A,A] f = kron_mvprod(
    diag_post_multiply( PHI1, sqrt_spd_1 ),
    diag_post_multiply( PHI2, sqrt_spd_2 ),
    z
  );

  return(f);
}

/** Hilbert Space approximate 2D Gaussian process (Matern 5/2)
  *
  * @param A: Number of ages for particiapnts
  * @param alpha: GP scaling parameter
  * @param rho1, rho2: GP length-scale parameter on participants and contacts age dimensions
  * @param L1, L2: HSGP parameters
  * @param M1, M2: HSGP parameters
  * @param z:
  * @return A two dimensional Gaussian process fuction
  */
matrix hsgp_matern52(int A, real alpha, real rho1, real rho2, real L1, real L2, int M1, int M2,
            matrix PHI1, matrix PHI2, matrix z)
{
  vector[M1] sqrt_spd_1 = diagSPD_Matern52(alpha, rho1, L1, M1);
  vector[M2] sqrt_spd_2 = diagSPD_Matern52(alpha, rho2, L2, M2);

  matrix[A,A] f = kron_mvprod(
    diag_post_multiply( PHI1, sqrt_spd_1 ),
    diag_post_multiply( PHI2, sqrt_spd_2 ),
    z
  );

  return(f);
}

/** Restructured Hilbert Space approximate 2D Gaussian process
  *
  * @param A: Number of ages for particiapnts
  * @param alpha: GP scaling parameter
  * @param rho1, rho2: GP length-scale parameter on age difference and cohort age dimensions
  * @param L1, L2: HSGP parameters
  * @param M1, M2: HSGP parameters
  * @param z:
  * @param nn_idx: Index of non-nuiance parameters
  * @return A two dimensional Gaussian process fuction
  */
matrix hsgp_restruct(int A, real alpha, real rho1, real rho2, real L1, real L2, int M1, int M2,
                     matrix PHI1, matrix PHI2, matrix z, array[] int nn_idx)
{
  vector[M1] sqrt_spd_1 = diagSPD_EQ(alpha, rho1, L1, M1);
  vector[M2] sqrt_spd_2 = diagSPD_EQ(alpha, rho2, L2, M2);

  matrix[2*A-1, A] f_restruct = kron_mvprod(
    diag_post_multiply( PHI2, sqrt_spd_2 ),
    diag_post_multiply( PHI1, sqrt_spd_1 ),
    z
  );

  return inverse_restruct(f_restruct, nn_idx);
}

/** Restructured Hilbert Space approximate 2D Gaussian process (Matern 3/2)
  *
  * @param A: Number of ages for particiapnts
  * @param alpha: GP scaling parameter
  * @param rho1, rho2: GP length-scale parameter on age difference and cohort age dimensions
  * @param L1, L2: HSGP parameters
  * @param M1, M2: HSGP parameters
  * @param z:
  * @param nn_idx: Index of non-nuiance parameters
  * @return A two dimensional Gaussian process fuction
  */
matrix hsgp_matern32_restruct(int A, real alpha, real rho1, real rho2, real L1, real L2, int M1, int M2,
                     matrix PHI1, matrix PHI2, matrix z, array[] int nn_idx)
{
  vector[M1] sqrt_spd_1 = diagSPD_Matern32(alpha, rho1, L1, M1);
  vector[M2] sqrt_spd_2 = diagSPD_Matern32(alpha, rho2, L2, M2);

  matrix[2*A-1, A] f_restruct = kron_mvprod(
    diag_post_multiply( PHI2, sqrt_spd_2 ),
    diag_post_multiply( PHI1, sqrt_spd_1 ),
    z
  );

  return inverse_restruct(f_restruct, nn_idx);
}

/** Restructured Hilbert Space approximate 2D Gaussian process (Matern 5/2)
  *
  * @param A: Number of ages for particiapnts
  * @param alpha: GP scaling parameter
  * @param rho1, rho2: GP length-scale parameter on age difference and cohort age dimensions
  * @param L1, L2: HSGP parameters
  * @param M1, M2: HSGP parameters
  * @param z:
  * @param nn_idx: Index of non-nuiance parameters
  * @return A two dimensional Gaussian process fuction
  */
matrix hsgp_matern52_restruct(int A, real alpha, real rho1, real rho2, real L1, real L2, int M1, int M2,
                              matrix PHI1, matrix PHI2, matrix z, array[] int nn_idx)
{
  vector[M1] sqrt_spd_1 = diagSPD_Matern52(alpha, rho1, L1, M1);
  vector[M2] sqrt_spd_2 = diagSPD_Matern52(alpha, rho2, L2, M2);

  matrix[2*A-1, A] f_restruct = kron_mvprod(
    diag_post_multiply( PHI2, sqrt_spd_2 ),
    diag_post_multiply( PHI1, sqrt_spd_1 ),
    z
  );

  return inverse_restruct(f_restruct, nn_idx);
}
