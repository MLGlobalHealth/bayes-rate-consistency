real lambda(real L, int m) {
		real lam;
		lam = ((m*pi())/(2*L))^2;
				
		return lam;
}

real SPD_EQ_2D(real alpha, real rho1, real rho2, real w1, real w2) {
		real S;
		S = alpha^2 * sqrt(2*pi())^2 * rho1*rho2 * exp(-0.5*(rho1^2*w1^2 + rho2^2*w2^2));
				
		return S;
}

vector diagSPD_EQ_2D(int M, real alpha, vector rho, real L1, real L2, matrix indices){
  int M;
  vector[M] diagSPD;
  for (m in 1:M){
    diagSPD[m] =  sqrt(SPD_EQ_2D(alpha, rho[1], rho[2], sqrt(lambda(L1, indices[m,1])), sqrt(lambda(L2, indices[m,2]))));
  }

  return diagSPD;
}

vector PHI_2D(real L1, real L2, int M1, int M2, vector x1, vector x2) {
  matrix[nrows(x1), M1*M2] PHI;
  for (m in 1:M) {
		vector[rows(x1)] fi1;
		vector[rows(x1)] fi2;
		fi1 = 1/sqrt(L1) * sin(M1 * pi() * (x1 + L1) / (2*L1));
		fi2 = 1/sqrt(L2) * sin(M2 * pi() * (x2 + L2) / (2*L2));
		PHI[,m] = fi1 .* fi2;
  }	
}



