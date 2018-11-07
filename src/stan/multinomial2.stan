// STAN model to implement aggregate random coefficients logit model
// 

// first we define the function that takes data and parameters and returns predicted market shares
functions {
  // calculates shares for a given market
  row_vector shares(int n_crops, vector beta, matrix X) {
    row_vector[n_crops] profits;
    row_vector[n_crops] shares;
    // 1. Rather than pass in p and x separately, we'll pass in bigX = append_col(p, X)
    // 2. append alpha_shock, beta_shock
    row_vector[n_crops] tmp;
    
    tmp = (X * beta)';
    profits = exp(tmp);
    shares = profits/(1 + sum(profits));
    return(shares);
  }
}

// Define data that our model accepts
data {
  int n_counties;               // number of counties
  int n_crops;                  // number of crops
  int k;                        // number of covariates
  int acres[n_counties, n_crops+1];  // acres across N_cty counties for N_crops + 1 crops (includes fallow)
  matrix[n_counties * n_crops, k] X; // X is a set of county-level characteristics.
  //matrix[n, k+1] z;             // random variation in individuals
}

// Define parameters - this is what we will estimate
parameters {
  vector[k] beta;
  //matrix[N_cty, N_crops] xi;
  //vector<lower = 0>[K+1] scales;
  //corr_matrix[K+1] Omega;
}

// Transformed parameters
transformed parameters {
  //cov_matrix[K+1] Sigma;
  //Sigma = quad_form_diag(Omega, scales);
}

// Model
model {
  // priors
  beta ~ beta(2, 5);
  //to_vector(xi) ~ normal(0, 2);
  //scales ~ inv_gamma(2,1);
  //Omega ~ lkj_corr(4);
  
  // model of acres 
  {
    matrix[n_counties, n_crops+1] pred_shares;
    for(g in 1:n_counties) {
      // predicted market shares given data and parameters
      pred_shares[g,1:n_crops] = shares(n_crops, beta, X[((g-1)*n_crops+1):(g*n_crops)]);
      pred_shares[g,n_crops+1] = 1 - sum(pred_shares[g,1:n_crops]);
      // acres are measured with multinomial measurement error
      acres[g] ~ multinomial(pred_shares[g]');
    }
  }
}
