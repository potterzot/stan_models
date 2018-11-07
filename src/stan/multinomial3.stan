// STAN model to implement aggregate random coefficients logit model
// 
  
  // first we define the function that takes data and parameters and returns predicted market shares
functions {
  // calculates shares for a given market
  row_vector shares(int n_crops, matrix beta, matrix X) {
    matrix[n_crops, cols(X)] tmp;
    row_vector[n_crops] tmpsum;
    row_vector[n_crops] profits;
    row_vector[n_crops] shares;
    
    tmp = X .* beta;
    
    for(i in 1:n_crops) {
      tmpsum[i] = sum(tmp[i]);
    }
    profits = exp(tmpsum);
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
}

// Define parameters - this is what we will estimate
parameters {
  //vector[k] beta;
  matrix[n_crops, k] beta;
}

// Transformed parameters
transformed parameters {
}

// Model
model {
  // priors
  for(j in 1:n_crops) {
    beta[j] ~ normal(0,1);
  }
  
  // model of acres 
  {
    matrix[n_counties, n_crops+1] pred_shares;
    real sum_pred_shares;
    for(g in 1:n_counties) {
      // predicted market shares given data and parameters
      pred_shares[g,1:n_crops] = shares(n_crops, beta, X[((g-1)*n_crops+1):(g*n_crops)]);
      sum_pred_shares = sum(pred_shares[g,1:n_crops]);
      pred_shares[g,n_crops+1] = 1 - sum_pred_shares; //(pred_shares[g,1:n_crops]);
      // acres are measured with multinomial measurement error
      acres[g] ~ multinomial(pred_shares[g]');
    }
  }
}
