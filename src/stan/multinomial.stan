functions {
  // calculate crop shares for a given county
  row_vector shares(vector beta, matrix X) {
    X*beta
    
  }
}
// Data block, determines data inputs.
data {
  int n_counties;
  int n_crops;
  int k; // number of covariates that determine shares
  int acres[n_counties, n_crops];
}

// Parameter block, determines what parameters are allowed
parameters {
  vector[k] beta; //probability of each crop being selected
}

// Model block, specifies model and priors
model {
  // priors
  beta ~ normal(0,1); // coefficients on covariates
  
  // model definition
  // Here we estimate shares based on county-level data
  // First calculate the shares for all crop choices, then add the share of the "no crop" choice
  for(g in 1:n_counties) {
    predicted_shares[g, 1:n_crops] = shares(beta, X[((g-1)*n_crops+1):(g*n_crops)]);
    predicted_shares[g, n_crops+1] = 1 - sum(predicted_shares[g, 1:n_crops]);
    acres[g] ~ multinomial(predicted_shares);
  }
}