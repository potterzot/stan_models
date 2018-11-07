// Data block, determines data inputs.
data {
  int n_counties;
  int n_crops;
  int k; // number of covariates that determine shares
  matrix[n_counties*(n_crops), k] X; //county-level data
  int acres[n_counties, n_crops]; //crop acres
}

// Parameter block, determines what parameters are allowed
parameters {
  vector[k] beta;           //coefficients on covariates
  simplex[n_crops] theta; //probabilities of each crop  
}

transformed parameters {
  //simplex[n_crops] theta; //probability of each crop being selected
  //theta = dirichlet( softmax( X * beta ) );
}
// Model block, specifies model and priors
model {
  // priors
  beta ~ lognormal(0,1);
  //theta ~ normal(0,1); // coefficients on covariates
  //theta[]
  
  // model definition
  // Here we estimate shares based on county-level data
  // First calculate the shares for all crop choices, then add the share of the "no crop" choice
  for(g in 1:n_counties) {
    //predicted_shares[g, 1:n_crops] = shares(beta, X[((g-1)*n_crops+1):(g*n_crops)]);
    //predicted_shares[g, 1:n_crops] ~ dirichlet( softmax( X[((g-1)*n_crops+1):(g*n_crops)] * beta ) );
    //predicted_shares[g, n_crops+1] = 1 - sum(predicted_shares[g, 1:n_crops]);
    theta ~ dirichlet(X[((g-1)*n_crops+1):(g*n_crops)]*beta);
    acres[g] ~ multinomial(theta);
  }
}
