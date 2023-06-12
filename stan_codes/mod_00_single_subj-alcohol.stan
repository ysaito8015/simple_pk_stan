data {
  int N;           // Total number of observations
  vector[N] TIME;  // TIME for each data
  real DOSE;       // DOSE for each subject
  vector[N] Y;     // Observation
}

parameters {
  real<lower=0> KA;  // Absorption rate constant (ka)
  real<lower=0> CL;  // Clearance (CL)
  real<lower=0> VD;  // Volume of distribution (Vd)
  real<lower=0> sd_Y; // SD of Y in log scale
}

// (ODE) Ordinary Differential Equations
transformed parameters {
  real KEL;     // Elimination rate constant (kel)
  vector[N] mu; // Calculated concentration

  // Calculate kel from CL and Vd
  KEL = CL / VD;

  // Analytical solution of 1 compartment model
  mu =
    (DOSE / VD) * KA * (exp(-KA * TIME) - exp(-KEL * TIME)) / (KEL - KA);
}

model {
  // Weak priors, Y ~ lognormal(mu, sigma)
  KA ~ lognormal(log(0.5), 1);  // Absorption rate constant (ka)
  CL ~ lognormal(log(0.5), 1);  // Clearance (CL)
  VD ~ lognormal(log(5),   1);  // Volume of distribution (Vd)

  // Assume Y follows log-normal distribution
  Y  ~ lognormal(log(mu),  sd_Y);
}

generated quantities {
  vector[N] y_new;

  for (n in 1:N){
    y_new[n]  = lognormal_rng(log(mu[n]), sd_Y);
  }
}

