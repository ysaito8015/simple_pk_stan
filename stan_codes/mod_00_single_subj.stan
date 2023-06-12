data {
  int N;           // Total number of observations
  vector[N] TIME;  // TIME for each data
  real DOSE;       // DOSE for each subject
  vector[N] Y;     // Observation
}

parameters {
  real<lower=0> K_a;  // Absorption rate constant (k_a)
  real<lower=0> Cl;  // Clearance (CL)
  real<lower=0> Vd;  // Volume of distribution (Vd)
  real<lower=0> sd_Y; // SD of Y in log scale
}

// (ODE) Ordinary Differential Equations
transformed parameters {
  real K_el;     // Elimination rate constant (k_el)
  vector[N] mu; // Calculated mean concentration in blood

  // Calculate kel from Cl and Vd
  K_el = Cl / Vd;

  // Closed-form expression of 1 compartment model
  // dC/dt
  // https://math.stackexchange.com/questions/1360981/analytic-solution-to-the-one-compartment-model
  mu =
    (DOSE / Vd) * K_a * (exp(-K_a * TIME) - exp(-K_el * TIME))
    / (K_el - K_a);
}

model {
  // Weak priors, Y ~ lognormal(mu, sigma)
  K_a ~ lognormal(log(0.5), 1);  // Absorption rate constant (k_a)
  Cl  ~ lognormal(log(0.5), 1);  // Clearance (Cl)
  Vd  ~ lognormal(log(5),   1);  // Volume of distribution (Vd)

  // Assume Y follows log-normal distribution
  Y  ~ lognormal(log(mu),  sd_Y);
}

generated quantities {
  vector[N] y_new;

  for (n in 1:N){
    y_new[n]  = lognormal_rng(log(mu[n]), sd_Y);
  }
}

