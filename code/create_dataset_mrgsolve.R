# setup
install.packages("mrgsolve")
pacman::p_load(tidyverse, mrgsolve)
set.seed(1)

# Ref: https://github.com/metrumresearchgroup/mrgsolve
# Note that OMEGA and SIGMA are defined as variance, not deviations
code <- "
  $SET delta = 0.1
  $PARAM TVCL = 0.3, TVVD = 5, KA = 1, WT = 70, SEX = 1
  $CMT GUT CENT
  $MAIN
    double CL = exp(log(TVCL) + 0.75 * log(WT/70) + ETA(1));
    double VD = exp(log(TVVD) +        log(WT/70) + ETA(2));
  $OMEGA 0.01 0.01
  $SIGMA 0.0225
  $ODE
    dxdt_GUT  = -KA * GUT;
    dxdt_CENT =  KA * GUT - (CL/VD) * CENT;
  $TABLE
    double CP_IPRED = CENT / VD;
    double CP = CENT / VD * exp(EPS(1));
  $CAPTURE CP CP_IPRED CL VD WT SEX
"
mod <- mcode("sim", code)

# Run simulation
# Set dose
dose_level <- c(10, 20, 40, 80)
nsub_dose <- 10
nsub <- length(dose_level) * nsub_dose

# Set covariates
covariates <-
  tibble(
    "ID" = 1:nsub,
    "WT" = rnorm(nsub, mean = 80, sd = 20),
    "SEX" = rbinom(nsub, 1, 0.5)
  )

# Run model for each dose
out <- tibble()

for(k in seq_along(dose_level)) {
  cov_each = covariates[((k-1) * nsub_dose + 1):(k * nsub_dose),]
  out_each <-
    mod %>%
      idata_set(cov_each) %>%
      ev(amt = dose_level[k]) %>%
      mrgsim(end = 24, delta = 0.5) %>%
      mutate(
        DOSE = dose_level[k],
        ID_each = ID - (k-1) * nsub_dose
      ) %>%
      filter(time %in% c(0.25, 0.5, 1, 2, 4, 6, 8, 12, 24))
  out <- bind_rows(out, out_each)
}


# Plot to check simulation profile
out %>%
  filter(time > 0) %>%
  ggplot(aes(time, CP, group = ID, color = factor(ID_each))) +
  geom_line(aes(y = CP_IPRED)) +
  geom_point() +
  facet_wrap(~DOSE, scales = "free")

out %>%
  filter(time > 0) %>%
  ggplot(aes(time, CP, group = ID, color = factor(ID_each))) +
  geom_line(aes(y = CP_IPRED)) +
  geom_point() +
  facet_wrap(~DOSE) +
  scale_y_log10()
