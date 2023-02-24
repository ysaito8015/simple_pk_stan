if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  rstan,
  ggmcmc,
  rio
)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

data.pk   <- rio::import("../data/sim_pk_20170521.csv") 
# dbl (4): ID, TIME, CONC, DOSE_LEVEL
data.subj <-rio::import("../data/subj_dose_20170521.csv") 
# dbl (2): ID, DOSE

cat("\n\n")
print("summary of pk data")
print(summary(data.pk))
print(head(data.pk))
cat("\n\n")
print("summary of subject with dose data")
print(summary(data.subj))
print(head(data.subj))

pdf("../outputs/01/01-data-pk-id1.pdf")
  data.pk %>%
    filter(ID == 1) %>% 
    ggplot(aes(TIME, CONC)) + # CONC: concentration in blood
    geom_line() +
    geom_point() 
dev.off()

pdf("../outputs/01/01-data-pk-facet-by-dose.pdf")
  full_join(data.pk, data.subj) %>% 
    ggplot(aes(TIME, CONC, group=ID)) +
    geom_line() +
    geom_point() +
    facet_wrap(~DOSE) +
    scale_y_log10()
dev.off()

data.pk.id1 <- data.pk %>% filter(ID == 1)

cat("\n\n")
print("first 6 rows of pk data with ID:1")
print(head(data.pk.id1))

# Initial states of variables
init <- function(){
    list(KA = exp(rnorm(1, log(0.5), 0.2)), # Absorption rate constant (k_a)
         CL = exp(rnorm(1, log(0.5), 0.2)), # Clearance (CL)
         VD = exp(rnorm(1, log(5), 0.2)),   # Volume of distribution (Vd)
         s_Y = runif(1, 0.5, 2))            # standard deviation of Y in log scale (observation, measured conc.)
}

# Join data into list class for stan execution
data <- 
  list(N    = nrow(data.pk.id1),
       TIME = data.pk.id1$TIME,
       DOSE = 10,
       Y    = data.pk.id1$CONC)

cat("\n\n")
print("list data for stan")
print(head(data))


# Execute stan
fit.stan <-
  stan(
    file = "../stan_codes/mod_00_single_subj.stan", 
    data = data,
    init = init
  )


cat("\n\n")
print("results of stan simulation")
print(fit.stan)


# http://xavier-fim.net/packages/ggmcmc/#using-ggmcmc

fit.param <- ggs(fit.stan)
list.param <- c("KA", "CL", "VD", "s_Y")

fit.param.plot <- 
  fit.param %>% filter(Parameter %in% c(list.param))

pdf("../outputs/01/01-posterior-density-plot.pdf")
  ggs_density(fit.param.plot)
dev.off()

pdf("../outputs/01/01-posterior-trace-plot.pdf")
  ggs_traceplot(fit.param.plot)
dev.off()

pdf("../outputs/01/01-posterior-autocorrelation-plot.pdf")
  ggs_autocorrelation(fit.param.plot)
dev.off()

pdf("../outputs/01/01-posterior-rhat-plot.pdf")
  ggs_Rhat(fit.param.plot)
dev.off()



mcmc.sample <- rstan::extract(fit.stan)

y.pred.interval <- 
  mcmc.sample$y_new %>% 
  apply(MARGIN=2, quantile, prob=c(0.05, 0.5, 0.95)) %>% 
  t() %>% 
  tibble::as_tibble()

print(y.pred.interval)

pdf("../outputs/01/01-estimate-mean-values-with-predicted-interval.pdf")
  bind_cols(data.pk.id1,
            y.pred.interval) %>% 
    ggplot(aes(TIME, `50%`)) +
    geom_line() +
    geom_ribbon(aes(ymin=`5%`, ymax=`95%`),alpha=0.1) +
    geom_point(data=data.pk.id1, aes(TIME,CONC))
dev.off()
