rm(list = ls())
gc()

source("./environments.R")
.libPaths(libDir)

pacman::p_load(
  tidyverse,
  rstan,
  ggmcmc,
  rio
)

rstan::rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

figDir <- file.path(outDir, "01")
if (!dir.exists(figDir)) {
  dir.create(figDir)
}


data.pk <-
  rio::import(file.path(dataDir, "sim_pk_20170521.csv") )
# dbl (4): ID, TIME, CONC, DOSE_LEVEL
data.subj <-
  rio::import(file.path(dataDir, "subj_dose_20170521.csv")) 
# dbl (2): ID, DOSE

cat("\n\n")
print("summary of pk data")
print(summary(data.pk))
print(head(data.pk))
cat("\n\n")
print("summary of subject with dose data")
print(summary(data.subj))
print(head(data.subj))

p <-
  data.pk %>%
  filter(ID == 1) %>% 
  ggplot(aes(TIME, CONC)) + # CONC: concentration in blood
  geom_line() +
  geom_point() 
ggsave(
  file.path(figDir, "01-data-pk-id1.pdf"),
  plot = p
)

p <-
  full_join(data.pk, data.subj) %>% 
  ggplot(aes(TIME, CONC, group=ID)) +
  geom_line() +
  geom_point() +
  facet_wrap(~DOSE) +
  scale_y_log10()
ggsave(
  file.path(figDir, "02-data-pk-facet-by-dose.pdf"),
  plot = p
)


data.pk.id1 <- data.pk %>% filter(ID == 1)

cat("\n\n")
print("first 6 rows of pk data with ID:1")
print(head(data.pk.id1))

# Initial states of variables
init <- function(){
  list(
    KA =
      exp(
        rnorm(
          n = 1,
          mean = log(0.5),
          sd = 0.2
        )
      ), # Absorption rate constant (k_a)
    CL =
      exp(
        rnorm(
          n = 1,
          mean = log(0.5),
          sd = 0.2
        )
      ), # Clearance (CL)
    VD =
      exp( 
        rnorm(
          n = 1,
          mean = log(0.5),
          sd = 0.2
        )
      ), # Volume of distribution (Vd)
    sd_Y =
      runif(
         n = 1,
         min = 0.5,
         max = 2
       ) # standard deviation of Y in log scale (observation, measured conc.)
  )
}

# Join data into list class for stan execution
data <- 
  list(
    N    = nrow(data.pk.id1),
    TIME = data.pk.id1$TIME,
    DOSE = 10,
    Y    = data.pk.id1$CONC
  )

cat("\n\n")
print("list data for stan")
print(str(data))


# Execute stan
fit.stan <-
  stan(
    file = file.path(stanDir, "mod_00_single_subj.stan"),
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

p <- ggs_density(fit.param.plot)
ggsave(
  file.path(figDir, "03-posterior-density-plot.pdf"),
  plot = p
)

p <- ggs_traceplot(fit.param.plot)
ggsave(
  file.path(figDir, "04-posterior-trace-plot.pdf"),
  plot = p
)

p <- ggs_autocorrelation(fit.param.plot)
ggsave(
  file.path(figDir, "05-posterior-autocorrelation-plot.pdf"),
  plot = p
)

p <- ggs_Rhat(fit.param.plot)
ggsave(
  file.path(figDir, "06-posterior-rhat-plot.pdf"),
  plot = p
)


mcmc.sample <- rstan::extract(fit.stan)

y.pred.interval <- 
  mcmc.sample$y_new %>% 
  apply(MARGIN = 2, quantile, prob = c(0.05, 0.5, 0.95)) %>% 
  t() %>% 
  tibble::as_tibble()

print(head(y.pred.interval))

p <-
  bind_cols(data.pk.id1, y.pred.interval) %>% 
  ggplot(aes(TIME, `50%`)) +
  geom_line() +
  geom_ribbon(aes(ymin = `5%`, ymax = `95%`),alpha = 0.1) +
  geom_point(data=data.pk.id1, aes(TIME,CONC))
ggsave(
  file.path(figDir, "07-estimate-mean-values-with-predicted-interval.pdf"),
  plot = p
)
