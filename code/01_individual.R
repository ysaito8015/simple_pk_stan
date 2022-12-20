library(tidyverse)
library(rstan)
library(ggmcmc)

rstan_options(auto_write = T)
options(mc.cores = parallel::detectCores())

data.pk   <- read_csv("../data/sim_pk_20170521.csv") 
data.subj <- read_csv("../data/subj_dose_20170521.csv") 

data.pk %>%
  filter(ID == 1) %>% 
  ggplot(aes(TIME, CONC)) +
  geom_line() +
  geom_point() 

full_join(data.pk, data.subj) %>% 
  ggplot(aes(TIME, CONC, group=ID)) +
  geom_line() +
  geom_point() +
  facet_wrap(~DOSE) +
  scale_y_log10()

data.pk.id1 <- data.pk %>% filter(ID == 1)

init <- function(){
    list(KA = exp(rnorm(1, log(0.5), 0.2)),
         CL = exp(rnorm(1, log(0.5), 0.2)),
         VD = exp(rnorm(1, log(5), 0.2)),
         s_Y = runif(1, 0.5, 2))
}

data <- 
  list(N    = nrow(data.pk.id1),
       TIME = data.pk.id1$TIME,
       DOSE = 10,
       Y    = data.pk.id1$CONC)


fit.stan <-
  stan(file = "mod_00_single_subj.stan", 
       data = data, init = init)

fit.stan


# http://xavier-fim.net/packages/ggmcmc/#using-ggmcmc

fit.param <- ggs(fit.stan)
list.param <- c("KA", "CL", "VD", "s_Y")

fit.param.plot <- 
  fit.param %>% filter(Parameter %in% c(list.param))

ggs_density(fit.param.plot)
ggs_traceplot(fit.param.plot)
ggs_autocorrelation(fit.param.plot)

ggs_Rhat(fit.param.plot)



mcmc.sample <- rstan::extract(fit.stan)

y.pred.interval <- 
  mcmc.sample$y_new %>% 
  apply(MARGIN=2, quantile, prob=c(0.05, 0.5, 0.95)) %>% 
  t() %>% 
  tibble::as_tibble()

pdf("../outputs/01.pdf")
bind_cols(data.pk.id1,
          y.pred.interval) %>% 
  ggplot(aes(TIME, `50%`)) +
  geom_line() +
  geom_ribbon(aes(ymin=`5%`, ymax=`95%`),alpha=0.1) +
  geom_point(data=data.pk.id1, aes(TIME,CONC))
dev.off()
