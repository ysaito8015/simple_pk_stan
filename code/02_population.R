if (!require("pacman")) {
    install.packages("pacman")
}
pacman::p_load(
    tidyverse,
    rstan,
    ggmcmc,
    rio,
    here
)

figDir <- here::here("outputs", "02")
if (!dir.exists(figDir)) {
    dir.create(figDir, recursive = TRUE)
}
stanDir <- here::here("stan_codes")
dataDir <- here::here("data")
toolsDir <- here::here("code", "tools")
source(here::here(toolsDir, "ipdb.R"))

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

data.pk <-
    rio::import(
        here::here(dataDir, "sim_pk_20170521.csv")
    )
# dbl (4): ID, TIME, CONC, DOSE_LEVEL
data.subj   <-
    rio::import(
        here::here(dataDir, "subj_dose_20170521.csv")
    )
# dbl (2): ID, DOSE

cat("\n\n")
print("summary of pk data")
print(summary(data.pk))
print(head(data.pk))
cat("\n\n")
print("summary of subject with dose data")
print(summary(data.subj))
print(head(data.subj))


cat("\n\n")
print("Data quick look")
data.pk.plot <-
    dplyr::mutate(
        data.pk,
        ID = factor(ID),
        DOSE_LEVEL = factor(DOSE_LEVEL)
    )

p1 <- data.pk.plot %>%
    # CONC: concentration in blood
    ggplot(aes(TIME, CONC, group = ID, color = DOSE_LEVEL)) + 
    geom_line() +
    facet_wrap(~DOSE_LEVEL) +
    scale_y_log10()
ggsave(here::here(figDir, "02-data-pk-all.pdf"), plot = p1)


cat("\n\n")
print("Without IIV, limit V_d")
print("Inter-individual variability (IIV)")
init <-
    function() {
        list(
            KA = exp(rnorm(1, log(0.5), 0.2)),
            CL = exp(rnorm(1, log(0.5), 0.2)),
            VD = exp(rnorm(1, log(5), 0.2)),
            s_Y = runif(1, 0.5, 2)
        )
    }

data <-
    list(
        N    = nrow(data.pk),
        N_ID = nrow(data.subj),
        ID   = data.pk$ID,
        TIME = data.pk$TIME,
        DOSE = data.subj$DOSE,
        Y    = data.pk$CONC
    )

fit.stan <-
    stan(
        file = here::here(stanDir, "mod_01_noIIV.stan"),
        data = data,
        init = init
    )
fit.stan


cat("\n\n")
print("Diagnostic plots")
fit.param <- ggmcmc::ggs(fit.stan)
list.param <- c("KA", "CL", "VD", "s_Y")

fit.param.plot <-
    fit.param %>%
    dplyr::filter(Parameter %in% c(list.param))

p2 <- ggmcmc::ggs_density(fit.param.plot)
p3 <- ggmcmc::ggs_traceplot(fit.param.plot)
p4 <- ggmcmc::ggs_autocorrelation(fit.param.plot)
p5 <- ggmcmc::ggs_Rhat(fit.param.plot)

ggsave(here::here(figDir, "02-fit-param-density.pdf"), plot = p2)
ggsave(here::here(figDir, "02-fit-param-traceplot.pdf"), plot = p3)
ggsave(
    here::here(figDir, "02-fit-param-autocorrelation.pdf"),
    plot = p4
)
ggsave(here::here(figDir, "02-fit-param-Rhat.pdf"), plot = p5)


cat("\n\n")
print("Prediction vs Observed")
mcmc.sample <- rstan::extract(fit.stan)

# Plot for each subjects
y.pred.interval <-
    mcmc.sample$y_new %>%
    apply(MARGIN = 2, quantile, prob = c(0.05, 0.5, 0.95)) %>%
    t() %>%
    tibble::as_tibble(.name_repair = "unique")

data.fit.plot.each.subject <-
    bind_cols(data.pk, y.pred.interval) %>%
    dplyr::filter(ID <= 9) %>%
    dplyr::mutate(ID = factor(ID))

p6 <-
    data.fit.plot.each.subject %>%
    ggplot(aes(TIME, `50%`, color = ID)) +
    facet_wrap(~ID) +
    geom_line() +
    geom_ribbon(
        aes(ymin = `5%`, ymax = `95%`, fill = ID),
        alpha = 0.1
    ) +
    geom_point(
        data = data.pk %>%
        dplyr::filter(ID <= 9) %>%
        dplyr::mutate(ID = factor(ID)),
        aes(TIME,CONC)
    ) +
    ylab("CONC")

# Plot for each dose levels
data.fit.plot.each.dose <-
    # Convert to tibble
    mcmc.sample$y_new %>%
    t() %>%
    tibble::as_tibble(.name_repair = "unique") %>%
    # Re-groupe predicted values according to TIMEand DOSE_LEVEL
    bind_cols(data.pk, .) %>%
    tidyr::gather(key, prediction, -(ID:DOSE_LEVEL)) %>%
    group_by(TIME, DOSE_LEVEL) %>%
    # Calculate quantiles
    summarize(
        `5%` = quantile(prediction, probs = 0.05),
        `50%` = quantile(prediction, probs = 0.5),
        `95%` = quantile(prediction, probs = 0.95)
    ) %>%
    ungroup() %>%
    dplyr::mutate(DOSE_LEVEL = factor(DOSE_LEVEL))

p7 <-
    data.fit.plot.each.dose %>%
    ggplot(aes(TIME, `50%`, color = DOSE_LEVEL)) +
    facet_wrap(~DOSE_LEVEL, scale = "free") +
    geom_line() +
    geom_ribbon(aes(ymin = `5%`, ymax = `95%`), alpha = 0.1) +
    geom_line(
        data = data.pk.plot,
        aes(TIME, CONC, group = ID),
        linetype = "dotted"
    ) +
    geom_point(
        data = data.pk.plot,
        aes(TIME, CONC, group = ID)
    ) +
    ylab("CONC")

ggsave(
    here::here(figDir, "02-posterior-facet-subjects.pdf"),
    plot = p6
)
ggsave(
    here::here(figDir, "02-posterior-facet-dose-level.pdf"),
    plot = p7
)


cat("\n\n")
print("With IIV")
print("Inter-individual variability (IIV)")

init <-
    function() {
        list(
            KA   = exp(rnorm(1, log(0.5), 0.2)),
            CL   = exp(rnorm(1, log(0.5), 0.2)),
            VD   = exp(rnorm(1, log(5.0), 0.2)),
            s_CL = exp(rnorm(1, log(0.2), 0.5)),
            s_VD = exp(rnorm(1, log(0.2), 0.5)),
            s_Y  = runif(1, 0.5, 2),
            CLi  = rep(0.5, nrow(data.subj)),
            VDi  = rep(5.0, nrow(data.subj))
        )
    }

data <-
    list(
        N    = nrow(data.pk),
        N_ID = nrow(data.subj),
        ID   = data.pk$ID,
        TIME = data.pk$TIME,
        DOSE = data.subj$DOSE,
        Y    = data.pk$CONC
    )

fit.stan <-
    stan(
        file = here::here(stanDir, "mod_02_withIIV.stan"),
        data = data,
        init = init
    )
fit.stan


cat("\n\n")
print("Parameter distributions")
print("Individual parameter estimates (IPRED)")

fit.CLi <-
    summary(fit.stan, pars = c("CLi"))$summary %>%
    tibble::as_tibble(.name_repair = "unique") %>%
    dplyr::select(mean)

fit.VDi <-
    summary(fit.stan, pars = c("VDi"))$summary %>%
    tibble::as_tibble(.name_repair = "unique") %>%
    dplyr::select(mean)

fit.individual.params <-
    dplyr::bind_cols(
        tibble::tibble(ID = 1:nrow(fit.CLi)),
        fit.CLi %>% dplyr::rename(CL = mean),
        fit.VDi %>% dplyr::rename(VD = mean)
    )

p8 <-
    fit.individual.params %>%
    ggplot(aes(CL, VD)) +
    geom_point() +
    geom_smooth(formula = y ~ x, method = "lm")

ggsave(
    here::here(figDir, "02-individual-params-estimates.pdf"),
    plot = p8
)


cat("\n\n")
print("Parameter distributions")
print("Population parameter estimates + IIV")
print("Inter-individual variability (IIV)")

mcmc.sample <- rstan::extract(fit.stan)

fit.CLiPred <- mcmc.sample$CLiPred[1, ]
fit.VDiPred <- mcmc.sample$VDiPred[1, ]

fit.individual.params.pred <-
    tibble::tibble(
        CL = fit.CLiPred,
        VD = fit.VDiPred
    )

p9 <-
    fit.individual.params.pred %>%
    ggplot(aes(CL, VD)) +
    geom_point() +
    geom_smooth(formula = y ~ x, method = "lm")

ggsave(
    here::here(figDir, "02-population-params-estimates.pdf"),
    plot = p9
)


cat("\n\n")
print("Parameter distributions")
print("Combined plot")

combined.data <-
    bind_rows(
        fit.individual.params %>%
        dplyr::mutate(TYPE = "Estimates"),
        fit.individual.params.pred %>%
        dplyr::mutate(TYPE = "Predicted")
    )

p10 <-
    combined.data %>%
    ggplot(aes(CL, VD, color = TYPE, fill = TYPE)) +
    geom_point() +
    geom_smooth(formula = y ~ x, method = "lm", alpha = 0.1)

ggsave(
    here::here(figDir, "02-combinded-plot.pdf"),
    plot = p10
)

cat("\n\n")
print("Check covariates")

data.cov <-
    rio::import(
        here::here(dataDir, "subj_dose_cov_20170521.csv")
    )

summary(data.cov)
head(data.cov)

fit.individual.params.cov <-
    dplyr::full_join(fit.individual.params, data.cov)

fit.individual.params.cov.plot <-
    fit.individual.params.cov %>%
    tidyr::gather(Parameter, Value, CL, VD)

p11 <-
    fit.individual.params.cov.plot %>%
    ggplot(aes(WT, Value)) +
    geom_point() +
    facet_wrap(~Parameter, scales = "free") +
    geom_smooth(formula = y ~ x, method = "lm")

ggsave(
    here::here(figDir, "02-covariate-weight-plot.pdf"),
    plot = p11
)

p12 <-
    fit.individual.params.cov.plot %>%
    ggplot(aes(factor(SEX), Value)) +
    geom_boxplot() +
    facet_wrap(~Parameter, scales = "free")

ggsave(
    here::here(figDir, "02-covariate-sex-plot.pdf"),
    plot = p12
)
