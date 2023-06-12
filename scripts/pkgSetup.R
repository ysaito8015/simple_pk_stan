pkgs <-
  c(
    "RcppParallel",
    "RcppEigen",
    "gridExtra",
    "V8",
    "inline",
    "loo",
    "tidyverse",
    "bayesplot",
    "ggmcmc",
    "mrgsolve",
    "pacman",
    "rio"
  )
stan_pkgs <-
  c(
    "StanHeaders",
    "rstan"
  )

source("./environments.R")
.libPaths(libDir)


install.packages(
  c("devtools", "BH", "remotes"),
  lib = libDir,
  contriburl = c(
    contrib.url("http://r-forge.r-project.org","source"),
    contrib.url("https://cran.rstudio.com/","source")
  ),
  destdir = pkgDir,
  type = "source",
  dependencies = c("Depends", "Imports", "LinkingTo"),
  INSTALL_opts = "--no-multiarch"
)

install.packages(
  pkgs,
  lib = libDir,
  contriburl = c(
    contrib.url("http://r-forge.r-project.org","source"),
    contrib.url("https://cran.rstudio.com/","source")
  ),
  destdir = pkgDir,
  type = "source",
  dependencies = c("Depends", "Imports", "LinkingTo"),
  INSTALL_opts = "--no-multiarch"
)

install.packages(
  stan_pkgs,
  lib = libDir,
  contriburl = c(
    contrib.url("https://mc-stan.org/r-packages/","source"),
    contrib.url("http://r-forge.r-project.org","source")
  ),
  destdir = pkgDir,
  type = "source",
  dependencies = c("Depends", "Imports", "LinkingTo"),
  INSTALL_opts = "--no-multiarch"
)
