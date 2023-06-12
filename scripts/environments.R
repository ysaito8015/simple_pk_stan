scriptDir <- getwd()  ## You may need to mod this to be in the top level of scriptDir
projectDir <- dirname(scriptDir)
dataDir <- file.path(projectDir, "data")
stanDir <- file.path(projectDir, "stan_codes")
pkgDir <- file.path(projectDir, "pkg")
if (!dir.exists(pkgDir)) {
    dir.create(pkgDir)
}
libDir <- file.path(projectDir, "lib")
if (!dir.exists(libDir)) {
    dir.create(libDir)
}
outDir <- file.path(projectDir, "outputs")
if (!dir.exists(outDir)) {
    dir.create(outDir)
}

.libPaths(libDir)

Sys.setenv("PKG_CXXFLAGS"="-std=gnu++20 -DBOOST_PHOENIX_NO_VARIADIC_EXPRESSION")
Sys.setenv("CXXFLAGS"="-std=gnu++20 -DBOOST_PHOENIX_NO_VARIADIC_EXPRESSION")
