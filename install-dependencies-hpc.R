# Install the required package dependencies
cat("Installing dependencies...\n")

# Install / load renv
if (require("renv")) {
  cat(" renv is installed and loaded")
} else {
  cat(" Installing renv...")
  install.packages("renv")
  if (require("renv")) {
    cat(" renv is installed and loaded")
  } else {
    stop(" Failed to install renv")
  }
}

# Restore the state of the project (i.e. install all dependencies)
renv::restore()

# Install cmdstanr and cmdstan
cat(" Installing cmdstanr...\n")
install.packages("https://mc-stan.org/r-packages/src/contrib/cmdstanr_0.5.2.tar.gz",
		 repos = NULL)

require("cmdstanr")
cmdstanr::check_cmdstan_toolchain()
cmdstanr::install_cmdstan()

cat(" DONE!\n")
