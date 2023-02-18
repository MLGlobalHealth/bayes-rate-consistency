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
install.packages("cmdstanr",
                 version = "0.5.2",
                 repos = c("https://mc-stan.org/r-packages/",
                 getOption("repos")))

require("cmdstanr")
cmdstanr::check_cmdstan_toolchain()
cmdstanr::install_cmdstan()

cat(" DONE!\n")
