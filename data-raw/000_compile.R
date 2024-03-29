
# Compile BritPol

# 1. Housekeeping ---------------------------------------------------------

# Load packages

library(rmarkdown)
library(here)



# 2. Compile data and models ----------------------------------------------

# Create mini datasets

source(here("data-raw", "001_mini_data.R"))


# Fit sample size imputation model

source(here("data-raw", "002_samplesizes.R"))


# Format raw pollbase data

source(here("data-raw", "003_pollbase.R"))


# Derive daily pollbasepro estimates

source(here("data-raw", "004_pollbasepro.R"))


# Validate the resulting estimates

source(here("data-raw", "005_validation.R"))


# Use data to fit applied example model for the paper

source(here("data-raw", "006_example.R"))



# 3. Compile documentation ------------------------------------------------

# Render paper

render(
  input = here("documentation", "paper.Rmd"),
  output_file = here("documentation", "paper.pdf")
)


# Render user guide and codebook

render(
  input = here("documentation", "userguide.Rmd"),
  output_file = here("documentation", "userguide.pdf")
)


# Render GitHub README file

render(
  input = here("documentation", "readme.Rmd"),
  output_format = "github_document",
  output_file = here("README.md")
)



# 4. Check and install ----------------------------------------------------

# We'll save session information to the "sessions" folder

britpol:::save_info(path = here("sessions","000_compile.txt"))


# Now, we'll run our unit tests to check that nothing weird is happening or
# that the data don't contain any obvious errors.

devtools::check()


# Then, we'll reinstall the package in full.

devtools::install()


