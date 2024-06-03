# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/tests.html
# * https://testthat.r-lib.org/reference/test_package.html#special-files

library(testthat)
library(dsDidClient)
#install.packages("dsBaseClient", repos = "http://cran.datashield.org", dependencies = TRUE)
#install.packages("dsBase", repos = "http://cran.datashield.org")
install.packages("DSLite", repos = "http://cran.us.r-project.org")
install.packages("devtools", repos = "http://cran.us.r-project.org")
library(devtools)
devtools::install_github("manuhuth/dsDid")
# devtools::install_github("manuhuth/dsDidClient") #
devtools::install_github("datashield/dsBase")
devtools::install_github("datashield/dsBaseClient")
devtools::install_github("datashield/DSI")
library(dsBase)
library(dsBaseClient)
library(DSLite)
library(dsDid)
library(DSI)
test_check("dsDidClient")
