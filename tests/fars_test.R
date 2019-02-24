library(testthat)
library(dplyr)
library(readr)
library(tidyr)

test_that(fars_summarize_years(2014), is_a("integer"))          
