# Author: Sanjt N# Author: Sanjt Narwekar
# Forked from Author: Alex Lemm
# Forked from Repo: alex23lemm/Advanced-R-Programming-Course
# Repo: https://github.com/SanjitNarwekar/Advanced-R-Programming-Course

# Load libraries ---------------------------------------------------------------

library(purrr)
library(microbenchmark)
library(installr)

# Define functions ------------------------------------------------------------


# function factorial_loop -----------------------------------------------------
#
# This function uses a for loop to calculate factorials. The function expects 
# only a postive integer as an input.
#
#------------------------------------------------------------------------------

factorial_loop <- function(x) {
  
  check.integer <- installr:::check.integer
  
  if (x == 0 ||  x == -0 || x == 1){
    return(1)
  }
  
  if (!check.integer(x)){
    message("Input ",x," is not an integer. abs(x) used. Results will not tally with internal R function")
    
  }
  
  if ( x < 0 ){
    message("Input ",x," is not positive. abs(x) used. Results will not tally with internal R function")
  }
  
  for (i in (x - 1):1) {
    x <- abs(x)
    x <- x * i
  }
  x
}

# function factorial_reduce -----------------------------------------------------
#
# This function uses the reduce function to calculate factorials. The function expects 
# only a postive integer as an input.
#
#------------------------------------------------------------------------------


factorial_reduce <- function(x) {
  
  check.integer <- installr:::check.integer
  
  if (x == 0 ||  x == -0 || x == 1){
    return(1)
  }
  
  if (!check.integer(x)){
    message("Input ",x," is not an integer. abs(x) used. Results will not tally with internal R function")
    
  }
  
  if ( x < 0 ){
    message("Input ",x," is not positive. abs(x) used. Results will not tally with internal R function")
  }
  
  reduce(1:x, `*`)
  
}

# function factorial_func -----------------------------------------------------
#
# This function uses recursion to calculate factorials. The function expects 
# only a postive integer as an input.
#
#------------------------------------------------------------------------------

factorial_func <- function(x) {
  
  check.integer <- installr:::check.integer
  
  if (x == 0 ||  x == -0 || x == 1){
    return(1)
  }
  
  if (!check.integer(x)){
    message("Input ",x," is not an integer. abs(x) used. Results will not tally with internal R function")
    
  }
  
  if ( x < 0 ){
    message("Input ",x," is not positive. abs(x) used. Results will not tally with internal R function")
  }
  
  x * factorial_func(x - 1)
  
}

# function factorial_mem -----------------------------------------------------
#
# This function uses memoisation to calculate factorials. The function expects 
# only a postive integer as an input.
#
#------------------------------------------------------------------------------

check.integer <- installr:::check.integer

# Create lookup table for memoization with 0 and 1 as  the 1st 2 values and 65 NA's
# This should give fast calculation of values of 0 and 1 and 65 other numbers sfter 
# they have been calculated atleast once.


fact_tbl <- c(rep(NA, 65))

factorial_mem <- function(x) {
  
  if (x == 0 ||  x == -0 || x == 1){
    return(1)
  }
  
  stopifnot(x>0)
  stopifnot(check.integer(x))
  
  
  if (!is.na(fact_tbl)[x])
    return(fact_tbl[x])
  fact_tbl[x] <<- x * factorial_mem(x - 1)
  fact_tbl[x]
}



# test and measure factorial functions ----------------------------------------
#
# This script calls 4 factorial functions written by me to compare with the 
# built in factorial function in R.
#
#------------------------------------------------------------------------------

# Create lookup table for memoization with 0 and 1 as  the 1st 2 values and 65 NA's
# This should give fast calculation of values of 0 and 1 and 65 other numbers sfter 
# they have been calculated atleast once.


fact_tbl <- c(rep(NA, 65))

# Test functions --------------------------------------------------------------

input <- c(0, 1, 4, 5)

# Check if all functions produce the same results. R's built-in function
# factorial() is used to compare the results
factorial(input)
map_dbl(input, factorial_loop)
map_dbl(input, factorial_reduce)
map_dbl(input, factorial_func)
map_dbl(input, factorial_mem)

# Measure performance and create output ----------------------------------------

# Use microbenchmark and purrr package to calculate performance for different 
# input values and for ranges of input values

sink("factorial_output.txt") 

cat("====== PART 1: Performance and comparison of individual input values ======\n")
cat("======================== across factorial functions ======================= \n\n")

# Calculate and compare perforamnce of individual input values
individual_results <- map(input, ~ microbenchmark(
  factorial_loop(.),
  factorial_reduce(.),
  factorial_func(.),
  factorial_mem(.)
))

names(individual_results) <- as.character(input)
individual_results

# Calculate and compare performance of ranges of input values

cat("====== PART 2: Performance and comparison of ranges of input values =======\n")
cat("======================== across factorial functions ======================= \n\n")

get_benchmark <- function(x) {
  #fact_tbl <<- c(rep(NA, 100))
  microbenchmark(map_dbl(x, factorial_loop),
                 map_dbl(x, factorial_reduce),
                 map_dbl(x, factorial_func),
                 map_dbl(x, factorial_mem))
}

ranges <- list(`range 1:10` = 1:10,
               `range 1:25` = 1:25,
               `range 1:65` = 1:65)

range_results <- map(ranges, get_benchmark)
range_results
sink()