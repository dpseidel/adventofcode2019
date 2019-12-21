## Puzzle 4
library(tidyverse)
library(testthat)
# Rules
## It is a six-digit number.
## the value is within the range given in your puzzle input.
## Two adjacent digits are the same (like 22 in 122345).
## Going from left to right, the digits never decrease; they only ever increase or stay the same (like 111123 or 135679).

# Other than the range rule, the following are true:
## 111111 meets these criteria (double 11, never decreases).
## 223450 does not meet these criteria (decreasing pair of digits 50).
## 123789 does not meet these criteria (no double).

range <- 125730:579381

check_number <- function(x){
  split = as.numeric(str_split(x,"")[[1]])
  
  if(length(split) != 6) return(FALSE)
  if(!any(split == c(999, split[1:5]))) return(FALSE)
  
  return(all(sort(split) == split))
  
  }

expect_true(check_number(111111))
expect_false(check_number(223450))
expect_false(check_number(123789))

# a little slower than I would expect..  ~20 secs
round1 <- map_lgl(range, check_number) 
sum(round1)
# 2081

### Part 2
subset <- range[round1]

check_round2 <- function(x){
  split = as.numeric(str_split(x,"")[[1]])
  
  # make sure their is an exact pair
  if(!(2 %in% table(split))) return(FALSE)
  # make sure they are adjacent 
  dups <- split[split %in% as.numeric(names(which(table(split)==2)))]
  
  return(any(map_lgl(dups,~diff(which(split == .x)) == 1)))
  
# something

}

# New rule! we must have an exact *pair* in the password
# tripples 
expect_true(check_round2(112233))
expect_false(check_round2(123444))
expect_true(check_round2(111122))

map_lgl(subset, check_round2) %>%  sum


