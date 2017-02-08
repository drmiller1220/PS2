# set working directory to location of scripts
setwd("C:\\Users\\drmiller1220\\Documents\\GitHub\\PS2")

# sourcing in each of the scripts
source("test_benfords.R")
source("print_benfords.R")
source("save_benfords.R")

## Tests

vote_sample <- sample(1:1000000, 100000, replace=TRUE)
# creates sample with which to execute tests

## 1) example for Leemis M only

test_m <- test.benfords(vote_sample, type="m")
test_m # will present 2 elements of list: proportions for each digit,
      # and the Leemis M statistic

print.benfords(test_m) # prints Leemis M test statistic with legend, and 
                      # asterisks if appropriate

save.benfords(test_m, file_name = "test_m.csv")
# saves table to csv file in working directory

## 2) example for Cho Gains D only

test_d <- test.benfords(vote_sample, type="d")
test_d # will present 2 elements of list: proportions for each digit,
# and the Cho Gains D statistic

print.benfords(test_d) # prints Cho Gains D test statistic with legend, 
# and asterisks if appropriate

save.benfords(test_d, file_name = "test_d.csv")
# saves table to csv file in working directory

## 3) example for both

test_both <- test.benfords(vote_sample, type="both")
test_both # will present 2 elements of list: proportions for each digit,
# and the Leemis M and  Cho Gains D statistic

print.benfords(test_both) # prints both test statistics with legend, 
# and asterisks if appropriate

save.benfords(test_both, file_name = "test_both.csv")
# saves table to csv file in working directory
