## Function used to calculate Leemis' M and/or Cho-Gains' D, given a matrix or
## a vector of vote totals.  Function returns Leemis' M if type 'm', Cho-Gains'
## D if type 'd,' and both statistics if type 'both.'
## The following function returns Leemis' M if its argument 'type' equals
## 'm' (which is its default value), Cho-Gains' D if 'type' equals 'd',
## or both if 'type' equals 'both.'

test.benfords <- function(x, type='m'){
  # First, we take the data x, convert the numeric values to strings, and 
  # remove all but the first character in each string.  In order to keep track of
  # counts of zeroes, we need to specify the numbers as factors.  Then, table()
  # tabulates the number of occurrences by each value, including counts of zeroes;
  # because we want to be able to store the distribution of digit appearances in
  # our list, we divide by the number of vote counts, or our input x, to generate
  # proportions
  data <- table(factor(substr(as.character(x), 1, 1), levels = 1:9))/length(x)
  # because we want these proportions to be returned by the list outputted by
  # the function, we store it as part of a list
  final_list <- list('Proportions' = data)
  # we convert the data to numeric and subtract log10(1 + 1/1:9) to generate
  # the inside statistic for each test
  v <- as.numeric(data) - log10(1 + 1/1:9)
  # Leemis M is the default test; if the user specifies 'm,' 'both,' or nothing,
  # the function calculates Leemis M
  if(type == 'm' | type == 'both'){
    M <- max(v) # The maximum value in 'v' is Leemis M
    final_list$Leemis <- M
  }
  # If the user specifies 'd' or 'both,' the function calculates Cho-Gains D
  if(type == 'd' | type == 'both'){
    D <- sqrt(sum(v^2))
    final_list$CG_D <- D
  }
  # We return the final list, which contains the distribution of the digits as counts,
  # and Leemis M and/or Cho-Gains D
  return(final_list)
}