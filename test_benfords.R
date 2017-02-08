##
#' Function for assessing election fraud using Benford's Law
#' 
#' This function produces test statistics and a descriptive summary of the distribution
#' of the data to assess whether election fraud has occurred given a vector or matrix 
#' of vote counts.
#' @param x A numeric vector or matrix of vote counts which are used to calculate
#' descriptive and test statistics
#' @param type A string indicating which test statistics should be generated. The
#' default, "m", generates only the Leemis' M.  "d" generates only the Cho-Gains' D.
#' "both" generates both the Leemis' M and the Cho-Gains' D.
#' 
#' @return A list containing the following:
#' \item{Proportions}{A table giving the proportion of occurrence of each number as
#' the leading digit for the vote counts.}
#' \item{Leemis}{A one element numeric vector containing the Leemis' M statistic
#' (only generated if type="m" or type="both").}
#' \item{CG_D}{A one element numeric vector containing the Cho-Gains' Dstatistic
#' (only generated if type="d" or type="both").}
#' 
#' @author David Ryan Miller

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