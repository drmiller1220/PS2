
##################################################
##                                              ##
## Problem Set 2                                ##
## David Miller                                 ##
##                                              ##
##################################################

## Function used to calculate Leemis' M and/or Cho-Gains' D, given a matrix or
## a vector of vote totals.  Function returns Leemis' M if type 'm', Cho-Gains'
## D if type 'd,' and both statistics if type 'both.'
## The following function returns Leemis' M if its argument 'type' equals
## 'm' (which is its default value), Cho-Gains' D if 'type' equals 'd',
## or both if 'type' equals 'both.'

violation_test <- function(x, type='m'){
  # First, we take the data x, convert the numeric values to strings, and 
  # remove all but the first character in each string.  In order to keep track of
  # counts of zeroes, we need to specify the numbers as factors.  Then, table()
  # tabulates the number of occurrences by each value, including counts of zeroes
  data <- table(factor(substr(as.character(x), 1, 1), levels = 1:9))
  # because we want these proportions to be returned by the list outputted by
  # the function, we store it as part of a list
  final_list <- list('Proportions' = data)
  # we convert the data to numeric, then divide by the sum of the table (where
  # we have counts of each digit, which sum to the number of total digits in the
  # sample) and subtract log10(1 + 1/1:9) to generate the inside statistic for each test
  v <- as.numeric(data)/sum(data) - log10(1 + 1/1:9)
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

violation_test(seq(from=1, to=100, by=1), type='both') # Examples
violation_test(rep(1, 100), type='d')
violation_test(rep(1, 100))

###################################################################################
#; the statistic is reported with one star
## if it is statistically reliable at an alpha level of 0.1, two stars for
## alpha level 0.05, and three stars for alpha level 0.01.

print.benfords <- function(y){
  
  # Leemis M is the default test; if the user specifies 'm,' 'both,' or nothing,
  # the function calculates Leemis M, names the statistic, and attaches the
  # proper number of stars (if any)
  if(type == 'm' | type == 'both'){
    M <- max(v) # The maximum value in 'v' is Leemis M
    final_list$Leemis <- M
    #names(M) <- 'Leemis M:' # Adds correct name to one-element vector
    #M <- ifelse(M < 0.851, M, # This ifelse chain appends the proper stars
    #            ifelse(M < 0.967, M <- paste0(M, '*'),
    #                   ifelse(M < 1.212, M <- paste0(M, '**'),
    #                          M <- paste0(M, '***'))))
  }
  if(type == 'd' | type == 'both'){
    D <- sqrt(sum(v^2))
    final_list$CG_D <- D
    # Cho-Gains D is the sqrt of the squared elements
    #names(D) <- 'Cho-Gains D:' # Adds correct name to one-element vector
    #D <- ifelse(D < 1.212, D, # This ifelse chain appends the proper stars
    #            ifelse(D < 1.330, D <- paste0(D, '*'),
    #                   ifelse(D < 1.569, D <- paste0(D, '**'),
    #                          D <- paste0(D, '***'))))
  }
  # Legend to indicate meaning of significance stars
  #sig <- "* indicates p<0.10, ** indicates p<0.05, *** indicates p<0.01"
  # Switch stores the desired test results as an object
  #result <- switch(type, m=c(M, sig), d=c(D, sig), both=c(D, M, sig))
  # Then we return the results with the legend
  #return(list(results=cat(paste(names(result), result), sep='\n'),
  #dist=data[1:9]))
  return(final_list)
}