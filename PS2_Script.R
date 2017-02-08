
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

test.benfords(seq(from=1, to=100, by=1), type='both') # Examples
test.benfords(rep(1, 100), type='d')
test.benfords(rep(1, 100))

###################################################################################

print.benfords <- function(x){
  # function takes list x derived from test.benfords function and prints
  # the results in an easily readable form
  #
  # so that we do not force the user to tell us again which tests were
  # run, we can use logicals to determine ourselves which test statistics
  # were calculated; if a test statistic was calculated, that element
  # of the list will be numeric; if not, then FALSE
  type_m <- is.numeric(x$Leemis)==TRUE & is.numeric(x$CG_D)==FALSE
  type_d <- is.numeric(x$Leemis)==FALSE & is.numeric(x$CG_D)==TRUE
  type_both <- is.numeric(x$Leemis)==TRUE & is.numeric(x$CG_D)==TRUE
  # create an empty list to store test statistics
  test_stats <- list()
  # if leemis' m was calculated, we want to determine the signficance level
  if(type_m==TRUE | type_both==TRUE){
    M <- x$Leemis
    M <- ifelse(M < 0.851, M, # This ifelse chain appends the proper stars
                ifelse(M < 0.967, M <- paste0(M, '*'),
                       ifelse(M < 1.212, M <- paste0(M, '**'),
                              M <- paste0(M, '***'))))
    test_stats$Leemis <- M
  }
  # if cho-gains' d was calculated, we want to determine the signficance level
  if(type_d==TRUE | type_both==TRUE){
    D <- x$CG_D
    D <- ifelse(D < 1.212, D, # This ifelse chain appends the proper stars
                ifelse(D < 1.330, D <- paste0(D, '*'),
                       ifelse(D < 1.569, D <- paste0(D, '**'),
                              D <- paste0(D, '***'))))
    test_stats$CG_D <- D
  }
  # we want to store some elements of the print out; we create a general
  # title, labels for each statistic, a line to divide the statistics
  # from the legend, and a legend; we use \n for carriage returns for
  # readability
  title <- "Benford's Law Test Statistics\n\n"
  leemis_label <- "Leemis' M\n"
  CG_D_label <- "Cho-Gains' D\n"
  dividing_line <- "_____________________________\n"
  legend <- "* signifies p<0.10\n ** signifies p<0.05\n *** signifies p<0.01"
  # if only leemis m is calculated, we concatenate the title, the leemis m
  # label, the statistic, the dividing line, and the legend, with a few 
  # carriage returns for readability
  if(type_m==TRUE){
    cat(title, leemis_label,
        M,"\n",
        dividing_line,legend,"\n\n")
  }
  # if only cho gains d is calculated, we concatenate the title, the cho gains d
  # label, the statistic, the dividing line, and the legend, with a few 
  # carriage returns for readability
  if(type_d==TRUE){
    cat(title, CG_D_label,
        D,"\n",
        dividing_line,legend,"\n\n")
  }
  # if both leemis m and cho gains d is calculated, we concatenate the title, 
  # the leemis m and cho gains d labels, the statistic, the dividing line, 
  # and the legend, with a few carriage returns for readability
  if(type_both==TRUE){
    cat(title, leemis_label,
        M,"\n\n",
        CG_D_label,
        D,"\n",
        dividing_line,legend,"\n\n")
  }
}

######################################

save.benfords <- function(x,file_name='benfords_results.csv'){
  # receives as input the object x, which is presumably a list from
  # test.benfords, and a desired file name; default file name
  # is specified, and it is assumed that a working directory has been set
  sink(file_name)
  # sink() initiates copying all output to a csv file with the specified name
  print.benfords(x)
  # we use print.benfords to generate the output
  sink()
  # sink() closes the file
}
save.benfords(test1)
