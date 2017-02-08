##
#' Function for printing Benford's Law test statistics
#' 
#' This function produces uses the output of the function "test.benfords" to create a
#' a table containing the test statistics, and indicating their statstical reliability
#' @param x A list which must be of class "benfords"
#' 
#' @return Prints a table in the R console containing each of the test statistics generated
#' by "test.benfords," as well as asterisks to indicate statistical reliability (and a legend).
#' 
#' @author David Ryan Miller

print.benfords <- function(x){
  # function takes list x derived from test.benfords function and prints
  # the results in an easily readable form
  #
  # checking to see if x is of class "benfords" so that the rest of the function
  # will work properly
  if(class(x)=="benfords"){
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
  } else{ # throw an error message if the inputted object is not of class benfords
    stop("Object must be of class benfords")
  }
}