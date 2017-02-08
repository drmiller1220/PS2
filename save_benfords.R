##
#' Function for saving Benford's Law test statistics
#' 
#' This function saves the results of the function "test.benford" as a .csv file.
#' @param x A list which must be of class "benfords"
#' @param file_name A string indicating the name of the .csv file.  Must contain
#' .csv suffix.  By default, .csv will be saved as "benfords_results.csv" in the
#' current working directory.
#' 
#' @return Saves the output from "print.benfords" as a .csv file with the specified 
#' file name and in the specified working directory.
#' 
#' @author David Ryan Miller

save.benfords <- function(x,file_name='benfords_results.csv'){
  # receives as input the object x, which is presumably a list from
  # test.benfords, and a desired file name; default file name
  # is specified, and it is assumed that a working directory has been set
  if(class(x)=="benfords"){ # checking to see if object is of class benfords
    sink(file_name)
    # sink() initiates copying all output to a csv file with the specified name
    print.benfords(x)
    # we use print.benfords to generate the output
    sink()
    # sink() closes the file
  } else{ # throw an error message if the inputted object is not of class benfords
    stop("Object must be of class benfords")
  }
}