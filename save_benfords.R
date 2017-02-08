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