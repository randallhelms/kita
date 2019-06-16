kita_info <- function(string,part1,part2) {

  require(purrr)
  require(tidyverse)

  before <- paste('.*',part1,sep='')
  after <- paste(part2,'.*',sep='')

  x <- str_replace_all(string,before,'')

  if (is_empty(part2) == FALSE) {

    x <- str_replace_all(x,after,'')

  }

  x <- gsub("(?<=\\b\\w)\\s(?=\\w\\b)", "", x,perl=T)

  return(x)

}