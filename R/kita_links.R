kita_links <- function(search_object) {

  require(rvest)
  require(tidyverse)

  kietzee <- 'https://www.kietzee.com'

  results <- search_object %>%
    html_nodes('.btn-small') %>%
    html_attr('href') %>%
    as_tibble() %>%
    filter(str_detect(value,'reviews')==FALSE) %>%
    rename(kita_url = 1) %>%
    mutate(kita_url = paste(kietzee,kita_url,sep=''))

  return(results)

}