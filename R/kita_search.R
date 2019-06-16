kita_search <- function(search_string) {

  require(rvest)
  require(tidyverse)

  kietzee <- 'https://www.kietzee.com'
  url <- paste(kietzee,'/en/kitas?q=',search_string,sep='')

  results <- read_html(url)

  links <- kita_links(results)

  #check for multiple pages

  more_pages <- results %>%
    html_nodes('a') %>%
    html_attr('href') %>%
    enframe() %>%
    filter(str_detect(value,'page=')==TRUE) %>%
    distinct() %>%
    pull(value)

  if (length(more_pages)>0) {

    pages_to_check <- tibble(search_url = paste(kietzee,more_pages,sep=''))

    for (i in 1:nrow(pages_to_check)) {

      new_page <- read_html(pages_to_check$search_url[[i]])

      new_links <- kita_links(new_page)

      links <- bind_rows(links,new_links)
    }
  }

  links <- links %>%
    arrange(kita_url)

  return(links)

}