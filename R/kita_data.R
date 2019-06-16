kita_data <- function(kita_list) {

  require(tidyverse)
  require(rvest)

  kita_tbl <- tibble()

  for (i in 1:nrow(kita_list)) {

    kita_link <- kita_list$kita_url[[i]]

    kita <- read_html(kita_link)

    name <- kita %>%
      html_nodes('h1') %>%
      html_text()

    info <- kita %>%
      html_nodes('p') %>%
      html_text()

    parentco <- str_detect(info,'Parent company: ')

    info <- info[parentco==TRUE]
    info <- str_sub(info,start=8)
    info <- str_sub(info,end=-6)
    info <- gsub('\n','',info,fixed=TRUE)

    other_stuff <- kita %>%
      html_nodes('.collapsible-body') %>%
      html_text()

    sa <- str_detect(other_stuff,'Total capacity: ')
    hr <- str_detect(other_stuff,'Monday:')

    size_age <- other_stuff[sa==TRUE]
    size_age <- str_sub(size_age,start=12)
    size_age <- str_sub(size_age,end=-10)
    size_age <- gsub(' (months)','',size_age,fixed=TRUE)

    hours <- other_stuff[hr==TRUE]
    hours <- gsub('\n','',hours,fixed=TRUE)

    single_kita <- tibble(
      kita_name = name,
      kita_url = kita_link,
      parent_co = kita_info(info,'Parent company: ','Kita'),
      kita_type = kita_info(info,'Kita type: ','Address'),
      address = kita_info(info,'Address: ','Telephone'),
      phone = kita_info(info,'Telephone: ','Email'),
      email = kita_info(info,'Email: ','Website'),
      website = kita_info(info,'Website: ',NULL),
      maps_url = str_replace_all(paste('https://www.google.de/maps?q=',address,sep=''),' ','+'),
      capacity = kita_info(size_age,'Total capacity: ','Under three'),
      under_three = kita_info(size_age,'Under three: ','Over three'),
      over_three = kita_info(size_age,'Over three: ','Min age'),
      min_age = kita_info(size_age,'Min age: ','Max age'),
      max_age = kita_info(size_age,'Max age: ',NULL),
      monday = trimws(kita_info(hours,'Monday:','Tuesday: ')),
      tuesday = trimws(kita_info(hours,'Tuesday:','Wednesday: ')),
      wednesday = trimws(kita_info(hours,'Wednesday:','Thursday: ')),
      thursday = trimws(kita_info(hours,'Thursday:','Friday: ')),
      friday = ifelse(nchar(trimws(kita_info(hours,'Friday:',NULL)))>15,thursday,trimws(kita_info(hours,'Friday:',NULL)))
      ) %>%
      mutate(monday = ifelse(nchar(monday)>14,tuesday,monday))

    kita_tbl <- bind_rows(kita_tbl,single_kita)

  }

  kita_tbl <- kita_tbl %>%
    arrange(kita_name)

  return(kita_tbl)

}