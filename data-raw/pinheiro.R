library(tidyverse)
conflicted::conflict_prefer("filter", "dplyr")

# create pinheiro dataset

# this is not ideal
pinheiro_dat <-
  simeta::pinheiro_data %>%
  select(-contains("_d")) %>%
  mutate(
    s_c = if_else(
    study == "Schjetlein",
    list(559),
    s_c
  ),
  s_c_d = map_dbl(s_c, .f = function(x){
    if (length(x) == 1) {as.numeric(x)}
    else {x[[2]] - x[[1]]}
    }),
  s_t_d = map_dbl(s_t, .f = function(x){
    if (length(x) == 1) {as.numeric(x)}
    else {x[[2]] - x[[1]]}
  })


  )

# usethis::use_data(pinheiro_dat, overwrite = TRUE)
