library(tidyverse)

# bare to quosure: quo
bare_to_quo <- function(x, var){
  x %>% select(!! var) %>% head(1)
}
