library(datavolley)
library(tidyverse)

BlockSummary <- function(plays, ...){
  blocks <- plays %>% 
    filter(skill == "Block")
  
  output <- blocks %>% 
    group_by(...) %>% 
    summarise(Stuffs = sum(evaluation_code == "#"),
              Touches = n(),
              GT = sum(evaluation_code %in% c("+", "#")),
              `GT%` = GT/Touches,
              Errors = sum(evaluation_code == "="),
              `Error%` = Errors/Touches)
  
  return(output)
}
