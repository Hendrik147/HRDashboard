library(tidyverse)
library(stringr)

mySummary <- function(dt,FilterBy,FilterValue="",GroupByValue){
  dt %>% select_if(is.atomic) %>% 
    filter(str_detect(!! rlang::sym(FilterBy),FilterValue)) %>% 
    group_by(!!! rlang::syms(GroupByValue)) %>% 
    summarise_if(is.numeric,mean,na.rm=T)
}

myselectSM <- function(dt,SelectSM,NameValue=""){
  dt %>% select_if(is.atomic) %>% 
    filter(str_detect(!! rlang::syms(SelectSM),NameValue)) %>% 
    group_by(!!! rlang::syms(NameByValue))
}


