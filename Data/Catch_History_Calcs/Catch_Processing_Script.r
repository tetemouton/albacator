library(tidyverse)
library(magrittr)


  theme_set(theme_bw())

  dir.pth <- "C:/GitRep/albacator/Data/Catch_History_Calcs/"


  dat.l <- read.csv(file = paste0(dir.pth, "Catch_Histories_LL.csv"))

  dtl_ln <- dat.l %>% pivot_longer(-year, names_to = "EEZ", values_to = "Catch")

  dat.tab1 <- dtl_ln %>% filter(between(year, 2010, 2019), !EEZ %in% c("HS","NC","PF")) %>% group_by(EEZ) %>% slice_max(order_by = Catch, n = 3) %>%
                         group_by(EEZ) %>% summarise(Catch.3.10.19 = mean(Catch)) %>% mutate(Per.3.10.19 = Catch.3.10.19/sum(Catch.3.10.19)*100)

  dat.tab2 <- dtl_ln %>% filter(between(year, 2005, 2019), !EEZ %in% c("HS","NC","PF")) %>% group_by(EEZ) %>% slice_max(order_by = Catch, n = 5) %>%
                         group_by(EEZ) %>% summarise(Catch.5.05.19 = mean(Catch)) %>% mutate(Per.5.05.19 = Catch.5.05.19/sum(Catch.5.05.19)*100)

  dat.tab3 <- dtl_ln %>% filter(between(year, 2000, 2019), !EEZ %in% c("HS","NC","PF")) %>% group_by(EEZ) %>% slice_max(order_by = Catch, n = 5) %>%
                         group_by(EEZ) %>% summarise(Catch.5.00.19 = mean(Catch)) %>% mutate(Per.5.00.19 = Catch.5.00.19/sum(Catch.5.00.19)*100)
  

  dat.tab <- cbind(dat.tab1, dat.tab2[,-1], dat.tab3[,-1])
  write.csv(dat.tab, file = paste0(dir.pth, "Catch_Summaries_LL.csv"), row.names = FALSE)

  
  
  
  
  
  dat.lt <- read.csv(file = paste0(dir.pth, "Catch_Histories_LLTR.csv"))
  
  dtlt_ln <- dat.lt %>% pivot_longer(-year, names_to = "EEZ", values_to = "Catch")
  
  dat.tab1 <- dtlt_ln %>% filter(between(year, 2010, 2019), !EEZ %in% c("HS","NC","PF")) %>% group_by(EEZ) %>% slice_max(order_by = Catch, n = 3) %>%
                          group_by(EEZ) %>% summarise(Catch.3.10.19 = mean(Catch)) %>% mutate(Per.3.10.19 = Catch.3.10.19/sum(Catch.3.10.19)*100)
  
  dat.tab2 <- dtlt_ln %>% filter(between(year, 2005, 2019), !EEZ %in% c("HS","NC","PF")) %>% group_by(EEZ) %>% slice_max(order_by = Catch, n = 5) %>%
                          group_by(EEZ) %>% summarise(Catch.5.05.19 = mean(Catch)) %>% mutate(Per.5.05.19 = Catch.5.05.19/sum(Catch.5.05.19)*100)
  
  dat.tab3 <- dtlt_ln %>% filter(between(year, 2000, 2019), !EEZ %in% c("HS","NC","PF")) %>% group_by(EEZ) %>% slice_max(order_by = Catch, n = 5) %>%
                          group_by(EEZ) %>% summarise(Catch.5.00.19 = mean(Catch)) %>% mutate(Per.5.00.19 = Catch.5.00.19/sum(Catch.5.00.19)*100)
  
  
  dat.tab <- cbind(dat.tab1, dat.tab2[,-1], dat.tab3[,-1])
  write.csv(dat.tab, file = paste0(dir.pth, "Catch_Summaries_LLTR.csv"), row.names = FALSE)
  
  
























