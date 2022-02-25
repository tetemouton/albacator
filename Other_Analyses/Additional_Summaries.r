library(shiny)
library(ggplot2)
library(plotly)
library(ggplot2)
library(tidyverse)
library(shinyWidgets)
library(magrittr)
library(scales)
library(shinydashboard)
library(data.table)
library(DT)
library(ggthemes)
library(cowplot)


  theme_set(theme_bw())

  zone_cols <- c("dodgerblue2","firebrick3","darkorchid","darkred","lightskyblue","yellow1","orange1","honeydew3","darkgoldenrod3","deeppink3","dodgerblue4","seagreen3","purple4")

  dat_l <- read.csv("C:/GitRep/albacator/Data/DatMat_LL.csv", header = TRUE)
  #dat_l_00 <- read.csv("./Data/DatMat_LL_00.csv", header = TRUE)

  TAC <- 1

  wgtvec <- c(.26,.16,.14,.18,.26)

  dat_per <- apply(dat_l[-c(1:2)],2,function(x){x/sum(x)})


  wgts <- wgtvec/sum(wgtvec)

  wgt_per <- t(t(dat_per)*wgts)
  wgt_all <- wgt_per*TAC
  wgt_all <- as.data.frame(wgt_all)

  all_per <- apply(wgt_per, 1, sum)
  all_cat <- apply(wgt_all, 1, sum)

  wgt_all$Cnt <- dat_l$Cnt

  all_tab <- wgt_all %>% pivot_longer(cols = -Cnt, names_to = "Criteria", values_to = "Allocation")


  windows(4000,2000)
    pl1 <- ggplot(all_tab, aes(x = Cnt, y = Allocation, fill = Criteria)) + geom_bar(stat = "identity") +
                  scale_fill_manual(values = alpha(c("firebrick","burlywood3","dodgerblue","navy","darkorchid"), 0.8)) +
                  theme_cowplot() + theme(legend.position = "top", plot.margin = unit(c(0, 0, 0, 0), "cm"),
                                          axis.title.x = element_blank(), axis.text.x = element_blank(), axis.line.x = element_blank())
    print(pl1)
  dev.off()
  
  
  windows(4000,2000)
    pl2 <- ggplot(all_tab, aes(x = Cnt, y = Allocation, fill = Criteria)) + geom_bar(stat = "identity", position = "fill") +
                  scale_fill_manual(values = alpha(c("firebrick","burlywood3","dodgerblue","navy","darkorchid"), 0.8)) +
                  theme_cowplot() + theme(legend.position = "none", plot.margin = unit(c(0, 0, 0, 0), "cm"),
                                          axis.title.x = element_blank())
  print(pl2)
  dev.off()

  windows(4000,4000)
    plot_grid(pl1, pl2, labels = "", ncol = 1)
    savePlot("C:/GitRep/albacator/Other_Analyses/Allocation_Breakdown_RecentCatches.png", type="png")
  dev.off()



  dat_l <- read.csv("C:/GitRep/albacator/Data/DatMat_LL_00.csv", header = TRUE)
  #dat_l_00 <- read.csv("./Data/DatMat_LL_00.csv", header = TRUE)
  
  TAC <- 1
  
  wgtvec <- c(.26,.16,.14,.18,.26)
  
  dat_per <- apply(dat_l[-c(1:2)],2,function(x){x/sum(x)})
  
  
  wgts <- wgtvec/sum(wgtvec)
  
  wgt_per <- t(t(dat_per)*wgts)
  wgt_all <- wgt_per*TAC
  wgt_all <- as.data.frame(wgt_all)
  
  all_per <- apply(wgt_per, 1, sum)
  all_cat <- apply(wgt_all, 1, sum)
  
  wgt_all$Cnt <- dat_l$Cnt
  
  all_tab <- wgt_all %>% pivot_longer(cols = -Cnt, names_to = "Criteria", values_to = "Allocation")
  
  
  windows(4000,2000)
    pl1 <- ggplot(all_tab, aes(x = Cnt, y = Allocation, fill = Criteria)) + geom_bar(stat = "identity") +
                  scale_fill_manual(values = alpha(c("firebrick","burlywood3","dodgerblue","navy","darkorchid"), 0.8)) +
                  theme_cowplot() + theme(legend.position = "top", plot.margin = unit(c(0, 0, 0, 0), "cm"),
                                          axis.title.x = element_blank(), axis.text.x = element_blank(), axis.line.x = element_blank())
    print(pl1)
  dev.off()
  
  
  windows(4000,2000)
    pl2 <- ggplot(all_tab, aes(x = Cnt, y = Allocation, fill = Criteria)) + geom_bar(stat = "identity", position = "fill") +
                  scale_fill_manual(values = alpha(c("firebrick","burlywood3","dodgerblue","navy","darkorchid"), 0.8)) +
                  theme_cowplot() + theme(legend.position = "none", plot.margin = unit(c(0, 0, 0, 0), "cm"),
                                          axis.title.x = element_blank())
    print(pl2)
  dev.off()
  
  windows(4000,4000)
    plot_grid(pl1, pl2, labels = "", ncol = 1)
    savePlot("C:/GitRep/albacator/Other_Analyses/Allocation_Breakdown_EarlyCatches.png", type="png")
  dev.off()


  
  
  
  