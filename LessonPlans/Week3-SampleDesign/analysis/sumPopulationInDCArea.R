setwd("/Users/bader/work/Data/Census/DCMetro/DCAndBorderingCounties/")

dcarea <- read.delim("R10835953_SL140.txt")

male.start <- c(1:200)[names(dcarea)=="Male..18.to.24.Years"]
male.end   <- c(1:200)[names(dcarea)=="Male..85.Years.and.over"]
female.start <- c(1:200)[names(dcarea)=="Female..18.to.24.Years"]
female.end   <- c(1:200)[names(dcarea)=="Female..85.Years.and.over"]

pop.data <- dcarea[2:nrow(dcarea),c(male.start:male.end,female.start:female.end)]
pop.data <- apply(pop.data,2,FUN=function(x){as.numeric(as.character(x))})
tot.18plus <- rowSums(pop.data)
sum(tot.18plus)
pop.data <- as.numeric(as.character(pop.data))
