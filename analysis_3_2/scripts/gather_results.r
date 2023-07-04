# Author:        Francesc Badia
# Date:         2018-01-18 12:39:25
# ------------------------------
# Description:  
# ------------------------------

rm(list=ls())
setwd("Y:/EPI CT Analysis/2022_new_grouping")


outcomes_order <- c("AML_prec_ALMP_ALAL", "MPN_MDSMPN_MDS", "AML_AL_ALMP_AL_excl_gen","MPN")


outcomes_order <- rep(outcomes_order,each=15)

files <- list.files("analysis_3_2/results/",full.names = T)
y <- data.frame()

for(i in 1:length(files))
{
  x <- read.table(files[i],header=T)
  names(x)[3] <- "subset"
  y <- rbind(y,x)
}

o <- order(outcomes_order)
y <- y[order(y$subset,o),]

write_xlsx(y,"results/epict_analysis_3_2_linERR_Myeloid_subseting_countries.xlsx")
