# Author:        Francesc Badia
# Date:         2018-01-18 12:39:25
# ------------------------------
# Description:  
# ------------------------------

rm(list=ls())
setwd("Y:/EPI CT Analysis/2022_new_grouping")

#outcomes_order <- c("leuk_noCLL","aleuk","lymph","lymph_no_leuk_noCLL","Bcell_lymph","HD","NHL")
#outcomes_order <- c("leuk_noCLL","aleuk","AcLL","AcML","lymph","lymph_no_leuk_noCLL","Bcell_lymph","HD","NHL","Tcell_and_NK_cell")
outcomes_order <- c("all_excl_therap_syndrel", "Lymphoid", "Myeloid")


outcomes_order <- rep(outcomes_order,each=15)

files <- list.files("analysis_3/results/",full.names = T)
y <- data.frame()

for(i in 1:length(files))
{
  x <- read.table(files[i],header=T)
  names(x)[3] <- "subset"
  y <- rbind(y,x)
}

o <- order(outcomes_order)
y <- y[order(y$subset,o),]

write_xlsx(y,"results/epict_analysis_3_linERR_subseting_countries_.xlsx")
