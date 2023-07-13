# Author:        Francesc Badia
# Date:         2018-01-18 12:39:25
# ------------------------------
# Description:  
# ------------------------------

rm(list=ls())
setwd('Y:/EPI CT Analysis/2022_new_grouping/')

outcomes_order <- c("all_excl_therap_syndrel", "Lymphoid", "Myeloid")

files <- list.files("analysis_18/results/",full.names = T)
y <- data.frame()

for(i in 1:length(files))
{
  x <- read.table(files[i],header=T)
  y <- rbind(y,x)
}
y$subset <- "Excl cumdose > 98%"

o <- order(outcomes_order)
y <- y[order(o),]

write_xlsx(y,"results/epict_analysis_18_linERR_pct98_newoutcomesv2.xlsx")

