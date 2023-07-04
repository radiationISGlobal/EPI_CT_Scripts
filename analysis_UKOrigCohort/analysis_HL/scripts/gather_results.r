# Author:        Francesc Badia
# Date:         2018-01-18 12:39:25
# ------------------------------
# Description:  
# ------------------------------

rm(list=ls())
setwd("Y:/EPI CT Analysis/2022_new_grouping/")

outcomes_order <- c("HL")


files <- list.files("analysis_UKOrigCohort/analysis_HL/results/",full.names = T)
y <- data.frame()

for(i in 1:length(files))
{
  x <- read_excel(files[i])
  y <- rbind(y,x)
}
y$subset <- "All"

o <- order(outcomes_order)
y <- y[order(o),]


write_xlsx(y,"results/ec_res_linERR_HL_ef_dose_cum_OriginalCohort.xlsx")
