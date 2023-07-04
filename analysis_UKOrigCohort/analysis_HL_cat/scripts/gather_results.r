

rm(list=ls())
setwd("Y:/EPI CT Analysis/2022_new_grouping/")

files <- list.files("analysis_UKOrigCohort/analysis_HL/results/",full.names = T)

outcomes_order <- c("HL")



y <- data.frame()
for(i in 1:length(files))
{
  x <- read_excel(files[i])
  y <- rbind(y,x)
}



write_xlsx(y,"results/ec_res_loglinERR_HL_ef_dose_cum_OriginalCohort.xlsx")
