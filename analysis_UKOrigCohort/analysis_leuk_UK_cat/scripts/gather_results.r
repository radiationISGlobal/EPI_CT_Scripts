

rm(list=ls())
setwd("Y:/EPI CT Analysis/2022_new_grouping/")

files <- list.files("analysis_UKOrigCohort/analysis_leuk_UK_cat/results/",full.names = T)

outcomes_order <- c("leuk_UK")

y <- data.frame()

for(i in 1:length(files))
{
  x <- read_excel(files[i])
  y <- rbind(y,x)
}
y$subset <- "All"

o <- order(outcomes_order)
y <- y[order(o),]


write_xlsx(y,"results/ec_res_loglinERR_leuk_UK_ef_dose_cum_OriginalCohort.xlsx")
