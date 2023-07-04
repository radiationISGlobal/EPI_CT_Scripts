

rm(list=ls())
setwd("Y:/EPI CT Analysis/2022_new_grouping/")

files <- list.files("n_ct/analysis_2/results/",full.names = T)

#outcomes_order <- c("leuk_noCLL","aleuk","AcLL","AcML","lymph","lymph_no_leuk_noCLL","Bcell_lymph","HD","NHL","Tcell_and_NK_cell")
outcomes_order <- c("all_excl_therap_syndrel", "Lymphoid", "Myeloid")

y <- data.frame()
for(i in 1:length(files))
{
  x <- read.table(files[i],header=T)
  y <- rbind(y,x)
}
y$subset <- "All"

write_xlsx(y,"n_ct/results/epict_analysis_2_loglinERR_n_ct_overall_countries_new_outcomesv2.xlsx")
