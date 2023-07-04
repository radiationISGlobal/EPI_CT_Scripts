

rm(list=ls())
setwd("Y:/EPI CT Analysis/2022_new_grouping/")

files <- list.files("analysis_2_2/results/",full.names = T)

#outcomes_order <- c("leuk_noCLL","aleuk","lymph","lymph_no_leuk_noCLL","Bcell_lymph","HD","NHL")
#outcomes_order <- c("leuk_noCLL","aleuk","AcLL","AcML","lymph","lymph_no_leuk_noCLL","Bcell_lymph","HD","NHL","Tcell_and_NK_cell")
outcomes_order <- c("all_excl_therap_syndrel", "Lymphoid", "Myeloid")

#outcomes_order <- rep(outcomes_order,each=2)

y <- data.frame()
for(i in 1:length(files))
{
  x <- read_excel(files[i])
  y <- rbind(y,x)
}
#y$subset <- "OnlyNotHead"

dt <- data.frame(files=files,otucome = rep(unique(y$outcome[!is.na(y$outcome)]),each=2))
dt[] <- lapply(dt,as.character)

o <- order(outcomes_order)
dt <- dt[order(o),]

y <- data.frame()
for(i in 1:length(dt$files))
{
  x <- read_excel(files[i])
  y <- rbind(y,x)
}
#y$subset <- "OnlyNotHead"

#WriteXLS::WriteXLS(y,"results/epict_analysis_2_loglinERR_cat_exp_overall_countries.xlsx",perl="C:/Strawberry/perl/bin/perl.exe")
write_xlsx(y,"results/epict_analysis_2_2_loglinERR_cat_exp_All.xlsx")
