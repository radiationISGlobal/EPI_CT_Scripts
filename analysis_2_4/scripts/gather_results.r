

rm(list=ls())
setwd("Y:/EPI CT Analysis/2022_new_grouping/")

files <- list.files("analysis_2_4/results/",full.names = T)

outcomes_order <- c("AML_prec_ALMP_ALAL", "MPN_MDSMPN_MDS", "AML_AL_ALMP_AL_excl_gen","MPN")

#outcomes_order <- rep(outcomes_order,each=2)

y <- data.frame()
for(i in 1:length(files))
{
  x <- read_excel(files[i])
  y <- rbind(y,x)
}

y$subset <- "All"

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
y$subset <- "All"

#WriteXLS::WriteXLS(y,"results/epict_analysis_2_loglinERR_cat_exp_overall_countries.xlsx",perl="C:/Strawberry/perl/bin/perl.exe")
write_xlsx(y,"results/epict_analysis_2_4_loglinERR_cat_exp_Myeloid_All.xlsx")
