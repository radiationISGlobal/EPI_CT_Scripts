

rm(list=ls())
setwd("Y:/EPI CT Analysis/2022_new_grouping/")

files <- list.files("analysis_2_3/results/",full.names = T)

outcomes_order <- c("NHL", "NHL_Bcell", "NHL_Tcell","NHL_Precursor_cell","HL")


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

write_xlsx(y,"results/epict_analysis_2_3_loglinERR_cat_exp_Lymphoid_All.xlsx")
