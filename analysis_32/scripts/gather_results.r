# Author:        Francesc Badia
# Date:         2018-01-22  
# ------------------------------
# Description:  
# ------------------------------

rm(list=ls())
setwd("Y:/EPI CT Analysis/2022_new_grouping")

files <- list.files("analysis_32/results/",full.names = T,pattern = "csv")

outcomes_order <- c("all_excl_therap_syndrel", "Lymphoid",  "Myeloid")

y <- data.frame()
for(i in 1:length(files))
{
  x <- read.table(files[i],header=T)
  y <- rbind(y,x)
}

dt <- data.frame(files=files,otucome = unique(y$outcome[!is.na(y$outcome)]))
dt[] <- lapply(dt,as.character)

o <- order(outcomes_order)
dt <- dt[order(o),]

y <- data.frame()
for(i in 1:length(dt$files))
{
  x <- read.table(dt$files[i],header=T)
  y <- rbind(y,x)
}

write_xlsx(y,"results/epict_analysis_32_linERR_dose_bysex.xlsx")
