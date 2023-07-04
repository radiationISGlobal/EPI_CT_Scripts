# Author:        Francesc Badia
# Date:         2018-01-18 12:39:25
# ------------------------------
# Description:  
# ------------------------------

rm(list=ls())
setwd("Y:/EPI CT Analysis/2022_new_grouping/")

#outcomes_order <- c("leuk_noCLL","aleuk","AcLL","AcML","lymph","lymph_no_leuk_noCLL","Bcell_lymph","HD","NHL","Tcell_and_NK_cell")
outcomes_order <- c("AML_prec_ALMP_ALAL", "MPN_MDSMPN_MDS", "AML_AL_ALMP_AL_excl_gen","MPN")


files <- list.files("analysis_1_9/results/",full.names = T)
y <- data.frame()

for(i in 1:length(files))
{
  x <- read_excel(files[i])
  y <- rbind(y,x)
}
y$subset <- "All"

o <- order(outcomes_order)
y <- y[order(o),]

#write.table(y, "epict_analysis_1_linERR_overall_countries.csv",row.names = FALSE,quote = FALSE)

write_xlsx(y,"results/epict_analysis_1_9_linERR_Myeloid_All.xlsx")
