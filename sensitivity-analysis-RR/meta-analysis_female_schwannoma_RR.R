#=====================================================
# Meta-Analysis of heart cancer endpoints
# Author: DBe with the analysis template from THG 
# Date: 06.08.2025
#=====================================================

if (!require(meta)) {install.packages("meta")} #"normal" meta-analysis
if (!require(tidyverse)) {install.packages("tidyverse")} #useful basic function
if (!require(readxl)) {install.packages("readxl")} #read excel files
if (!require(writexl)) {install.packages("writexl")}  #read excel files
if (!require(grid)) {install.packages("grid")}

settings.meta("revman5")

#work directory
source("config.R") # load working directory from config file
setwd(workdir)

# Create 'meta' directory if it doesn't exist
saveto <- "./meta"
if (!dir.exists(saveto)) {
  dir.create(saveto, recursive = TRUE)
}


##### Step 1: Data preparation & formatting #####
# load data 
dat  <- read_excel("data_female_schwannoma_poly3adj_alldoses.xlsx")
# we only analyze the highest doses used in the studies (high- versus-low meta-analysis)
dat <-  dat[c(3,6),]


##### Step 2: Meta-analysis and forest plot for females for highest doses #####
# we use the fixed-effects Mantel-Haenszel approach since random-effect and inverse variance models are not recommended for sparse events and few studies (Sweeting 2004)(Cochrane Handbook)
# since we have 0 cases in the control group of both studies, we use the treatment arm continuity correction constrained to kT + kC = 1 (see Sweeting 2004, Diamond 2007), using incr = "TACC"

ma <<- metabin(event.e = exposed_N_cases,
                            n.e = exposed_N_all,
                            event.c =sham_N_cases,
                            n.c = sham_N_all,
                            label.e = "RF-EMF",
                            label.c = "Sham",
                            text.common = "Risk Ratio with 95% CI",
                            label.left = "Favours Exposure",
                            col.label.left = "purple",
                            col.label.right = "red",
                            label.right = "Favours Non-Exposure" ,
                            studlab = RefID.No,  #study labels
                            data =  dat,                  #data
                            
                            title = "Meta-Analysis of cardiac schwannomas in female rats",
                            
                            random = FALSE,                #no random effects modeled
                            common = TRUE,                #default fixed effect model
                            sm = "RR",                   #use RR as summary measure 
                            incr = "TACC",
                            method = "MH",

                            #method.tau = "DL",            #use DerSimonian-Laird estimator to calculate between-study variance tau
                            #method.tau.ci = "J",          #use Method by Jackson (2013) to calculate confidence interval for tau^2 and tau
                            
                            level = 0.95,                 #level used to calculate confidence intervals for individual studies
                            level.ma = 0.95, 	            #level used to calculate confidence intervals for meta-analysis estimate
                            #subgroup = _,
                            #subgroup.name = "_",
                            #test.subgroup = TRUE,
)

# we save the model output to text
output_file = "/females_schwannoma_highestdose_fixed-MH-TACC_RR.txt"
sink(paste0(saveto, output_file))
ma
sink()

# we save the forest plots
png(paste0(saveto, "/females_schwannoma_highestdose_RR.png"), 11,4, units = "in", res = 300)
forest(ma)
grid.text( "Meta-analysis of cardiac schwannomas in female rats", x = 0.5, y = 0.9, gp = gpar(cex = 1.5))
grid.text( "(NTP numbers are poly3-adjusted, treatment arm continuity correction)", x = 0.5, y = 0.1, gp = gpar(cex = 1))
dev.off()

pdf(paste0(saveto, "/females_schwannoma_highestdose_RR.pdf"), 11,4)
forest(ma)
grid.text( "Meta-analysis of cardiac schwannomas in female rats", x = 0.5, y = 0.9, gp = gpar(cex = 1.5))
grid.text( "(NTP numbers are poly3-adjusted, treatment arm continuity correction)", x = 0.5, y = 0.1, gp = gpar(cex = 1))
dev.off()


##### Step 3: Descriptive forest plot for females for aggregated dose groups#####
# load the according Excel file

dat_aggr  <- read_excel("data_female_schwannoma_poly3adj_dose-aggreg.xlsx")

#calculate effect sizes without design effect correction  ---> preferred ?
ma_aggr <<- metabin(event.e = exposed_N_cases,
                                 n.e = exposed_N_all,
                                 event.c =sham_N_cases,
                                 n.c = sham_N_all,
                                 label.e = "RF-EMF",
                                 label.c = "Sham",
                                 text.common = "Risk Ratio with 95% CI",
                                 label.left = "Favours Exposure",
                                 col.label.left = "purple",
                                 col.label.right = "red",
                                 label.right = "Favours Non-Exposure" ,
                                 studlab = RefID.No,  #study labels
                                 data =  dat_aggr,                  #data
                                 
                                 title = "Meta-Analysis of cardiac schwannomas in female rats",
                                 
                                random = FALSE,                #no random effects modeled
                                common = TRUE,                #default fixed effect model
                                sm = "RR",                   #use RR as summary measure 
                                incr = "TACC",
                                method = "MH",

                                #method.tau = "DL",            #use DerSimonian-Laird estimator to calculate between-study variance tau
                                #method.tau.ci = "J",          #use Method by Jackson (2013) to calculate confidence interval for tau^2 and tau
                                level = 0.95,                 #level used to calculate confidence intervals for individual studies
                                level.ma = 0.95, 	            #level used to calculate confidence intervals for meta-analysis estimate
                                #subgroup = _,
                                #subgroup.name = "_",
                                #test.subgroup = TRUE,
)

# we save the model output to text
output_file = "/females_schwannoma_doseaggr_fixed-MH-TACC_RR.txt"
sink(paste0(saveto, output_file))
ma_aggr
sink()

# we save the forest plots
png(paste0(saveto, "/females_schwannoma_dosesaggr_RR.png"), 11,4, units = "in", res = 300)
forest(ma_aggr)
grid.text( "Meta-analysis of cardiac schwannomas in female rats,\nnumbers from three exposure levels combined", x = 0.5, y = 0.9, gp = gpar(cex = 1.5))
grid.text( "(NTP numbers are poly3-adjusted, treatment arm continuity correction)", x = 0.5, y = 0.1, gp = gpar(cex = 1))
dev.off()

pdf(paste0(saveto, "/females_schwannoma_dosesaggr_RR.pdf"), 11,4)
forest(ma_aggr)
grid.text( "Meta-analysis of cardiac schwannomas in female rats,\nnumbers from three exposure levels combined", x = 0.5, y = 0.9, gp = gpar(cex = 1.5))
grid.text( "(NTP numbers are poly3-adjusted, treatment arm continuity correction)", x = 0.5, y = 0.1, gp = gpar(cex = 1))
dev.off()

