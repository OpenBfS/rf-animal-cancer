#=====================================================
# Meta-Analysis of brain glial-cell derived cancer endpoints
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
dat     <- read_excel("data_male_glioma_poly3adj_alldoses.xlsx")
# we only analyze the highest doses used in the studies  (high- versus-low meta-analysis)
dat <-  dat[c(3,6,8,9,10),] # Chou 1992 included

# create a variable describing the exposure design of the studies, for subgroup analysis
dat$design = c("free, >18h/day","free, >18h/day", "restrained, <= 4h/day", "restrained, <= 4h/day","free, >18h/day")



##### Step 2: Meta-analysis and forest plot for males for highest doses #####

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
                            
                            title = "Meta-Analysis of gliomas in  male rats",
                            
                            random = FALSE,                #no random effects modeled
                            common = TRUE,                #default fixed effect model
                            sm = "RR",                   #use RR as summary measure 
                            method = "MH",
                            incr = "TACC",  # otherwise we'd get an RR of 2.0 for the Falcioni 0.1 W/kg group
                            allstudies = TRUE, # to force RR calculation for Falcioni 0.1 W/kg 
         
                            #method.tau = "DL",            #use DerSimonian-Laird estimator to calculate between-study variance tau
                            #method.tau.ci = "J",          #use Method by Jackson (2013) to calculate confidence interval for tau^2 and tau
                            
                            level = 0.95,                 #level used to calculate confidence intervals for individual studies
                            level.ma = 0.95, 	            #level used to calculate confidence intervals for meta-analysis estimate
                            subgroup = design,
                            subgroup.name = "Exposure condition and duration",
                            test.subgroup = TRUE,
)

output_file = "/males_glioma_highestdose_fixed-MH-TACC_RR.txt"
sink(paste0(saveto, output_file))
ma
sink()

# we save the forest plots
png(paste0(saveto, "/males_glioma_highestdose_RR.png"), 11,6, units = "in", res = 300)
forest(ma)
grid.text( "Meta-analysis of gliomas in male rats", x = 0.5, y = 0.9, gp = gpar(cex = 1.5))
grid.text( "(NTP numbers are poly3-adjusted, treatment arm continuity correction)", x = 0.5, y = 0.1, gp = gpar(cex = 1))
dev.off()

pdf(paste0(saveto, "/males_glioma_highestdose_RR.pdf"), 11,6)
forest(ma)
grid.text( "Meta-analysis of gliomas in male rats", x = 0.5, y = 0.9, gp = gpar(cex = 1.5))
grid.text( "(NTP numbers are poly3-adjusted, treatment arm continuity correction)", x = 0.5, y = 0.1, gp = gpar(cex = 1))
dev.off()


###### Funnel Plot ######
png(paste0(saveto, "/males_glioma_highestdose_FUNNEL_RR.png"), 10,6, units = "in", res = 300)
meta::funnel(ma, cex=1.2, common=TRUE, log = "x", yaxis = "se", studlab=TRUE, xlab="Risk Ratio (log axis)")
title(main="Funnel plot: highest exposure contrast, male rats")
dev.off()

pdf(paste0(saveto, "/males_glioma_highestdose_FUNNEL_RR.pdf"), 10,6)
meta::funnel(ma, cex=1.2, common=TRUE, log = "x", yaxis = "se", studlab=TRUE, xlab="Risk Ratio (log axis)")
title(main="Funnel plot: highest exposure contrast, male rats")
dev.off()


###### Sensitivity analysis - exclude double-zero studies ######
 
dat <-  dat[-c(2,5),] # Falcioni 0.1 W/kg and Chou 1992 excluded


ma_sens <<- metabin(event.e = exposed_N_cases,
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
               
               title = "Meta-Analysis of gliomas in  male rats",
               
               random = FALSE,                #no random effects modeled
               common = TRUE,                #default fixed effect model
               sm = "RR",                   #use RR as summary measure 
               method = "MH",
               incr = "TACC",  # otherwise we'd get an RR of 2.0 for the Falcioni 0.1 W/kg group
               allstudies = TRUE, # to force RR calculation for double-zero studies
               
               #method.tau = "DL",            #use DerSimonian-Laird estimator to calculate between-study variance tau
               #method.tau.ci = "J",          #use Method by Jackson (2013) to calculate confidence interval for tau^2 and tau
               
               level = 0.95,                 #level used to calculate confidence intervals for individual studies
               level.ma = 0.95, 	            #level used to calculate confidence intervals for meta-analysis estimate
               subgroup = design,
               subgroup.name = "Exposure condition and duration",
               test.subgroup = TRUE,
)

output_file = "/males_glioma_highestdose_fixed-MH-TACC_noDblZero_RR.txt"
sink(paste0(saveto, output_file))
ma_sens
sink()

# we save the forest plots
png(paste0(saveto, "/males_glioma_highestdose_noDblZero_RR.png"), 11,6, units = "in", res = 300)
forest(ma_sens)
grid.text( "Meta-analysis of gliomas in male rats", x = 0.5, y = 0.9, gp = gpar(cex = 1.5))
grid.text( "(NTP numbers are poly3-adjusted, treatment arm continuity correction)", x = 0.5, y = 0.1, gp = gpar(cex = 1))
dev.off()

pdf(paste0(saveto, "/males_glioma_highestdose_noDblZero_RR.pdf"), 11,6)
forest(ma_sens)
grid.text( "Meta-analysis of gliomas in male rats", x = 0.5, y = 0.9, gp = gpar(cex = 1.5))
grid.text( "(NTP numbers are poly3-adjusted, treatment arm continuity correction)", x = 0.5, y = 0.1, gp = gpar(cex = 1))
dev.off()



##### Step 3: Descriptive forest plot for males for aggregated dose groups#####
# load the according Excel file

dat_aggr  <- read_excel("data_male_glioma_poly3adj_dose-aggreg.xlsx")

# create a variable describing the exposure design of the studies, for subgroup analysis
dat_aggr$design = c("free, >18h/day","free, >18h/day", "restrained, <= 4h/day", "restrained, <= 4h/day","free, >18h/day")


#calculate effect sizes without design effect correction  ---> preferred 
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
                                 
                                 title = "Meta-Analysis of gliomas in  male rats",
                                 
                                random = FALSE,                #no random effects modeled
                                common = TRUE,                #default fixed effect model
                                sm = "RR",                   #use RR as summary measure 
                                method = "MH",
                                incr = "TACC",  # otherwise we'd get an RR of 2.0 for the Falcioni 0.1 W/kg group
                                allstudies = TRUE, # to force RR calculation for double-zero studies
      
                                #method.tau = "DL",            #use DerSimonian-Laird estimator to calculate between-study variance tau
                                #method.tau.ci = "J",          #use Method by Jackson (2013) to calculate confidence interval for tau^2 and tau
                                level = 0.95,                 #level used to calculate confidence intervals for individual studies
                                level.ma = 0.95, 	            #level used to calculate confidence intervals for meta-analysis estimate
                                subgroup = design,
                                subgroup.name = "Exposure condition and duration",
                                test.subgroup = TRUE,
)

# we save the model output to text
output_file = "/males_glioma_doseaggr_fixed-MH-TACC_RR.txt"
sink(paste0(saveto, output_file))
ma_aggr
sink()

# we save the forest plots
png(paste0(saveto, "/males_glioma_dosesaggr_RR.png"), 11,6.5, units = "in", res = 300)
forest(ma_aggr)
grid.text( "Meta-analysis of gliomas in male rats,\nnumbers from different exposure levels combined", x = 0.5, y = 0.9, gp = gpar(cex = 1.5))
grid.text( "(NTP numbers are poly3-adjusted, treatment arm continuity correction)", x = 0.5, y = 0.1, gp = gpar(cex = 1))
dev.off()

pdf(paste0(saveto, "/males_glioma_dosesaggr_RR.pdf"), 11,6.5)
forest(ma_aggr)
grid.text( "Meta-analysis of gliomas in male rats,\nnumbers from different exposure levels combined", x = 0.5, y = 0.9, gp = gpar(cex = 1.5))
grid.text( "(NTP numbers are poly3-adjusted, treatment arm continuity correction)", x = 0.5, y = 0.1, gp = gpar(cex = 1))
dev.off()
 
###### Funnel Plot ######
png(paste0(saveto, "/males_glioma_dosesaggr_FUNNEL_RR.png"), 10,6, units = "in", res = 300)
meta::funnel(ma_aggr, cex=1.2, common=TRUE, log = "x", yaxis = "se", studlab=TRUE, xlab="Risk Ratio (log axis)")
title(main="Funnel plot: aggregated exposure levels, male rats")
dev.off()

pdf(paste0(saveto, "/males_glioma_dosesaggr_FUNNEL_RR.pdf"), 10,6)
meta::funnel(ma_aggr, cex=1.2, common=TRUE, log = "x", yaxis = "se", studlab=TRUE, xlab="Risk Ratio (log axis)")
title(main="Funnel plot: aggregated exposure levels, male rats")
dev.off()



###### Sensitivity analysis - exclude double-zero studies ######

dat_aggr <-  dat_aggr[-c(5),] #  Chou 1992 excluded

ma_aggr_sens <<- metabin(event.e = exposed_N_cases,
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
                    
                    title = "Meta-Analysis of gliomas in  male rats",
                    
                    random = FALSE,                #no random effects modeled
                    common = TRUE,                #default fixed effect model
                    sm = "RR",                   #use RR as summary measure 
                    method = "MH",
                    incr = "TACC",  # otherwise we'd get an RR of 2.0 for the Falcioni 0.1 W/kg group
                    allstudies = FALSE,  # to force RR calculation for double-zero studies
                    
                    #method.tau = "DL",            #use DerSimonian-Laird estimator to calculate between-study variance tau
                    #method.tau.ci = "J",          #use Method by Jackson (2013) to calculate confidence interval for tau^2 and tau
                    level = 0.95,                 #level used to calculate confidence intervals for individual studies
                    level.ma = 0.95, 	            #level used to calculate confidence intervals for meta-analysis estimate
                    subgroup = design,
                    subgroup.name = "Exposure condition and duration",
                    test.subgroup = TRUE,
)

# we save the model output to text
output_file = "/males_glioma_doseaggr_fixed-MH-TACC_noDblZero_RR.txt"
sink(paste0(saveto, output_file))
ma_aggr_sens
sink()

# we save the forest plots
png(paste0(saveto, "/males_glioma_dosesaggr_noDblZero_RR.png"), 11,6.5, units = "in", res = 300)
forest(ma_aggr_sens)
grid.text( "Meta-analysis of gliomas in male rats,\nnumbers from different exposure levels combined", x = 0.5, y = 0.9, gp = gpar(cex = 1.5))
grid.text( "(NTP numbers are poly3-adjusted, treatment arm continuity correction)", x = 0.5, y = 0.1, gp = gpar(cex = 1))
dev.off()

pdf(paste0(saveto, "/males_glioma_dosesaggr_noDblZero_RR.pdf"), 11,6.5)
forest(ma_aggr_sens)
grid.text( "Meta-analysis of gliomas in male rats,\nnumbers from different exposure levels combined", x = 0.5, y = 0.9, gp = gpar(cex = 1.5))
grid.text( "(NTP numbers are poly3-adjusted, treatment arm continuity correction)", x = 0.5, y = 0.1, gp = gpar(cex = 1))
dev.off()

