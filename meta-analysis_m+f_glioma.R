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
dat     <- read_excel("data_m+f_glioma_poly3adj_alldoses.xlsx")
# we only analyze the highest doses used in the studies  (high- versus-low meta-analysis)
dat <-  dat[c(3,6,8,9,10,11,12,13),]

# create a variable describing the exposure design of the studies, for subgroup analysis
dat$design = c("free, >18h/day","free, >18h/day", "restrained, <= 6h/day", "restrained, <= 6h/day","restrained, <= 6h/day","restrained, <= 6h/day","restrained, <= 6h/day","restrained, <= 6h/day")



##### Step 2: Meta-analysis and forest plot for m+f for highest doses #####

# we use the fixed-effects Mantel-Haenszel approach since random-effect and inverse variance models are not recommended for sparse events and few studies (Sweeting 2004)(Cochrane Handbook)
# since we have 0 cases in the control group of both studies, we use the treatment arm continuity correction constrained to kT + kC = 1 (see Sweeting 2004, Diamond 2007), using incr = "TACC"


ma <<- metabin(event.e = exposed_N_cases,
                            n.e = exposed_N_all,
                            event.c =sham_N_cases,
                            n.c = sham_N_all,
                            label.e = "RF-EMF",
                            label.c = "Sham",
                            text.common = "Odds Ratio with 95% CI",
                            label.left = "Favours Exposure",
                            col.label.left = "purple",
                            col.label.right = "red",
                            label.right = "Favours Non-Exposure" ,
                            studlab = RefID.No,  #study labels
                            data =  dat,                  #data
                            
                            title = "Meta-Analysis of gliomas in m+f rats",
                            
                            random = FALSE,                #no random effects modeled
                            common = TRUE,                #default fixed effect model
                            sm = "OR",                   #use OR as summary measure 
                            method = "MH",
                            incr = "TACC",  # otherwise we'd get an OR of 2.0 for the Adey1999 study
                            allstudies = TRUE, # to force OR calculation for double-zero studies
                            #method.tau = "DL",            #use DerSimonian-Laird estimator to calculate between-study variance tau
                            #method.tau.ci = "J",          #use Method by Jackson (2013) to calculate confidence interval for tau^2 and tau
                            
                            level = 0.95,                 #level used to calculate confidence intervals for individual studies
                            level.ma = 0.95, 	            #level used to calculate confidence intervals for meta-analysis estimate
                            subgroup = design,
                            subgroup.name = "Exposure condition and duration",
                            test.subgroup = TRUE,
)

output_file = "/m+f_glioma_highestdose_fixed-MH-TACC.txt"
sink(paste0(saveto, output_file))
ma
sink()

# we save the forest plots
png(paste0(saveto, "/m+f_glioma_highestdose.png"), 11,7, units = "in", res = 300)
forest(ma)
grid.text( "Meta-analysis of gliomas in m+f rats", x = 0.5, y = 0.9, gp = gpar(cex = 1.5))
grid.text( "(NTP numbers are poly3-adjusted, treatment arm continuity correction)", x = 0.5, y = 0.1, gp = gpar(cex = 1))
dev.off()

pdf(paste0(saveto, "/m+f_glioma_highestdose.pdf"), 11,7)
forest(ma)
grid.text( "Meta-analysis of gliomas in m+f rats", x = 0.5, y = 0.9, gp = gpar(cex = 1.5))
grid.text( "(NTP numbers are poly3-adjusted, treatment arm continuity correction)", x = 0.5, y = 0.1, gp = gpar(cex = 1))
dev.off()

###### Funnel Plot ######
png(paste0(saveto, "/m+f_glioma_highestdose_FUNNEL.png"), 10,6, units = "in", res = 300)
meta::funnel(ma, cex=1.2, common=TRUE, log = "x", yaxis = "se", studlab=TRUE, xlab="Odds Ratio (log axis)")
title(main="Funnel plot: highest exposure contrast, male+female rats")
dev.off()

pdf(paste0(saveto, "/m+f_glioma_highestdose_FUNNEL.pdf"), 10,6)
meta::funnel(ma, cex=1.2, common=TRUE, log = "x", yaxis = "se", studlab=TRUE, xlab="Odds Ratio (log axis)")
title(main="Funnel plot: highest exposure contrast, male+female rats")
dev.off()


##### Step 3: Descriptive forest plot for m+f for aggregated dose groups#####
# load the according Excel file

dat_aggr  <- read_excel("data_m+f_glioma_poly3adj_dose-aggreg.xlsx")

# create a variable describing the exposure design of the studies, for subgroup analysis
dat_aggr$design = c("free, >18h/day","free, >18h/day", "restrained, <= 6h/day", "restrained, <= 6h/day","restrained, <= 6h/day","restrained, <= 6h/day","restrained, <= 6h/day","restrained, <= 6h/day")


ma_aggr <<- metabin(event.e = exposed_N_cases,
                                 n.e = exposed_N_all,
                                 event.c = sham_N_cases,
                                 n.c = sham_N_all,
                                 label.e = "RF-EMF",
                                 label.c = "Sham",
                                 text.common = "Odds Ratio with 95% CI",
                                 label.left = "Favours Exposure",
                                 col.label.left = "purple",
                                 col.label.right = "red",
                                 label.right = "Favours Non-Exposure" ,
                                 studlab = RefID.No,  #study labels
                                 data =  dat_aggr,                  #data
                                 
                                 title = "Meta-Analysis of gliomas in m+f rats",
                                 
                                random = FALSE,                #no random effects modeled
                                common = TRUE,                #default fixed effect model
                                sm = "OR",                   #use OR as summary measure 
                                method = "MH",
                                incr = "TACC",  # otherwise we'd get an OR of 2.0 for the Falcioni 0.1 W/kg group
                                allstudies = TRUE, # to force OR calculation for double-zero studies

                                #method.tau = "DL",            #use DerSimonian-Laird estimator to calculate between-study variance tau
                                #method.tau.ci = "J",          #use Method by Jackson (2013) to calculate confidence interval for tau^2 and tau
                                
                                level = 0.95,                 #level used to calculate confidence intervals for individual studies
                                level.ma = 0.95, 	            #level used to calculate confidence intervals for meta-analysis estimate
                                subgroup = design,
                                subgroup.name = "Exposure condition and duration",
                                test.subgroup = TRUE,
)

# we save the model output to text
output_file = "/m+f_glioma_doseaggr_fixed-MH-TACC.txt"
sink(paste0(saveto, output_file))
ma_aggr
sink()

# we save the forest plots
png(paste0(saveto, "/m+f_glioma_dosesaggr.png"), 11,7.5, units = "in", res = 300)
forest(ma_aggr)
grid.text( "Meta-analysis of gliomas in m+f rats,\nnumbers from different exposure levels combined", x = 0.5, y = 0.9, gp = gpar(cex = 1.5))
grid.text( "(NTP numbers are poly3-adjusted, treatment arm continuity correction)", x = 0.5, y = 0.1, gp = gpar(cex = 1))
dev.off()

pdf(paste0(saveto, "/m+f_glioma_dosesaggr.pdf"), 11,7.5)
forest(ma_aggr)
grid.text( "Meta-analysis of gliomas in m+f rats,\nnumbers from different exposure levels combined", x = 0.5, y = 0.9, gp = gpar(cex = 1.5))
grid.text( "(NTP numbers are poly3-adjusted, treatment arm continuity correction)", x = 0.5, y = 0.1, gp = gpar(cex = 1))
dev.off()
 

###### Funnel Plot ######
png(paste0(saveto, "/m+f_glioma_dosesaggr_FUNNEL.png"), 10,6, units = "in", res = 300)
meta::funnel(ma_aggr, cex=1.2, common=TRUE, log = "x", yaxis = "se", studlab=TRUE, xlab="Odds Ratio (log axis)")
title(main="Funnel plot: aggregated exposure levels, male rats")
dev.off()

pdf(paste0(saveto, "/m+f_glioma_dosesaggr_FUNNEL.pdf"), 10,6)
meta::funnel(ma_aggr, cex=1.2, common=TRUE, log = "x", yaxis = "se", studlab=TRUE, xlab="Odds Ratio (log axis)")
title(main="Funnel plot: aggregated exposure levels, male rats")
dev.off()


#### Meta-Analysis with males and females as subgroup - treat male and females as separate experiments ####

###### Step 1: Data preparation & formatting ######
# load data 
dat_male  <- read_excel("data_male_glioma_poly3adj_alldoses.xlsx")
dat_female <- read_excel("data_female_glioma_poly3adj_alldoses.xlsx")
# we only analyze the highest doses used in the studies (high- versus-low meta-analysis) and only use the core studies that have provided separate information on males vs females
dat <-  dat_male %>% rbind(dat_female)
dat <-  dat[c(3,6,8,9,13,16,18,19),]

###### Step 2: Meta-analysis and forest plot for m vs. f for highest doses ######
# we use the fixed-effects Mantel-Haenszel approach since random-effect and inverse variance models are not recommended for sparse events and few studies (Sweeting 2004)(Cochrane Handbook)
# since we have 0 cases in the control group of both studies, we use the treatment arm continuity correction constrained to kT + kC = 1 (see Sweeting 2004, Diamond 2007), using incr = "TACC"

ma <<- metabin(event.e = exposed_N_cases,
               n.e = exposed_N_all,
               event.c =sham_N_cases,
               n.c = sham_N_all,
               label.e = "RF-EMF",
               label.c = "Sham",
               text.common = "Odds Ratio with 95% CI",
               label.left = "Favours Exposure",
               col.label.left = "purple",
               col.label.right = "red",
               label.right = "Favours Non-Exposure" ,
               studlab = RefID.No,  #study labels
               data =  dat,                  #data
               
               title = "Meta-Analysis of cardiac gliomas in m vs. f rats",
               
               random = FALSE,                #no random effects modeled
               common = TRUE,                #default fixed effect model
               sm = "OR",                   #use OR as summary measure 
               incr = "TACC",
               allstudies = TRUE, # to force OR calculation for double-zero studies (Falcioni 0.1 W/kg)
               method = "MH", 
  
               #method.tau = "DL",            #use DerSimonian-Laird estimator to calculate between-study variance tau
               #method.tau.ci = "J",          #use Method by Jackson (2013) to calculate confidence interval for tau^2 and tau

               level = 0.95,                 #level used to calculate confidence intervals for individual studies
               level.ma = 0.95, 	            #level used to calculate confidence intervals for meta-analysis estimate
               subgroup = Sex,
               subgroup.name = "Sex",
               test.subgroup = TRUE,
)

# we save the model output to text
output_file = "/m-vs-f_glioma_highestdose_fixed-MH-TACC.txt"
sink(paste0(saveto, output_file))
ma
sink()

# we save the forest plots
png(paste0(saveto, "/m-vs-f_glioma_highestdose.png"), 11,7, units = "in", res = 300)
forest(ma)
grid.text( "Meta-analysis of cardiac gliomas in m-vs-f rats", x = 0.5, y = 0.9, gp = gpar(cex = 1.5))
grid.text( "(NTP numbers are poly3-adjusted, treatment arm continuity correction)", x = 0.5, y = 0.1, gp = gpar(cex = 1))
dev.off()

pdf(paste0(saveto, "/m-vs-f_glioma_highestdose.pdf"), 11,7)
forest(ma)
grid.text( "Meta-analysis of cardiac gliomas in m-vs-f rats", x = 0.5, y = 0.9, gp = gpar(cex = 1.5))
grid.text( "(NTP numbers are poly3-adjusted, treatment arm continuity correction)", x = 0.5, y = 0.1, gp = gpar(cex = 1))
dev.off()



###### Step 3: Descriptive forest plot for m-vs-f for aggregated dose groups ######
# load the according Excel file

dat_aggr_male  <- read_excel("data_male_glioma_poly3adj_dose-aggreg.xlsx")
dat_aggr_female  <- read_excel("data_female_glioma_poly3adj_dose-aggreg.xlsx")
dat_aggr <-  dat_aggr_male %>% rbind(dat_aggr_female)
dat_aggr <- dat_aggr[-c(5),] # remove Chou et al.

#calculate effect sizes without design effect correction  ---> preferred ?
ma_aggr <<- metabin(event.e = exposed_N_cases,
                    n.e = exposed_N_all,
                    event.c =sham_N_cases,
                    n.c = sham_N_all,
                    label.e = "RF-EMF",
                    label.c = "Sham",
                    text.common = "Odds Ratio with 95% CI",
                    label.left = "Favours Exposure",
                    col.label.left = "purple",
                    col.label.right = "red",
                    label.right = "Favours Non-Exposure" ,
                    studlab = RefID.No,  #study labels
                    data =  dat_aggr,                  #data
                    
                    title = "Meta-Analysis of cardiac gliomas in m-vs-f rats",
                    
                    random = FALSE,                #no random effects modeled
                    common = TRUE,                #default fixed effect model
                    sm = "OR",                   #use OR as summary measure 
                    incr = "TACC",
                    method = "MH",
        
                    #method.tau = "DL",            #use DerSimonian-Laird estimator to calculate between-study variance tau
                    #method.tau.ci = "J",          #use Method by Jackson (2013) to calculate confidence interval for tau^2 and tau

                    level = 0.95,                 #level used to calculate confidence intervals for individual studies
                    level.ma = 0.95, 	            #level used to calculate confidence intervals for meta-analysis estimate
                    subgroup = Sex,
                    subgroup.name = "Sex",
                    test.subgroup = TRUE,
)

# we save the model output to text
output_file = "/m-vs-f_glioma_doseaggr_fixed-MH-TACC.txt"
sink(paste0(saveto, output_file))
ma_aggr
sink()

# we save the forest plots
png(paste0(saveto, "/m-vs-f_glioma_dosesaggr.png"), 11,7, units = "in", res = 300)
forest(ma_aggr)
grid.text( "Meta-analysis of cardiac gliomas in m-vs-f rats,\nnumbers from three exposure levels combined", x = 0.5, y = 0.9, gp = gpar(cex = 1.5))
grid.text( "(NTP numbers are poly3-adjusted, treatment arm continuity correction)", x = 0.5, y = 0.1, gp = gpar(cex = 1))
dev.off()

pdf(paste0(saveto, "/m-vs-f_glioma_dosesaggr.pdf"), 11,7)
forest(ma_aggr)
grid.text( "Meta-analysis of cardiac gliomas in m-vs-f rats,\nnumbers from three exposure levels combined", x = 0.5, y = 0.9, gp = gpar(cex = 1.5))
grid.text( "(NTP numbers are poly3-adjusted, treatment arm continuity correction)", x = 0.5, y = 0.1, gp = gpar(cex = 1))
dev.off()


###### Step4: Sensitivity analysis - check for sex difference only within NTP+Falcioni (free-moving exposure) studies ######

dat <-  dat[c(1,2,5,6),]

####### Meta-analysis and forest plot for m vs. f for highest doses in free-moving exposure studies####### 

ma <<- metabin(event.e = exposed_N_cases,
               n.e = exposed_N_all,
               event.c =sham_N_cases,
               n.c = sham_N_all,
               label.e = "RF-EMF",
               label.c = "Sham",
               text.common = "Odds Ratio with 95% CI",
               label.left = "Favours Exposure",
               col.label.left = "purple",
               col.label.right = "red",
               label.right = "Favours Non-Exposure" ,
               studlab = RefID.No,  #study labels
               data =  dat,                  #data
               
               title = "Meta-Analysis of cardiac gliomas in m vs. f rats (NTP+Falcioni)",
               
               random = FALSE,                #no random effects modeled
               common = TRUE,                #default fixed effect model
               sm = "OR",                   #use OR as summary measure 
               incr = "TACC",
               allstudies = TRUE, # to force OR calculation for double-zero studies
               method = "MH", 
               #method.tau = "DL",            #use DerSimonian-Laird estimator to calculate between-study variance tau
               #method.tau.ci = "J",          #use Method by Jackson (2013) to calculate confidence interval for tau^2 and tau
     
               level = 0.95,                 #level used to calculate confidence intervals for individual studies
               level.ma = 0.95, 	            #level used to calculate confidence intervals for meta-analysis estimate
               subgroup = Sex,
               subgroup.name = "Sex",
               test.subgroup = TRUE,
)

# we save the model output to text
output_file = "/m-vs-f_glioma_highestdose_NTPFalc_fixed-MH-TACC.txt"
sink(paste0(saveto, output_file))
ma
sink()

# we save the forest plots
png(paste0(saveto, "/m-vs-f_glioma_highestdose_NTPFalc.png"), 11,7, units = "in", res = 300)
forest(ma)
grid.text( "Meta-analysis of cardiac gliomas in m-vs-f rats", x = 0.5, y = 0.9, gp = gpar(cex = 1.5))
grid.text( "(NTP numbers are poly3-adjusted, treatment arm continuity correction)", x = 0.5, y = 0.1, gp = gpar(cex = 1))
dev.off()

pdf(paste0(saveto, "/m-vs-f_glioma_highestdose_NTPFalc.pdf"), 11,7)
forest(ma)
grid.text( "Meta-analysis of cardiac gliomas in m-vs-f rats", x = 0.5, y = 0.9, gp = gpar(cex = 1.5))
grid.text( "(NTP numbers are poly3-adjusted, treatment arm continuity correction)", x = 0.5, y = 0.1, gp = gpar(cex = 1))
dev.off()



####### Meta-analysis and forest plot for m vs. f for aggregated dose groups in free-moving exposure studies ####### 

dat_aggr <-  dat_aggr[c(1,2,5,6), ]

ma_aggr <<- metabin(event.e = exposed_N_cases,
                    n.e = exposed_N_all,
                    event.c =sham_N_cases,
                    n.c = sham_N_all,
                    label.e = "RF-EMF",
                    label.c = "Sham",
                    text.common = "Odds Ratio with 95% CI",
                    label.left = "Favours Exposure",
                    col.label.left = "purple",
                    col.label.right = "red",
                    label.right = "Favours Non-Exposure" ,
                    studlab = RefID.No,  #study labels
                    data =  dat_aggr,                  #data
                    
                    title = "Meta-Analysis of cardiac gliomas in m-vs-f rats (NTP+Falcioni)",
                    
                    random = FALSE,                #no random effects modeled
                    common = TRUE,                #default fixed effect model
                    sm = "OR",                   #use OR as summary measure 
                    incr = "TACC",
                    method = "MH",
        
                    #method.tau = "DL",            #use DerSimonian-Laird estimator to calculate between-study variance tau
                    #method.tau.ci = "J",          #use Method by Jackson (2013) to calculate confidence interval for tau^2 and tau
             
                    level = 0.95,                 #level used to calculate confidence intervals for individual studies
                    level.ma = 0.95, 	            #level used to calculate confidence intervals for meta-analysis estimate
                    subgroup = Sex,
                    subgroup.name = "Sex",
                    test.subgroup = TRUE,
)

# we save the model output to text
output_file = "/m-vs-f_glioma_doseaggr_NTPFalc_fixed-MH-TACC.txt"
sink(paste0(saveto, output_file))
ma_aggr
sink()

# we save the forest plots
png(paste0(saveto, "/m-vs-f_glioma_dosesaggr_NTPFalc.png"), 11,7, units = "in", res = 300)
forest(ma_aggr)
grid.text( "Meta-analysis of cardiac gliomas in m-vs-f rats,\nnumbers from three exposure levels combined", x = 0.5, y = 0.9, gp = gpar(cex = 1.5))
grid.text( "(NTP numbers are poly3-adjusted, treatment arm continuity correction)", x = 0.5, y = 0.1, gp = gpar(cex = 1))
dev.off()

pdf(paste0(saveto, "/m-vs-f_glioma_dosesaggr_NTPFalc.pdf"), 11,7)
forest(ma_aggr)
grid.text( "Meta-analysis of cardiac gliomas in m-vs-f rats,\nnumbers from three exposure levels combined", x = 0.5, y = 0.9, gp = gpar(cex = 1.5))
grid.text( "(NTP numbers are poly3-adjusted, treatment arm continuity correction)", x = 0.5, y = 0.1, gp = gpar(cex = 1))
dev.off()

