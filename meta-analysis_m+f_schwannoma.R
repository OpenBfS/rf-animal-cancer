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
dat  <- read_excel("data_m+f_schwannoma_poly3adj_alldoses.xlsx")
# we only analyze the highest doses used in the studies (high- versus-low meta-analysis)
dat <-  dat[c(3,6),]


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
                            
                            title = "Meta-Analysis of cardiac schwannomas in m+f rats",
                            
                            random = FALSE,                #no random effects modeled
                            common = TRUE,                #default fixed effect model
                            sm = "OR",                   #use OR as summary measure 
                            incr = "TACC",
                            method = "MH",

                            # method.tau = "DL",            #use DerSimonian-Laird estimator to calculate between-study variance tau
                            # method.tau.ci = "J",          #use Method by Jackson (2013) to calculate confidence interval for tau^2 and tau

                            level = 0.95,                 #level used to calculate confidence intervals for individual studies
                            level.ma = 0.95, 	            #level used to calculate confidence intervals for meta-analysis estimate
                            #subgroup = _,
                            #subgroup.name = "_",
                            #test.subgroup = TRUE,
)

# we save the model output to text
output_file = "/m+f_schwannoma_highestdose_fixed-MH-TACC.txt"
sink(paste0(saveto, output_file))
ma
sink()

# we save the forest plots
png(paste0(saveto, "/m+f_schwannoma_highestdose.png"), 11,4, units = "in", res = 300)
forest(ma)
grid.text( "Meta-analysis of cardiac schwannomas in m+f rats", x = 0.5, y = 0.9, gp = gpar(cex = 1.5))
grid.text( "(NTP numbers are poly3-adjusted, treatment arm continuity correction)", x = 0.5, y = 0.1, gp = gpar(cex = 1))
dev.off()

pdf(paste0(saveto, "/m+f_schwannoma_highestdose.pdf"), 11,4)
forest(ma)
grid.text( "Meta-analysis of cardiac schwannomas in m+f rats", x = 0.5, y = 0.9, gp = gpar(cex = 1.5))
grid.text( "(NTP numbers are poly3-adjusted, treatment arm continuity correction)", x = 0.5, y = 0.1, gp = gpar(cex = 1))
dev.off()


##### Step 3: Descriptive forest plot for m+f for aggregated dose groups#####
# load the according Excel file

dat_aggr  <- read_excel("data_m+f_schwannoma_poly3adj_dose-aggreg.xlsx")

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
                                 
                                 title = "Meta-Analysis of cardiac schwannomas in m+f rats",
                                 
                                random = FALSE,                #no random effects modeled
                                common = TRUE,                #default fixed effect model
                                sm = "OR",                   #use OR as summary measure 
                                incr = "TACC",
                                method = "MH",
    
                                # method.tau = "DL",            #use DerSimonian-Laird estimator to calculate between-study variance tau
                                # method.tau.ci = "J",          #use Method by Jackson (2013) to calculate confidence interval for tau^2 and tau
                                level = 0.95,                 #level used to calculate confidence intervals for individual studies
                                level.ma = 0.95, 	            #level used to calculate confidence intervals for meta-analysis estimate
                                #subgroup = _,
                                #subgroup.name = "_",
                                #test.subgroup = TRUE,
)

# we save the model output to text
output_file = "/m+f_schwannoma_doseaggr_fixed-MH-TACC.txt"
sink(paste0(saveto, output_file))
ma_aggr
sink()

# we save the forest plots
png(paste0(saveto, "/m+f_schwannoma_dosesaggr.png"), 11,4, units = "in", res = 300)
forest(ma_aggr)
grid.text( "Meta-analysis of cardiac schwannomas in m+f rats,\nnumbers from three exposure levels combined", x = 0.5, y = 0.9, gp = gpar(cex = 1.5))
grid.text( "(NTP numbers are poly3-adjusted, treatment arm continuity correction)", x = 0.5, y = 0.1, gp = gpar(cex = 1))
dev.off()

pdf(paste0(saveto, "/m+f_schwannoma_dosesaggr.pdf"), 11,4)
forest(ma_aggr)
grid.text( "Meta-analysis of cardiac schwannomas in m+f rats,\nnumbers from three exposure levels combined", x = 0.5, y = 0.9, gp = gpar(cex = 1.5))
grid.text( "(NTP numbers are poly3-adjusted, treatment arm continuity correction)", x = 0.5, y = 0.1, gp = gpar(cex = 1))
dev.off()




#### Meta-Analysis with males and females as subgroup - treat male and females as separate experiments ####

##### Step 1: Data preparation & formatting #####
# load data 
dat_male  <- read_excel("data_male_schwannoma_poly3adj_alldoses.xlsx")
dat_female <- read_excel("data_female_schwannoma_poly3adj_alldoses.xlsx")
# we only analyze the highest doses used in the studies (high- versus-low meta-analysis)
dat <-  dat_male %>% rbind(dat_female)
dat <-  dat[c(3,6,10,13),] # also exclude Chou since they used only male rats

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
               
               title = "Meta-Analysis of cardiac schwannomas in m vs. f rats",
               
               random = FALSE,                #no random effects modeled
               common = TRUE,                #default fixed effect model
               sm = "OR",                   #use OR as summary measure 
               incr = "TACC",
               method = "MH",

               # method.tau = "DL",            #use DerSimonian-Laird estimator to calculate between-study variance tau
               # method.tau.ci = "J",          #use Method by Jackson (2013) to calculate confidence interval for tau^2 and tau

               level = 0.95,                 #level used to calculate confidence intervals for individual studies
               level.ma = 0.95, 	            #level used to calculate confidence intervals for meta-analysis estimate
               subgroup = Sex,
               subgroup.name = "Sex",
               test.subgroup = TRUE,
)

# we save the model output to text
output_file = "/m-vs-f_schwannoma_highestdose_fixed-MH-TACC.txt"
sink(paste0(saveto, output_file))
ma
sink()

# we save the forest plots
png(paste0(saveto, "/m-vs-f_schwannoma_highestdose.png"), 11,6, units = "in", res = 300)
forest(ma)
grid.text( "Meta-analysis of cardiac schwannomas in m-vs-f rats", x = 0.5, y = 0.9, gp = gpar(cex = 1.5))
grid.text( "(NTP numbers are poly3-adjusted, treatment arm continuity correction)", x = 0.5, y = 0.1, gp = gpar(cex = 1))
dev.off()

pdf(paste0(saveto, "/m-vs-f_schwannoma_highestdose.pdf"), 11,6)
forest(ma)
grid.text( "Meta-analysis of cardiac schwannomas in m-vs-f rats", x = 0.5, y = 0.9, gp = gpar(cex = 1.5))
grid.text( "(NTP numbers are poly3-adjusted, treatment arm continuity correction)", x = 0.5, y = 0.1, gp = gpar(cex = 1))
dev.off()



##### Step 3: Descriptive forest plot for m-vs-f for aggregated dose groups#####
# load the according Excel file

dat_aggr_male  <- read_excel("data_male_schwannoma_poly3adj_dose-aggreg.xlsx")
dat_aggr_female  <- read_excel("data_female_schwannoma_poly3adj_dose-aggreg.xlsx")
dat_aggr <-  dat_aggr_male %>% rbind(dat_aggr_female)
dat_aggr <-  dat_aggr[-c(3),] # remove Chou et al.
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
                    
                    title = "Meta-Analysis of cardiac schwannomas in m-vs-f rats",
                    
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
output_file = "/m-vs-f_schwannoma_doseaggr_fixed-MH-TACC.txt"
sink(paste0(saveto, output_file))
ma_aggr
sink()

# we save the forest plots
png(paste0(saveto, "/m-vs-f_schwannoma_dosesaggr.png"), 11,6, units = "in", res = 300)
forest(ma_aggr)
grid.text( "Meta-analysis of cardiac schwannomas in m-vs-f rats,\nnumbers from three exposure levels combined", x = 0.5, y = 0.9, gp = gpar(cex = 1.5))
grid.text( "(NTP numbers are poly3-adjusted, treatment arm continuity correction)", x = 0.5, y = 0.1, gp = gpar(cex = 1))
dev.off()

pdf(paste0(saveto, "/m-vs-f_schwannoma_dosesaggr.pdf"), 11,6)
forest(ma_aggr)
grid.text( "Meta-analysis of cardiac schwannomas in m-vs-f rats,\nnumbers from three exposure levels combined", x = 0.5, y = 0.9, gp = gpar(cex = 1.5))
grid.text( "(NTP numbers are poly3-adjusted, treatment arm continuity correction)", x = 0.5, y = 0.1, gp = gpar(cex = 1))
dev.off()

