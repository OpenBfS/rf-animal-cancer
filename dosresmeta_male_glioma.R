source("config.R") # load working directory from config file
setwd(workdir)


#### Meta-analysis per dose-level to obtain ORs and SEs for use with dosresmeta ####

# We are using treatment arm continuity correction (TACC) according to Sweeting 2004 but modify it to accommodate the situation of more than two groups per study and more than one study in the analysis.
# Specifically, we distribute the constraint of one case per zero-cell study group  among the groups of the zero-cell-affected studies proportionally to the group size. 

if (!require(meta)) {install.packages("meta")} #"normal" meta-analysis

if (!require(tidyverse)) {install.packages("tidyverse")} #useful basic function
if (!require(readxl)) {install.packages("readxl")} #read excel files
if (!require(writexl)) {install.packages("writexl")}  #read excel files
if (!require(grid)) {install.packages("grid")}

settings.meta("revman5")

saveto <- "meta"
# Create a results directory if it doesn't exist
if (!dir.exists(saveto)) {
  dir.create(saveto, recursive = TRUE)
}


dat  <- read_excel("data_male_glioma_poly3adj_alldoses.xlsx")

##### Step 1: Manual modified per-study treatment arm continuity correction (described in the paper as generalized TACC; here we call it fr(actional)TACC ) #####

# we only modify those studies affected  by the zero-cell problem

total_N_NTP <- sum(dat$exposed_N_all[1:3]) + sum(dat$sham_N_all[1])

total_N_Falcioni <- sum(dat$exposed_N_all[4:6]) + sum(dat$sham_N_all[4])

total_N_Chou <- sum(dat$exposed_N_all[10]) + sum(dat$sham_N_all[10])

# create a group size vector that has the same length as the total number of experimental groups per study

group_sizes_NTP <- c(dat$sham_N_all[1], dat$exposed_N_all[1:3])

group_sizes_Falcioni <- c(dat$sham_N_all[4], dat$exposed_N_all[4:6])

group_sizes_Chou <- c(dat$sham_N_all[10], dat$exposed_N_all[10])

# create a vector to proportionally distribute 4(arms) x 0.5 = 2 total cases among the case cells of the groups
cases_to_distribute = 2
frac_CC_NTP <- cases_to_distribute * (group_sizes_NTP / total_N_NTP)
frac_CC_Falcioni <- cases_to_distribute * (group_sizes_Falcioni / total_N_Falcioni)


frac_CC_Chou <- 1 * (group_sizes_Chou / total_N_Chou) # only 2 arms, therefore only 2 * 0.5 = 1 case to distribute

# round to three decimals
frac_CC_NTP <- round(frac_CC_NTP, 3)
frac_CC_Falcioni <- round(frac_CC_Falcioni, 3)
frac_CC_Chou <- round(frac_CC_Chou, 3)

# adjust vectors to match data frame structure with the exposed and sham columns. Round to two decimals
exposed_add <- round ( c(frac_CC_NTP[2:4], frac_CC_Falcioni[2:4], frac_CC_Chou[2]), 2)
sham_add <- round ( c(rep(frac_CC_NTP[1], 3), rep(frac_CC_Falcioni[1], 3), frac_CC_Chou[1]), 2)

# check, should result in (2x2 + 1) x2=10 (last x2 because of the same number of animals added to non-events)
sum(2*exposed_add) + 2* sum(sham_add[c(1,4,7)])

# modify data frame - total cases are going to get doubled (numbers get added to all four cells of a 2x2 table)
dat$exposed_N_cases[c(1:6, 10)]  = dat$exposed_N_cases[c(1:6, 10)] + exposed_add
dat$exposed_N_all[c(1:6, 10)]  = dat$exposed_N_all[c(1:6, 10)]  + 2*exposed_add

dat$sham_N_cases[c(1:6, 10)]  = dat$sham_N_cases[c(1:6, 10)]  + sham_add
dat$sham_N_all[c(1:6, 10)]  = dat$sham_N_all[c(1:6, 10)]  + 2*sham_add



##### Step 2: Descriptive forest plot for males #####
# Note: this meta-analysis is not valid since the Sham group is shared between different exposure levels. This is only to obtain / verify ORs and SEs.

ma <<- metabin(event.e = exposed_N_cases,
               n.e = exposed_N_all ,
               event.c =sham_N_cases ,
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
               
               title = "Meta-Analysis of gliomas in  male rats",
               
               random = FALSE,                #random effects
               common = TRUE,                #fixed effect model
               sm = "OR",                   #use OR as summary measure 
               method = "MH",
               incr = 0,
               #allstudies = TRUE,
               #method.tau = "DL",            #use DerSimonian-Laird estimator to calculate between-study variance tau
               #method.tau.ci = "J",          #use Method by Jackson (2013) to calculate confidence interval for tau^2 and tau
               level = 0.95,                 #level used to calculate confidence intervals for individual studies
               level.ma = 0.95, 	            #level used to calculate confidence intervals for meta-analysis estimate
)

ma 

png(paste0(saveto, "/males_glioma_alldoses_frTACC.png"), 11,6, units = "in", res = 300)
forest(ma)
grid.text( "Meta-analysis of gliomas in male rats", x = 0.5, y = 0.9, gp = gpar(cex = 1.5))
grid.text( "(NTP numbers are poly3-adjusted, fractional per study treatment arm continuity correction)", x = 0.5, y = 0.1, gp = gpar(cex = 1))
dev.off()


pdf(paste0(saveto, "/males_glioma_alldoses_frTACC.pdf"), 11,6)
forest(ma)
grid.text( "Meta-analysis of gliomas in male rats", x = 0.5, y = 0.9, gp = gpar(cex = 1.5))
grid.text( "(NTP numbers are poly3-adjusted, fractional per study treatment arm continuity correction)", x = 0.5, y = 0.1, gp = gpar(cex = 1))
dev.off()



##### Step 3: extract ORs with SE from the meta-analysis, to pass it on to dose-response meta-analysis #####

# extract Odds Ratios
forest = forest(ma)
forest$effect.format
ORs = as.numeric(forest$effect.format[4:length(forest$effect.format)])
ORs

# extract confidence intervals
CI = forest$ci.format[4:length(forest$effect.format)]

# Remove brackets and split the strings
cleaned_CI<- gsub("\\[|\\]", "", CI)
split_CI <- strsplit(cleaned_CI, ",\\s*")

# Extract lower and upper bounds
CI_lower <- sapply(split_CI, function(x) as.numeric(x[1]))
CI_upper <- sapply(split_CI, function(x) as.numeric(x[2]))


# Calculate log odds ratios
logOR <- log(ORs) # this is a natural logarithm

# Calculate the standard error of the log odds ratios
z_value <- 1.96  # For a 95% CI
logSE <- (log(CI_upper) - log(CI_lower)) / (2 * z_value)

# Display results
print("Odds ratios:")
print(ORs)

print("Standard Error of Log odds ratios:")
print(logSE)


##### calculate ORs and SEs manually #####
logORs <-  with(dat, log((exposed_N_cases * (sham_N_all - sham_N_cases) ) / ((exposed_N_all - exposed_N_cases)*sham_N_cases)) )

logORs
exp(logORs)

logSEs <- with(dat, sqrt( 1/exposed_N_cases + 1 /(exposed_N_all - exposed_N_cases) + 1/sham_N_cases + 1/(sham_N_all - sham_N_cases)))

# get CI
CI_lower <-  round(exp(c(logORs - qnorm ( 1- 0.05/2) * logSEs)) , 4)
CI_upper <-  round(exp(c(logORs + qnorm ( 1- 0.05/2) * logSEs)) , 4)

# check if own calculation matches the metabin computation to two decimals
round(exp(logORs), 2) == round(exp(logOR), 2)



##### Step 4: we generate a data frame with all data and the format necessary for dosresmeta ##### 

# Define the other vectors for your data frame

doses <- c(1.5, 3, 6, 0.001, 0.03, 0.1, 0.16, 1.6, 1.3, 0.4)

events_exposed <- dat$exposed_N_cases 
total_exposed <- dat$exposed_N_all 

# Create the initial data frame
data_frame <- data.frame(
  author = "authors",
  id = "ids",
  type = "types",
  dose = doses,
  cases = events_exposed,
  n = total_exposed,
  logrr = logORs,
  se = logSEs,
  ci_lower = CI_lower,
  ci_upper = CI_upper,
  stringsAsFactors = FALSE
)

data_frame

events_control <- dat$sham_N_cases 
total_control <- dat$sham_N_all

# We need to insert the reference row values
NTP_reference_row <- data.frame(author = "", id = "", type = "",
                            dose = 0, cases = events_control[1], n = total_control[1],
                            logrr = 0, se = NA, ci_lower= NA, ci_upper = NA, stringsAsFactors = FALSE)

Ramazzini_reference_row <- data.frame(author = "", id = "", type = "",
                                dose = 0, cases = events_control[4], n = total_control[4],
                                logrr = 0, se = NA, ci_lower= NA, ci_upper = NA, stringsAsFactors = FALSE)

Anderson_reference_row <- data.frame(author = "", id = "", type = "",
                                      dose = 0, cases = events_control[7], n = total_control[7],
                                      logrr = 0, se = NA, ci_lower= NA, ci_upper = NA, stringsAsFactors = FALSE)

LaRegina_reference_row <- data.frame(author = "", id = "", type = "",
                                      dose = 0, cases = events_control[9], n = total_control[9],
                                      logrr = 0, se = NA, ci_lower= NA, ci_upper = NA, stringsAsFactors = FALSE)

Chou_reference_row <- data.frame(author = "", id = "", type = "", 
                                 dose = 0, cases = events_control[10], n = total_control[10],  
                                 logrr = 0, se = NA, ci_lower= NA, ci_upper =  NA, stringsAsFactors = FALSE)

# Insert reference row at the beginning (position 0)
data_frame <- rbind(NTP_reference_row, data_frame)

# Insert another reference row at position 4, 7 and 9
data_frame <- rbind(data_frame[1:4,], Ramazzini_reference_row, data_frame[-(1:4),])
data_frame <- rbind(data_frame[1:8,], Anderson_reference_row, data_frame[-(1:8),])
data_frame <- rbind(data_frame[1:11,], LaRegina_reference_row, data_frame[-(1:11),])
data_frame <- rbind(data_frame[1:13,], Chou_reference_row, data_frame[-(1:13),])

data_frame$author <- rep(c("NTP[2018][rats][GSM+CDMA]", "Ramazzini[2018]", "Anderson[2004]", "LaRegina[2003]", "Chou[1992]"), c(4,4,3,2,2))
data_frame$id <- rep(c(1, 2, 3, 4, 5), c(4,4,3,2,2))
data_frame$type <- rep("ci", length(dim(data_frame)[1]))  # "ci" stands for cumulative incidence

data_frame

saveto <- "./dosresmeta"
if (!dir.exists(saveto)) {
  dir.create(saveto, recursive = TRUE)
}

write_xlsx(data_frame, path  = paste0(saveto, "/glioma_frTACC_dosres.xlsx"))


           
#### Dose-Response Meta-Analysis ####

if (!require(dosresmeta)) {install.packages("dosresmeta")}
if (!require(mvtnorm)) {install.packages("mvtnorm")}
if (!require(ellipse)) {install.packages("ellipse")} 
if (!require(mvmeta)) {install.packages("mvmeta")}
#if (!require(rms)) {install.packages("rms")} # install from source v 6.7.0 for older R versions:
# require(devtools)
# install_version("rms", version = "6.7.0", repos = "http://cran.us.r-project.org")
# packageurl <- "https://cran.r-project.org/src/contrib/Archive/rms/rms_6.7-0.tar.gz"
# install.packages(packageurl, repos=NULL, type="source")
if (!require(splines)) {install.packages("splines")}
if (!require(rmeta)) {install.packages("rmeta")}
if (!require(grid)) {install.packages("grid")}


saveto <- "./dosresmeta"
data_bin <- read_excel(paste0(saveto, "/glioma_frTACC_dosres.xlsx"))

# log transform dose representing the SAR levels
data_bin$dose <-  log(data_bin$dose + 1e-6)

##### linear model #####
# because we have very few studies with sparse event rates and non-overlapping exposure level ranges, we choose to perform a one-stage procedure with a fixed-effect model. See also Crippa 2018 (one-stage dose-response meta-analysis)

###### subgroup analysis - only free-moving exposure studies ######
# we can only analyse subgroups since the free-moving and restraint studies did not uniformly provide whole-body averaged and brain-averaged SAR values and cannot be analysed using the same metric. For free-moving exposure studies, whole-body averaged SAR values are available, while for the restrained exposure studies, brain-averaged SAR values are available.

lin_bin <- dosresmeta(formula=logrr ~ dose, type=type, id=id,
                      lb=ci_lower, ub=ci_upper, 
                      #se=se, 
                      cases=cases, n=n, covariance = "gl", method="fixed", proc="1stage",
                      data=data_bin[c(1:8,14:15),] ) 

output_file <- "/males_glioma_frTACC_lin_freemov_ModelSummary.txt"

# Open the sink connection
sink(paste0(saveto, output_file))
summary(lin_bin)
print("OR increase for every unit of ln(SAR):")
predict(lin_bin, delta=1, exp=TRUE)
sink()

dosex_bin <- data.frame(dose=seq(round(min(data_bin$dose)), 2, 0.1)) # this code generates the x-values for the predictions to plot, and covers the SAR range

obj <-  summary(lin_bin) # we save the model results into an object, so we can call the p-value

#display plots on log-log scale
# For log-log plot: Define SAR tick values to be shown on x-axis
xticks_SAR <- c(6, 3, 1, 0.1, 0.03, 0,01, 0.001, 10^-6)

# Convert them to ln(SAR) positions
xticks_lnSAR <- log(xticks_SAR)


#plot pdf
pdf(paste0(saveto, "/males_glioma_frTACC_lin_freemov.pdf"), 7,4)

with(predict(lin_bin, dosex_bin, order=TRUE, exp=TRUE), {plot(dose, pred, type="l", col="blue", log="y", ylim=c(0.3, (max(exp(data_bin$logrr))+0.1*(max(exp(data_bin$logrr))))), ylab="glioma odds ratio", xlab="SAR, W/kg", axes=FALSE)
  
  lines(dose, ci.lb, lty=2)
  
  lines(dose, ci.ub, lty=2)})
points(data_bin[c(1:8,14:15),]$dose, exp(data_bin[c(1:8,14:15),]$logrr))

axis(2)  # y-axis default (log scale)
axis(1, at=xticks_lnSAR, labels=xticks_SAR)  # x-axis with SAR values
box()

grid.text( "Dose-response meta-analysis (linear) \n of gliomas in male rats", x = 0.5, y = 0.9, gp = gpar(cex = 1.3))
grid.text( paste0("p-value = ", round(obj$Wald.test[3], 4), "\nAIC = ", round(obj$AIC, 4)), x = 0.5, y = 0.7, gp = gpar(cex = 1))

dev.off()


#plot png
png(paste0(saveto, "/males_glioma_frTACC_lin_freemov.png"), 7,4, unit = "in", res= 300)

with(predict(lin_bin, dosex_bin, order=TRUE, exp=TRUE), {plot(dose, pred, type="l", col="blue", log="y", ylim=c(0.3, (max(exp(data_bin$logrr))+0.1*(max(exp(data_bin$logrr))))), ylab="glioma odds ratio", xlab="SAR, W/kg", axes=FALSE)
  
  lines(dose, ci.lb, lty=2)
  
  lines(dose, ci.ub, lty=2)})
points(data_bin[c(1:8,14:15),]$dose, exp(data_bin[c(1:8,14:15),]$logrr))

axis(2)  # y-axis default (log scale)
axis(1, at=xticks_lnSAR, labels=xticks_SAR)  # x-axis with SAR values
box()

grid.text( "Dose-response meta-analysis (linear) \n of gliomas in male rats", x = 0.5, y = 0.9, gp = gpar(cex = 1.3))
grid.text( paste0("p-value = ", round(obj$Wald.test[3], 4), "\nAIC = ", round(obj$AIC, 4)), x = 0.5, y = 0.7, gp = gpar(cex = 1))

dev.off()

####### sensitivity analysis - exclude double-zero studies #######
# we will exclude Chou et al. 1992 and Falcioni 0.1 W/kg

lin_bin <- dosresmeta(formula=logrr ~ dose, type=type, id=id,
                      lb=ci_lower, ub=ci_upper, 
                      #se=se, 
                      cases=cases, n=n, covariance = "gl",  method="fixed", proc="1stage", data=data_bin[c(1:7),] ) 

output_file <- "/males_glioma_frTACC_lin_freemov_noDblZero_ModelSummary.txt"

# Open the sink connection
sink(paste0(saveto, output_file))
summary(lin_bin)
print("OR increase for every unit of ln(SAR):")
predict(lin_bin, delta=1, exp=TRUE)
sink()

dosex_bin <- data.frame(dose=seq(round(min(data_bin$dose)), 2, 0.1)) # this code generates the x-values for the predictions to plot, and covers the SAR range

obj <-  summary(lin_bin)


#plot pdf
pdf(paste0(saveto, "/males_glioma_frTACC_lin_freemov_noDblZero.pdf"), 7,4)

with(predict(lin_bin, dosex_bin, order=TRUE, exp=TRUE), {plot(dose, pred, type="l", col="blue", log="y", ylim=c(0.3, (max(exp(data_bin$logrr))+0.1*(max(exp(data_bin$logrr))))), ylab="glioma odds ratio", xlab="SAR, W/kg", axes=FALSE)
  
  lines(dose, ci.lb, lty=2)
  
  lines(dose, ci.ub, lty=2)})
points(data_bin[c(1:7),]$dose, exp(data_bin[c(1:7),]$logrr))

axis(2)  # y-axis default (log scale)
axis(1, at=xticks_lnSAR, labels=xticks_SAR)  # x-axis with SAR values
box()

grid.text( "Dose-response meta-analysis (linear) \n of gliomas in male rats", x = 0.5, y = 0.9, gp = gpar(cex = 1.3))
grid.text( paste0("p-value = ", round(obj$Wald.test[3], 4), "\nAIC = ", round(obj$AIC, 4)), x = 0.5, y = 0.7, gp = gpar(cex = 1))

dev.off()

#plot png
png(paste0(saveto, "/males_glioma_frTACC_lin_freemov_noDblZero.png"), 7,4, unit = "in", res= 300)

with(predict(lin_bin, dosex_bin, order=TRUE, exp=TRUE), {plot(dose, pred, type="l", col="blue", log="y", ylim=c(0.3, (max(exp(data_bin$logrr))+0.1*(max(exp(data_bin$logrr))))), ylab="glioma odds ratio", xlab="SAR, W/kg", axes=FALSE)
  
  lines(dose, ci.lb, lty=2)
  
  lines(dose, ci.ub, lty=2)})
points(data_bin[c(1:7),]$dose, exp(data_bin[c(1:7),]$logrr))

axis(2)  # y-axis default (log scale)
axis(1, at=xticks_lnSAR, labels=xticks_SAR)  # x-axis with SAR values
box()

grid.text( "Dose-response meta-analysis (linear) \n of gliomas in male rats", x = 0.5, y = 0.9, gp = gpar(cex = 1.3))
grid.text( paste0("p-value = ", round(obj$Wald.test[3], 4), "\nAIC = ", round(obj$AIC, 4)), x = 0.5, y = 0.7, gp = gpar(cex = 1))

dev.off()



###### subgroup analysis - only restraint / brain-average SAR studies ######
lin_bin <- dosresmeta(formula=logrr ~ dose, type=type, id=id,
                      lb=ci_lower, ub=ci_upper, 
                      #se=se, 
                      cases=cases, n=n, covariance = "gl",  method="fixed", proc="1stage", data=data_bin[9:13,] ) 

output_file <- "/males_glioma_frTACC_lin_Restr_ModelSummary.txt"

# Open the sink connection
sink(paste0(saveto, output_file))
summary(lin_bin)
print("OR increase for every unit of ln(SAR):")
predict(lin_bin, delta=1, exp=TRUE)
sink()

dosex_bin <- data.frame(dose=seq(round(min(data_bin$dose)), 2, 0.1))

obj <-  summary(lin_bin)

#plot pdf
pdf(paste0(saveto, "/males_glioma_frTACC_lin_Restr.pdf"), 7,4)


with(predict(lin_bin, dosex_bin, order=TRUE, exp=TRUE), {plot(dose, pred, type="l", col="blue", log="y", ylim=c(0.3, (max(exp(data_bin[9:13,]$logrr))+0.1*(max(exp(data_bin[9:13,]$logrr))))), ylab="glioma odds ratio", xlab="SAR, W/kg", axes=FALSE)
  
  lines(dose, ci.lb, lty=2)
  
  lines(dose, ci.ub, lty=2)})
points(data_bin[9:13,]$dose, exp(data_bin[9:13,]$logrr))
axis(2)  # y-axis default (log scale)
axis(1, at=xticks_lnSAR, labels=xticks_SAR)  # x-axis with SAR values
box()

grid.text( "Dose-response meta-analysis (linear) \n of gliomas in male rats", x = 0.5, y = 0.9, gp = gpar(cex = 1.3))
grid.text( paste0("p-value = ", round(obj$Wald.test[3], 4), "\nAIC = ", round(obj$AIC, 4)), x = 0.5, y = 0.7, gp = gpar(cex = 1))

dev.off()

#plot png
png(paste0(saveto, "/males_glioma_frTACC_lin_Restr.png"), 7,4, unit = "in", res= 300)

with(predict(lin_bin, dosex_bin, order=TRUE, exp=TRUE), {plot(dose, pred, type="l", col="blue", log="y", ylim=c(0.3, (max(exp(data_bin[9:13,]$logrr))+0.1*(max(exp(data_bin[9:13,]$logrr))))), ylab="glioma odds ratio", xlab="SAR, W/kg", axes=FALSE)
  
  lines(dose, ci.lb, lty=2)
  
  lines(dose, ci.ub, lty=2)})
points(data_bin[9:13,]$dose, exp(data_bin[9:13,]$logrr))
axis(2)  # y-axis default (log scale)
axis(1, at=xticks_lnSAR, labels=xticks_SAR)  # x-axis with SAR values
box()

grid.text( "Dose-response meta-analysis (linear) \n of gliomas in male rats", x = 0.5, y = 0.9, gp = gpar(cex = 1.3))
grid.text( paste0("p-value = ", round(obj$Wald.test[3], 4), "\nAIC = ", round(obj$AIC, 4)), x = 0.5, y = 0.7, gp = gpar(cex = 1))

dev.off()


##### quadratic model - sensitivity analysis#####
###### subgroup analysis - only NTP and Falcioni ######
quad_bin <- dosresmeta(formula=logrr ~ dose + I(dose^2), id=id, type=type, 
                       #lb=ci_lower, ub=ci_upper, 
                       se=se, 
                       cases=cases, n=n, covariance = "gl", method="fixed", proc="1stage", data=data_bin[c(1:8,14:15),])


output_file <- "/males_glioma_frTACC_quad_freemov_ModelSummary.txt"

# Open the sink connection
sink(paste0(saveto, output_file))
summary(quad_bin)
sink()

dosex_bin <- data.frame(dose=seq(round(min(data_bin$dose)), 2, 0.1))

obj <-  summary(quad_bin)

#plot pdf
pdf(paste0(saveto, "/males_glioma_frTACC_quad_freemov.pdf"), 7,4)

with(predict(quad_bin, dosex_bin, order=TRUE, exp=TRUE), {plot(dose, pred, type="l", col="blue", log="y", ylim=c(0.1, (max(exp(data_bin$logrr))+0.1*(max(exp(data_bin$logrr))))), ylab="glioma odds ratio", xlab="SAR, W/kg", axes=FALSE)
  
  lines(dose, ci.lb, lty=2)
  
  lines(dose, ci.ub, lty=2)})
points(data_bin[c(1:8,14:15),]$dose, exp(data_bin[c(1:8,14:15),]$logrr))
axis(2)  # y-axis default (log scale)
axis(1, at=xticks_lnSAR, labels=xticks_SAR)  # x-axis with SAR values
box()
grid.text( "Dose-response meta-analysis (quadratic) \n of gliomas in male rats", x = 0.5, y = 0.9, gp = gpar(cex = 1.3))
grid.text( paste0("p-value = ", round(obj$Wald.test[3], 4), "\n AIC = ", round(obj$AIC, 4), "\n p-value I(dose^2) = ", round(obj$coefficients[8], 4)), x = 0.5, y = 0.7, gp = gpar(cex = 1))

dev.off()

#plot png
png(paste0(saveto, "/males_glioma_frTACC_quad_freemov.png"), 7,4, unit = "in", res= 300)

with(predict(quad_bin, dosex_bin, order=TRUE, exp=TRUE), {plot(dose, pred, type="l", col="blue", log="y", ylim=c(0.1, (max(exp(data_bin$logrr))+0.1*(max(exp(data_bin$logrr))))), ylab="glioma odds ratio", xlab="SAR, W/kg", axes=FALSE)
  
  lines(dose, ci.lb, lty=2)
  
  lines(dose, ci.ub, lty=2)})
points(data_bin[c(1:8,14:15),]$dose, exp(data_bin[c(1:8,14:15),]$logrr))
axis(2)  # y-axis default (log scale)
axis(1, at=xticks_lnSAR, labels=xticks_SAR)  # x-axis with SAR values
box()
grid.text( "Dose-response meta-analysis (quadratic) \n of gliomas in male rats", x = 0.5, y = 0.9, gp = gpar(cex = 1.3))
grid.text( paste0("p-value = ", round(obj$Wald.test[3], 4), "\n AIC = ", round(obj$AIC, 4), "\n p-value I(dose^2) = ", round(obj$coefficients[8], 4)), x = 0.5, y = 0.7, gp = gpar(cex = 1))

dev.off()



###### subgroup analysis - only restraint / brain-average SAR studies ######
quad_bin <- dosresmeta(formula=logrr ~ dose + I(dose^2), id=id, type=type, 
                       #lb=ci_lower, ub=ci_upper, 
                       se=se, 
                       cases=cases, n=n, covariance = "gl", method="fixed", proc="1stage", data=data_bin[9:13,])


output_file <- "/males_glioma_frTACC_quad_Restr_ModelSummary.txt"

# Open the sink connection
sink(paste0(saveto, output_file))
summary(quad_bin)
sink()

dosex_bin <- data.frame(dose=seq(round(min(data_bin$dose)), 2, 0.1))

obj <-  summary(quad_bin)

#plot pdf
pdf(paste0(saveto, "/males_glioma_frTACC_quad_Restr.pdf"), 7,4)

with(predict(quad_bin, dosex_bin, order=TRUE, exp=TRUE), {plot(dose, pred, type="l", col="blue", log="y", ylim=c(0.05, (max(exp(data_bin[9:13,]$logrr))+0.1*(max(exp(data_bin[9:13,]$logrr))))), ylab="glioma odds ratio", xlab="SAR, W/kg", axes=FALSE)
  
  lines(dose, ci.lb, lty=2)
  
  lines(dose, ci.ub, lty=2)})
points(data_bin[9:13,]$dose, exp(data_bin[9:13,]$logrr))
axis(2)  # y-axis default (log scale)
axis(1, at=xticks_lnSAR, labels=xticks_SAR)  # x-axis with SAR values
box()
grid.text( "Dose-response meta-analysis (quadratic) \n of gliomas in male rats", x = 0.5, y = 0.9, gp = gpar(cex = 1.3))
grid.text( paste0("p-value = ", round(obj$Wald.test[3], 4), "\n AIC = ", round(obj$AIC, 4), "\n p-value I(dose^2) = ", round(obj$coefficients[8], 4)), x = 0.5, y = 0.7, gp = gpar(cex = 1))

dev.off()

#plot png
png(paste0(saveto, "/males_glioma_frTACC_quad_Restr.png"), 7,4, unit = "in", res= 300)

with(predict(quad_bin, dosex_bin, order=TRUE, exp=TRUE), {plot(dose, pred, type="l", col="blue", log="y", ylim=c(0.05, (max(exp(data_bin[9:13,]$logrr))+0.1*(max(exp(data_bin[9:13,]$logrr))))), ylab="glioma odds ratio", xlab="SAR, W/kg", axes=FALSE)
  
  lines(dose, ci.lb, lty=2)
  
  lines(dose, ci.ub, lty=2)})
points(data_bin[9:13,]$dose, exp(data_bin[9:13,]$logrr))
axis(2)  # y-axis default (log scale)
axis(1, at=xticks_lnSAR, labels=xticks_SAR)  # x-axis with SAR values
box()

grid.text( "Dose-response meta-analysis (quadratic) \n of gliomas in male rats", x = 0.5, y = 0.9, gp = gpar(cex = 1.3))
grid.text( paste0("p-value = ", round(obj$Wald.test[3], 4), "\n AIC = ", round(obj$AIC, 4), "\n p-value I(dose^2) = ", round(obj$coefficients[8], 4)), x = 0.5, y = 0.7, gp = gpar(cex = 1))
dev.off()

