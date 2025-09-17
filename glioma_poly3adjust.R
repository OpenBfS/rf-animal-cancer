if (!require(tidyverse)) {install.packages("tidyverse")} #useful basic functions, data frames and plots
if (!require(readxl)) {install.packages("readxl")} #read excel files
if (!require(writexl)) {install.packages("writexl")} #write excel files
if (!require(MCPAN)) {install.packages("MCPAN")} #statistics for poly-3-adjusted quantal responses

source("config.R") # load working directory from config file
setwd(workdir)

#### Pre-processing of NTP data ####
##### Males #####
# For this analysis, we use the data provided in NTP's official repository

# download individual animal level data from NTP repository at https://cebs.niehs.nih.gov/cebs/data/publication/TR-595
dest_dir <- "./NTP-data/GSM-rats/2yr-indiv/"
dest_dir2 <- "./NTP-data/CDMA-rats/2yr-indiv/"

# Create the directory if it doesn't exist
if (!dir.exists(dest_dir)) {
  dir.create(dest_dir, recursive = TRUE)
}

if (!dir.exists(dest_dir2)) {
  dir.create(dest_dir2, recursive = TRUE)
}


url1 <- "https://cebs.niehs.nih.gov/cebs/get_file/accno/3800_7860/file/2010559_Male_Individual_Animal_Survival_Data.xls"

dest_file1 <- file.path(dest_dir, "2010559_Male_Individual_Animal_Survival_Data.xls")

url2 <- "https://cebs.niehs.nih.gov/cebs/get_file/accno/3800_7860/file/2010559_Male_Individual_Animal_Neoplastic_Pathology_Data.xls"
dest_file2 <- file.path(dest_dir, "2010559_Male_Individual_Animal_Neoplastic_Pathology_Data.xls")

url3 <- "https://cebs.niehs.nih.gov/cebs/get_file/accno/3800_7854/file/2010556_Male_Individual_Animal_Survival_Data.xls"
dest_file3 <- file.path(dest_dir2, "2010556_Male_Individual_Animal_Survival_Data.xls")


url4 <- "https://cebs.niehs.nih.gov/cebs/get_file/accno/3800_7854/file/2010556_Male_Individual_Animal_Neoplastic_Pathology_Data.xls"
dest_file4 <- file.path(dest_dir2, "2010556_Male_Individual_Animal_Neoplastic_Pathology_Data.xls")

urls <- c(url1, url2, url3, url4)
dest_files <- c(dest_file1, dest_file2, dest_file3, dest_file4)

# if files do not exist yet -> download
for (i in seq_along(urls)){
  if (!file.exists(dest_files[i])) {
    download.file(urls[i], destfile = dest_files[i], mode = "wb")
    cat("File downloaded to:", dest_files[i], "\n")
  } else {
    cat("File already exists:", dest_files[i], "\n")
  }
}


# !!!!! Open the files in Excel and save them as .xlsx !!!!!


###### GSM rats, males, gliomas  ######
# Specify the paths to your Excel files
file1_path <- "./NTP-data/GSM-rats/2yr-indiv/2010559_Male_Individual_Animal_Survival_Data.xlsx"
file2_path <- "./NTP-data/GSM-rats/2yr-indiv/2010559_Male_Individual_Animal_Neoplastic_Pathology_Data.xlsx"


# Import the data from each Excel file into dataframes
df1 <- read_excel(file1_path, sheet = 1, skip = 15) # Adjust 'sheet' if necessary
df2 <- read_excel(file2_path, sheet = 1, skip = 15) # Adjust 'sheet' if necessary

# Select the required columns from the second dataframe
# Include only the columns that are present in both dataframes,
# plus the extra columns we want to add from the second file.
extra_columns <- c("System", "Organ", "Site", "Number Examined", "Lesion Name")
df2_selected <- df2[, c("Animal Number", extra_columns)]

# Merge the dataframes on the "Animal Number" column, using a left join
merged_df <- merge(df1, df2_selected, by = "Animal Number", all.x = TRUE)

# Convert "Vehicle Control" in the "Treatment" column to "0" in the merged_df
merged_df <- merged_df %>%
  mutate(Treatment = ifelse(Treatment == "Vehicle Control", "0", Treatment),
         Treatment = factor(Treatment, levels = c("0", "1.5", "3", "6")))


# Remove rows where "Removal Reason" is "Scheduled Sacrifice" which represents animals removed for the 14w interim analysis
filtered_df <- merged_df %>%
  filter(`Removal Reason` != "Scheduled Sacrifice")

# Create a new variable "tumor" with conditions
filtered_df <- filtered_df %>%
  mutate(tumor = ifelse(Organ == "Brain" & grepl("Glioma Malignant", `Lesion Name`), 1, 0))

# Remove replicate rows for the same animals, but ensure that rows with tumor = 1 are prioritized
survival_df <- filtered_df %>%
  arrange(`Animal Number`, desc(tumor)) %>%  # Sort by "Animal Number" and prioritize rows with tumor = 1
  group_by(`Animal Number`) %>%              # Group by "Animal Number"
  slice(1) %>%                               # Keep the first row in each group
  ungroup()                                  # Ungroup to return a regular dataframe
survival_gsm <- survival_df


###### CDMA rats, males, brain gliomas ######

# Specify the paths to your Excel files
file1_path <- "./NTP-data/CDMA-rats/2yr-indiv/2010556_Male_Individual_Animal_Survival_Data.xlsx"
file2_path <- "./NTP-data/CDMA-rats/2yr-indiv/2010556_Male_Individual_Animal_Neoplastic_Pathology_Data.xlsx"

# Import the data from each Excel file into dataframes
df1 <- read_excel(file1_path, sheet = 1, skip = 15) # Adjust 'sheet' if necessary
df2 <- read_excel(file2_path, sheet = 1, skip = 15) # Adjust 'sheet' if necessary

# Select the required columns from the second dataframe
# Include only the columns that are present in both dataframes,
# plus the extra columns you want to add from the second file.
extra_columns <- c("System", "Organ", "Site", "Number Examined", "Lesion Name")
df2_selected <- df2[, c("Animal Number", extra_columns)]

# Merge the dataframes on the "Animal Number" column, using a left join
merged_df <- merge(df1, df2_selected, by = "Animal Number", all.x = TRUE)

# Convert "Vehicle Control" in the "Treatment" column to "0" in the merged_df
merged_df <- merged_df %>%
  mutate(Treatment = ifelse(Treatment == "Vehicle Control", "0", Treatment),
         Treatment = factor(Treatment, levels = c("0", "1.5", "3", "6")))


# Remove rows where "Removal Reason" is "Scheduled Sacrifice" which represents animals removed for the 14w interim analysis
filtered_df <- merged_df %>%
  filter(`Removal Reason` != "Scheduled Sacrifice")

# Create a new variable "tumor" with conditions
filtered_df <- filtered_df %>%
  mutate(tumor = ifelse(Organ == "Brain" & grepl("Glioma Malignant", `Lesion Name`), 1, 0))

# Remove replicate rows for the same animals, but ensure that rows with tumor = 1 are prioritized
survival_df <- filtered_df %>%
  arrange(`Animal Number`, desc(tumor)) %>%  # Sort by "Animal Number" and prioritize rows with tumor = 1
  group_by(`Animal Number`) %>%              # Group by "Animal Number"
  slice(1) %>%                               # Keep the first row in each group
  ungroup()                                  # Ungroup to return a regular dataframe
survival_cdma <- survival_df



###### GSM and CDMA combined ######
# combine GSM an CDMA data frames into a single df
survival_df_male <- survival_gsm %>% filter(Treatment != 0) %>% rbind(survival_cdma)

###### Poly-3 Adjustment of NTP animal numbers ######

# pairwise poly-k-test, no Rao-Scott litter adjustment, as litter data not available

# Create a results directory if it doesn't exist
if (!dir.exists("res")) {
  dir.create("res", recursive = TRUE)
}

output_file <- "res/all-male_glioma_poly3add1_formeta.txt"


# Open the sink connection
sink(output_file)

# Any command output following this will be written to the file
# Example function calls
print("poly3test: Dunnett & ADD1 in GSM+CDMA male rats - glioma incidence")
ntp_595male_pw <- poly3test(time=survival_df_male$`Days on Study`, status=survival_df_male$tumor,
                            f=as.factor(survival_df_male$Treatment), type = "Dunnett", method = "ADD1",  alternative="greater" )

ntp_595male_pw

# Close the sink connection
sink()

# fetch the adjusted animal case numbers from the ntp_595male_pw object
nadj <- ntp_595male_pw$sample.estimate$nadj
nadj_table <- data.frame("0" = nadj[1],
                         "1.5" = nadj[2],
                         "3" = nadj[3],
                         "6" = nadj[4])
colnames(nadj_table) <- c("0 W/kg", "1.5 W/kg", "3 W/kg", "6 W/kg")
# Display the table of poly-3 adjusted animal numbes per exposure group
print(nadj_table)

### !!!! ---> Use these numbers to fill data for the Excel table: 
# data_male_glioma_poly3adj_alldoses.xlsx


##### Females #####
# For this analysis, we use the data provided in NTP's official repository

# download individual animal level data from NTP repository at https://cebs.niehs.nih.gov/cebs/data/publication/TR-595
dest_dir <- "./NTP-data/GSM-rats/2yr-indiv/"
dest_dir2 <- "./NTP-data/CDMA-rats/2yr-indiv/"

# Create the directory if it doesn't exist
if (!dir.exists(dest_dir)) {
  dir.create(dest_dir, recursive = TRUE)
}

if (!dir.exists(dest_dir2)) {
  dir.create(dest_dir2, recursive = TRUE)
}


url1 <- "https://cebs.niehs.nih.gov/cebs/get_file/accno/3800_7860/file/2010559_Female_Individual_Animal_Survival_Data.xls"

dest_file1 <- file.path(dest_dir, "2010559_Female_Individual_Animal_Survival_Data.xls")

url2 <- "https://cebs.niehs.nih.gov/cebs/get_file/accno/3800_7860/file/2010559_Female_Individual_Animal_Neoplastic_Pathology_Data.xls"
dest_file2 <- file.path(dest_dir, "2010559_Female_Individual_Animal_Neoplastic_Pathology_Data.xls")

url3 <- "https://cebs.niehs.nih.gov/cebs/get_file/accno/3800_7854/file/2010556_Female_Individual_Animal_Survival_Data.xls"
dest_file3 <- file.path(dest_dir2, "2010556_Female_Individual_Animal_Survival_Data.xls")


url4 <- "https://cebs.niehs.nih.gov/cebs/get_file/accno/3800_7854/file/2010556_Female_Individual_Animal_Neoplastic_Pathology_Data.xls"
dest_file4 <- file.path(dest_dir2, "2010556_Female_Individual_Animal_Neoplastic_Pathology_Data.xls")

urls <- c(url1, url2, url3, url4)
dest_files <- c(dest_file1, dest_file2, dest_file3, dest_file4)

# if files do not exist yet -> download
for (i in seq_along(urls)){
  if (!file.exists(dest_files[i])) {
    download.file(urls[i], destfile = dest_files[i], mode = "wb")
    cat("File downloaded to:", dest_files[i], "\n")
  } else {
    cat("File already exists:", dest_files[i], "\n")
  }
}


# !!!!! Open the files in Excel and save them as .xlsx !!!!!


###### GSM rats, females, gliomas  ######
# Specify the paths to your Excel files
file1_path <- "./NTP-data/GSM-rats/2yr-indiv/2010559_Female_Individual_Animal_Survival_Data.xlsx"
file2_path <- "./NTP-data/GSM-rats/2yr-indiv/2010559_Female_Individual_Animal_Neoplastic_Pathology_Data.xlsx"


# Import the data from each Excel file into dataframes
df1 <- read_excel(file1_path, sheet = 1, skip = 15) # Adjust 'sheet' if necessary
df2 <- read_excel(file2_path, sheet = 1, skip = 15) # Adjust 'sheet' if necessary

# Select the required columns from the second dataframe
# Include only the columns that are present in both dataframes,
# plus the extra columns we want to add from the second file.
extra_columns <- c("System", "Organ", "Site", "Number Examined", "Lesion Name")
df2_selected <- df2[, c("Animal Number", extra_columns)]

# Merge the dataframes on the "Animal Number" column, using a left join
merged_df <- merge(df1, df2_selected, by = "Animal Number", all.x = TRUE)

# Convert "Vehicle Control" in the "Treatment" column to "0" in the merged_df
merged_df <- merged_df %>%
  mutate(Treatment = ifelse(Treatment == "Vehicle Control", "0", Treatment),
         Treatment = factor(Treatment, levels = c("0", "1.5", "3", "6")))


# Remove rows where "Removal Reason" is "Scheduled Sacrifice" which represents animals removed for the 14w interim analysis
filtered_df <- merged_df %>%
  filter(`Removal Reason` != "Scheduled Sacrifice")

# Create a new variable "tumor" with conditions
filtered_df <- filtered_df %>%
  mutate(tumor = ifelse(Organ == "Brain" & grepl("Glioma Malignant", `Lesion Name`), 1, 0))

# Remove replicate rows for the same animals, but ensure that rows with tumor = 1 are prioritized
survival_df <- filtered_df %>%
  arrange(`Animal Number`, desc(tumor)) %>%  # Sort by "Animal Number" and prioritize rows with tumor = 1
  group_by(`Animal Number`) %>%              # Group by "Animal Number"
  slice(1) %>%                               # Keep the first row in each group
  ungroup()                                  # Ungroup to return a regular dataframe
survival_gsm <- survival_df


###### CDMA rats, females, brain gliomas ######

# Specify the paths to your Excel files
file1_path <- "./NTP-data/CDMA-rats/2yr-indiv/2010556_Female_Individual_Animal_Survival_Data.xlsx"
file2_path <- "./NTP-data/CDMA-rats/2yr-indiv/2010556_Female_Individual_Animal_Neoplastic_Pathology_Data.xlsx"

# Import the data from each Excel file into dataframes
df1 <- read_excel(file1_path, sheet = 1, skip = 15) # Adjust 'sheet' if necessary
df2 <- read_excel(file2_path, sheet = 1, skip = 15) # Adjust 'sheet' if necessary

# Select the required columns from the second dataframe
# Include only the columns that are present in both dataframes,
# plus the extra columns you want to add from the second file.
extra_columns <- c("System", "Organ", "Site", "Number Examined", "Lesion Name")
df2_selected <- df2[, c("Animal Number", extra_columns)]

# Merge the dataframes on the "Animal Number" column, using a left join
merged_df <- merge(df1, df2_selected, by = "Animal Number", all.x = TRUE)

# Convert "Vehicle Control" in the "Treatment" column to "0" in the merged_df
merged_df <- merged_df %>%
  mutate(Treatment = ifelse(Treatment == "Vehicle Control", "0", Treatment),
         Treatment = factor(Treatment, levels = c("0", "1.5", "3", "6")))


# Remove rows where "Removal Reason" is "Scheduled Sacrifice" which represents animals removed for the 14w interim analysis
filtered_df <- merged_df %>%
  filter(`Removal Reason` != "Scheduled Sacrifice")

# Create a new variable "tumor" with conditions
filtered_df <- filtered_df %>%
  mutate(tumor = ifelse(Organ == "Brain" & grepl("Glioma Malignant", `Lesion Name`), 1, 0))

# Remove replicate rows for the same animals, but ensure that rows with tumor = 1 are prioritized
survival_df <- filtered_df %>%
  arrange(`Animal Number`, desc(tumor)) %>%  # Sort by "Animal Number" and prioritize rows with tumor = 1
  group_by(`Animal Number`) %>%              # Group by "Animal Number"
  slice(1) %>%                               # Keep the first row in each group
  ungroup()                                  # Ungroup to return a regular dataframe
survival_cdma <- survival_df



###### GSM and CDMA combined ######
# combine GSM an CDMA data frames into a single df
survival_df_female <- survival_gsm %>% filter(Treatment != 0) %>% rbind(survival_cdma)

###### Poly-3 Adjustment of NTP animal numbers ######

# pairwise poly-k-test, no Rao-Scott litter adjustment, as litter data not available

# Create a results directory if it doesn't exist
if (!dir.exists("res")) {
  dir.create("res", recursive = TRUE)
}

output_file <- "res/all-female_glioma_poly3add1_formeta.txt"


# Open the sink connection
sink(output_file)

# Any command output following this will be written to the file
# Example function calls
print("poly3test: Dunnett & ADD1 in GSM+CDMA female rats - glioma incidence")
ntp_595female_pw <- poly3test(time=survival_df_female$`Days on Study`, status=survival_df_female$tumor,
                              f=as.factor(survival_df_female$Treatment), type = "Dunnett", method = "ADD1",  alternative="greater" )

ntp_595female_pw

# Close the sink connection
sink()

# fetch the adjusted animal case numbers from the ntp_595female_pw object
nadj <- ntp_595female_pw$sample.estimate$nadj
nadj_table <- data.frame("0" = nadj[1],
                         "1.5" = nadj[2],
                         "3" = nadj[3],
                         "6" = nadj[4])
colnames(nadj_table) <- c("0 W/kg", "1.5 W/kg", "3 W/kg", "6 W/kg")
# Display the table of poly-3 adjusted animal numbes per exposure group
print(nadj_table)

### !!!! ---> Use these numbers to fill data for the Excel table: data_female_glioma_poly3adj_alldoses.xlsx


##### Male + Female poly-3 #####

survival_df_mf <- survival_df_male %>% rbind(survival_df_female)


# Create a results directory if it doesn't exist
if (!dir.exists("res")) {
  dir.create("res", recursive = TRUE)
}

output_file <- "res/all-m+f_glioma_poly3add1_formeta.txt"


# Open the sink connection
sink(output_file)

# Any command output following this will be written to the file
# Example function calls
print("poly3test: Dunnett & ADD1 in GSM+CDMA male+female rats - glioma incidence")
ntp_595mf_pw <- poly3test(time=survival_df_mf$`Days on Study`, status=survival_df_mf$tumor,
                          f=as.factor(survival_df_mf$Treatment), type = "Dunnett", method = "ADD1",  alternative="greater" )

ntp_595mf_pw


print("poly3test: Williams-Type & ADD1 in GSM+CDMA male+female rats - glioma incidence - test for trend")
ntp_595mf_pw_williams <- poly3test(time=survival_df_mf$`Days on Study`, status=survival_df_mf$tumor,
                                   f=as.factor(survival_df_mf$Treatment), type = "Williams", method = "ADD1",  alternative="greater" )

ntp_595mf_pw_williams
# Close the sink connection
sink()

# fetch the adjusted animal case numbers from the ntp_595mf_pw object
nadj <- ntp_595mf_pw$sample.estimate$nadj
nadj_table <- data.frame("0" = nadj[1],
                         "1.5" = nadj[2],
                         "3" = nadj[3],
                         "6" = nadj[4])
colnames(nadj_table) <- c("0 W/kg", "1.5 W/kg", "3 W/kg", "6 W/kg")
# Display the table of poly-3 adjusted animal numbes per exposure group
print(nadj_table)

### !!!! ---> Use these numbers to fill data for the Excel table: 
# data_m+f_glioma_poly3adj_alldoses.xlsx

