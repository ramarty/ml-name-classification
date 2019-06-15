# Names to Ethnicity Classification
# Master File

# Filepaths --------------------------------------------------------------------
if(Sys.info()["user"] == "WB521633") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/Side Work/Names to Ethnicity Classification"
if(Sys.info()["user"] == "robmarty") project_file_path <- "~/Dropbox/World Bank/Side Work/Names to Ethnicity Classification"

raw_data_file_path <- file.path(project_file_path, "data", "rawdata")
final_data_file_path <- file.path(project_file_path, "data", "finaldata")
figures_file_path <- file.path(project_file_path, "Results", "figures")
tables_file_path <- file.path(project_file_path, "Results", "tables")

# Packages ---------------------------------------------------------------------
library(dplyr)
library(quanteda)
library(tm)

