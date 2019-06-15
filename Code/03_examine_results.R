# Ethnicity and Religion Classification

# Load Data --------------------------------------------------------------------
results_all <- read.csv(file.path(final_data_file_path, "results", "results_table.csv"))


# Examine Results --------------------------------------------------------------
#### Ethnicity
# Global Results
results_ethnicity_all <- results_ethnicity[results_ethnicity$group == "all" & results_ethnicity$SUB_SAMPLE == "Christian,Muslim",] %>% dplyr::select(prop_correct, prop_keep, NGRAMS, CLEAN_NAMES_METHOD, prob_restrict)
results_ethnicity_christian <- results_ethnicity[results_ethnicity$group == "all" & results_ethnicity$SUB_SAMPLE == "Christian",] %>% dplyr::select(prop_correct, prop_keep, NGRAMS, CLEAN_NAMES_METHOD, prob_restrict)
results_ethnicity_muslim <- results_ethnicity[results_ethnicity$group == "all" & results_ethnicity$SUB_SAMPLE == "Muslim",] %>% dplyr::select(prop_correct, prop_keep, NGRAMS, CLEAN_NAMES_METHOD, prob_restrict)

results_ethnicity_all_0 <- results_ethnicity_all[results_ethnicity_all$prob_restrict == 0,]
results_ethnicity_christian_0 <- results_ethnicity_christian[results_ethnicity_christian$prob_restrict == 0,]
results_ethnicity_muslim_0 <- results_ethnicity_muslim[results_ethnicity_muslim$prob_restrict == 0,]

results_ethnicity_all_75 <- results_ethnicity_all[results_ethnicity_all$prob_restrict == 0.75,]
results_ethnicity_christian_75 <- results_ethnicity_christian[results_ethnicity_christian$prob_restrict == .75,]
results_ethnicity_muslim_75 <- results_ethnicity_muslim[results_ethnicity_muslim$prob_restrict == 0.75,]

# By Group Results with Optimal Global Parameters
results_ethnicity_bygroup <- results_ethnicity[results_ethnicity$NGRAMS == "2,3,4" & results_ethnicity$CLEAN_NAMES_METHOD == "startend_cap" & results_ethnicity$prob_restrict == 0.75,]
results_ethnicity_bygroup_christian <- results_ethnicity[results_ethnicity$NGRAMS == "2,3,4" & results_ethnicity$CLEAN_NAMES_METHOD == "start_cap" & results_ethnicity$prob_restrict == 0 & results_ethnicity$SUB_SAMPLE == "Christian",]
results_ethnicity_bygroup_muslim <- results_ethnicity[results_ethnicity$NGRAMS == "2,3,4" & results_ethnicity$CLEAN_NAMES_METHOD == "start_cap" & results_ethnicity$prob_restrict == 0 & results_ethnicity$SUB_SAMPLE == "Muslim",]

#### Relgion
# Global Results
results_religion_all <- results_religion[results_religion$group == "all",] %>% dplyr::select(prop_correct, prop_keep, NGRAMS, CLEAN_NAMES_METHOD, prob_restrict)
results_religion_all_0 <- results_religion_all[results_religion_all$prob_restrict == 0,]

# Add Results to Data ----------------------------------------------------------
data <- read.csv(file.path(project_file_path, "data", "training_data_011719.csv"))
data_target <- read.csv(file.path(project_file_path, "data", "target_data_011719.csv"))

train_test <- sample(nrow(data), x=c("train","test"),prob=c(0.75,0.25),replace=T)
train_test <- c(train_test, rep("neither", nrow(data_target)))

# Clean Dependent Variable
data$d10 <- data$d10 %>% iconv("WINDOWS-1252","UTF-8") %>% tolower
data$name <- data$name %>% iconv("WINDOWS-1252","UTF-8") %>% tolower
data_target$name <- data_target$name %>% iconv("WINDOWS-1252","UTF-8") %>% tolower

CLEAN_NAMES_METHOD <- "start_cap"
NGRAMS <- c(2,3,4)
TRIM_PROP_MIN <- 0.0001
TRIM_PROP_MAX <- 0.99

if(CLEAN_NAMES_METHOD == "startend_cap"){
  data$name_clean <- lapply(data$name, capitalize_firstlast_charword) %>% unlist
  data_target$name_clean <- lapply(data_target$name, capitalize_firstlast_charword) %>% unlist
}

if(CLEAN_NAMES_METHOD == "start_cap"){
  data$name_clean <- tools::toTitleCase(data$name)
  data_target$name_clean <- tools::toTitleCase(data_target$name)
}

if(CLEAN_NAMES_METHOD == "lower"){
  data$name_clean <- data$name
  data_target$name_clean <- data_target$name
}

tokens_v1 <- tokens(x=gsub("\\s", "_", c(data$name_clean, data_target$name_clean)), what="character", ngrams=NGRAMS, conc="")

dfm_v1 <- dfm(tokens_v1, tolower=F) %>%
  dfm_trim(min_docfreq=TRIM_PROP_MIN, docfreq_type = "prop")  %>%
  dfm_trim(max_docfreq=TRIM_PROP_MAX, docfreq_type = "prop") 

# Predict d10
nb_d10_model <- textmodel_nb(x=dfm_v1[train_test == "train",], y=data$d10[train_test == "train"])
data$d10_predict <- predict(nb_d10_model, newdata = dfm_v1[train_test != "neither",], type="class")
d10_predict_prob_df <- predict(nb_d10_model, newdata = dfm_v1[train_test != "neither",], type="probability") %>% as.data.frame
data$d10_predict_prob <- apply(d10_predict_prob_df, 1, max) %>% as.numeric

# Predict religion
nb_relgion_model <- textmodel_nb(x=dfm_v1[train_test == "train",], y=data$religion[train_test == "train"])
data$religion_predict <- predict(nb_relgion_model, newdata = dfm_v1[train_test != "neither",], type="class")
religion_predict_prob_df <- predict(nb_relgion_model, newdata = dfm_v1[train_test != "neither",], type="probability") %>% as.data.frame
data$religion_predict_prob <- apply(religion_predict_prob_df, 1, max) %>% as.numeric

# Confirm results are good
table(as.character(data$religion) == as.character(data$religion_predict)) / nrow(data)

data <- subset(data, select=-c(d10_predict_prob, religion_predict_prob))
write.csv(data, file.path(project_file_path, "data_with_predictions", "training_data_011719_withpredictions.csv"), row.names=F)

# Add Predictions to Target Data -----------------------------------------------
# Predict d10
data_target$d10_predict <- predict(nb_d10_model, newdata = dfm_v1[train_test == "neither",], type="class")
d10_predict_prob_df <- predict(nb_d10_model, newdata = dfm_v1[train_test == "neither",], type="probability") %>% as.data.frame
data_target$d10_predict_prob <- apply(d10_predict_prob_df, 1, max) %>% as.numeric

# Predict religion
data_target$religion_predict <- predict(nb_relgion_model, newdata = dfm_v1[train_test == "neither",], type="class")
religion_predict_prob_df <- predict(nb_relgion_model, newdata = dfm_v1[train_test == "neither",], type="probability") %>% as.data.frame
data_target$religion_predict_prob <- apply(religion_predict_prob_df, 1, max) %>% as.numeric

data_target <- subset(data_target, select=-c(d10_predict_prob, religion_predict_prob))
write.csv(data_target, file.path(project_file_path, "data_with_predictions", "target_data_011719_withpredictions.csv"), row.names=F)

