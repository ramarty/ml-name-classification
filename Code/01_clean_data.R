# Ethnicity and Religion Classification

# Load and Prep Training Data --------------------------------------------------
data_training <- read.csv(file.path(raw_data_file_path, "training_data_011719.csv"))

data_training$d10 <- data_training$d10 %>% as.character %>% iconv("WINDOWS-1252","UTF-8") %>% tolower
data_training$name <- data_training$name %>% as.character %>% iconv("WINDOWS-1252","UTF-8") %>% tolower
data_training$sr_ethnicity <- data_training$sr_ethnicity %>% as.character %>% iconv("WINDOWS-1252","UTF-8") %>% tolower

data_training <- data_training[data_training$d10 != "",]
data_training <- data_training[data_training$name != "",]

# Load and Prep Target Data ----------------------------------------------------
data_target <- read.csv(file.path(raw_data_file_path, "target_data_011719.csv"))

data_target$mtaa <- data_target$mtaa %>% as.character %>% iconv("WINDOWS-1252","UTF-8") %>% tolower
data_target$name <- data_target$name %>% as.character %>% iconv("WINDOWS-1252","UTF-8") %>% tolower
data_target$kata <- data_target$kata %>% as.character %>% iconv("WINDOWS-1252","UTF-8") %>% tolower

# Load and Prep Ethnicity Table ------------------------------------------------
eth_table <- read.csv(file.path(raw_data_file_path, "ethtable.csv")) %>% 
  select(c(-F,-Real.Name)) %>%
  dplyr::rename(sr_ethnicity = Survey.Name)
names(eth_table) <- names(eth_table) %>% tolower

for(var in names(eth_table)){
  eth_table[[var]] <- eth_table[[var]] %>% as.character %>% iconv("WINDOWS-1252","UTF-8") %>% tolower
}

## Fill in blanks
  # If d_i is blank, use d_{i-1}
for(i in 2:11){
  eth_table[[paste0("d",i)]][eth_table[[paste0("d",i)]] == ""] <- eth_table[[paste0("d",i-1)]][eth_table[[paste0("d",i)]] == ""]
}

# Merge Data with Ethnicity Table ----------------------------------------------
data_training$d10 <- NULL
data_training <- merge(data_training, eth_table, by="sr_ethnicity", all.x=T, all.y=F)
data_training <- data_training[!is.na(data_training$d1),] # TODO: Some d groups didn't match

# Export -----------------------------------------------------------------------
write.csv(data_training, file.path(final_data_file_path, "training_data_011719_clean.csv"), row.names=F)
saveRDS(data_training, file.path(final_data_file_path, "training_data_011719_clean.Rds"))

write.csv(data_target, file.path(final_data_file_path, "target_data_011719_clean.csv"), row.names=F)
saveRDS(data_target, file.path(final_data_file_path, "target_data_011719_clean.Rds"))



# SEND TO FINAL DATA





data$d10[!(data$d10 %in% eth_table$d10)] %>% table
eth_table$d10[!(eth_table$d10 %in% data$d10)] %>% table


eth_table$D10 <- eth_table$D10 %>% as.character %>% iconv("WINDOWS-1252","UTF-8") %>% tolower





# Usefu Functions --------------------------------------------------------------
capitalize_firstlast_charword <- function(s){
  s <- strsplit(s, " ")[[1]]
  s_new <- paste(toupper(substring(s, 1,1)), substring(s, 2, nchar(s)-1), toupper(substring(s, nchar(s), nchar(s))),
        sep="", collapse=" ")
  return(s_new)
}

second_highest_number <- function(x){
  x_2nd_highest <- sort(x)[length(x)-1]
  return(x_2nd_highest)
}

### Train/Test Set
train_test <- sample(size=nrow(data), x=c("train", "test"), prob=c(TRAIN_PROP, test=(1-TRAIN_PROP)), replace=T)
train_test_christian <- sample(size=nrow(data[data$religion == "Christian",]), x=c("train", "test"), prob=c(TRAIN_PROP, test=(1-TRAIN_PROP)), replace=T)
train_test_muslim <- sample(size=nrow(data[data$religion == "Muslim",]), x=c("train", "test"), prob=c(TRAIN_PROP, test=(1-TRAIN_PROP)), replace=T)

# Naive Bayes Function ---------------------------------------------------------
if(F){
DEP_VAR <- "religion"
CLEAN_NAMES_METHOD <- "lower" # startend_cap, start_cap, lower
NGRAMS <- c(2L, 3L, 4L) 
TRAIN_PROP <- 0.75
TRIM_PROP_MIN <- 0.01
TRIM_PROP_MAX <- 0.9
}

implement_nb <- function(data, DEP_VAR, CLEAN_NAMES_METHOD, NGRAMS, TRIM_PROP_MIN, TRIM_PROP_MAX, train_test){
  
  if(CLEAN_NAMES_METHOD == "startend_cap"){
    data$name_clean <- lapply(data$name, capitalize_firstlast_charword) %>% unlist
  }
  
  if(CLEAN_NAMES_METHOD == "start_cap"){
    data$name_clean <- tools::toTitleCase(data$name)
  }
  
  if(CLEAN_NAMES_METHOD == "lower"){
    data$name_clean <- data$name
  }
  
  tokens_v1 <- tokens(x=gsub("\\s", "_", data$name_clean), what="character", ngrams=NGRAMS, conc="")
  
  dfm_v1 <- dfm(tokens_v1, tolower=F) %>%
    dfm_trim(min_docfreq=TRIM_PROP_MIN, docfreq_type = "prop")  %>%
    dfm_trim(max_docfreq=TRIM_PROP_MAX, docfreq_type = "prop") 
  
  # Add interactions of top X words
  if(F){
  dfm_v1_df <- convert(dfm_v1, to = "data.frame")[,-1]
  
  top_grams <- colSums(dfm_v1_df)
  top_grams <- sort(top_grams, decreasing = T)[1:10] %>% names
  top_grams <- paste0(".*`",top_grams,"`") %>% paste(collapse=" + ")
  }
  
  dfm_v1 <- model.matrix(as.formula(paste0(" ~ . +", top_grams)), data=dfm_v1_df) %>% as.dfm
  
  nb_1 <- textmodel_nb(x=dfm_v1[train_test == "train",], y=data[[DEP_VAR]][train_test == "train"])
  
  data$predict1 <- predict(nb_1, newdata = dfm_v1, type="class")
  predict_1_prob <- predict(nb_1, newdata = dfm_v1, type="probability") %>% as.data.frame
  data$predict1_prob <- apply(predict_1_prob, 1, max) %>% as.numeric
  data$predict1_prob_secondhigh <- apply(predict_1_prob, 1, second_highest_number) %>% as.numeric
  
  # Grab Results ---------------------------------------------------------------
  data_test <- data[train_test == "test",]
  
  df_results <- lapply(c(0,.25,0.5, 0.75, 0.9, 0.95, 0.99), function(prob_restrict){
    data_sub <- data_test[data_test$predict1_prob > prob_restrict,]
    data_sub$predict1_correct <- as.character(data_sub[[DEP_VAR]]) == as.character(data_sub$predict1)
    
    df_out <- data.frame(prop_correct = mean(data_sub$predict1_correct),
                         N_sub = nrow(data_sub), 
                         N_test = nrow(data_test),
                         prop_keep = nrow(data_sub) / nrow(data_test),
                         prob_restrict = prob_restrict,
                         group = "all")
    
    data_sum <- aggregate(predict1_correct ~ predict1, data=data_sub, FUN=mean) %>%
      dplyr::rename(group = predict1) %>%
      dplyr::rename(prop_correct = predict1_correct)
    
    data_sum_N_sub <- aggregate(N ~ predict1, data=data_sub %>% mutate(N=1), FUN=sum)  %>%
      dplyr::rename(group = predict1) %>%
      dplyr::rename(N_sub = N)
    
    data_sum_N_test <- aggregate(N ~ predict1, data=data_test %>% mutate(N=1), FUN=sum) %>%
      dplyr::rename(group = predict1) %>%
      dplyr::rename(N_test = N)
    
    data_sum_all <- merge(data_sum, data_sum_N_sub, by="group", all=T)
    data_sum_all <- merge(data_sum_all, data_sum_N_test, by="group", all=T)
    data_sum_all$prop_keep <- data_sum_all$N_sub / data_sum_all$N_test
    data_sum_all$prob_restrict <- prob_restrict

    df_out <- bind_rows(df_out, data_sum_all)
    
    return(df_out)
  }) %>% bind_rows
  
  df_results$DEP_VAR <- DEP_VAR
  df_results$CLEAN_NAMES_METHOD <- CLEAN_NAMES_METHOD
  df_results$NGRAMS <- paste(NGRAMS, collapse=",")
  df_results$TRIM_PROP_MIN <- TRIM_PROP_MIN
  df_results$TRIM_PROP_MAX <- TRIM_PROP_MAX
  df_results$train_N <- sum(train_test == "train")
  df_results$test_N <- sum(train_test == "test")
  
  return(df_results)
}

# Implement Naive Bayes --------------------------------------------------------
results_ethnicity <- as.data.frame(matrix(nrow=0, ncol=0))
results_religion <- as.data.frame(matrix(nrow=0, ncol=0))

for(CLEAN_NAMES_METHOD in c("startend_cap", "start_cap", "lower")){
  for(NGRAMS in c("2","3","4","5","6","2,3","3,4","2,3,4","2,3,4,5","2,3,4,5,6","3,4,5","3,4,5,6")){
    for(SUB_SAMPLE_str in c("Christian", "Muslim", "All")){
      for(TRIM_PROP_MIN in c(0.001, 0.01, 0.02)){
        for(TRIM_PROP_MAX in c(0.9)){
          
          print(paste(CLEAN_NAMES_METHOD, NGRAMS, SUB_SAMPLE, TRIM_PROP_MIN, TRIM_PROP_MAX, sep=", "))
          
          if(SUB_SAMPLE_str == "Christian"){
            SUB_SAMPLE <- SUB_SAMPLE_str
            train_test_use <- train_test_christian
          }
          
          if(SUB_SAMPLE_str == "Muslim"){
            SUB_SAMPLE <- SUB_SAMPLE_str
            train_test_use <- train_test_muslim
          }
          
          if(SUB_SAMPLE_str == "All"){
            SUB_SAMPLE <- c("Christian", "Muslim")
            train_test_use <- train_test
          }
        
          # Ethnicity Results
          results_ethnicity_i <- implement_nb(
                       data[data$religion %in% SUB_SAMPLE,], 
                       DEP_VAR = "d10", 
                       CLEAN_NAMES_METHOD = CLEAN_NAMES_METHOD,
                       NGRAMS = as.numeric(strsplit(NGRAMS,",")[[1]]),
                       TRIM_PROP_MIN = TRIM_PROP_MIN,
                       TRIM_PROP_MAX = TRIM_PROP_MAX,
                       train_test = train_test_use)
          results_ethnicity_i$SUB_SAMPLE <- paste(SUB_SAMPLE, collapse=",")
          results_ethnicity <- bind_rows(results_ethnicity, results_ethnicity_i)
        
          # Religion Results
            # Only run on full sample
          if(SUB_SAMPLE == "All"){
            results_religion_i <- implement_nb(data, 
                                               DEP_VAR = "religion", 
                                               CLEAN_NAMES_METHOD = CLEAN_NAMES_METHOD,
                                               NGRAMS = as.numeric(strsplit(NGRAMS,",")[[1]]),
                                               TRIM_PROP_MIN = TRIM_PROP_MIN,
                                               TRIM_PROP_MAX = TRIM_PROP_MAX,
                                               train_test = train_test)
            results_religion <- bind_rows(results_religion, results_religion_i)
          }
        
        }
      }
    }
  }
}


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

