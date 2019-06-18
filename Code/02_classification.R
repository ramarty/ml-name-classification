# Ethnicity and Religion Classification

# Parameters -------------------------------------------------------------------
TRAIN_PROP <- 0.75
HERF_N <- 50

set.seed(42)

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

predicted <- data$predict_nb1[train_test == "test"]
truth <- data[[DEP_VAR]][train_test == "test"]
herf_group <- data$herf_group[train_test == "test"]

calc_accuracy_stats <- function(predicted, truth, herf_group, name){
  
  ## Accuracy
  predicted <- predicted %>% as.character
  truth <- truth %>% as.character
  
  accuracy <- mean(predicted == truth)
  
  ## Herf
  herf_true <- lapply(unique(herf_group), function(group_i) Herfindahl(as.numeric(as.factor(truth))[herf_group == group_i])) %>% unlist
  herf_predicted <- lapply(unique(herf_group), function(group_i) Herfindahl(as.numeric(as.factor(predicted))[herf_group == group_i])) %>% unlist
  
  mean_herf_diff <- abs(herf_true - herf_predicted) %>% mean
  mean_herf_percentdiff <- mean(abs(herf_predicted) - abs(herf_true) / abs(herf_true))
  herf_correlation <- cor(herf_true, herf_predicted)
  if(is.na(herf_correlation)) herf_correlation <- 0
  
  df_out <- data.frame(accuracy = accuracy,
                       mean_herf_diff = mean_herf_diff,
                       mean_herf_percentdiff = mean_herf_percentdiff,
                       herf_correlation = herf_correlation)
  
  names(df_out) <- paste0(names(df_out), "_", name)

  return(df_out)
}

# Load Data --------------------------------------------------------------------
data <- readRDS(file.path(final_data_file_path, "training_target_data", "training_data_011719_clean.Rds"))
data$id <- 1:nrow(data)
data <- data[order(runif(nrow(data))),]

### Groups to Calculate Herf-Index
data$herf_group <- rep(1:nrow(data), each=HERF_N, length.out = nrow(data))

#lapply(unique(data$herf_group), function(i) Herfindahl(as.numeric(as.factor(data$d10[data$herf_group == i])))) %>% unlist %>% hist

### Train/Test Set
#train_test <- sample(size=nrow(data), x=c("train", "test"), prob=c(TRAIN_PROP, test=(1-TRAIN_PROP)), replace=T)
herf_ids_train_sample <- sample(x=unique(data$herf_group), 
                         size=round(length(unique(data$herf_group))*TRAIN_PROP), 
                         replace=F)
train_test <- ifelse(data$herf_group %in% herf_ids_train_sample, "train", "test")

train_test_christian <- sample(size=nrow(data[data$religion == "Christian",]), x=c("train", "test"), prob=c(TRAIN_PROP, test=(1-TRAIN_PROP)), replace=T)
train_test_muslim <- sample(size=nrow(data[data$religion == "Muslim",]), x=c("train", "test"), prob=c(TRAIN_PROP, test=(1-TRAIN_PROP)), replace=T)

#### Export Data with Random Herf Groups 
write.csv(data, file.path(final_data_file_path, "training_target_data", "training_data_011719_clean_herfgroups.csv"), row.names=F)
saveRDS(data, file.path(final_data_file_path, "training_target_data", "training_data_011719_clean_herfgroups.Rds"))

# Function to Implement Algorithm ----------------------------------------------
# 1. Develop Feature Set
# 2. Implement Models
# 3. Grab results

implement_models <- function(data, DEP_VAR, CLEAN_NAMES_METHOD, NGRAMS, TRIM_PROP_MIN, TRIM_PROP_MAX, train_test){
  
  # Create Features ------------------------------------------------------------
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
    
    # ???   dfm_v1 <- model.matrix(as.formula(paste0(" ~ . +", top_grams)), data=dfm_v1_df) %>% as.dfm
  }
  
  # Implement Models -----------------------------------------------------------
  nb1 <- textmodel_nb(x=dfm_v1[train_test == "train",], y=data[[DEP_VAR]][train_test == "train"])
  svm1 <- textmodel_svm(x=dfm_v1[train_test == "train",], y=data[[DEP_VAR]][train_test == "train"])
  nnseq1 <- textmodel_nnseq(x=dfm_v1[train_test == "train",], y=data[[DEP_VAR]][train_test == "train"])
  
  data$predict_nb1 <- predict(nb1, newdata = dfm_v1, type="class")
  data$predict_svm1 <- predict(svm1, newdata = dfm_v1, type="class")
  data$predict_nnseq1 <- predict(nnseq1, newdata = dfm_v1, type="class")
  
  # Accuracy / Results ---------------------------------------------------------
  df_results <- cbind(
    calc_accuracy_stats(data$predict_nb1[train_test == "test"], data[[DEP_VAR]][train_test == "test"], data$herf_group[train_test == "test"], "nb1_test"),
    calc_accuracy_stats(data$predict_nb1[train_test == "train"], data[[DEP_VAR]][train_test == "train"], data$herf_group[train_test == "train"], "nb1_train"),
    
    calc_accuracy_stats(data$predict_svm1[train_test == "test"], data[[DEP_VAR]][train_test == "test"], data$herf_group[train_test == "test"], "svm1_test"),
    calc_accuracy_stats(data$predict_svm1[train_test == "train"], data[[DEP_VAR]][train_test == "train"], data$herf_group[train_test == "train"], "svm1_train"),
    
    calc_accuracy_stats(data$predict_nnseq1[train_test == "test"], data[[DEP_VAR]][train_test == "test"], data$herf_group[train_test == "test"], "nnseq1_test"),
    calc_accuracy_stats(data$predict_nnseq1[train_test == "train"], data[[DEP_VAR]][train_test == "train"], data$herf_group[train_test == "train"], "nnseq1_train")
  )
  
  df_results$DEP_VAR <- DEP_VAR
  df_results$CLEAN_NAMES_METHOD <- CLEAN_NAMES_METHOD
  df_results$NGRAMS <- paste(NGRAMS, collapse=",")
  df_results$TRIM_PROP_MIN <- TRIM_PROP_MIN
  df_results$TRIM_PROP_MAX <- TRIM_PROP_MAX
  df_results$train_N <- sum(train_test == "train")
  df_results$test_N <- sum(train_test == "test")
  
  return(df_results)
}

# Implement Algorithms ---------------------------------------------------------
results_all <- data.frame(NULL)

#for(CLEAN_NAMES_METHOD in c("startend_cap", "start_cap", "lower")){
#  for(NGRAMS in c("2","3","4","5","6","2,3","3,4","2,3,4","2,3,4,5","2,3,4,5,6","3,4,5","3,4,5,6")){
#    for(SUB_SAMPLE_str in c("All")){ # "Christian", "Muslim", TODO: adapat herf_index for subgroups 
#      for(TRIM_PROP_MIN in c(0.001, 0.01, 0.02)){
#        for(TRIM_PROP_MAX in c(0.9)){
#          for(DEP_VAR in c("d1","d3","d5","d7","d10","religion")){

for(CLEAN_NAMES_METHOD in c("lower", "startend_cap")){
  for(NGRAMS in c("1", "2","3","4","5","6","2,3","3,4","2,3,4","2,3,4,5","2,3,4,5,6","3,4,5","3,4,5,6")){
    for(SUB_SAMPLE_str in c("All")){ # "Christian", "Muslim", TODO: adapat herf_index for subgroups 
      for(TRIM_PROP_MIN in c(0.001, 0.01, 0.02)){
        for(TRIM_PROP_MAX in c(0.9)){
          for(DEP_VAR in c("sr_ethnicity")){ # "d1","d3","d5","d7","d10","religion"
          
            print(paste(DEP_VAR, CLEAN_NAMES_METHOD, NGRAMS, SUB_SAMPLE_str, TRIM_PROP_MIN, TRIM_PROP_MAX, sep=", "))
            
            # Configure Samples --------------------------------------------------
            # If dep var is religion and sub-sample is not all, skip
            if((DEP_VAR == "religion") & SUB_SAMPLE_str != "All") next
            
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
            results_i <- implement_models(
              data[data$religion %in% SUB_SAMPLE,], 
              DEP_VAR = DEP_VAR, 
              CLEAN_NAMES_METHOD = CLEAN_NAMES_METHOD,
              NGRAMS = as.numeric(unlist(strsplit(NGRAMS, ","))),
              TRIM_PROP_MIN = TRIM_PROP_MIN,
              TRIM_PROP_MAX = TRIM_PROP_MAX,
              train_test = train_test_use)
            
            results_i$SUB_SAMPLE <- SUB_SAMPLE_str
            results_i$TRAIN_PROP <- TRAIN_PROP
            
            results_all <- bind_rows(results_all, results_i)
          
          }
        }
        
        # Export temporary files
        time <- Sys.time() %>% str_replace_all("-|:| ", "")
        write.csv(results_all, file.path(final_data_file_path, "results", paste0("results_table_",time,".csv")), row.names=F)
        
      }
    }
  }
}

# Export -----------------------------------------------------------------------
#write.csv(results_all, file.path(final_data_file_path, "results", "results_table.csv"), row.names=F)


