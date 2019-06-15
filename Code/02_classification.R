# Ethnicity and Religion Classification

# Parameters -------------------------------------------------------------------
TRAIN_PROP <- 0.75

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

calc_accuracy_stats <- function(predicted, truth){
  predicted <- predicted %>% as.character
  truth <- truth %>% as.character
  
  out <- mean(predicted == truth)
  return(out)
}

# Load Data --------------------------------------------------------------------
data <- readRDS(file.path(final_data_file_path, "training_target_data", "training_data_011719_clean.Rds"))

### Train/Test Set
train_test <- sample(size=nrow(data), x=c("train", "test"), prob=c(TRAIN_PROP, test=(1-TRAIN_PROP)), replace=T)
train_test_christian <- sample(size=nrow(data[data$religion == "Christian",]), x=c("train", "test"), prob=c(TRAIN_PROP, test=(1-TRAIN_PROP)), replace=T)
train_test_muslim <- sample(size=nrow(data[data$religion == "Muslim",]), x=c("train", "test"), prob=c(TRAIN_PROP, test=(1-TRAIN_PROP)), replace=T)

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
  #nnseq1 <- textmodel_nnseq(x=dfm_v1[train_test == "train",], y=data[[DEP_VAR]][train_test == "train"])
  
  data$predict_nb1 <- predict(nb1, newdata = dfm_v1, type="class")
  data$predict_svm1 <- predict(svm1, newdata = dfm_v1, type="class")
  #data$predict_nnseq1 <- predict(nnseq1, newdata = dfm_v1, type="class")
  
  # Accuracy / Results ---------------------------------------------------------
  df_results <- data.frame(
    nb1_outsample_accuracy = calc_accuracy_stats(data$predict_nb1[train_test == "test"], data[[DEP_VAR]][train_test == "test"]),
    nb1_insample_accuracy = calc_accuracy_stats(data$predict_nb1[train_test == "train"], data[[DEP_VAR]][train_test == "train"]),
    
    svm1_outsample_accuracy = calc_accuracy_stats(data$predict_svm1[train_test == "test"], data[[DEP_VAR]][train_test == "test"]),
    svm1_insample_accuracy = calc_accuracy_stats(data$predict_svm1[train_test == "train"], data[[DEP_VAR]][train_test == "train"]),
    
    #nnseq1_outsample_accuracy = calc_accuracy_stats(data$predict_nnseq1[train_test == "test"], data[[DEP_VAR]][train_test == "test"]),
    #nnseq1_insample_accuracy = calc_accuracy_stats(data$predict_nnseq1[train_test == "train"], data[[DEP_VAR]][train_test == "train"]),
    
    DEP_VAR = DEP_VAR,
    CLEAN_NAMES_METHOD = CLEAN_NAMES_METHOD,
    NGRAMS = paste(NGRAMS, collapse=","),
    TRIM_PROP_MIN = TRIM_PROP_MIN,
    TRIM_PROP_MAX = TRIM_PROP_MAX,
    train_N = sum(train_test == "train"),
    test_N = sum(train_test == "test")
  )

  return(df_results)
}

# Implement Algorithms ---------------------------------------------------------
results_all <- data.frame(NULL)

for(CLEAN_NAMES_METHOD in c("lower")){
  for(NGRAMS in c("2,3,4,5,6")){
    for(SUB_SAMPLE_str in c("All")){
      for(TRIM_PROP_MIN in c(0.001)){
        for(TRIM_PROP_MAX in c(0.9)){
          for(DEP_VAR in c("d10", "religion")){
          
            print(paste(DEP_VAR, CLEAN_NAMES_METHOD, NGRAMS, SUB_SAMPLE_str, TRIM_PROP_MIN, TRIM_PROP_MAX, sep=", "))
            
            # Configure Samples --------------------------------------------------
            # Subsets only relevant for ethnicity classification
            if(DEP_VAR != "religion") SUB_SAMPLE_str <- "All"
            
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
              NGRAMS = as.numeric(strsplit(NGRAMS,",")[[1]]),
              TRIM_PROP_MIN = TRIM_PROP_MIN,
              TRIM_PROP_MAX = TRIM_PROP_MAX,
              train_test = train_test_use)
            results_i$SUB_SAMPLE <- SUB_SAMPLE_str
            results_i$TRAIN_PROP <- TRAIN_PROP
            
            results_all <- bind_rows(results_all, results_i)
          
          }
        }
      }
    }
  }
}

# Export -----------------------------------------------------------------------
write.csv(results_all, file.path(final_data_file_path, "results", "results_table.csv"), row.names=F)
