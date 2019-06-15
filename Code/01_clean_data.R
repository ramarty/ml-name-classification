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
write.csv(data_training, file.path(final_data_file_path, "training_target_data", "training_data_011719_clean.csv"), row.names=F)
saveRDS(data_training, file.path(final_data_file_path, "training_target_data", "training_data_011719_clean.Rds"))

write.csv(data_target, file.path(final_data_file_path, "training_target_data", "target_data_011719_clean.csv"), row.names=F)
saveRDS(data_target, file.path(final_data_file_path, "training_target_data", "target_data_011719_clean.Rds"))

