# Ethnicity and Religion Classification

# CORRELATION OF TRUE VS PREDICTEd

# Load Data --------------------------------------------------------------------
data <- readRDS(file.path(final_data_file_path, "training_target_data", "training_data_011719_clean_herfgroups.Rds"))

# Herf Index -------------------------------------------------------------------
herf_index_df <- lapply(unique(data$herf_group), function(i){
  herf_index <- Herfindahl(as.numeric(table(as.numeric(as.factor(data$d10[data$herf_group == i])))))
  N <- length(data$d10[data$herf_group == i])
  
  df_out <- data.frame(id = i,
             herf_index = herf_index,
             N = N)
  
  return(df_out)
  
}) %>% bind_rows

# Figures ----------------------------------------------------------------------
herf_index_df <- herf_index_df[herf_index_df$N >= 30,]

herf_hist <- ggplot() +
  geom_histogram(data=herf_index_df, aes(herf_index),
            bins = 50, fill="dodgerblue3", color="dodgerblue4") +
  theme_minimal() +
  labs(x="", y="",
       title="") +
  theme(axis.text = element_text(size=12, color="black"),
        plot.margin = margin(0, 20, 0, 0))
  
ggsave(herf_hist, filename = file.path(figures_file_path, "herfindalh_truth_index.png"),
       height=5, width=6)


