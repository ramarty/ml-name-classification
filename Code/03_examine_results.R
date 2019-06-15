# Ethnicity and Religion Classification

# Load Data --------------------------------------------------------------------
results_all <- read.csv(file.path(final_data_file_path, "results", "results_table.csv"))

# Prep Data for Tables ---------------------------------------------------------
results_all$CLEAN_NAMES_METHOD <- results_all$CLEAN_NAMES_METHOD %>% as.character

results_all <- results_all[order(results_all$accuracy_svm1_test, decreasing=T),]

float_vars <- names(results_all)[grepl("nb|svm|nnseq", names(results_all))]

for(var in float_vars){
  results_all[[var]] <- round(results_all[[var]],4)
}

results_all <- subset(results_all, select=c(accuracy_nb1_test, accuracy_svm1_test,
                                            mean_herf_diff_nb1_test, mean_herf_diff_svm1_test,
                                            mean_herf_percentdiff_nb1_test, mean_herf_percentdiff_svm1_test,
                                            CLEAN_NAMES_METHOD, NGRAMS, TRIM_PROP_MIN, TRIM_PROP_MAX,
                                            DEP_VAR))

results_d3 <- results_all[results_all$DEP_VAR %in% "d3",]
results_d10 <- results_all[results_all$DEP_VAR %in% "d10",]
results_religion <- results_all[results_all$DEP_VAR %in% "religion",]

results_d3$DEP_VAR <- NULL
results_d10$DEP_VAR <- NULL
results_religion$DEP_VAR <- NULL

# Limit to first 10 best results (or if less than 10, keep all)
results_d3 <- results_d3[1:min(10, nrow(results_d3)),]
results_d10 <- results_d10[1:min(10, nrow(results_d10)),]
results_religion <- results_religion[1:min(10, nrow(results_religion)),]

# Remove irrelevant variables for relgioon
herf_vars <- names(results_all)[grepl("herf", names(results_all))]
for(var in herf_vars){
  results_religion[[var]] <- NULL
}

# Ethnicity Tables -------------------------------------------------------------
for(dep_var in c("d3", "d10")){
  
  results_temp <- eval(parse(text=paste0("results_",dep_var)))

  sink(file.path(tables_file_path, paste0(dep_var,"_results.tex")))
  
  cat("\\begin{tabular}{cc | cc | cc | cccc} ")
  cat("\\hline ")
  cat("\\multicolumn{2}{c |}{Accuracy} & \\multicolumn{2}{c |}{Avg Diff Herf} & \\multicolumn{2}{c}{Avg \\% Diff Herf} & \\multicolumn{4}{c}{Parameters} \\\\ ")
  cat("NB & SVM &  NB & SVM &  NB & SVM & Case & ngrams & trim min & trim max \\\\ ")
  cat("\\hline ")
  
  for(i in 1:nrow(results_temp)){
    cat(paste(results_temp[i,], collapse=" & ") %>% paste(" \\\\ "))
  }
  
  cat("\\hline")
  cat("\\end{tabular}")
  
  sink()

}

# Religion Table ---------------------------------------------------------------
sink(file.path(tables_file_path, "religion_results.tex"))

cat("\\begin{tabular}{cc | cccc} ")
cat("\\hline ")
cat("\\multicolumn{2}{c |}{Accuracy} & \\multicolumn{4}{c}{Parameters} \\\\ ")
cat("NB & SVM  & Case & ngrams & trim min & trim max \\\\ ")
cat("\\hline ")

for(i in 1:nrow(results_temp)){
  cat(paste(results_religion[i,], collapse=" & ") %>% paste(" \\\\ "))
}

cat("\\hline")
cat("\\end{tabular}")

sink()
  


