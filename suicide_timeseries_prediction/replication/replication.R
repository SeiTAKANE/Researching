#Author's environment
"""
R version 4.3.2 (2023-10-31)
Platform: aarch64-apple-darwin20 (64-bit)
Running under: macOS Ventura 13.5

Matrix products: default
BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
LAPACK: /Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.11.0

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

time zone: Asia/Tokyo
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     
 

other attached packages:
[1] patchwork_1.2.0     bsts_0.9.10         xts_0.13.2          BoomSpikeSlab_1.2.6 Boom_0.9.15        
[6] gridExtra_2.3       vars_1.6-0          lmtest_0.9-40       strucchange_1.5-3   sandwich_3.1-0     
[11] zoo_1.8-12          MASS_7.3-60         tseries_0.10-55     urca_1.3-3          KFAS_1.5.1         
[16] forecast_8.21.1     lubridate_1.9.3     forcats_1.0.0       stringr_1.5.1       dplyr_1.1.4        
[21] purrr_1.0.2         readr_2.1.5         tidyr_1.3.0         tibble_3.2.1        ggplot2_3.4.4      
[26] tidyverse_2.0.0    

loaded via a namespace (and not attached):
 [1] utf8_1.2.4        generics_0.1.3    stringi_1.8.3     lattice_0.21-9    hms_1.1.3        
 [6] digest_0.6.34     magrittr_2.0.3    evaluate_0.23     grid_4.3.2        timechange_0.2.0 
[11] fastmap_1.1.1     nnet_7.3-19       fansi_1.0.6       scales_1.3.0      cli_3.6.2        
[16] rlang_1.1.3       munsell_0.5.0     withr_2.5.2       yaml_2.3.8        tools_4.3.2      
[21] parallel_4.3.2    tzdb_0.4.0        colorspace_2.1-0  pacman_0.5.1      curl_5.2.0       
[26] vctrs_0.6.5       R6_2.5.1          lifecycle_1.0.4   pkgconfig_2.0.3   pillar_1.9.0     
[31] gtable_0.3.4      quantmod_0.4.26   glue_1.7.0        Rcpp_1.0.12       xfun_0.41        
[36] tidyselect_1.2.0  rstudioapi_0.15.0 knitr_1.45        nlme_3.1-163      htmltools_0.5.7  
[41] rmarkdown_2.25    timeDate_4032.109 fracdiff_1.5-3    compiler_4.3.2    quadprog_1.5-8   
[46] TTR_0.24.4  
"""


#set up package
#install.packages("pacman")
pacman::p_load(tidyverse,forecast,forecast,KFAS,urca,tseries,vars,gridExtra,bsts,patchwork)

#set seed
set.seed(42)

# pre processing ----------------------------------------------------------
#load data
#US case
df_main_us <- read.csv("data/df_main_us.csv")
df_main_us$term <- as.Date(df_main_us$term)


#JP case
df_main_jp <- read.csv("data/df_main_jp.csv")
df_main_jp$term <- as.Date(df_main_jp$term)

#colname transform japanese to english
df_query_en_jp <- read.csv("data/query_en_jp.csv")
df_query_en_jp$query_en <- paste0('query_', df_query_en_jp$query_en)
df_query_en_jp$query_jp <- paste0('query_', df_query_en_jp$query_jp)

#create map
query_jp_en_map <- list(
  "query_自殺" = "query_suicide",
  "query_鬱" = "query_depression",
  "query_虐待" = "query_abuse",
  "query_自殺方法" = "query_suicide_methods", 
  "query_離婚" = "query_divorce",
  "query_不安" = "query_anxiety",
  "query_失業" = "query_unemployment",
  "query_双極性障害" = "query_bipolar_disorder",
  "query_鬱病" = "query_major_depression",
  "query_孤独" = "query_loneliness",
  "query_不眠" = "query_insomnia",
  "query_結婚" = "query_marriage",
  "query_失恋" = "query_relationship_breakup",
  "query_癌" = "query_cancer",
  "query_頭痛" = "query_headache",
  "query_自殺_どうやって" = "query_how_to_commit_suicide",
  "query_喘息" = "query_asthma",
  "query_不安障害" = "query_anxiety_disorder",
  "query_抗鬱剤" = "query_antidepressant",
  "query_社会福祉" = "query_social_welfare",
  "query_アレルギー" = "query_allergy",
  "query_痛み" = "query_pain",
  "query_統合失調症" = "query_schizophrenia",
  "query_ストレス" = "query_stress",
  "query_酒" = "query_alcohol",
  "query_若者_自殺" = "query_teen_suicide",
  "query_自殺_助け" = "query_suicide_help",
  "query_仕事" = "query_job",
  "query_自殺願望" = "query_suicide_ideation",
  "query_自殺_ホットライン" = "query_suicide_hotline",
  "query_飲み過ぎ" = "query_drunkenness",
  "query_首吊り" = "query_hanging",
  "query_違法薬物" = "query_illicit_drugs",
  "query_自殺未遂" = "query_suicide_attempt", 
  "query_自殺_考え" = "query_suicidal_thoughts",
  "query_硫化水素" = "query_hydrogen_sulfide",
  "query_株式市場" = "query_stock_market",
  "query_DV" = "query_domestic_violence",
  "query_睡眠薬" = "query_hypnotics",
  "query_練炭" = "query_charcoal_burning",
  "query_慢性疾患" = "query_chronic_illness",
  "query_自殺_予防" = "query_suicide_prevention",
  "query_訴訟" = "query_lawsuit",
  "query_禁酒" = "query_alcohol_abstinence",
  "query_信仰" = "query_religious_belief",
  "query_躁鬱病" = "query_manic_depression"
)




#transform
jp_column_names <- names(df_main_jp)[names(df_main_jp) %in% names(query_jp_en_map)]
en_column_names <- unlist(query_jp_en_map[jp_column_names])

# Create a named vector for easier mapping
name_map <- setNames(en_column_names, jp_column_names)

# Replace the Japanese column names with English ones
names(df_main_jp)[names(df_main_jp) %in% jp_column_names] <- name_map[names(df_main_jp)[names(df_main_jp) %in% jp_column_names]]

# the function to return a vector of scaled values (Max-min)
scale_max_min_vector <- function(df, column_name, cutoff_date) {
  # Filter data to include only entries up to the specified cutoff date.
  filtered_df <- df %>% filter(term <= cutoff_date)
  
  # Calculate the minimum and maximum values for the specified column within the filtered dataset.
  min_val <- min(filtered_df[[column_name]], na.rm = TRUE)
  max_val <- max(filtered_df[[column_name]], na.rm = TRUE)
  
  # Apply Max-Min scaling and return the scaled values as a vector.
  scaled_values <- df[[column_name]] %>%
    sapply(function(x) if (!is.na(x)) ((x - min_val) / (max_val - min_val)) * 100 else NA)
  
  return(scaled_values)
}


#the function to inverse scaling
scale_max_min_vector_inverse <- function(scaled_values, min_val, max_val) {
  original_values <- sapply(scaled_values, function(x) if (!is.na(x)) (x / 100) * (max_val - min_val) + min_val else NA)
  return(original_values)
}

#colnames want to scale
scale_column_names_us <- names(df_main_us)[which(names(df_main_us) == "query_suicide"):which(names(df_main_us) == "suicide_rate_female")]
scale_column_names_jp <- names(df_main_jp)[which(names(df_main_jp) == "query_suicide"):which(names(df_main_jp) == "suicide_rate_female")]

# Loop through each specified column, scale it, and add the scaled values as a new column in the dataframe.
#US
for(column_name in scale_column_names_us){
  scaled_vector <- scale_max_min_vector(df_main_us, column_name, "2016-12-01")
  new_column_name <- paste(column_name, "scaled", sep = "_")
  df_main_us[[new_column_name]] <- scaled_vector
}

#JP
for(column_name in scale_column_names_jp){
  scaled_vector <- scale_max_min_vector(df_main_jp, column_name, "2016-12-01")
  new_column_name <- paste(column_name, "scaled", sep = "_")
  df_main_jp[[new_column_name]] <- scaled_vector
}

#ceate data frame for model
#US
train_for_bsts_us_suicide <- df_main_us %>%
  filter(term <= "2016-12-01") %>% 
  dplyr::select(suicide_rate_total_scaled, query_suicide_scaled:query_religious_belief_scaled)


test_for_bsts_us_suicide<- df_main_us %>%
  dplyr::select(suicide_rate_total_scaled,query_suicide_scaled:query_religious_belief_scaled)


test_for_bsts_us_suicide$suicide_rate_total_scaled[(nrow(test_for_bsts_us_suicide)-36+1):nrow(test_for_bsts_us_suicide)] <- NA
#JP
train_for_bsts_jp_suicide <- df_main_jp %>%
  filter(term <= "2016-12-01") %>% 
  dplyr::select(suicide_rate_total_scaled, query_suicide_scaled:query_major_depression_scaled)



names(df_main_jp)
test_for_bsts_jp_suicide <- df_main_jp %>%
  dplyr::select(suicide_rate_total_scaled,query_suicide_scaled:query_major_depression_scaled)


test_for_bsts_jp_suicide$suicide_rate_total_scaled[(nrow(test_for_bsts_jp_suicide)-36+1):nrow(test_for_bsts_jp_suicide)] <- NA



#create data frame for validation
#US
val_us_suicide <- df_main_us$suicide_rate_total[df_main_us$term >= "2017-01-01" & df_main_us$term <= "2017-12-01" ]
val_jp_suicide <- df_main_jp$suicide_rate_total[df_main_jp$term >= "2017-01-01" & df_main_jp$term <= "2017-12-01" ]



#JP
#create data frame for test
#US
test_us_suicide <- data.frame(term = df_main_us$term[df_main_us$term >= "2018-01-01"],
                              pop = df_main_us$pop_total[df_main_us$term >= "2018-01-01"],
                              true_rate = df_main_us$suicide_rate_total[df_main_us$term >= "2018-01-01"],
                              true_num = df_main_us$num_suicide_total[df_main_us$term >= "2018-01-01"])



#JP
test_jp_suicide <- data.frame(term = df_main_jp$term[df_main_jp$term >= "2018-01-01"],
                              pop = df_main_jp$pop_total[df_main_jp$term >= "2018-01-01"],
                              true_rate = df_main_jp$suicide_rate_total[df_main_jp$term >= "2018-01-01"],
                              true_num = df_main_jp$num_suicide_total[df_main_jp$term >= "2018-01-01"])



# descriptive statistics --------------------------------------------------
p_trend_us <- ggplot(df_main_us, aes(x = term, y = suicide_rate_total)) +
  geom_line() +
  theme_minimal() +
  labs(title = "US Suicide Rate Over Time (Jan 2004 - Dec2019)",
       x = "Term (Jan 2004 - Dec2019)",
       y = "Suicide Rate") +
  scale_y_continuous(limits = c(0,NA)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot(p_trend_us)

p_trend_jp <- ggplot(df_main_jp, aes(x = term, y = suicide_rate_total)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Japan Suicide Rate Over Time (Jan 2008 - Dec2019)",
       x = "Term (Jan 2008 - Dec2019)",
       y = "Suicide Rate",
       caption = "Source: US Data") +
  scale_y_continuous(limits = c(0,NA)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot(p_trend_jp)


# model selection (US) ---------------------------------------------------------
# model (1) ----------------------------------------------------------------
#add local level
ss_us_suicide_trend_seas <- AddLocalLevel(state.specification=list(), train_for_bsts_us_suicide$suicide_rate_total_scaled)
#add seasonality
ss_us_suicide_trend_seas <- AddSeasonal(ss_us_suicide_trend_seas, train_for_bsts_us_suicide$suicide_rate_total_scaled, nseasons=12)
# Define grid search parameters
iterations <- c(2000,4000,8000,16000)
burn_in_percentages <- c(0.1,0.20,0.30,0.40)
# Create a dataframe to store results
model_selection_us_model_1 <- data.frame()
# Perform grid search
for (iter in iterations) {
  for (burn_percent in burn_in_percentages) {
    # Calculate burn-in period
    burn <- round(iter * burn_percent)
    
    # Fit the model
    model_bsts_us_suicide_trend_seas <- bsts(train_for_bsts_us_suicide$suicide_rate_total_scaled,
                                             state.specification = ss_us_suicide_trend_seas,
                                             niter = iter,
                                             family = "gaussian",
                                             seed = 42)
    
    # Make predictions
    pre_bsts_us_suicide_trend_seas <- predict(model_bsts_us_suicide_trend_seas, 
                                              horizon = 12,
                                              niter = iter,
                                              burn = burn,
                                              seed = 42)
    
    pre_bsts_us_suicide_trend_seas <- pre_bsts_us_suicide_trend_seas$mean
    
    # Inverse scale transformation
    pre_bsts_us_suicide_rate_trend_seas <- scale_max_min_vector_inverse(
      pre_bsts_us_suicide_trend_seas, 
      min(df_main_us$suicide_rate_total[df_main_us$term <= "2016-12-01"]),
      max(df_main_us$suicide_rate_total[df_main_us$term <= "2016-12-01"])
    )
    
    # Calculate accuracy metrics
    accuracy_results <- accuracy(pre_bsts_us_suicide_rate_trend_seas,val_us_suicide)
    
    # Add results to the dataframe
    model_selection_us_model_1 <- rbind(model_selection_us_model_1, 
                                        data.frame(iterations = iter,
                                                   burn_in_percent = burn_percent,
                                                   burn_in = burn,
                                                   MAPE = accuracy_results[5],
                                                   RMSE = accuracy_results[2],
                                                   MAE = accuracy_results[3]))
  }
}
# Find the best model based on MAPE
best_model_us_suicide_trend_seas <- model_selection_us_model_1[which.min(model_selection_us_model_1$MAPE), ]
#calculation final model
model_bsts_us_suicide_trend_seas <- bsts(train_for_bsts_us_suicide$suicide_rate_total_scaled,state.specification = ss_us_suicide_trend_seas , niter = best_model_us_suicide_trend_seas$iterations,
                                         family = "gaussian",seed = 42)

#components
plot(model_bsts_us_suicide_trend_seas,"comp")+
title("US Model (1)")


#predict suicide rate
pre_bsts_us_suicide_trend_seas <- predict(model_bsts_us_suicide_trend_seas, horizon = 36,niter = best_model_us_suicide_trend_seas$iterations,burn = best_model_us_suicide_trend_seas$burn_in,seed = 42)
pre_bsts_us_suicide_trend_seas <- pre_bsts_us_suicide_trend_seas$mean
pre_bsts_us_suicide_rate_trend_seas <- scale_max_min_vector_inverse(pre_bsts_us_suicide_trend_seas, min(df_main_us$suicide_rate_total[df_main_us$term <= "2016-12-01"]), max(df_main_us$suicide_rate_total[df_main_us$term <= "2016-12-01"]))
#predict suicide rate and number for test set
test_us_suicide$pre_rate_model_1  <- pre_bsts_us_suicide_rate_trend_seas[13:length(pre_bsts_us_suicide_rate_trend_seas)]
test_us_suicide$pre_num_model_1 <- (test_us_suicide$pre_rate_model_1*test_us_suicide$pop)/100000

#confirm accuracy for test set
#rate
print(accuracy(test_us_suicide$pre_rate_model_1,test_us_suicide$true_rate))
#number
print(accuracy(test_us_suicide$pre_num_model_1,test_us_suicide$true_num))

# model (2) ----------------------------------------------------------------
#add local level
ss_us_suicide_tv_sd <- AddLocalLevel(state.specification=list(), train_for_bsts_us_suicide$suicide_rate_total_scaled)

#add seasonality
ss_us_suicide_tv_sd <- AddSeasonal(ss_us_suicide_tv_sd, train_for_bsts_us_suicide$suicide_rate_total_scaled, nseasons=12)

#add regression
ss_us_suicide_tv_sd <- AddDynamicRegression(ss_us_suicide_tv_sd,formula(suicide_rate_total_scaled ~ 
                                                                          query_suicide_scaled + query_depression_scaled),data = train_for_bsts_us_suicide)

# Define grid search parameters
iterations <- c(2000,4000,8000,16000)
burn_in_percentages <- c(0.1,0.20,0.30,0.40)

# Create a dataframe to store results
model_selection_us_model_2 <- data.frame()

# Perform grid search
for (iter in iterations) {
  for (burn_percent in burn_in_percentages) {
    # Calculate burn-in period
    burn <- round(iter * burn_percent)
    
    # Fit the model
    model_bsts_us_suicide_tv_sd <- bsts(formula = train_for_bsts_us_suicide$suicide_rate_total_scaled,
                                        state.specification = ss_us_suicide_tv_sd,
                                        niter = iter,
                                        family = "gaussian",
                                        seed = 42)
    
    # Make predictions
    pre_bsts_us_suicide_tv_sd <- predict(model_bsts_us_suicide_tv_sd,
                                         newdata = test_for_bsts_us_suicide,
                                         niter = iter,
                                         burn = burn,
                                         seed = 42)
    
    pre_bsts_us_suicide_tv_sd <- pre_bsts_us_suicide_tv_sd$mean[(nrow(test_for_bsts_us_suicide)-36+1):(nrow(test_for_bsts_us_suicide)-24)]
    
    # Inverse scale transformation
    pre_bsts_us_suicide_rate_tv_sd <- scale_max_min_vector_inverse(
      pre_bsts_us_suicide_tv_sd, 
      min(df_main_us$suicide_rate_total[df_main_us$term <= "2016-12-01"]),
      max(df_main_us$suicide_rate_total[df_main_us$term <= "2016-12-01"])
    )
    
    # Calculate accuracy metrics
    accuracy_results <- accuracy(pre_bsts_us_suicide_rate_tv_sd, val_us_suicide)
    
    # Add results to the dataframe
    model_selection_us_model_2 <- rbind(model_selection_us_model_2, 
                                        data.frame(iterations = iter,
                                                   burn_in_percent = burn_percent,
                                                   burn_in = burn,
                                                   MAPE = accuracy_results[5],
                                                   RMSE = accuracy_results[2],
                                                   MAE = accuracy_results[3]))
  }
}

# Find the best model based on MAPE
best_model_us_suicide_tv_sd <- model_selection_us_model_2[which.min(model_selection_us_model_2$MAPE), ]

# Calculate final model with best parameters
model_bsts_us_suicide_tv_sd <- bsts(formula = train_for_bsts_us_suicide$suicide_rate_total_scaled,
                                    state.specification = ss_us_suicide_tv_sd,
                                    niter = best_model_us_suicide_tv_sd$iterations,
                                    family = "gaussian",
                                    seed = 42)

#components
plot(model_bsts_us_suicide_tv_sd ,"comp")+
title("US Model (2)")

# Predict suicide rate for test set
pre_bsts_us_suicide_tv_sd <- predict(model_bsts_us_suicide_tv_sd,
                                     newdata = test_for_bsts_us_suicide,
                                     niter = best_model_us_suicide_tv_sd$iterations,
                                     burn = best_model_us_suicide_tv_sd$burn_in,
                                     seed = 42)

pre_bsts_us_suicide_tv_sd <- pre_bsts_us_suicide_tv_sd$mean

# Inverse scale transformation for entire prediction
pre_bsts_us_suicide_rate_tv_sd <- scale_max_min_vector_inverse(
  pre_bsts_us_suicide_tv_sd, 
  min(df_main_us$suicide_rate_total[df_main_us$term <= "2016-12-01"]),
  max(df_main_us$suicide_rate_total[df_main_us$term <= "2016-12-01"])
)

# Predict suicide rate and number for test set
test_us_suicide$pre_rate_model_2 <- pre_bsts_us_suicide_rate_tv_sd[(nrow(test_for_bsts_us_suicide)-24+1):nrow(test_for_bsts_us_suicide)]
test_us_suicide$pre_num_model_2 <- (test_us_suicide$pre_rate_model_2 * test_us_suicide$pop) / 100000

# Confirm accuracy for test set
print(accuracy(test_us_suicide$pre_rate_model_2, test_us_suicide$true_rate))
print(accuracy(test_us_suicide$pre_num_model_2, test_us_suicide$true_num))

# model (3) ----------------------------------------------------------------
#add local levels
ss_us_suicide_tv_all <- AddLocalLevel(state.specification=list(), train_for_bsts_us_suicide$suicide_rate_total_scaled)

#add seasonality
ss_us_suicide_tv_all <- AddSeasonal(ss_us_suicide_tv_all, train_for_bsts_us_suicide$suicide_rate_total_scaled, nseasons=12)

#regression
ss_us_suicide_tv_all <- AddDynamicRegression(ss_us_suicide_tv_all,
                                             formula(suicide_rate_total_scaled ~                                                                      
                                                       query_suicide_scaled + query_depression_scaled + query_anxiety_scaled +                                                                    
                                                       query_suicide_methods_scaled + query_abuse_scaled + query_commit_suicide_scaled +                                                                    
                                                       query_loneliness_scaled + query_divorce_scaled + query_unemployment_scaled +                                                                    
                                                       query_how_to_commit_suicide_scaled +                                                                    
                                                       query_manic_depression_scaled + query_social_welfare_scaled +                                                                    
                                                       query_kill_yourself_scaled + query_alcohol_scaled + query_suicide_ideation_scaled +                                                                    
                                                       query_suicide_hotline_scaled + query_suicide_help_scaled +                                                                    
                                                       query_severe_depression_scaled + query_suicide_attempt_scaled +                                                                    
                                                       query_insomnia_scaled +                                                                    
                                                       query_headache_scaled + query_suicidal_thoughts_scaled +                                                                    
                                                       query_how_to_kill_yourself_scaled + query_major_depression_scaled +                                                                    
                                                       query_schizophrenia_scaled + query_stress_scaled + query_anxiety_disorder_scaled +                                                                    
                                                       query_marriage_scaled + query_bipolar_disorder_scaled + query_cancer_scaled +                                                                    
                                                       query_asthma_scaled + query_pain_scaled + query_antidepressant_scaled +                                                                    
                                                       query_relationship_breakup_scaled + query_allergy_scaled + query_teen_suicide_scaled +                                                                    
                                                       query_job_scaled + query_drunkenness_scaled + query_hanging_scaled +                                                                    
                                                       query_illicit_drugs_scaled + query_social_benefits_scaled +                                                                    
                                                       query_hydrogen_sulfide_scaled + query_stock_market_scaled +                                                                    
                                                       query_domestic_violence_scaled + query_hypnotics_scaled +                                                                    
                                                       query_charcoal_burning_scaled + query_chronic_illness_scaled +                                                                    
                                                       query_suicide_prevention_scaled + query_lawsuit_scaled +                                                                    
                                                       query_alcohol_abstinence_scaled + query_religious_belief_scaled),                                         
                                             data = train_for_bsts_us_suicide)

# Define grid search parameters
iterations <- c(2000,4000,8000,16000)
burn_in_percentages <- c(0.1, 0.20,0.30,0.40)

# Create a dataframe to store results
model_selection_us_model_3 <- data.frame()

# Perform grid search
for (iter in iterations) {
  for (burn_percent in burn_in_percentages) {
    # Calculate burn-in period
    burn <- round(iter * burn_percent)
    
    # Fit the model
    model_bsts_us_suicide_tv_all <- bsts(formula = train_for_bsts_us_suicide$suicide_rate_total_scaled,
                                         state.specification = ss_us_suicide_tv_all,
                                         niter = iter,
                                         family = "gaussian",
                                         seed = 42)
    
    # Make predictions
    pre_bsts_us_suicide_tv_all <- predict(model_bsts_us_suicide_tv_all,
                                          newdata = test_for_bsts_us_suicide,
                                          niter = iter,
                                          burn = burn,
                                          seed = 42)
    
    pre_bsts_us_suicide_tv_all <- pre_bsts_us_suicide_tv_all$mean[(nrow(test_for_bsts_us_suicide)-36+1):(nrow(test_for_bsts_us_suicide)-24)]
    
    # Inverse scale transformation
    pre_bsts_us_suicide_rate_tv_all <- scale_max_min_vector_inverse(
      pre_bsts_us_suicide_tv_all, 
      min(df_main_us$suicide_rate_total[df_main_us$term <= "2016-12-01"]),
      max(df_main_us$suicide_rate_total[df_main_us$term <= "2016-12-01"])
    )
    
    # Calculate accuracy metrics
    accuracy_results <- accuracy(pre_bsts_us_suicide_rate_tv_all, val_us_suicide)
    
    # Add results to the dataframe
    model_selection_us_model_3 <- rbind(model_selection_us_model_3, 
                                        data.frame(iterations = iter,
                                                   burn_in_percent = burn_percent,
                                                   burn_in = burn,
                                                   MAPE = accuracy_results[5],
                                                   RMSE = accuracy_results[2],
                                                   MAE = accuracy_results[3]))
  }
}

# Find the best model based on MAPE
best_model_us_suicide_tv_all <- model_selection_us_model_3[which.min(model_selection_us_model_3$MAPE), ]

# Calculate final model with best parameters
model_bsts_us_suicide_tv_all <- bsts(formula = train_for_bsts_us_suicide$suicide_rate_total_scaled,
                                     state.specification = ss_us_suicide_tv_all,
                                     niter = best_model_us_suicide_tv_all$iterations,
                                     family = "gaussian",
                                     seed = 42)

# Plot components
plot(model_bsts_us_suicide_tv_all, "comp")+
title("US Model (3)")


# Predict suicide rate for test set
pre_bsts_us_suicide_tv_all <- predict(model_bsts_us_suicide_tv_all,
                                      newdata = test_for_bsts_us_suicide,
                                      niter = best_model_us_suicide_tv_all$iterations,
                                      burn = best_model_us_suicide_tv_all$burn_in,
                                      seed = 42)

pre_bsts_us_suicide_tv_all <- pre_bsts_us_suicide_tv_all$mean

# Inverse scale transformation for entire prediction
pre_bsts_us_suicide_rate_tv_all <- scale_max_min_vector_inverse(
  pre_bsts_us_suicide_tv_all, 
  min(df_main_us$suicide_rate_total[df_main_us$term <= "2016-12-01"]),
  max(df_main_us$suicide_rate_total[df_main_us$term <= "2016-12-01"])
)

# Predict suicide rate and number for test set
test_us_suicide$pre_rate_model_3 <- pre_bsts_us_suicide_rate_tv_all[(nrow(test_for_bsts_us_suicide)-24+1):nrow(test_for_bsts_us_suicide)]
test_us_suicide$pre_num_model_3 <- (test_us_suicide$pre_rate_model_3 * test_us_suicide$pop) / 100000

# Confirm accuracy for test set
print(accuracy(test_us_suicide$pre_rate_model_3, test_us_suicide$true_rate))
print(accuracy(test_us_suicide$pre_num_model_3, test_us_suicide$true_num))
# model (4) ---------------------------------------------------------------
# Add local level
ss_us_suicide_spike_slab <- AddLocalLevel(state.specification=list(), train_for_bsts_us_suicide$suicide_rate_total_scaled)

# Add seasonality
ss_us_suicide_spike_slab <- AddSeasonal(ss_us_suicide_spike_slab, train_for_bsts_us_suicide$suicide_rate_total_scaled, nseasons=12)

# Define the formula
formula_us <- formula(suicide_rate_total_scaled ~                                                            
                        query_suicide_scaled + query_depression_scaled + query_anxiety_scaled +                                                           
                        query_suicide_methods_scaled + query_abuse_scaled + query_commit_suicide_scaled +                                                           
                        query_loneliness_scaled + query_divorce_scaled + query_unemployment_scaled +                                                           
                        query_how_to_commit_suicide_scaled +                                                           
                        query_manic_depression_scaled + query_social_welfare_scaled +                                                           
                        query_kill_yourself_scaled + query_alcohol_scaled + query_suicide_ideation_scaled +                                                           
                        query_suicide_hotline_scaled + query_suicide_help_scaled +                                                           
                        query_severe_depression_scaled + query_suicide_attempt_scaled +                                                           
                        query_insomnia_scaled +                                                           
                        query_headache_scaled + query_suicidal_thoughts_scaled +                                                           
                        query_how_to_kill_yourself_scaled + query_major_depression_scaled +                                                           
                        query_schizophrenia_scaled + query_stress_scaled + query_anxiety_disorder_scaled +                                                           
                        query_marriage_scaled + query_bipolar_disorder_scaled + query_cancer_scaled +                                                           
                        query_asthma_scaled + query_pain_scaled + query_antidepressant_scaled +                                                           
                        query_relationship_breakup_scaled + query_allergy_scaled + query_teen_suicide_scaled +                                                           
                        query_job_scaled + query_drunkenness_scaled + query_hanging_scaled +                                                           
                        query_illicit_drugs_scaled + query_social_benefits_scaled +                                                           
                        query_hydrogen_sulfide_scaled + query_stock_market_scaled +                                                           
                        query_domestic_violence_scaled + query_hypnotics_scaled +                                                           
                        query_charcoal_burning_scaled + query_chronic_illness_scaled +                                                           
                        query_suicide_prevention_scaled + query_lawsuit_scaled +                                                           
                        query_alcohol_abstinence_scaled + query_religious_belief_scaled)

# Count the number of query
n_predictors <- ncol(train_for_bsts_us_suicide)-1

# Define grid search parameters
iterations <- c(2000,4000,8000,16000)
burn_in_percentages <- c(0.1, 0.20,0.30,0.40)
#expected model size
pi_values <- c(0.05, 0.1, 0.15, 0.20)

# Create a dataframe to store results
model_selection_us_model_4 <- data.frame()

# Perform grid search
for (iter in iterations) {
  for (burn_percent in burn_in_percentages) {
    for (pi in pi_values) {
      # Calculate burn-in period and expected model size
      burn <- round(iter * burn_percent)
      expected_model_size <- n_predictors * pi
      
      # Fit the model
      model_bsts_us_suicide_spike_slab <- bsts(formula_us,
                                               data = train_for_bsts_us_suicide, 
                                               state.specification = ss_us_suicide_spike_slab, 
                                               niter = iter,
                                               expected.model.size = expected_model_size, 
                                               family = "gaussian", 
                                               seed = 42)
      
      # Make predictions
      pre_bsts_us_suicide_spike_slab <- predict(model_bsts_us_suicide_spike_slab,
                                                newdata = test_for_bsts_us_suicide,
                                                niter = iter,
                                                burn = burn,
                                                seed = 42)
      
      pre_bsts_us_suicide_spike_slab <- pre_bsts_us_suicide_spike_slab$mean[(nrow(test_for_bsts_us_suicide)-36+1):(nrow(test_for_bsts_us_suicide)-24)]
      
      # Inverse scale transformation
      pre_bsts_us_suicide_rate_spike_slab <- scale_max_min_vector_inverse(
        pre_bsts_us_suicide_spike_slab, 
        min(df_main_us$suicide_rate_total[df_main_us$term <= "2016-12-01"]),
        max(df_main_us$suicide_rate_total[df_main_us$term <= "2016-12-01"])
      )
      
      # Calculate accuracy metrics
      accuracy_results <- accuracy(pre_bsts_us_suicide_rate_spike_slab, val_us_suicide)
      
      # Add results to the dataframe
      model_selection_us_model_4 <- rbind(model_selection_us_model_4, 
                                          data.frame(iterations = iter,
                                                     burn_in_percent = burn_percent,
                                                     pi = pi,
                                                     expected_model_size = expected_model_size,
                                                     burn_in = burn,
                                                     MAPE = accuracy_results[5],
                                                     RMSE = accuracy_results[2],
                                                     MAE = accuracy_results[3]))
    }
  }
}

# Find the best model based on MAPE
best_model_us_suicide_spike_slab <- model_selection_us_model_4[which.min(model_selection_us_model_4$MAPE), ]

# Calculate final model with best parameters
final_model_bsts_us_suicide_spike_slab <- bsts(formula_us,
                                               data = train_for_bsts_us_suicide, 
                                               state.specification = ss_us_suicide_spike_slab, 
                                               niter = best_model_us_suicide_spike_slab$iterations,
                                               expected.model.size = best_model_us_suicide_spike_slab$expected_model_size, 
                                               family = "gaussian", 
                                               seed = 42)

# Plot components
plot(final_model_bsts_us_suicide_spike_slab, "comp")+
title("US Model (4)")


# Predict suicide rate for test set
pre_bsts_us_suicide_spike_slab <- predict(final_model_bsts_us_suicide_spike_slab,
                                          newdata = test_for_bsts_us_suicide,
                                          niter = best_model_us_suicide_spike_slab$iterations,
                                          burn = best_model_us_suicide_spike_slab$burn_in,
                                          seed = 42)

pre_bsts_us_suicide_spike_slab <- pre_bsts_us_suicide_spike_slab$mean

# Inverse scale transformation for entire prediction
pre_bsts_us_suicide_rate_spike_slab <- scale_max_min_vector_inverse(
  pre_bsts_us_suicide_spike_slab, 
  min(df_main_us$suicide_rate_total[df_main_us$term <= "2016-12-01"]),
  max(df_main_us$suicide_rate_total[df_main_us$term <= "2016-12-01"])
)

# Predict suicide rate and number for test set
test_us_suicide$pre_rate_model_4 <- pre_bsts_us_suicide_rate_spike_slab[(nrow(test_for_bsts_us_suicide)-24+1):nrow(test_for_bsts_us_suicide)]
test_us_suicide$pre_num_model_4 <- (test_us_suicide$pre_rate_model_4 * test_us_suicide$pop) / 100000

# Confirm accuracy for test set
print(accuracy(test_us_suicide$pre_rate_model_4,test_us_suicide$true_rate))
print(accuracy(test_us_suicide$pre_num_model_4,test_us_suicide$true_num))

# Model selection (JP) ----------------------------------------------------
# model (1) ----------------------------------------------------------------
#add local level
ss_jp_suicide_trend_seas <- AddLocalLevel(state.specification=list(), train_for_bsts_jp_suicide$suicide_rate_total_scaled)

#add seasonality
ss_jp_suicide_trend_seas <- AddSeasonal(ss_jp_suicide_trend_seas, train_for_bsts_jp_suicide$suicide_rate_total_scaled, nseasons=12)

# Define grid search parameters
iterations <- c(2000,4000,8000,16000)
burn_in_percentages <- c(0.1,0.20,0.30,0.40)

# Create a dataframe to store results
model_selection_jp_model_1 <- data.frame()

# Perform grid search
for (iter in iterations) {
  for (burn_percent in burn_in_percentages) {
    # Calculate burn-in period
    burn <- round(iter * burn_percent)
    
    # Fit the model
    model_bsts_jp_suicide_trend_seas <- bsts(train_for_bsts_jp_suicide$suicide_rate_total_scaled,
                                             state.specification = ss_jp_suicide_trend_seas,
                                             niter = iter,
                                             family = "gaussian",
                                             seed = 42)
    
    # Make predictions
    pre_bsts_jp_suicide_trend_seas <- predict(model_bsts_jp_suicide_trend_seas, 
                                              horizon = 12,
                                              niter = iter,
                                              burn = burn,
                                              seed = 42)
    
    pre_bsts_jp_suicide_trend_seas <- pre_bsts_jp_suicide_trend_seas$mean
    
    # Inverse scale transformation
    pre_bsts_jp_suicide_rate_trend_seas <- scale_max_min_vector_inverse(
      pre_bsts_jp_suicide_trend_seas, 
      min(df_main_jp$suicide_rate_total[df_main_jp$term <= "2016-12-01"]),
      max(df_main_jp$suicide_rate_total[df_main_jp$term <= "2016-12-01"])
    )
    
    # Calculate accuracy metrics
    accuracy_results <- accuracy(pre_bsts_jp_suicide_rate_trend_seas,val_jp_suicide)
    
    # Add results to the dataframe
    model_selection_jp_model_1 <- rbind(model_selection_jp_model_1, 
                                        data.frame(iterations = iter,
                                                   burn_in_percent = burn_percent,
                                                   burn_in = burn,
                                                   MAPE = accuracy_results[5],
                                                   RMSE = accuracy_results[2],
                                                   MAE = accuracy_results[3]))
  }
}


# Find the best model based on MAPE
best_model_jp_suicide_trend_seas <- model_selection_jp_model_1[which.min(model_selection_jp_model_1$MAPE), ]

#calculation final model
model_bsts_jp_suicide_trend_seas <- bsts(train_for_bsts_jp_suicide$suicide_rate_total_scaled,state.specification = ss_jp_suicide_trend_seas , niter = best_model_jp_suicide_trend_seas$iterations,
                                         family = "gaussian",seed = 42)

#components
plot(model_bsts_jp_suicide_trend_seas,"comp")+
title("Japan Model (1)")

#predict suicide rate
pre_bsts_jp_suicide_trend_seas <- predict(model_bsts_jp_suicide_trend_seas, horizon = 36,niter = best_model_jp_suicide_trend_seas$iterations,burn = best_model_jp_suicide_trend_seas$burn_in,seed = 42)
pre_bsts_jp_suicide_trend_seas <- pre_bsts_jp_suicide_trend_seas$mean
pre_bsts_jp_suicide_rate_trend_seas <- scale_max_min_vector_inverse(pre_bsts_jp_suicide_trend_seas, min(df_main_jp$suicide_rate_total[df_main_jp$term <= "2016-12-01"]), max(df_main_jp$suicide_rate_total[df_main_jp$term <= "2016-12-01"]))

#predict suicide rate and number for test set
test_jp_suicide$pre_rate_model_1  <- pre_bsts_jp_suicide_rate_trend_seas[13:length(pre_bsts_jp_suicide_rate_trend_seas)]
test_jp_suicide$pre_num_model_1 <- (test_jp_suicide$pre_rate_model_1*test_jp_suicide$pop)/100000

#confirm accuracy for test set
#rate
print(accuracy(test_jp_suicide$pre_rate_model_1,test_jp_suicide$true_rate))
#number
print(accuracy(test_jp_suicide$pre_num_model_1,test_jp_suicide$true_num))

# model (2) ----------------------------------------------------------------
#add local level
ss_jp_suicide_tv_sd <- AddLocalLevel(state.specification=list(), train_for_bsts_jp_suicide$suicide_rate_total_scaled)

#add seasonality
ss_jp_suicide_tv_sd <- AddSeasonal(ss_jp_suicide_tv_sd, train_for_bsts_jp_suicide$suicide_rate_total_scaled, nseasons=12)

#add regression
ss_jp_suicide_tv_sd <- AddDynamicRegression(ss_jp_suicide_tv_sd,formula(suicide_rate_total_scaled ~ 
                                                                          query_suicide_scaled + query_depression_scaled),data = train_for_bsts_jp_suicide)

# Define grid search parameters
iterations <- c(2000,4000,8000,16000)
burn_in_percentages <- c(0.1,0.20,0.30,0.40)

# Create a dataframe to store results
model_selection_jp_model_2 <- data.frame()

# Perform grid search
for (iter in iterations) {
  for (burn_percent in burn_in_percentages) {
    # Calculate burn-in period
    burn <- round(iter * burn_percent)
    
    # Fit the model
    model_bsts_jp_suicide_tv_sd <- bsts(formula = train_for_bsts_jp_suicide$suicide_rate_total_scaled,
                                        state.specification = ss_jp_suicide_tv_sd,
                                        niter = iter,
                                        family = "gaussian",
                                        seed = 42)
    
    # Make predictions
    pre_bsts_jp_suicide_tv_sd <- predict(model_bsts_jp_suicide_tv_sd,
                                         newdata = test_for_bsts_jp_suicide,
                                         niter = iter,
                                         burn = burn,
                                         seed = 42)
    
    pre_bsts_jp_suicide_tv_sd <- pre_bsts_jp_suicide_tv_sd$mean[(nrow(test_for_bsts_jp_suicide)-36+1):(nrow(test_for_bsts_jp_suicide)-24)]
    
    # Inverse scale transformation
    pre_bsts_jp_suicide_rate_tv_sd <- scale_max_min_vector_inverse(
      pre_bsts_jp_suicide_tv_sd, 
      min(df_main_jp$suicide_rate_total[df_main_jp$term <= "2016-12-01"]),
      max(df_main_jp$suicide_rate_total[df_main_jp$term <= "2016-12-01"])
    )
    
    # Calculate accuracy metrics
    accuracy_results <- accuracy(pre_bsts_jp_suicide_rate_tv_sd, val_jp_suicide)
    
    # Add results to the dataframe
    model_selection_jp_model_2 <- rbind(model_selection_jp_model_2, 
                                        data.frame(iterations = iter,
                                                   burn_in_percent = burn_percent,
                                                   burn_in = burn,
                                                   MAPE = accuracy_results[5],
                                                   RMSE = accuracy_results[2],
                                                   MAE = accuracy_results[3]))
  }
}

# Find the best model based on MAPE
best_model_jp_suicide_tv_sd <- model_selection_jp_model_2[which.min(model_selection_jp_model_2$MAPE), ]

# Calculate final model with best parameters
model_bsts_jp_suicide_tv_sd <- bsts(formula = train_for_bsts_jp_suicide$suicide_rate_total_scaled,
                                    state.specification = ss_jp_suicide_tv_sd,
                                    niter = best_model_jp_suicide_tv_sd$iterations,
                                    family = "gaussian",
                                    seed = 42)

#components
plot(model_bsts_jp_suicide_tv_sd ,"comp")+
title("Japan Model (2)")

# Predict suicide rate for test set
pre_bsts_jp_suicide_tv_sd <- predict(model_bsts_jp_suicide_tv_sd,
                                     newdata = test_for_bsts_jp_suicide,
                                     niter = best_model_jp_suicide_tv_sd$iterations,
                                     burn = best_model_jp_suicide_tv_sd$burn_in,
                                     seed = 42)

pre_bsts_jp_suicide_tv_sd <- pre_bsts_jp_suicide_tv_sd$mean

# Inverse scale transformation for entire prediction
pre_bsts_jp_suicide_rate_tv_sd <- scale_max_min_vector_inverse(
  pre_bsts_jp_suicide_tv_sd, 
  min(df_main_jp$suicide_rate_total[df_main_jp$term <= "2016-12-01"]),
  max(df_main_jp$suicide_rate_total[df_main_jp$term <= "2016-12-01"])
)

# Predict suicide rate and number for test set
test_jp_suicide$pre_rate_model_2 <- pre_bsts_jp_suicide_rate_tv_sd[(nrow(test_for_bsts_jp_suicide)-24+1):nrow(test_for_bsts_jp_suicide)]
test_jp_suicide$pre_num_model_2 <- (test_jp_suicide$pre_rate_model_2 * test_jp_suicide$pop) / 100000

# Confirm accuracy for test set
print(accuracy(test_jp_suicide$pre_rate_model_2, test_jp_suicide$true_rate))
print(accuracy(test_jp_suicide$pre_num_model_2, test_jp_suicide$true_num))

# model (3) ---------------------------------------------------------------
#add local levels
ss_jp_suicide_tv_all <- AddLocalLevel(state.specification=list(), train_for_bsts_jp_suicide$suicide_rate_total_scaled)

#add seasonality
ss_jp_suicide_tv_all <- AddSeasonal(ss_jp_suicide_tv_all, train_for_bsts_jp_suicide$suicide_rate_total_scaled, nseasons=12)

#regression
ss_jp_suicide_tv_all <- AddDynamicRegression(ss_jp_suicide_tv_all,
                                             formula(suicide_rate_total_scaled ~ query_suicide_scaled + query_depression_scaled + query_anxiety_scaled +
                                                       query_suicide_methods_scaled + query_abuse_scaled + query_loneliness_scaled + query_divorce_scaled +
                                                       query_unemployment_scaled + query_how_to_commit_suicide_scaled + query_manic_depression_scaled  +
                                                       query_social_welfare_scaled + query_alcohol_scaled + query_suicide_ideation_scaled +
                                                       query_suicide_hotline_scaled + query_suicide_help_scaled + query_suicide_attempt_scaled +
                                                       query_insomnia_scaled + query_headache_scaled + query_allergy_scaled + query_suicidal_thoughts_scaled +
                                                       query_major_depression_scaled + query_schizophrenia_scaled + query_stress_scaled +
                                                       query_anxiety_disorder_scaled + query_marriage_scaled + query_bipolar_disorder_scaled +
                                                       query_cancer_scaled + query_asthma_scaled + query_pain_scaled + query_antidepressant_scaled +
                                                       query_relationship_breakup_scaled + query_teen_suicide_scaled + query_job_scaled +
                                                       query_drunkenness_scaled + query_hanging_scaled + query_illicit_drugs_scaled +
                                                       query_hydrogen_sulfide_scaled + query_stock_market_scaled + query_domestic_violence_scaled +
                                                       query_hypnotics_scaled + query_charcoal_burning_scaled + query_chronic_illness_scaled +
                                                       query_suicide_prevention_scaled + query_lawsuit_scaled + query_alcohol_abstinence_scaled +
                                                       query_religious_belief_scaled),                                         
                                             data = train_for_bsts_jp_suicide)

# Define grid search parameters
iterations <- c(2000,4000,8000,16000)
burn_in_percentages <- c(0.1, 0.20,0.30,0.40)

# Create a dataframe to store results
model_selection_jp_model_3 <- data.frame()

# Perform grid search
for (iter in iterations) {
  for (burn_percent in burn_in_percentages) {
    # Calculate burn-in period
    burn <- round(iter * burn_percent)
    
    # Fit the model
    model_bsts_jp_suicide_tv_all <- bsts(formula = train_for_bsts_jp_suicide$suicide_rate_total_scaled,
                                         state.specification = ss_jp_suicide_tv_all,
                                         niter = iter,
                                         family = "gaussian",
                                         seed = 42)
    
    # Make predictions
    pre_bsts_jp_suicide_tv_all <- predict(model_bsts_jp_suicide_tv_all,
                                          newdata = test_for_bsts_jp_suicide,
                                          niter = iter,
                                          burn = burn,
                                          seed = 42)
    
    pre_bsts_jp_suicide_tv_all <- pre_bsts_jp_suicide_tv_all$mean[(nrow(test_for_bsts_jp_suicide)-36+1):(nrow(test_for_bsts_jp_suicide)-24)]
    
    # Inverse scale transformation
    pre_bsts_jp_suicide_rate_tv_all <- scale_max_min_vector_inverse(
      pre_bsts_jp_suicide_tv_all, 
      min(df_main_jp$suicide_rate_total[df_main_jp$term <= "2016-12-01"]),
      max(df_main_jp$suicide_rate_total[df_main_jp$term <= "2016-12-01"])
    )
    
    # Calculate accuracy metrics
    accuracy_results <- accuracy(pre_bsts_jp_suicide_rate_tv_all, val_jp_suicide)
    
    # Add results to the dataframe
    model_selection_jp_model_3 <- rbind(model_selection_jp_model_3, 
                                        data.frame(iterations = iter,
                                                   burn_in_percent = burn_percent,
                                                   burn_in = burn,
                                                   MAPE = accuracy_results[5],
                                                   RMSE = accuracy_results[2],
                                                   MAE = accuracy_results[3]))
  }
}

# Find the best model based on MAPE
best_model_jp_suicide_tv_all <- model_selection_jp_model_3[which.min(model_selection_jp_model_3$MAPE), ]

# Calculate final model with best parameters
model_bsts_jp_suicide_tv_all <- bsts(formula = train_for_bsts_jp_suicide$suicide_rate_total_scaled,
                                     state.specification = ss_jp_suicide_tv_all,
                                     niter = best_model_jp_suicide_tv_all$iterations,
                                     family = "gaussian",
                                     seed = 42)

# Plot components
plot(model_bsts_jp_suicide_tv_all, "comp")+
title("Japan Model (3)")


# Predict suicide rate for test set
pre_bsts_jp_suicide_tv_all <- predict(model_bsts_jp_suicide_tv_all,
                                      newdata = test_for_bsts_jp_suicide,
                                      niter = best_model_jp_suicide_tv_all$iterations,
                                      burn = best_model_jp_suicide_tv_all$burn_in,
                                      seed = 42)

pre_bsts_jp_suicide_tv_all <- pre_bsts_jp_suicide_tv_all$mean

# Inverse scale transformation for entire prediction
pre_bsts_jp_suicide_rate_tv_all <- scale_max_min_vector_inverse(
  pre_bsts_jp_suicide_tv_all, 
  min(df_main_jp$suicide_rate_total[df_main_jp$term <= "2016-12-01"]),
  max(df_main_jp$suicide_rate_total[df_main_jp$term <= "2016-12-01"])
)

# Predict suicide rate and number for test set
test_jp_suicide$pre_rate_model_3 <- pre_bsts_jp_suicide_rate_tv_all[(nrow(test_for_bsts_jp_suicide)-24+1):nrow(test_for_bsts_jp_suicide)]
test_jp_suicide$pre_num_model_3 <- (test_jp_suicide$pre_rate_model_3 * test_jp_suicide$pop) / 100000

# Confirm accuracy for test set
print(accuracy(test_jp_suicide$pre_rate_model_3, test_jp_suicide$true_rate))
print(accuracy(test_jp_suicide$pre_num_model_3, test_jp_suicide$true_num))

# model (4) ---------------------------------------------------------------
# Add local level
ss_jp_suicide_spike_slab <- AddLocalLevel(state.specification=list(), train_for_bsts_jp_suicide$suicide_rate_total_scaled)

# Add seasonality
ss_jp_suicide_spike_slab <- AddSeasonal(ss_jp_suicide_spike_slab, train_for_bsts_jp_suicide$suicide_rate_total_scaled, nseasons=12)

# Define the formula
formula_jp <- formula(suicide_rate_total_scaled ~ query_suicide_scaled + query_depression_scaled + query_anxiety_scaled +
                        query_suicide_methods_scaled + query_abuse_scaled + query_loneliness_scaled + query_divorce_scaled +
                        query_unemployment_scaled + query_how_to_commit_suicide_scaled + query_manic_depression_scaled+
                        query_social_welfare_scaled + query_alcohol_scaled + query_suicide_ideation_scaled +
                        query_suicide_hotline_scaled + query_suicide_help_scaled + query_suicide_attempt_scaled +
                        query_insomnia_scaled + query_headache_scaled + query_allergy_scaled + query_suicidal_thoughts_scaled +
                        query_major_depression_scaled + query_schizophrenia_scaled + query_stress_scaled +
                        query_anxiety_disorder_scaled + query_marriage_scaled + query_bipolar_disorder_scaled +
                        query_cancer_scaled + query_asthma_scaled + query_pain_scaled + query_antidepressant_scaled +
                        query_relationship_breakup_scaled + query_teen_suicide_scaled + query_job_scaled +
                        query_drunkenness_scaled + query_hanging_scaled + query_illicit_drugs_scaled +
                        query_hydrogen_sulfide_scaled + query_stock_market_scaled + query_domestic_violence_scaled +
                        query_hypnotics_scaled + query_charcoal_burning_scaled + query_chronic_illness_scaled +
                        query_suicide_prevention_scaled + query_lawsuit_scaled + query_alcohol_abstinence_scaled +
                        query_religious_belief_scaled)

# Count the number of query
n_predictors <- ncol(train_for_bsts_jp_suicide)-1

# Define grid search parameters
iterations <- c(2000,4000,8000,16000)
burn_in_percentages <- c(0.1, 0.20,0.30,0.40)
#expected model size
pi_values <- c(0.05, 0.1, 0.15, 0.20)

# Create a dataframe to store results
model_selection_jp_model_4 <- data.frame()

# Perform grid search
for (iter in iterations) {
  for (burn_percent in burn_in_percentages) {
    for (pi in pi_values) {
      # Calculate burn-in period and expected model size
      burn <- round(iter * burn_percent)
      expected_model_size <- n_predictors * pi
      
      # Fit the model
      model_bsts_jp_suicide_spike_slab <- bsts(formula_jp,
                                               data = train_for_bsts_jp_suicide, 
                                               state.specification = ss_jp_suicide_spike_slab, 
                                               niter = iter,
                                               expected.model.size = expected_model_size, 
                                               family = "gaussian", 
                                               seed = 42)
      
      # Make predictions
      pre_bsts_jp_suicide_spike_slab <- predict(model_bsts_jp_suicide_spike_slab,
                                                newdata = test_for_bsts_jp_suicide,
                                                niter = iter,
                                                burn = burn,
                                                seed = 42)
      
      pre_bsts_jp_suicide_spike_slab <- pre_bsts_jp_suicide_spike_slab$mean[(nrow(test_for_bsts_jp_suicide)-36+1):(nrow(test_for_bsts_jp_suicide)-24)]
      
      # Inverse scale transformation
      pre_bsts_jp_suicide_rate_spike_slab <- scale_max_min_vector_inverse(
        pre_bsts_jp_suicide_spike_slab, 
        min(df_main_jp$suicide_rate_total[df_main_jp$term <= "2016-12-01"]),
        max(df_main_jp$suicide_rate_total[df_main_jp$term <= "2016-12-01"])
      )
      
      # Calculate accuracy metrics
      accuracy_results <- accuracy(pre_bsts_jp_suicide_rate_spike_slab, val_jp_suicide)
      
      # Add results to the dataframe
      model_selection_jp_model_4 <- rbind(model_selection_jp_model_4, 
                                          data.frame(iterations = iter,
                                                     burn_in_percent = burn_percent,
                                                     pi = pi,
                                                     expected_model_size = expected_model_size,
                                                     burn_in = burn,
                                                     MAPE = accuracy_results[5],
                                                     RMSE = accuracy_results[2],
                                                     MAE = accuracy_results[3]))
    }
  }
}

# Find the best model based on MAPE
best_model_jp_suicide_spike_slab <- model_selection_jp_model_4[which.min(model_selection_jp_model_4$MAPE), ]

# Calculate final model with best parameters
final_model_bsts_jp_suicide_spike_slab <- bsts(formula_jp,
                                               data = train_for_bsts_jp_suicide, 
                                               state.specification = ss_jp_suicide_spike_slab, 
                                               niter = best_model_jp_suicide_spike_slab$iterations,
                                               expected.model.size =best_model_jp_suicide_spike_slab$expected_model_size, 
                                               family = "gaussian", 
                                               seed = 42)

# Plot components
plot(final_model_bsts_jp_suicide_spike_slab, "comp")+
title("Japan Model (4)")


# Predict suicide rate for test set
pre_bsts_jp_suicide_spike_slab <- predict(final_model_bsts_jp_suicide_spike_slab,
                                          newdata = test_for_bsts_jp_suicide,
                                          niter = best_model_jp_suicide_spike_slab$iterations,
                                          burn = best_model_jp_suicide_spike_slab$burn_in,
                                          seed = 42)

pre_bsts_jp_suicide_spike_slab <- pre_bsts_jp_suicide_spike_slab$mean

# Inverse scale transformation for entire prediction
pre_bsts_jp_suicide_rate_spike_slab <- scale_max_min_vector_inverse(
  pre_bsts_jp_suicide_spike_slab, 
  min(df_main_jp$suicide_rate_total[df_main_jp$term <= "2016-12-01"]),
  max(df_main_jp$suicide_rate_total[df_main_jp$term <= "2016-12-01"])
)

# Predict suicide rate and number for test set
test_jp_suicide$pre_rate_model_4 <- pre_bsts_jp_suicide_rate_spike_slab[(nrow(test_for_bsts_jp_suicide)-24+1):nrow(test_for_bsts_jp_suicide)]
test_jp_suicide$pre_num_model_4 <- (test_jp_suicide$pre_rate_model_4 * test_jp_suicide$pop) / 100000

# Confirm accuracy for test set
print(accuracy(test_jp_suicide$pre_rate_model_4,test_jp_suicide$true_rate))
print(accuracy(test_jp_suicide$pre_num_model_4,test_jp_suicide$true_num))



# model vilification (US) -------------------------------------------------
#sumary bsts
#variables choose by spike and slab
summary_model_bsts_us_suicide_spike_slab <- summary(model_bsts_us_suicide_spike_slab)
summary_model_bsts_us_suicide_spike_slab$coefficients[,"inc.prob"]

#create data frame
coefficients_df_model_bsts_us_suicide_spike_slab  <- data.frame(
  variable = rownames(summary_model_bsts_us_suicide_spike_slab$coefficients),
  inc_prob = summary_model_bsts_us_suicide_spike_slab$coefficients[, "inc.prob"],
  mean = summary_model_bsts_us_suicide_spike_slab$coefficients[, "mean"])

#ordered by incprob
coefficients_df_model_bsts_us_suicide_spike_slab <- coefficients_df_model_bsts_us_suicide_spike_slab %>%
  arrange(desc(inc_prob))
coefficients_df_model_bsts_us_suicide_spike_slab$variable[coefficients_df_model_bsts_us_suicide_spike_slab$inc_prob >= 0.1]

#edit variable names for visualization
new_variable_names_label_spike_slab_us
new_variable_names_label_spike_slab_us <- str_remove(str_remove(coefficients_df_model_bsts_us_suicide_spike_slab$variable, "^query_"), "_scaled$")
new_variable_names_label_spike_slab_us <- str_replace_all(new_variable_names_label_spike_slab_us, "_", " ")
coefficients_df_model_bsts_us_suicide_spike_slab$variable <- new_variable_names_label_spike_slab_us
p_coef_spike_slab_us <- ggplot(coefficients_df_model_bsts_us_suicide_spike_slab%>% filter(variable != "(Intercept)"), aes(x = reorder(variable, inc_prob), y = inc_prob, fill = mean < 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("FALSE" = "blue", "TRUE" = "red"),
                    labels = c("Plus", "Minus"),
                    name = "Coefficient Sign") + 
  labs(x = "Variables",
       y = "Inclusion Probability",
       title = "Inclusion Probabilities for Spike-and-Slab Regression in the US",
       fill = "Coefficient Sign") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(color = "black", size = 12)
  )

plot(p_coef_spike_slab_us)

#compare final results
#accuracy
#rate
accuracy(test_us_suicide$pre_rate_model_1,test_us_suicide$true_rate)
accuracy(test_us_suicide$pre_rate_model_2,test_us_suicide$true_rate)
accuracy(test_us_suicide$pre_rate_model_3,test_us_suicide$true_rate)
accuracy(test_us_suicide$pre_rate_model_4,test_us_suicide$true_rate)

#num
accuracy(test_us_suicide$pre_num_model_1,test_us_suicide$true_num)
accuracy(test_us_suicide$pre_num_model_2,test_us_suicide$true_num)
accuracy(test_us_suicide$pre_num_model_3,test_us_suicide$true_num)
accuracy(test_us_suicide$pre_num_model_4,test_us_suicide$true_num)

#cumulative AE for test
#calculate AE
test_us_suicide$abs_error_model_1 <- abs(test_us_suicide$true_num - test_us_suicide$pre_num_model_1)
test_us_suicide$abs_error_model_2 <- abs(test_us_suicide$true_num - test_us_suicide$pre_num_model_2)
test_us_suicide$abs_error_model_3 <- abs(test_us_suicide$true_num - test_us_suicide$pre_num_model_3)
test_us_suicide$abs_error_model_4 <- abs(test_us_suicide$true_num - test_us_suicide$pre_num_model_4)

#calculate comulative AE
test_us_suicide$cumulative_abs_error_model_1 <- cumsum(test_us_suicide$abs_error_model_1)
test_us_suicide$cumulative_abs_error_model_2<- cumsum(test_us_suicide$abs_error_model_2)
test_us_suicide$cumulative_abs_error_model_3 <- cumsum(test_us_suicide$abs_error_model_3)
test_us_suicide$cumulative_abs_error_model_4 <- cumsum(test_us_suicide$abs_error_model_4)

#visualizing accucacy
p_pre_accu_us <- ggplot(test_us_suicide, aes(x = term)) +
  geom_line(aes(y = true_num, color = "TRUE")) +
  geom_line(aes(y = pre_num_model_1, color = "model(1)"), linetype = "dashed") +
  geom_line(aes(y = pre_num_model_2, color = "model(2)"), linetype = "dashed") +
  geom_line(aes(y = pre_num_model_3, color = "model(3)"), linetype = "dashed") +
  geom_line(aes(y = pre_num_model_4, color = "model(4)"), linetype = "dashed") +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title = "(a)Predicted Number of Suicides in the US",
    x = "Term (Jan 2018 - Dec 2019)",
    y = "The number of Suicide",
    color = ""
  ) +
  scale_color_manual(values = c("black", "purple", "green", "orange", "red"),
                     breaks = c("TRUE", "model(1)", "model(2)", "model(3)", "model(4)")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

plot(p_pre_accu_us)


p_cumu_ae_us <- ggplot(test_us_suicide, aes(x = term)) +
  geom_line(aes(y = cumulative_abs_error_model_1,  color = "model(1)"),linetype = "dashed") +
  geom_line(aes(y = cumulative_abs_error_model_2 ,  color = "model(2)"),linetype = "dashed") +
  geom_line(aes(y = cumulative_abs_error_model_3, color = "model(3)"),linetype = "dashed") +
  geom_line(aes(y = cumulative_abs_error_model_4, color = "model(4)"),linetype = "dashed") +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title = "(b)Cumulative Absolute Error in Test Set for the Number of Suicides in the US",
    x = "Term (Jan 2018 - Dec 2019)",
    y = "Comulative Absolutely Error",
    color = ""
  ) +
  scale_color_manual(values = c("purple", "green", "orange", "red"),
                     breaks = c("model(1)", "model(2)", "model(3)", "model(4)")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

plot(p_cumu_ae_us)



# Combine the two plots vertically
combined_plot_suicide_us <- p_pre_accu_us / p_cumu_ae_us +
  plot_layout(heights = c(1, 1)) +  # Set equal heights for both plots
  plot_annotation(tag_levels = 'a') &  # Automatically add labels (a), (b)
  theme(plot.tag = element_text(face = "bold"))  # Make labels bold

# Display the combined plot
plot(combined_plot_suicide_us) 

# model vilification (JP) -------------------------------------------------
#sumary bsts
#variables choose by spike and slab
summary_model_bsts_jp_suicide_spike_slab <- summary(model_bsts_jp_suicide_spike_slab)
summary_model_bsts_jp_suicide_spike_slab$coefficients[,"inc.prob"]
#create data frame
coefficients_df_model_bsts_jp_suicide_spike_slab  <- data.frame(
  variable = rownames(summary_model_bsts_jp_suicide_spike_slab$coefficients),
  inc_prob = summary_model_bsts_jp_suicide_spike_slab$coefficients[, "inc.prob"],
  mean = summary_model_bsts_jp_suicide_spike_slab$coefficients[, "mean"])
#ordered by incprob
coefficients_df_model_bsts_jp_suicide_spike_slab <- coefficients_df_model_bsts_jp_suicide_spike_slab %>%
  arrange(desc(inc_prob))
coefficients_df_model_bsts_jp_suicide_spike_slab$variable[coefficients_df_model_bsts_jp_suicide_spike_slab$inc_prob >= 0.1]

#edit labels of variables for visualization
new_variable_names_label_spike_slab_jp <- str_remove(str_remove(coefficients_df_model_bsts_jp_suicide_spike_slab$variable, "^query_"), "_scaled$")
new_variable_names_label_spike_slab_jp <- str_replace_all(new_variable_names_label_spike_slab_jp, "_", " ")
coefficients_df_model_bsts_jp_suicide_spike_slab$variable <- new_variable_names_label_spike_slab_jp


p_coef_spike_slab_jp <-  ggplot(coefficients_df_model_bsts_jp_suicide_spike_slab %>% filter(variable != "(Intercept)"), aes(x = reorder(variable, inc_prob), y = inc_prob, fill = mean < 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("FALSE" = "blue", "TRUE" = "red"),
                    labels = c("Plus", "Minus"),
                    name = "Coefficient Sign") + 
  labs(x = "Variables",
       y = "Inclusion Probability",
       title = "Inclusion Probabilities for Spike-and-Slab Regression in Japan",
       fill = "Coefficient Sign") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(color = "black", size = 12)
  )


plot(p_coef_spike_slab_jp)

#compare final results
#accuracy
#rate
accuracy(test_jp_suicide$pre_rate_model_1,test_jp_suicide$true_rate)
accuracy(test_jp_suicide$pre_rate_model_2,test_jp_suicide$true_rate)
accuracy(test_jp_suicide$pre_rate_model_3,test_jp_suicide$true_rate)
accuracy(test_jp_suicide$pre_rate_model_4,test_jp_suicide$true_rate)
#num
accuracy(test_jp_suicide$pre_num_model_1,test_jp_suicide$true_num)
accuracy(test_jp_suicide$pre_num_model_2,test_jp_suicide$true_num)
accuracy(test_jp_suicide$pre_num_model_3,test_jp_suicide$true_num)
accuracy(test_jp_suicide$pre_num_model_4,test_jp_suicide$true_num)


#cumulative AE for test
#calculate AE
test_jp_suicide$abs_error_model_1 <- abs(test_jp_suicide$true_num - test_jp_suicide$pre_num_model_1)
test_jp_suicide$abs_error_model_2 <- abs(test_jp_suicide$true_num - test_jp_suicide$pre_num_model_2)
test_jp_suicide$abs_error_model_3 <- abs(test_jp_suicide$true_num - test_jp_suicide$pre_num_model_3)
test_jp_suicide$abs_error_model_4 <- abs(test_jp_suicide$true_num - test_jp_suicide$pre_num_model_4)

#calculate comulative AE
test_jp_suicide$cumulative_abs_error_model_1 <- cumsum(test_jp_suicide$abs_error_model_1)
test_jp_suicide$cumulative_abs_error_model_2 <- cumsum(test_jp_suicide$abs_error_model_2)
test_jp_suicide$cumulative_abs_error_model_3 <- cumsum(test_jp_suicide$abs_error_model_3)
test_jp_suicide$cumulative_abs_error_model_4 <- cumsum(test_jp_suicide$abs_error_model_4)




#visualizing accucacy
p_pre_accu_jp <- ggplot(test_jp_suicide, aes(x = term)) +
  geom_line(aes(y = true_num, color = "TRUE")) +
  geom_line(aes(y = pre_num_model_1, color = "model(1)"), linetype = "dashed") +
  geom_line(aes(y = pre_num_model_2, color = "model(2)"), linetype = "dashed") +
  geom_line(aes(y = pre_num_model_3, color = "model(3)"), linetype = "dashed") +
  geom_line(aes(y = pre_num_model_4, color = "model(4)"), linetype = "dashed") +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title = "(a)Predicted Number of Suicides in Japan",
    x = "Term (Jan 2018 - Dec 2019)",
    y = "The number of Suicide",
    color = ""
  ) +
  scale_color_manual(values = c("black", "purple", "green", "orange", "red"),
                     breaks = c("TRUE", "model(1)", "model(2)", "model(3)", "model(4)")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")
plot(p_pre_accu_jp)



p_cumu_ae_jp <- ggplot(test_jp_suicide, aes(x = term)) +
  geom_line(aes(y = cumulative_abs_error_model_1,  color = "model(1)"),linetype = "dashed") +
  geom_line(aes(y = cumulative_abs_error_model_2 ,  color = "model(2)"),linetype = "dashed") +
  geom_line(aes(y = cumulative_abs_error_model_3, color = "model(3)"),linetype = "dashed") +
  geom_line(aes(y = cumulative_abs_error_model_4, color = "model(4)"),linetype = "dashed") +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title = "(b)Cumulative Absolute Error in Test Set for the Number of Suicides in the Japan",
    x = "Term (Jan 2018 - Dec 2019)",
    y = "Comulative Absolutely Error",
    color = ""
  ) +
  scale_color_manual(values = c("purple", "green", "orange", "red"),
                     breaks = c("model(1)", "model(2)", "model(3)", "model(4)")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

plot(p_cumu_ae_jp)


# Combine the two plots for Japan vertically
combined_plot_suicide_jp <- p_pre_accu_jp / p_cumu_ae_jp +
  plot_layout(heights = c(1, 1)) +  # Set equal heights for both plots
  plot_annotation(tag_levels = 'a') &  # Automatically add labels (a), (b)
  theme(plot.tag = element_text(face = "bold"))  # Make labels bold

# Display the combined plot
print(combined_plot_suicide_jp)


