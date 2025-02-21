"""
> sessionInfo()
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
[1] parallel  stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] forecast_8.21.1     furrr_0.3.1         future_1.34.0       patchwork_1.2.0     bsts_0.9.10        
 [6] xts_0.13.2          BoomSpikeSlab_1.2.6 Boom_0.9.15         gridExtra_2.3       vars_1.6-0         
[11] lmtest_0.9-40       strucchange_1.5-3   sandwich_3.1-0      zoo_1.8-12          MASS_7.3-60        
[16] tseries_0.10-55     urca_1.3-3          KFAS_1.5.1          lubridate_1.9.3     forcats_1.0.0      
[21] stringr_1.5.1       dplyr_1.1.4         purrr_1.0.2         readr_2.1.5         tidyr_1.3.0        
[26] tibble_3.2.1        ggplot2_3.4.4       tidyverse_2.0.0    

loaded via a namespace (and not attached):
 [1] gtable_0.3.4      xfun_0.41         lattice_0.21-9    tzdb_0.4.0        quadprog_1.5-8   
 [6] vctrs_0.6.5       tools_4.3.2       generics_0.1.3    curl_5.2.0        fansi_1.0.6      
[11] pacman_0.5.1      pkgconfig_2.0.3   lifecycle_1.0.4   compiler_4.3.2    munsell_0.5.0    
[16] codetools_0.2-19  htmltools_0.5.7   yaml_2.3.8        pillar_1.9.0      parallelly_1.41.0
[21] nlme_3.1-163      fracdiff_1.5-3    tidyselect_1.2.0  digest_0.6.34     stringi_1.8.3    
[26] listenv_0.9.1     fastmap_1.1.1     grid_4.3.2        colorspace_2.1-0  cli_3.6.2        
[31] magrittr_2.0.3    utf8_1.2.4        withr_2.5.2       scales_1.3.0      timechange_0.2.0 
[36] TTR_0.24.4        rmarkdown_2.25    globals_0.16.3    quantmod_0.4.26   nnet_7.3-19      
[41] timeDate_4032.109 hms_1.1.3         evaluate_0.23     knitr_1.45        rlang_1.1.3      
[46] Rcpp_1.0.12       glue_1.7.0        rstudioapi_0.15.0 R6_2.5.1    
"""

# setup----------------------------------------------------------
#install.packages("pacman")
pacman::p_load(tidyverse,KFAS,urca,tseries,vars,gridExtra,bsts,patchwork,furrr,parallel,forecast)
#set seed
set.seed(42)
# Utils----------------------------------------------------------
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


# preprocessing----------------------------------------------------------
# create main data
# es_pop <- read.csv("data/es_pop.csv")
# query_es <- read.csv("data/query_es.csv")
# suicide_es <- read.csv("data/suicide_es.csv")


# es_pop$term <- as.Date(es_pop[[1]])
# query_es$term <- as.Date(query_es[[1]])
# suicide_es$term <- as.Date(suicide_es[[1]])

# es_pop <- es_pop[-1]
# query_es <- query_es[-1]
# suicide_es <- suicide_es[-1]

# es_pop <- es_pop[, c("term", setdiff(names(es_pop), "term"))]
# query_es <- query_es[, c("term", setdiff(names(query_es), "term"))]
# suicide_es <- suicide_es[, c("term", setdiff(names(suicide_es), "term"))]


# df_main_es <- suicide_es %>% filter (term <= "2019-12-01" & term >= "2004-01-01") %>%
#   left_join(es_pop, by = "term") %>%
#   left_join(query_es, by = "term")
# df_main_es <- df_main_es %>% arrange(term)

#calculate suicide rate
# df_main_es <- df_main_es %>%
#   mutate(
#     suicide_rate_total = (num_suicide_total / pop_total) * 100000,
#     suicide_rate_male = (num_suicide_male / pop_male) * 100000,
#     suicide_rate_female = (num_suicide_female / pop_female) * 100000
#   )
# # Write df_main_es to a CSV file in the data directory
# write.csv(df_main_es, "data/df_main_es.csv", row.names = FALSE)
# Read df_main_es from the data directory
df_main_es <- read.csv("data/df_main_es.csv")
df_main_es %>% names()

df_main_es$term <- as.Date(df_main_es$term)
  
# Read query_en_es from the data directory
query_en_es <- read.csv("data/query_en_es.csv")
query_en_es$query_en <- paste0('query_', query_en_es$query_en)
query_en_es$query_es <- paste0('query_', query_en_es$query_es)
# Replace spaces with underscores in query_en and query_es columns
query_en_es$query_en <- gsub(" ", "_", query_en_es$query_en)
query_en_es$query_es <- gsub(" ", "_", query_en_es$query_es)

# Filter query_en_es to keep only rows where query_es is in the column names of df_main_es
query_en_es <- query_en_es %>% filter(query_es %in% names(df_main_es))

query_columns_es <- grep("^query_", names(df_main_es), value = TRUE)
all_queries_present <- all(query_columns_es %in% query_en_es$query_es)
# Output the result
print(all_queries_present)

# Create a named vector for mapping query_es to query_en
query_mapping <- setNames(query_en_es$query_en, query_en_es$query_es)

# Rename the columns in df_main_es using the query_mapping
names(df_main_es) <- ifelse(names(df_main_es) %in% names(query_mapping), query_mapping[names(df_main_es)], names(df_main_es))

query_columns <- grep("^query_", names(df_main_es), value = TRUE)

#select scale columns
scale_column_names_es <- names(df_main_es)[which(names(df_main_es) == query_columns[1]):which(names(df_main_es) ==  "suicide_rate_female")]

# Apply the scale_max_min_vector function to each of the scale_column_names_es
for(column_name in scale_column_names_es){
  scaled_vector <- scale_max_min_vector(df_main_es, column_name, "2016-12-01")
  new_column_name <- paste(column_name, "scaled", sep = "_")
  df_main_es[[new_column_name]] <- scaled_vector
}

scaled_query_columns <- paste(query_columns, "scaled", sep = "_")
#ceate data frame for model
train_for_bsts_es_suicide <- df_main_es %>%
  filter(term <= "2016-12-01") %>%
  dplyr::select(suicide_rate_total_scaled, all_of(scaled_query_columns))

train_for_bsts_es_suicide_male <- df_main_es %>%
  filter(term <= "2016-12-01") %>%
  dplyr::select(suicide_rate_male_scaled, all_of(scaled_query_columns))

train_for_bsts_es_suicide_female <- df_main_es %>%
  filter(term <= "2016-12-01") %>%
  dplyr::select(suicide_rate_female_scaled, all_of(scaled_query_columns))

# Test data
test_for_bsts_es_suicide <- df_main_es %>%
  dplyr::select(suicide_rate_total_scaled, all_of(scaled_query_columns))

test_for_bsts_es_suicide_male <- df_main_es %>%
  dplyr::select(suicide_rate_male_scaled, all_of(scaled_query_columns))

test_for_bsts_es_suicide_female <- df_main_es %>%
  dplyr::select(suicide_rate_female_scaled, all_of(scaled_query_columns))

# Set the last 36 rows of the target columns to NA
test_for_bsts_es_suicide$suicide_rate_total_scaled[(nrow(test_for_bsts_es_suicide)-36+1):nrow(test_for_bsts_es_suicide)] <- NA
test_for_bsts_es_suicide_male$suicide_rate_male_scaled[(nrow(test_for_bsts_es_suicide_male)-36+1):nrow(test_for_bsts_es_suicide_male)] <- NA
test_for_bsts_es_suicide_female$suicide_rate_female_scaled[(nrow(test_for_bsts_es_suicide_female)-36+1):nrow(test_for_bsts_es_suicide_female)] <- NA


#create data frame for validation
#ES
val_es_suicide <- df_main_es$suicide_rate_total[df_main_es$term >= "2017-01-01" & df_main_es$term <= "2017-12-01"]
val_es_suicide_male <- df_main_es$suicide_rate_male[df_main_es$term >= "2017-01-01" & df_main_es$term <= "2017-12-01"]
val_es_suicide_female <- df_main_es$suicide_rate_female[df_main_es$term >= "2017-01-01" & df_main_es$term <= "2017-12-01"]

test_es_suicide <- data.frame(
  term = df_main_es$term[df_main_es$term >= "2018-01-01"],
  pop = df_main_es$pop_total[df_main_es$term >= "2018-01-01"],
  true_rate = df_main_es$suicide_rate_total[df_main_es$term >= "2018-01-01"],
  true_num = df_main_es$num_suicide_total[df_main_es$term >= "2018-01-01"]
)
#create data frame for test
test_es_suicide_male <- data.frame(
  term = df_main_es$term[df_main_es$term >= "2018-01-01"],
  pop = df_main_es$pop_male[df_main_es$term >= "2018-01-01"],
  true_rate = df_main_es$suicide_rate_male[df_main_es$term >= "2018-01-01"],
  true_num = df_main_es$num_suicide_male[df_main_es$term >= "2018-01-01"]
)

test_es_suicide_female <- data.frame(
  term = df_main_es$term[df_main_es$term >= "2018-01-01"],
  pop = df_main_es$pop_female[df_main_es$term >= "2018-01-01"],
  true_rate = df_main_es$suicide_rate_female[df_main_es$term >= "2018-01-01"],
  true_num = df_main_es$num_suicide_female[df_main_es$term >= "2018-01-01"]
)

# add_new_query(BZD) -----------------------------------------------------------
query_BZD <- read.csv("data/query_BZD.csv")
query_BZD$month <- as.Date(query_BZD$month)
query_BZD <- query_BZD %>% rename(term = month)
df_main_es_add_BZD <- df_main_es
#add new query about 
df_main_es_add_BZD <- left_join(df_main_es_add_BZD,query_BZD, by = "term")


query_BZD %>% names()
#scale BZD
for(column_name in c("query_benzodiacepina","query_benzodiacepinas")){
  scaled_vector <- scale_max_min_vector(df_main_es_add_BZD, column_name, "2016-12-01")
  new_column_name <- paste(column_name, "scaled", sep = "_")
  df_main_es_add_BZD[[new_column_name]] <- scaled_vector
}

# lag1
for(column_name in c("query_benzodiacepina_scaled","query_benzodiacepinas_scaled","query_alcohol_scaled")){
  new_column_name <- paste(column_name, "lag1", sep = "_")
  df_main_es_add_BZD[[new_column_name]] <- dplyr::lag(df_main_es_add_BZD[[column_name]], n = 1)
}

#df_main_es_add_BZD <- df_main_es_add_BZD　%>% filter(term >= "2004-02-01")

# Create training datasets for BSTS model (including BZD-related queries and lag1)
train_for_bsts_es_suicide_BZD <- df_main_es_add_BZD %>%
  filter(term <= "2016-12-01") %>%
  dplyr::select(suicide_rate_total_scaled, all_of(scaled_query_columns), 
                query_benzodiacepina_scaled, query_benzodiacepina_scaled_lag1,
                query_benzodiacepinas_scaled, query_benzodiacepinas_scaled_lag1,
                query_alcohol_scaled, query_alcohol_scaled_lag1)

train_for_bsts_es_suicide_male_BZD <- df_main_es_add_BZD %>%
  filter(term <= "2016-12-01") %>%
  dplyr::select(suicide_rate_male_scaled, all_of(scaled_query_columns), 
                query_benzodiacepina_scaled, query_benzodiacepina_scaled_lag1,
                query_benzodiacepinas_scaled, query_benzodiacepinas_scaled_lag1,
                query_alcohol_scaled, query_alcohol_scaled_lag1)

train_for_bsts_es_suicide_female_BZD <- df_main_es_add_BZD %>%
  filter(term <= "2016-12-01") %>%
  dplyr::select(suicide_rate_female_scaled, all_of(scaled_query_columns), 
                query_benzodiacepina_scaled, query_benzodiacepina_scaled_lag1,
                query_benzodiacepinas_scaled, query_benzodiacepinas_scaled_lag1,
                query_alcohol_scaled, query_alcohol_scaled_lag1)

# Create test datasets
test_for_bsts_es_suicide_BZD <- df_main_es_add_BZD %>%
  dplyr::select(suicide_rate_total_scaled, all_of(scaled_query_columns), 
                query_benzodiacepina_scaled, query_benzodiacepina_scaled_lag1,
                query_benzodiacepinas_scaled, query_benzodiacepinas_scaled_lag1,
                query_alcohol_scaled, query_alcohol_scaled_lag1)

test_for_bsts_es_suicide_male_BZD <- df_main_es_add_BZD %>%
  dplyr::select(suicide_rate_male_scaled, all_of(scaled_query_columns), 
                query_benzodiacepina_scaled, query_benzodiacepina_scaled_lag1,
                query_benzodiacepinas_scaled, query_benzodiacepinas_scaled_lag1,
                query_alcohol_scaled, query_alcohol_scaled_lag1)

test_for_bsts_es_suicide_female_BZD <- df_main_es_add_BZD %>%
  dplyr::select(suicide_rate_female_scaled, all_of(scaled_query_columns), 
                query_benzodiacepina_scaled, query_benzodiacepina_scaled_lag1,
                query_benzodiacepinas_scaled, query_benzodiacepinas_scaled_lag1,
                query_alcohol_scaled, query_alcohol_scaled_lag1)
# Set the last 36 rows of the target columns to NA
test_for_bsts_es_suicide_BZD$suicide_rate_total_scaled[(nrow(test_for_bsts_es_suicide_BZD)-36+1):nrow(test_for_bsts_es_suicide_BZD)] <- NA
test_for_bsts_es_suicide_male_BZD$suicide_rate_male_scaled[(nrow(test_for_bsts_es_suicide_male_BZD)-36+1):nrow(test_for_bsts_es_suicide_male_BZD)] <- NA
test_for_bsts_es_suicide_female_BZD$suicide_rate_female_scaled[(nrow(test_for_bsts_es_suicide_female_BZD)-36+1):nrow(test_for_bsts_es_suicide_female_BZD)] <- NA

# Create validation datasets
val_es_suicide_BZD <- df_main_es_add_BZD$suicide_rate_total[df_main_es_add_BZD$term >= "2017-01-01" & df_main_es_add_BZD$term <= "2017-12-01"]
val_es_suicide_male_BZD <- df_main_es_add_BZD$suicide_rate_male[df_main_es_add_BZD$term >= "2017-01-01" & df_main_es_add_BZD$term <= "2017-12-01"]
val_es_suicide_female_BZD <- df_main_es_add_BZD$suicide_rate_female[df_main_es_add_BZD$term >= "2017-01-01" & df_main_es_add_BZD$term <= "2017-12-01"]






ggplot(df_main_es_add_BZD, aes(x = term)) +
  geom_line(aes(y = query_benzodiacepina_scaled, color = "query_benzodiacepina_scaled")) +
  geom_line(aes(y = query_benzodiacepinas_scaled, color = "query_benzodiacepinas_scaled")) +
  labs(title = "BZD Query Trends (Scaled)",
       x = "Term",
       y = "Scaled Query Count",
       color = "Legend") +
  theme_minimal()


# descriptive statistics --------------------------------------------------
window()
p_trend_es <- ggplot(df_main_es, aes(x = term)) +
  geom_line(aes(y = suicide_rate_total, color = "Total", group = 1), size = 1) +
  geom_line(aes(y = suicide_rate_male, color = "Male", group = 1), size = 1) +
  geom_line(aes(y = suicide_rate_female, color = "Female", group = 1), size = 1) +
  theme_minimal() +
  labs(title = "ES Suicide Rate Over Time (Jan 2004 - Dec 2019)",
       x = "Term (Jan 2004 - Dec 2019)",
       y = "Suicide Rate",
       color = "Gender") +
  scale_y_continuous(limits = c(0, NA)) +
  scale_x_date(breaks = seq(from = min(na.omit(df_main_es$term)), 
                            to = max(na.omit(df_main_es$term)), 
                            by = "6 months")) +
  scale_color_manual(values = c("Total" = "black", "Male" = "blue", "Female" = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")
print(p_trend_es)


p_trend_es_scaled <- ggplot(df_main_es, aes(x = term)) +
  geom_line(aes(y = suicide_rate_total_scaled, color = "Total", group = 1), size = 1) +
  geom_line(aes(y = suicide_rate_male_scaled, color = "Male", group = 1), size = 1) +
  geom_line(aes(y = suicide_rate_female_scaled, color = "Female", group = 1), size = 1) +
  theme_minimal() +
  labs(title = "ES Suicide Rate Over Time (Jan 2004 - Dec 2019)",
       x = "Term (Jan 2004 - Dec 2019)",
       y = "Suicide Rate",
       color = "Gender") +
  scale_y_continuous(limits = c(0, NA)) +
  scale_x_date(breaks = seq(from = min(na.omit(df_main_es$term)), 
                            to = max(na.omit(df_main_es$term)), 
                            by = "6 months")) +
  scale_color_manual(values = c("Total" = "black", "Male" = "blue", "Female" = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")
print(p_trend_es_scaled)


#confirm yearly data
df_main_es %>%
  mutate(year = year(as.Date(term))) %>%  
  group_by(year) %>%
  summarise(total_suicide_rate = sum(suicide_rate_total, na.rm = TRUE),
            male_suicide_rate = sum(suicide_rate_male, na.rm = TRUE),
            female_suicide_rate = sum(suicide_rate_female, na.rm = TRUE)) %>%
  print()


#confirm ccorrelation
# Create scaled query columns
query_columns_scaled <- paste0(query_columns, "_scaled")

# Remove constant columns
constant_columns <- names(train_for_bsts_es_suicide)[sapply(train_for_bsts_es_suicide, function(x) length(unique(x)) == 1)]
query_columns_scaled_for_train <- setdiff(query_columns_scaled, constant_columns)

# 1. Compute correlation coefficients
cor_matrix_before <- cor(df_main_es[df_main_es$term <= as.Date("2017-12-31"), query_columns_scaled_for_train], 
                         df_main_es[df_main_es$term <= as.Date("2017-12-31"), c("suicide_rate_total_scaled", "suicide_rate_male_scaled", "suicide_rate_female_scaled")], 
                         use = "pairwise.complete.obs")

cor_matrix_after <- cor(df_main_es[df_main_es$term >= as.Date("2018-01-01"), query_columns_scaled_for_train], 
                        df_main_es[df_main_es$term >= as.Date("2018-01-01"), c("suicide_rate_total_scaled", "suicide_rate_male_scaled", "suicide_rate_female_scaled")], 
                        use = "pairwise.complete.obs")

# 2. Replace NA values with 0
cor_matrix_before[is.na(cor_matrix_before)] <- 0
cor_matrix_after[is.na(cor_matrix_after)] <- 0

# 3. Convert correlation matrices into data frames
df_cor_before <- as.data.frame(cor_matrix_before)
df_cor_after <- as.data.frame(cor_matrix_after)

# 4. Remove "_scaled" from variable names
df_cor_before$variable <- gsub("_scaled$", "", rownames(df_cor_before))
df_cor_after$variable <- gsub("_scaled$", "", rownames(df_cor_after))

# 5. Add the `test_set` column (0: before 2018, 1: after 2018)
df_cor_before$test_set <- 0
df_cor_after$test_set <- 1

# 6. Merge both datasets vertically
df_summary_cor <- rbind(df_cor_before, df_cor_after)

# 7. Rename columns for clarity
colnames(df_summary_cor) <- c("total", "male", "female", "variable", "test_set")

# 1. Remove "query_" and "_" from variable names for cleaner y-axis labels
df_summary_cor <- df_summary_cor %>%
  mutate(variable_label = gsub("query_|_", " ", variable),
         test_set = factor(test_set, levels = c(0, 1), labels = c("Train", "Test")))  # Convert test_set values

# 2. Convert variable to a factor to maintain ordering in the plot
df_summary_cor$variable_label <- factor(df_summary_cor$variable_label, levels = unique(df_summary_cor$variable_label))
df_summary_cor %>% names()
df_summary_cor %>% head()
# 3. Reshape the data for ggplot (long format)
df_summary_cor_long <- df_summary_cor %>%
  dplyr::select(variable_label, test_set, total, male, female) %>%
  pivot_longer(cols = c(total, male, female), names_to = "gender", values_to = "correlation")

df_summary_cor_long %>% names()

ggplot(df_summary_cor_long, aes(x = correlation, y = variable_label, fill = gender)) +
  geom_col(position = "dodge") +  # Bars are placed side by side
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # Vertical line at 0
  facet_wrap(~ test_set) +  # Split by test_set (0: before 2018, 1: after 2018)
  theme_minimal() +
  labs(title = "Correlation of Variables with Suicide Rates",
       x = "Correlation Coefficient",
       y = "Variable",
       fill = "Gender") +
  theme(axis.text.y = element_text(size = 10),  # Adjust text size if necessary
        legend.position = "bottom")


cor(df_summary_cor$male[df_summary_cor$test_set == "Train"], df_summary_cor$female[df_summary_cor$test_set == "Train"], use = "complete.obs")
cor(df_summary_cor$male[df_summary_cor$test_set == "Test"], df_summary_cor$female[df_summary_cor$test_set == "Test"], use = "complete.obs")


# 1. Compute the difference between Train and Test correlations
df_cor_diff <- df_summary_cor %>%
  pivot_wider(names_from = test_set, values_from = c(total, male, female)) %>%
  mutate(
    total_diff = total_Train - total_Test,
    male_diff = male_Train - male_Test,
    female_diff = female_Train - female_Test
  ) %>%
  dplyr::select(variable, total_diff, male_diff, female_diff)  # Keep only differences

# 2. Remove "query_" and replace "_" with spaces for better labels
df_cor_diff <- df_cor_diff %>%
  mutate(variable_label = gsub("query_|_", " ", variable))

# 3. Convert data to long format for ggplot
df_cor_diff_long <- df_cor_diff %>%
  pivot_longer(cols = c(total_diff, male_diff, female_diff), 
               names_to = "gender", values_to = "correlation_diff") %>%
  mutate(gender = gsub("_diff", "", gender))  # Clean column names for legend

ggplot(df_cor_diff_long, aes(x = correlation_diff, y = variable_label, fill = gender)) +
  geom_col(position = "dodge") +  # Bars placed side by side
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # Vertical reference line at 0
  theme_minimal() +
  labs(title = "Difference in Correlation Coefficients (Train - Test)",
       x = "Correlation Difference (Train - Test)",
       y = "Variable",
       fill = "Gender") +
  theme(axis.text.y = element_text(size = 10), 
        legend.position = "bottom")


cor(df_cor_diff$male_diff, df_cor_diff$female_diff, use = "complete.obs")



# Function to get the top 15 correlated variables for a given column and test_set
get_top_correlations <- function(df, target_column, test_set_value) {
  df %>%
    filter(test_set == test_set_value) %>%
    arrange(desc(!!sym(target_column))) %>%
    head(15) %>%
    mutate(formatted = paste0(variable_label, " (", round(!!sym(target_column), 3), ")")) %>%  # Append correlation in parentheses
    dplyr::select(formatted) %>%
    rename(!!target_column := formatted)  # Rename column for merging
}

# Get top 15 correlated queries for Train
top_train_total  <- get_top_correlations(df_summary_cor, "total", "Train")
top_train_male   <- get_top_correlations(df_summary_cor, "male", "Train")
top_train_female <- get_top_correlations(df_summary_cor, "female", "Train")

# Get top 15 correlated queries for Test
top_test_total  <- get_top_correlations(df_summary_cor, "total", "Test")
top_test_male   <- get_top_correlations(df_summary_cor, "male", "Test")
top_test_female <- get_top_correlations(df_summary_cor, "female", "Test")

# Combine into a single data frame for Train
df_top_train <- data.frame(
  rank = 1:15,
  total = top_train_total$total,
  male = top_train_male$male,
  female = top_train_female$female
)

# Combine into a single data frame for Test
df_top_test <- data.frame(
  rank = 1:15,
  total = top_test_total$total,
  male = top_test_male$male,
  female = top_test_female$female
)

# Show results
print("Top 15 Correlated Queries for Train:")
print(df_top_train)

print("Top 15 Correlated Queries for Test:")
print(df_top_test)

# Function to extract only the query name (remove correlation values in parentheses)
clean_query_names <- function(query_list) {
  str_remove(query_list, " \\(.*\\)")  # Remove everything inside and including the parentheses
}

# Clean up Train and Test data before comparison
train_total_clean  <- clean_query_names(df_top_train$total)
train_male_clean   <- clean_query_names(df_top_train$male)
train_female_clean <- clean_query_names(df_top_train$female)

test_total_clean  <- clean_query_names(df_top_test$total)
test_male_clean   <- clean_query_names(df_top_test$male)
test_female_clean <- clean_query_names(df_top_test$female)

# Function to calculate the proportion of overlap between Train and Test top 15 queries
calculate_overlap_ratio <- function(train_col, test_col) {
  overlap <- sum(train_col %in% test_col)  # Count matching queries
  return(overlap / 15)  # Compute proportion
}

# Compute overlap proportions for total, male, and female
total_overlap_ratio  <- calculate_overlap_ratio(train_total_clean, test_total_clean)
male_overlap_ratio   <- calculate_overlap_ratio(train_male_clean, test_male_clean)
female_overlap_ratio <- calculate_overlap_ratio(train_female_clean, test_female_clean)

# Create a summary data frame
df_overlap_ratio <- data.frame(
  category = c("total", "male", "female"),
  overlap_ratio = c(total_overlap_ratio, male_overlap_ratio, female_overlap_ratio)
)

# Display results
print("Proportion of Train Top 15 Queries that also appear in Test:")
print(df_overlap_ratio)


# split data by between Train and Test
df_wide <- df_summary_cor_long %>%
  pivot_wider(names_from = test_set, values_from = correlation)

gender_levels <- c("total", "male", "female")
df_wide$gender <- factor(df_wide$gender, levels = gender_levels)

#create scatter plot
ggplot(df_wide, aes(x = Train, y = Test, color = gender)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +  # genderごとの回帰直線を追加
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(x = "Train Correlation", y = "Test Correlation",
       title = "Train vs Test Correlation Scatter Plot") +
  theme_minimal() +
  scale_color_manual(values = c("total" = "blue", "female" = "red", "male" = "green"))

#create scatter plot with labels
ggplot(df_wide, aes(x = Train, y = Test, color = gender, label = variable_label)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +  # genderごとの回帰直線を追加
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  geom_text(vjust = -0.5, hjust = 0.5, size = 4) +  # ラベルを追加
  labs(x = "Train Correlation", y = "Test Correlation",
       title = "Train vs Test Correlation Scatter Plot") +
  theme_minimal() +
  scale_color_manual(values = c("total" = "blue", "female" = "red", "male" = "green"))


cor_results <- df_wide %>%
  group_by(gender) %>%
  summarise(correlation = cor(Train, Test, use = "complete.obs"))



# create scatter plot with labels
ggplot(df_wide, aes(x = Train, y = Test, color = gender, label = variable_label)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +  # genderごとの回帰直線を追加
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +  # ラベルを追加
  labs(x = "Train Correlation", y = "Test Correlation",
       title = "Train vs Test Correlation Between Suicide Rate and Search Queries") +
  theme_minimal() +
  scale_color_manual(values = c("total" = "blue", "male" = "green", "female" = "red")) +
  guides(color = guide_legend(order = 1)) +  # 凡例の順番指定
  facet_wrap(~ gender, scales = "free") +
  theme(
    strip.text = element_text(size = 14, face = "bold")  # ファセットタイトルのフォントサイズを大きく
  ) +
  geom_text(data = cor_results, aes(x = min(df_wide$Train), y = max(df_wide$Test), 
                                    label = paste0("r = ", round(correlation, 3))),
            color = "black", hjust = 0, size = 5, inherit.aes = FALSE)

#calculate corre;ation

# all----------------------------------------------------------
#  model1----------------------------------------------------------
# Add local level
ss_es_suicide_trend_seas <- AddLocalLevel(state.specification = list(), train_for_bsts_es_suicide$suicide_rate_total_scaled)
# Add seasonality
ss_es_suicide_trend_seas <- AddSeasonal(ss_es_suicide_trend_seas, train_for_bsts_es_suicide$suicide_rate_total_scaled, nseasons=12)

# Define grid search parameters
iterations <- c(2000, 4000, 8000, 16000)
burn_in_percentages <- c(0.1, 0.20, 0.30, 0.40)



# Set up parallel processing using furrr
plan(multisession, workers = detectCores() - 1)
# Perform grid search using parallel execution
model_selection_es_model_1 <- future_map_dfr(iterations, function(iter) {
  future_map_dfr(burn_in_percentages, function(burn_percent) {
    burn <- round(iter * burn_percent)
    
    # Fit the model
    model_bsts_es_suicide_trend_seas <- bsts(
      train_for_bsts_es_suicide$suicide_rate_total_scaled,
      state.specification = ss_es_suicide_trend_seas,
      niter = iter,
      family = "gaussian",
      seed = 42
    )
    
    # Make predictions
    pre_bsts_es_suicide_trend_seas <- predict(
      model_bsts_es_suicide_trend_seas, 
      horizon = 12,
      niter = iter,
      burn = burn,
      seed = 42
    )$mean
    print(pre_bsts_es_suicide_trend_seas)
    # Inverse scale transformation
    pre_bsts_es_suicide_rate_trend_seas <- scale_max_min_vector_inverse(
      pre_bsts_es_suicide_trend_seas, 
      min(df_main_es$suicide_rate_total[df_main_es$term <= "2016-12-01"]),
      max(df_main_es$suicide_rate_total[df_main_es$term <= "2016-12-01"])
    )
    # Calculate accuracy metrics
    accuracy_results <- forecast::accuracy(pre_bsts_es_suicide_rate_trend_seas, val_es_suicide)
    
    return(data.frame(
      iterations = iter,
      burn_in_percent = burn_percent,
      burn_in = burn,
      MAPE = accuracy_results[5],
      RMSE = accuracy_results[2],
      MAE = accuracy_results[3]
    ))
  })
})


plan(sequential)

# Find the best model based on MAPE
best_model_es_suicide_trend_seas <- model_selection_es_model_1[which.min(model_selection_es_model_1$MAPE), ]

# Fit the final model
model_bsts_es_suicide_trend_seas <- bsts(
  train_for_bsts_es_suicide$suicide_rate_total_scaled,
  state.specification = ss_es_suicide_trend_seas,
  niter = best_model_es_suicide_trend_seas$iterations,
  family = "gaussian",
  seed = 42
)

# Plot components
plot(model_bsts_es_suicide_trend_seas, "comp") +
  title("ES Model (1)")

# Predict suicide rate
pre_bsts_es_suicide_trend_seas <- predict(
  model_bsts_es_suicide_trend_seas,
  horizon = 36,
  niter = best_model_es_suicide_trend_seas$iterations,
  burn = best_model_es_suicide_trend_seas$burn_in,
  seed = 42
)$mean

pre_bsts_es_suicide_rate_trend_seas <- scale_max_min_vector_inverse(
  pre_bsts_es_suicide_trend_seas, 
  min(df_main_es$suicide_rate_total[df_main_es$term <= "2016-12-01"]), 
  max(df_main_es$suicide_rate_total[df_main_es$term <= "2016-12-01"])
)

# Predict suicide rate and number for test set
test_es_suicide$pre_rate_model_1 <- pre_bsts_es_suicide_rate_trend_seas[13:length(pre_bsts_es_suicide_rate_trend_seas)]
test_es_suicide$pre_num_model_1 <- (test_es_suicide$pre_rate_model_1 * test_es_suicide$pop) / 100000

# Confirm accuracy for test set
print(accuracy(test_es_suicide$pre_rate_model_1, test_es_suicide$true_rate))
print(accuracy(test_es_suicide$pre_num_model_1, test_es_suicide$true_num))

# model2-1--------------------------------------------------------------
# Add local level
ss_es_suicide_tv_sd <- AddLocalLevel(state.specification = list(), train_for_bsts_es_suicide$suicide_rate_total_scaled)
# Add seasonality
ss_es_suicide_tv_sd <- AddSeasonal(ss_es_suicide_tv_sd, train_for_bsts_es_suicide$suicide_rate_total_scaled, nseasons=12)
# Add regression
ss_es_suicide_tv_sd <- AddDynamicRegression(ss_es_suicide_tv_sd, formula(suicide_rate_total_scaled ~ 
                                                                          query_suicide_scaled + query_depression_scaled),
                                            data = train_for_bsts_es_suicide)

# Define grid search parameters
iterations <- c(2000, 4000, 8000, 16000)
burn_in_percentages <- c(0.1, 0.20, 0.30, 0.40)


# Set up parallel processing using furrr
plan(multisession, workers = detectCores() - 1)

# Perform grid search using parallel execution
model_selection_es_model_2 <- future_map_dfr(iterations, function(iter) {
  future_map_dfr(burn_in_percentages, function(burn_percent) {
    burn <- round(iter * burn_percent)
    
    # Fit the model
    model_bsts_es_suicide_tv_sd <- bsts(
      formula = train_for_bsts_es_suicide$suicide_rate_total_scaled,
      state.specification = ss_es_suicide_tv_sd,
      niter = iter,
      family = "gaussian",
      seed = 42
    )
    
    # Make predictions
    pre_bsts_es_suicide_tv_sd <- predict(
      model_bsts_es_suicide_tv_sd,
      newdata = test_for_bsts_es_suicide,
      niter = iter,
      burn = burn,
      seed = 42
    )
    
    #fixed
    pre_bsts_es_suicide_tv_sd <- pre_bsts_es_suicide_tv_sd$mean[(nrow(test_for_bsts_es_suicide)-36+1):(nrow(test_for_bsts_es_suicide)-24)]
    
    # Inverse scale transformation
    pre_bsts_es_suicide_rate_tv_sd <- scale_max_min_vector_inverse(
      pre_bsts_es_suicide_tv_sd, 
      min(df_main_es$suicide_rate_total[df_main_es$term <= "2016-12-01"]),
      max(df_main_es$suicide_rate_total[df_main_es$term <= "2016-12-01"])
    )
  
    # Calculate accuracy metrics
    accuracy_results <- forecast::accuracy(pre_bsts_es_suicide_rate_tv_sd, val_es_suicide)
    
    return(data.frame(
      iterations = iter,
      burn_in_percent = burn_percent,
      burn_in = burn,
      MAPE = accuracy_results[5],
      RMSE = accuracy_results[2],
      MAE = accuracy_results[3]
    ))
  })
})

plan(sequential)

# Find the best model based on MAPE
best_model_es_suicide_tv_sd <- model_selection_es_model_2[which.min(model_selection_es_model_2$MAPE), ]

# Fit the final model
model_bsts_es_suicide_tv_sd <- bsts(
  formula = train_for_bsts_es_suicide$suicide_rate_total_scaled,
  state.specification = ss_es_suicide_tv_sd,
  niter = best_model_es_suicide_tv_sd$iterations,
  family = "gaussian",
  seed = 42
)

# Plot components
plot(model_bsts_es_suicide_tv_sd, "comp") +
  title("ES Model (2)")

# Predict suicide rate
pre_bsts_es_suicide_tv_sd <- predict(
  model_bsts_es_suicide_tv_sd,
  newdata = test_for_bsts_es_suicide,
  niter = best_model_es_suicide_tv_sd$iterations,
  burn = best_model_es_suicide_tv_sd$burn_in,
  seed = 42
)$mean

pre_bsts_es_suicide_rate_tv_sd <- scale_max_min_vector_inverse(
  pre_bsts_es_suicide_tv_sd, 
  min(df_main_es$suicide_rate_total[df_main_es$term <= "2016-12-01"]), 
  max(df_main_es$suicide_rate_total[df_main_es$term <= "2016-12-01"])
)

# Predict suicide rate and number for test set
test_es_suicide$pre_rate_model_2 <- pre_bsts_es_suicide_rate_tv_sd[(nrow(test_for_bsts_es_suicide)-24+1):nrow(test_for_bsts_es_suicide)]
test_es_suicide$pre_num_model_2 <- (test_es_suicide$pre_rate_model_2 * test_es_suicide$pop) / 100000

# Confirm accuracy for test set
print(accuracy(test_es_suicide$pre_rate_model_2, test_es_suicide$true_rate))
print(accuracy(test_es_suicide$pre_num_model_2, test_es_suicide$true_num))




# model2-2 ------------------------------------------------------------------
# Add local level
ss_es_suicide_tv_all <- AddLocalLevel(state.specification = list(), train_for_bsts_es_suicide$suicide_rate_total_scaled)
# Add seasonality
ss_es_suicide_tv_all <- AddSeasonal(ss_es_suicide_tv_all, train_for_bsts_es_suicide$suicide_rate_total_scaled, nseasons=12)

# Create scaled query columns
query_columns_scaled <- paste0(query_columns, "_scaled")

# Remove constant columns
constant_columns <- names(train_for_bsts_es_suicide)[sapply(train_for_bsts_es_suicide, function(x) length(unique(x)) == 1)]
query_columns_scaled_for_train <- setdiff(query_columns_scaled, constant_columns)

# Add regression with all scaled query columns
ss_es_suicide_tv_all <- AddDynamicRegression(
  ss_es_suicide_tv_all, 
  as.formula(paste("suicide_rate_total_scaled ~", paste(query_columns_scaled_for_train, collapse = " + "))),
  data = train_for_bsts_es_suicide
)

# Define grid search parameters
iterations <- c(2000, 4000, 8000, 16000)
burn_in_percentages <- c(0.1, 0.20, 0.30, 0.40)

# Set up parallel processing using furrr
plan(multisession, workers = detectCores() - 1)

# Perform grid search using parallel execution
model_selection_es_model_3 <- future_map_dfr(iterations, function(iter) {
  future_map_dfr(burn_in_percentages, function(burn_percent) {
    burn <- round(iter * burn_percent)
    
    # Fit the model
    model_bsts_es_suicide_tv_all <- bsts(
      formula = train_for_bsts_es_suicide$suicide_rate_total_scaled,
      state.specification = ss_es_suicide_tv_all,
      niter = iter,
      family = "gaussian",
      seed = 42
    )
    
    # Make predictions
    pre_bsts_es_suicide_tv_all <- predict(
      model_bsts_es_suicide_tv_all,
      newdata = test_for_bsts_es_suicide,
      niter = iter,
      burn = burn,
      seed = 42
    )$mean[(nrow(test_for_bsts_es_suicide)-36+1):(nrow(test_for_bsts_es_suicide)-24)]
    
    # Inverse scale transformation
    pre_bsts_es_suicide_rate_tv_all <- scale_max_min_vector_inverse(
      pre_bsts_es_suicide_tv_all, 
      min(df_main_es$suicide_rate_total[df_main_es$term <= "2016-12-01"]),
      max(df_main_es$suicide_rate_total[df_main_es$term <= "2016-12-01"])
    )
    
    # Calculate accuracy metrics
    accuracy_results <- forecast::accuracy(pre_bsts_es_suicide_rate_tv_all, val_es_suicide)
    
    return(data.frame(
      iterations = iter,
      burn_in_percent = burn_percent,
      burn_in = burn,
      MAPE = accuracy_results[5],
      RMSE = accuracy_results[2],
      MAE = accuracy_results[3]
    ))
  })
})

plan(sequential)

# Find the best model based on MAPE
best_model_es_suicide_tv_all <- model_selection_es_model_3[which.min(model_selection_es_model_3$MAPE), ]

# Fit the final model
model_bsts_es_suicide_tv_all <- bsts(
  formula = train_for_bsts_es_suicide$suicide_rate_total_scaled,
  state.specification = ss_es_suicide_tv_all,
  niter = best_model_es_suicide_tv_all$iterations,
  family = "gaussian",
  seed = 42
)

# Plot components
plot(model_bsts_es_suicide_tv_all, "comp") +
  title("ES Model (3)")

# Predict suicide rate
pre_bsts_es_suicide_tv_all <- predict(
  model_bsts_es_suicide_tv_all,
  newdata = test_for_bsts_es_suicide,
  niter = best_model_es_suicide_tv_all$iterations,
  burn = best_model_es_suicide_tv_all$burn_in,
  seed = 42
)$mean

pre_bsts_es_suicide_rate_tv_all <- scale_max_min_vector_inverse(
  pre_bsts_es_suicide_tv_all, 
  min(df_main_es$suicide_rate_total[df_main_es$term <= "2016-12-01"]), 
  max(df_main_es$suicide_rate_total[df_main_es$term <= "2016-12-01"])
)

# Predict suicide rate and number for test set
test_es_suicide$pre_rate_model_3 <- pre_bsts_es_suicide_rate_tv_all[(nrow(test_for_bsts_es_suicide)-24+1):nrow(test_for_bsts_es_suicide)]
test_es_suicide$pre_num_model_3 <- (test_es_suicide$pre_rate_model_3 * test_es_suicide$pop) / 100000

# Confirm accuracy for test set
print(accuracy(test_es_suicide$pre_rate_model_3, test_es_suicide$true_rate))
print(accuracy(test_es_suicide$pre_num_model_3, test_es_suicide$true_num))


# model3 ------------------------------------------------------------------
# Add local level
ss_es_suicide_spike_slab <- AddLocalLevel(state.specification = list(), train_for_bsts_es_suicide$suicide_rate_total_scaled)

# Add seasonality
ss_es_suicide_spike_slab <- AddSeasonal(ss_es_suicide_spike_slab, train_for_bsts_es_suicide$suicide_rate_total_scaled, nseasons=12)

# Define the formula
formula_es <- as.formula(paste("suicide_rate_total_scaled ~", paste(query_columns_scaled_for_train, collapse = " + ")))

# Count the number of query
n_predictors <- length(query_columns_scaled_for_train)

# Define grid search parameters
iterations <- c(2000, 4000, 8000, 16000)
burn_in_percentages <- c(0.1, 0.20, 0.30, 0.40)
pi_values <- c(0.05, 0.1, 0.15, 0.20)

# Set up parallel processing using furrr
plan(multisession, workers = detectCores() - 1)

# Perform grid search using parallel execution
model_selection_es_model_4 <- future_map_dfr(iterations, function(iter) {
  future_map_dfr(burn_in_percentages, function(burn_percent) {
    future_map_dfr(pi_values, function(pi) {
      burn <- round(iter * burn_percent)
      expected_model_size <- n_predictors * pi
      
      # Fit the model
      model_bsts_es_suicide_spike_slab <- bsts(formula_es,
                                               data = train_for_bsts_es_suicide, 
                                               state.specification = ss_es_suicide_spike_slab, 
                                               niter = iter,
                                               expected.model.size = expected_model_size, 
                                               family = "gaussian", 
                                               seed = 42)
      
      # Make predictions
      pre_bsts_es_suicide_spike_slab <- predict(model_bsts_es_suicide_spike_slab,
                                                newdata = test_for_bsts_es_suicide,
                                                niter = iter,
                                                burn = burn,
                                                seed = 42)$mean[(nrow(test_for_bsts_es_suicide)-36+1):(nrow(test_for_bsts_es_suicide)-24)]
      
      # Inverse scale transformation
      pre_bsts_es_suicide_rate_spike_slab <- scale_max_min_vector_inverse(
        pre_bsts_es_suicide_spike_slab, 
        min(df_main_es$suicide_rate_total[df_main_es$term <= "2016-12-01"]),
        max(df_main_es$suicide_rate_total[df_main_es$term <= "2016-12-01"])
      )
      
      # Calculate accuracy metrics
      accuracy_results <- forecast::accuracy(pre_bsts_es_suicide_rate_spike_slab, val_es_suicide)
      
      return(data.frame(
        iterations = iter,
        burn_in_percent = burn_percent,
        pi = pi,
        expected_model_size = expected_model_size,
        burn_in = burn,
        MAPE = accuracy_results[5],
        RMSE = accuracy_results[2],
        MAE = accuracy_results[3]
      ))
    })
  })
})

plan(sequential)

# Find the best model based on MAPE
best_model_es_suicide_spike_slab <- model_selection_es_model_4[which.min(model_selection_es_model_4$MAPE), ]

# Fit the final model
final_model_bsts_es_suicide_spike_slab <- bsts(formula_es,
                                               data = train_for_bsts_es_suicide, 
                                               state.specification = ss_es_suicide_spike_slab, 
                                               niter = best_model_es_suicide_spike_slab$iterations,
                                               expected.model.size = best_model_es_suicide_spike_slab$expected_model_size, 
                                               family = "gaussian", 
                                               seed = 42)

# Plot components
plot(final_model_bsts_es_suicide_spike_slab, "comp") +
  title("ES Model (4)")

# Predict suicide rate
pre_bsts_es_suicide_spike_slab <- predict(
  final_model_bsts_es_suicide_spike_slab,
  newdata = test_for_bsts_es_suicide,
  niter = best_model_es_suicide_spike_slab$iterations,
  burn = best_model_es_suicide_spike_slab$burn_in,
  seed = 42
)$mean

pre_bsts_es_suicide_rate_spike_slab <- scale_max_min_vector_inverse(
  pre_bsts_es_suicide_spike_slab, 
  min(df_main_es$suicide_rate_total[df_main_es$term <= "2016-12-01"]), 
  max(df_main_es$suicide_rate_total[df_main_es$term <= "2016-12-01"])
)

# Predict suicide rate and number for test set
test_es_suicide$pre_rate_model_4 <- pre_bsts_es_suicide_rate_spike_slab[(nrow(test_for_bsts_es_suicide)-24+1):nrow(test_for_bsts_es_suicide)]
test_es_suicide$pre_num_model_4 <- (test_es_suicide$pre_rate_model_4 * test_es_suicide$pop) / 100000

# Confirm accuracy for test set
print(accuracy(test_es_suicide$pre_rate_model_4, test_es_suicide$true_rate))
print(accuracy(test_es_suicide$pre_num_model_4, test_es_suicide$true_num))


# male --------------------------------------------------------------------
# model1 ------------------------------------------------------------------
# Add local level
ss_es_suicide_trend_seas_male <- AddLocalLevel(state.specification = list(), train_for_bsts_es_suicide_male$suicide_rate_male_scaled)
# Add seasonality
ss_es_suicide_trend_seas_male <- AddSeasonal(ss_es_suicide_trend_seas_male, train_for_bsts_es_suicide_male$suicide_rate_male_scaled, nseasons=12)

# Define grid search parameters
iterations <- c(2000, 4000, 8000, 16000)
burn_in_percentages <- c(0.1, 0.20, 0.30, 0.40)

# Set up parallel processing using furrr
plan(multisession, workers = detectCores() - 1)

# Perform grid search using parallel execution
model_selection_es_model_1_male <- future_map_dfr(iterations, function(iter) {
  future_map_dfr(burn_in_percentages, function(burn_percent) {
    burn <- round(iter * burn_percent)
    
    # Fit the model
    model_bsts_es_suicide_trend_seas_male <- bsts(
      train_for_bsts_es_suicide_male$suicide_rate_male_scaled,
      state.specification = ss_es_suicide_trend_seas_male,
      niter = iter,
      family = "gaussian",
      seed = 42
    )
    
    # Make predictions
    pre_bsts_es_suicide_trend_seas_male <- predict(
      model_bsts_es_suicide_trend_seas_male, 
      horizon = 12,
      niter = iter,
      burn = burn,
      seed = 42
    )$mean
    
    # Inverse scale transformation
    pre_bsts_es_suicide_rate_trend_seas_male <- scale_max_min_vector_inverse(
      pre_bsts_es_suicide_trend_seas_male, 
      min(df_main_es$suicide_rate_male[df_main_es$term <= "2016-12-01"]),
      max(df_main_es$suicide_rate_male[df_main_es$term <= "2016-12-01"])
    )
    
    # Calculate accuracy metrics
    accuracy_results <- forecast::accuracy(pre_bsts_es_suicide_rate_trend_seas_male, val_es_suicide_male)
    
    return(data.frame(
      iterations = iter,
      burn_in_percent = burn_percent,
      burn_in = burn,
      MAPE = accuracy_results[5],
      RMSE = accuracy_results[2],
      MAE = accuracy_results[3]
    ))
  })
})

plan(sequential)

# Find the best model based on MAPE
best_model_es_suicide_trend_seas_male <- model_selection_es_model_1_male[which.min(model_selection_es_model_1_male$MAPE), ]

# Fit the final model
model_bsts_es_suicide_trend_seas_male <- bsts(
  train_for_bsts_es_suicide_male$suicide_rate_male_scaled,
  state.specification = ss_es_suicide_trend_seas_male,
  niter = best_model_es_suicide_trend_seas_male$iterations,
  family = "gaussian",
  seed = 42
)

# Plot components
plot(model_bsts_es_suicide_trend_seas_male, "comp") +
  title("ES Model (1) - Male")

# Predict suicide rate
pre_bsts_es_suicide_trend_seas_male <- predict(
  model_bsts_es_suicide_trend_seas_male,
  horizon = 36,
  niter = best_model_es_suicide_trend_seas_male$iterations,
  burn = best_model_es_suicide_trend_seas_male$burn_in,
  seed = 42
)$mean

pre_bsts_es_suicide_rate_trend_seas_male <- scale_max_min_vector_inverse(
  pre_bsts_es_suicide_trend_seas_male, 
  min(df_main_es$suicide_rate_male[df_main_es$term <= "2016-12-01"]), 
  max(df_main_es$suicide_rate_male[df_main_es$term <= "2016-12-01"])
)

# Predict suicide rate and number for test set
test_es_suicide_male$pre_rate_model_1_male <- pre_bsts_es_suicide_rate_trend_seas_male[13:length(pre_bsts_es_suicide_rate_trend_seas_male)]
test_es_suicide_male$pre_num_model_1_male <- (test_es_suicide_male$pre_rate_model_1_male * test_es_suicide_male$pop) / 100000

# Confirm accuracy for test set
print(accuracy(test_es_suicide_male$pre_rate_model_1_male, test_es_suicide_male$true_rate))
print(accuracy(test_es_suicide_male$pre_num_model_1_male, test_es_suicide_male$true_num))


# model2-1 ------------------------------------------------------------------
# Add local level
ss_es_suicide_tv_sd_male <- AddLocalLevel(state.specification = list(), train_for_bsts_es_suicide_male$suicide_rate_male_scaled)
# Add seasonality
ss_es_suicide_tv_sd_male <- AddSeasonal(ss_es_suicide_tv_sd_male, train_for_bsts_es_suicide_male$suicide_rate_male_scaled, nseasons=12)
# Add regression
ss_es_suicide_tv_sd_male <- AddDynamicRegression(ss_es_suicide_tv_sd_male, formula(suicide_rate_male_scaled ~ 
                                                                                     query_suicide_scaled + query_depression_scaled),
                                                 data = train_for_bsts_es_suicide_male)

# Define grid search parameters
iterations <- c(2000, 4000, 8000, 16000)
burn_in_percentages <- c(0.1, 0.20, 0.30, 0.40)

# Set up parallel processing using furrr
plan(multisession, workers = detectCores() - 1)

# Perform grid search using parallel execution
model_selection_es_model_2_male <- future_map_dfr(iterations, function(iter) {
  future_map_dfr(burn_in_percentages, function(burn_percent) {
    burn <- round(iter * burn_percent)
    
    # Fit the model
    model_bsts_es_suicide_tv_sd_male <- bsts(
      formula = train_for_bsts_es_suicide_male$suicide_rate_male_scaled,
      state.specification = ss_es_suicide_tv_sd_male,
      niter = iter,
      family = "gaussian",
      seed = 42
    )
    
    # Make predictions
    pre_bsts_es_suicide_tv_sd_male <- predict(
      model_bsts_es_suicide_tv_sd_male,
      newdata = test_for_bsts_es_suicide_male,
      niter = iter,
      burn = burn,
      seed = 42
    )$mean[(nrow(test_for_bsts_es_suicide_male)-36+1):(nrow(test_for_bsts_es_suicide_male)-24)]
    
    # Inverse scale transformation
    pre_bsts_es_suicide_rate_tv_sd_male <- scale_max_min_vector_inverse(
      pre_bsts_es_suicide_tv_sd_male, 
      min(df_main_es$suicide_rate_male[df_main_es$term <= "2016-12-01"]),
      max(df_main_es$suicide_rate_male[df_main_es$term <= "2016-12-01"])
    )
    
    # Calculate accuracy metrics
    accuracy_results <- forecast::accuracy(pre_bsts_es_suicide_rate_tv_sd_male, val_es_suicide_male)
    
    return(data.frame(
      iterations = iter,
      burn_in_percent = burn_percent,
      burn_in = burn,
      MAPE = accuracy_results[5],
      RMSE = accuracy_results[2],
      MAE = accuracy_results[3]
    ))
  })
})

plan(sequential)

# Find the best model based on MAPE
best_model_es_suicide_tv_sd_male <- model_selection_es_model_2_male[which.min(model_selection_es_model_2_male$MAPE), ]

# Fit the final model
model_bsts_es_suicide_tv_sd_male <- bsts(
  formula = train_for_bsts_es_suicide_male$suicide_rate_male_scaled,
  state.specification = ss_es_suicide_tv_sd_male,
  niter = best_model_es_suicide_tv_sd_male$iterations,
  family = "gaussian",
  seed = 42
)

# Plot components
plot(model_bsts_es_suicide_tv_sd_male, "comp") +
  title("ES Model (2) - Male")

# Predict suicide rate
pre_bsts_es_suicide_tv_sd_male <- predict(
  model_bsts_es_suicide_tv_sd_male,
  newdata = test_for_bsts_es_suicide_male,
  niter = best_model_es_suicide_tv_sd_male$iterations,
  burn = best_model_es_suicide_tv_sd_male$burn_in,
  seed = 42
)$mean

pre_bsts_es_suicide_rate_tv_sd_male <- scale_max_min_vector_inverse(
  pre_bsts_es_suicide_tv_sd_male, 
  min(df_main_es$suicide_rate_male[df_main_es$term <= "2016-12-01"]), 
  max(df_main_es$suicide_rate_male[df_main_es$term <= "2016-12-01"])
)

# Predict suicide rate and number for test set
test_es_suicide_male$pre_rate_model_2_male <- pre_bsts_es_suicide_rate_tv_sd_male[(nrow(test_for_bsts_es_suicide_male)-24+1):nrow(test_for_bsts_es_suicide_male)]
test_es_suicide_male$pre_num_model_2_male <- (test_es_suicide_male$pre_rate_model_2_male * test_es_suicide_male$pop) / 100000

# Confirm accuracy for test set
print(accuracy(test_es_suicide_male$pre_rate_model_2_male, test_es_suicide_male$true_rate))
print(accuracy(test_es_suicide_male$pre_num_model_2_male, test_es_suicide_male$true_num))

# model2-2 ------------------------------------------------------------------
# Add local level
ss_es_suicide_tv_all_male <- AddLocalLevel(state.specification = list(), train_for_bsts_es_suicide_male$suicide_rate_male_scaled)
# Add seasonality
ss_es_suicide_tv_all_male <- AddSeasonal(ss_es_suicide_tv_all_male, train_for_bsts_es_suicide_male$suicide_rate_male_scaled, nseasons=12)

# Create scaled query columns
query_columns_scaled_male <- paste0(query_columns, "_scaled")

# Remove constant columns
constant_columns_male <- names(train_for_bsts_es_suicide_male)[sapply(train_for_bsts_es_suicide_male, function(x) length(unique(x)) == 1)]
query_columns_scaled_for_train_male <- setdiff(query_columns_scaled_male, constant_columns_male)

# Add regression with all scaled query columns
ss_es_suicide_tv_all_male <- AddDynamicRegression(
  ss_es_suicide_tv_all_male, 
  as.formula(paste("suicide_rate_male_scaled ~", paste(query_columns_scaled_for_train_male, collapse = " + "))),
  data = train_for_bsts_es_suicide_male
)

# Define grid search parameters
iterations <- c(2000, 4000, 8000, 16000)
burn_in_percentages <- c(0.1, 0.20, 0.30, 0.40)

# Set up parallel processing using furrr
plan(multisession, workers = detectCores() - 1)

# Perform grid search using parallel execution
model_selection_es_model_3_male <- future_map_dfr(iterations, function(iter) {
  future_map_dfr(burn_in_percentages, function(burn_percent) {
    burn <- round(iter * burn_percent)
    
    # Fit the model
    model_bsts_es_suicide_tv_all_male <- bsts(
      formula = train_for_bsts_es_suicide_male$suicide_rate_male_scaled,
      state.specification = ss_es_suicide_tv_all_male,
      niter = iter,
      family = "gaussian",
      seed = 42
    )
    
    # Make predictions
    pre_bsts_es_suicide_tv_all_male <- predict(
      model_bsts_es_suicide_tv_all_male,
      newdata = test_for_bsts_es_suicide_male,
      niter = iter,
      burn = burn,
      seed = 42
    )$mean[(nrow(test_for_bsts_es_suicide_male)-36+1):(nrow(test_for_bsts_es_suicide_male)-24)]
    
    # Inverse scale transformation
    pre_bsts_es_suicide_rate_tv_all_male <- scale_max_min_vector_inverse(
      pre_bsts_es_suicide_tv_all_male, 
      min(df_main_es$suicide_rate_male[df_main_es$term <= "2016-12-01"]),
      max(df_main_es$suicide_rate_male[df_main_es$term <= "2016-12-01"])
    )
    
    # Calculate accuracy metrics
    accuracy_results <- forecast::accuracy(pre_bsts_es_suicide_rate_tv_all_male, val_es_suicide_male)
    
    return(data.frame(
      iterations = iter,
      burn_in_percent = burn_percent,
      burn_in = burn,
      MAPE = accuracy_results[5],
      RMSE = accuracy_results[2],
      MAE = accuracy_results[3]
    ))
  })
})

plan(sequential)

# Find the best model based on MAPE
best_model_es_suicide_tv_all_male <- model_selection_es_model_3_male[which.min(model_selection_es_model_3_male$MAPE), ]

# Fit the final model
model_bsts_es_suicide_tv_all_male <- bsts(
  formula = train_for_bsts_es_suicide_male$suicide_rate_male_scaled,
  state.specification = ss_es_suicide_tv_all_male,
  niter = best_model_es_suicide_tv_all_male$iterations,
  family = "gaussian",
  seed = 42
)

# Plot components
plot(model_bsts_es_suicide_tv_all_male, "comp") +
  title("ES Model (3) - Male")

# Predict suicide rate
pre_bsts_es_suicide_tv_all_male <- predict(
  model_bsts_es_suicide_tv_all_male,
  newdata = test_for_bsts_es_suicide_male,
  niter = best_model_es_suicide_tv_all_male$iterations,
  burn = best_model_es_suicide_tv_all_male$burn_in,
  seed = 42
)$mean

pre_bsts_es_suicide_rate_tv_all_male <- scale_max_min_vector_inverse(
  pre_bsts_es_suicide_tv_all_male, 
  min(df_main_es$suicide_rate_male[df_main_es$term <= "2016-12-01"]), 
  max(df_main_es$suicide_rate_male[df_main_es$term <= "2016-12-01"])
)

# Predict suicide rate and number for test set
test_es_suicide_male$pre_rate_model_3_male <- pre_bsts_es_suicide_rate_tv_all_male[(nrow(test_for_bsts_es_suicide_male)-24+1):nrow(test_for_bsts_es_suicide_male)]
test_es_suicide_male$pre_num_model_3_male <- (test_es_suicide_male$pre_rate_model_3_male * test_es_suicide_male$pop) / 100000

# Confirm accuracy for test set
print(accuracy(test_es_suicide_male$pre_rate_model_3_male, test_es_suicide_male$true_rate))
print(accuracy(test_es_suicide_male$pre_num_model_3_male, test_es_suicide_male$true_num))


# model3 ------------------------------------------------------------------
# Add local level
ss_es_suicide_spike_slab_male <- AddLocalLevel(state.specification = list(), train_for_bsts_es_suicide_male$suicide_rate_male_scaled)

# Add seasonality
ss_es_suicide_spike_slab_male <- AddSeasonal(ss_es_suicide_spike_slab_male, train_for_bsts_es_suicide_male$suicide_rate_male_scaled, nseasons=12)

# Define the formula
formula_es_male <- as.formula(paste("suicide_rate_male_scaled ~", paste(query_columns_scaled_for_train_male, collapse = " + ")))

# Count the number of query
n_predictors_male <- length(query_columns_scaled_for_train_male)

# Define grid search parameters
iterations <- c(2000, 4000, 8000, 16000)
burn_in_percentages <- c(0.1, 0.20, 0.30, 0.40)
pi_values <- c(0.05, 0.1, 0.15, 0.20)

# Set up parallel processing using furrr
plan(multisession, workers = detectCores() - 1)

# Perform grid search using parallel execution
model_selection_es_model_4_male <- future_map_dfr(iterations, function(iter) {
  future_map_dfr(burn_in_percentages, function(burn_percent) {
    future_map_dfr(pi_values, function(pi) {
      burn <- round(iter * burn_percent)
      expected_model_size <- n_predictors_male * pi
      
      # Fit the model
      model_bsts_es_suicide_spike_slab_male <- bsts(formula_es_male,
                                                    data = train_for_bsts_es_suicide_male, 
                                                    state.specification = ss_es_suicide_spike_slab_male, 
                                                    niter = iter,
                                                    expected.model.size = expected_model_size, 
                                                    family = "gaussian", 
                                                    seed = 42)
      
      # Make predictions
      pre_bsts_es_suicide_spike_slab_male <- predict(model_bsts_es_suicide_spike_slab_male,
                                                     newdata = test_for_bsts_es_suicide_male,
                                                     niter = iter,
                                                     burn = burn,
                                                     seed = 42)$mean[(nrow(test_for_bsts_es_suicide_male)-36+1):(nrow(test_for_bsts_es_suicide_male)-24)]
      
      # Inverse scale transformation
      pre_bsts_es_suicide_rate_spike_slab_male <- scale_max_min_vector_inverse(
        pre_bsts_es_suicide_spike_slab_male, 
        min(df_main_es$suicide_rate_male[df_main_es$term <= "2016-12-01"]),
        max(df_main_es$suicide_rate_male[df_main_es$term <= "2016-12-01"])
      )
      
      # Calculate accuracy metrics
      accuracy_results <- forecast::accuracy(pre_bsts_es_suicide_rate_spike_slab_male, val_es_suicide_male)
      
      return(data.frame(
        iterations = iter,
        burn_in_percent = burn_percent,
        pi = pi,
        expected_model_size = expected_model_size,
        burn_in = burn,
        MAPE = accuracy_results[5],
        RMSE = accuracy_results[2],
        MAE = accuracy_results[3]
      ))
    })
  })
})

plan(sequential)

# Find the best model based on MAPE
best_model_es_suicide_spike_slab_male <- model_selection_es_model_4_male[which.min(model_selection_es_model_4_male$MAPE), ]

# Fit the final model
final_model_bsts_es_suicide_spike_slab_male <- bsts(formula_es_male,
                                                    data = train_for_bsts_es_suicide_male, 
                                                    state.specification = ss_es_suicide_spike_slab_male, 
                                                    niter = best_model_es_suicide_spike_slab_male$iterations,
                                                    expected.model.size = best_model_es_suicide_spike_slab_male$expected_model_size, 
                                                    family = "gaussian", 
                                                    seed = 42)

# Plot components
plot(final_model_bsts_es_suicide_spike_slab_male, "comp") +
  title("ES Model (4) - Male")

# Predict suicide rate
pre_bsts_es_suicide_spike_slab_male <- predict(
  final_model_bsts_es_suicide_spike_slab_male,
  newdata = test_for_bsts_es_suicide_male,
  niter = best_model_es_suicide_spike_slab_male$iterations,
  burn = best_model_es_suicide_spike_slab_male$burn_in,
  seed = 42
)$mean

pre_bsts_es_suicide_rate_spike_slab_male <- scale_max_min_vector_inverse(
  pre_bsts_es_suicide_spike_slab_male, 
  min(df_main_es$suicide_rate_male[df_main_es$term <= "2016-12-01"]), 
  max(df_main_es$suicide_rate_male[df_main_es$term <= "2016-12-01"])
)

# Predict suicide rate and number for test set
test_es_suicide_male$pre_rate_model_4_male <- pre_bsts_es_suicide_rate_spike_slab_male[(nrow(test_for_bsts_es_suicide_male)-24+1):nrow(test_for_bsts_es_suicide_male)]
test_es_suicide_male$pre_num_model_4_male <- (test_es_suicide_male$pre_rate_model_4_male * test_es_suicide_male$pop) / 100000

# Confirm accuracy for test set
print(accuracy(test_es_suicide_male$pre_rate_model_4_male, test_es_suicide_male$true_rate))
print(accuracy(test_es_suicide_male$pre_num_model_4_male, test_es_suicide_male$true_num))


# female ------------------------------------------------------------------
# model1 ------------------------------------------------------------------
# Add local level
ss_es_suicide_trend_seas_female <- AddLocalLevel(state.specification = list(), train_for_bsts_es_suicide_female$suicide_rate_female_scaled)
# Add seasonality
ss_es_suicide_trend_seas_female <- AddSeasonal(ss_es_suicide_trend_seas_female, train_for_bsts_es_suicide_female$suicide_rate_female_scaled, nseasons=12)

# Define grid search parameters
iterations <- c(2000, 4000, 8000, 16000)
burn_in_percentages <- c(0.1, 0.20, 0.30, 0.40)

# Set up parallel processing using furrr
plan(multisession, workers = detectCores() - 1)

# Perform grid search using parallel execution
model_selection_es_model_1_female <- future_map_dfr(iterations, function(iter) {
  future_map_dfr(burn_in_percentages, function(burn_percent) {
    burn <- round(iter * burn_percent)
    
    # Fit the model
    model_bsts_es_suicide_trend_seas_female <- bsts(
      train_for_bsts_es_suicide_female$suicide_rate_female_scaled,
      state.specification = ss_es_suicide_trend_seas_female,
      niter = iter,
      family = "gaussian",
      seed = 42
    )
    
    # Make predictions
    pre_bsts_es_suicide_trend_seas_female <- predict(
      model_bsts_es_suicide_trend_seas_female, 
      horizon = 12,
      niter = iter,
      burn = burn,
      seed = 42
    )$mean
    
    # Inverse scale transformation
    pre_bsts_es_suicide_rate_trend_seas_female <- scale_max_min_vector_inverse(
      pre_bsts_es_suicide_trend_seas_female, 
      min(df_main_es$suicide_rate_female[df_main_es$term <= "2016-12-01"]),
      max(df_main_es$suicide_rate_female[df_main_es$term <= "2016-12-01"])
    )
    
    # Calculate accuracy metrics
    accuracy_results <- forecast::accuracy(pre_bsts_es_suicide_rate_trend_seas_female, val_es_suicide_female)
    
    return(data.frame(
      iterations = iter,
      burn_in_percent = burn_percent,
      burn_in = burn,
      MAPE = accuracy_results[5],
      RMSE = accuracy_results[2],
      MAE = accuracy_results[3]
    ))
  })
})

plan(sequential)

# Find the best model based on MAPE
best_model_es_suicide_trend_seas_female <- model_selection_es_model_1_female[which.min(model_selection_es_model_1_female$MAPE), ]

# Fit the final model
model_bsts_es_suicide_trend_seas_female <- bsts(
  train_for_bsts_es_suicide_female$suicide_rate_female_scaled,
  state.specification = ss_es_suicide_trend_seas_female,
  niter = best_model_es_suicide_trend_seas_female$iterations,
  family = "gaussian",
  seed = 42
)

# Plot components
plot(model_bsts_es_suicide_trend_seas_female, "comp") +
  title("ES Model (1) - Female")

# Predict suicide rate
pre_bsts_es_suicide_trend_seas_female <- predict(
  model_bsts_es_suicide_trend_seas_female,
  horizon = 36,
  niter = best_model_es_suicide_trend_seas_female$iterations,
  burn = best_model_es_suicide_trend_seas_female$burn_in,
  seed = 42
)$mean

pre_bsts_es_suicide_rate_trend_seas_female <- scale_max_min_vector_inverse(
  pre_bsts_es_suicide_trend_seas_female, 
  min(df_main_es$suicide_rate_female[df_main_es$term <= "2016-12-01"]), 
  max(df_main_es$suicide_rate_female[df_main_es$term <= "2016-12-01"])
)

# Predict suicide rate and number for test set
test_es_suicide_female$pre_rate_model_1_female <- pre_bsts_es_suicide_rate_trend_seas_female[13:length(pre_bsts_es_suicide_rate_trend_seas_female)]
test_es_suicide_female$pre_num_model_1_female <- (test_es_suicide_female$pre_rate_model_1_female * test_es_suicide_female$pop) / 100000

# Confirm accuracy for test set
print(accuracy(test_es_suicide_female$pre_rate_model_1_female, test_es_suicide_female$true_rate))
print(accuracy(test_es_suicide_female$pre_num_model_1_female, test_es_suicide_female$true_num))

# model2-1 ------------------------------------------------------------------
# Add local level
ss_es_suicide_tv_sd_female <- AddLocalLevel(state.specification = list(), train_for_bsts_es_suicide_female$suicide_rate_female_scaled)
# Add seasonality
ss_es_suicide_tv_sd_female <- AddSeasonal(ss_es_suicide_tv_sd_female, train_for_bsts_es_suicide_female$suicide_rate_female_scaled, nseasons=12)
# Add regression
ss_es_suicide_tv_sd_female <- AddDynamicRegression(ss_es_suicide_tv_sd_female, formula(suicide_rate_female_scaled ~ 
                                                                                         query_suicide_scaled + query_depression_scaled),
                                                   data = train_for_bsts_es_suicide_female)

# Define grid search parameters
iterations <- c(2000, 4000, 8000, 16000)
burn_in_percentages <- c(0.1, 0.20, 0.30, 0.40)

# Set up parallel processing using furrr
plan(multisession, workers = detectCores() - 1)

?bst
# Perform grid search using parallel execution
model_selection_es_model_2_female <- future_map_dfr(iterations, function(iter) {
  future_map_dfr(burn_in_percentages, function(burn_percent) {
    burn <- round(iter * burn_percent)
    
    # Fit the model
    model_bsts_es_suicide_tv_sd_female <- bsts(
      formula = train_for_bsts_es_suicide_female$suicide_rate_female_scaled,
      state.specification = ss_es_suicide_tv_sd_female,
      niter = iter,
      family = "gaussian",
      seed = 42
    )
    
    # Make predictions
    pre_bsts_es_suicide_tv_sd_female <- predict(
      model_bsts_es_suicide_tv_sd_female,
      newdata = test_for_bsts_es_suicide_female,
      niter = iter,
      burn = burn,
      seed = 42
    )$mean[(nrow(test_for_bsts_es_suicide_female)-36+1):(nrow(test_for_bsts_es_suicide_female)-24)]
    
    # Inverse scale transformation
    pre_bsts_es_suicide_rate_tv_sd_female <- scale_max_min_vector_inverse(
      pre_bsts_es_suicide_tv_sd_female, 
      min(df_main_es$suicide_rate_female[df_main_es$term <= "2016-12-01"]),
      max(df_main_es$suicide_rate_female[df_main_es$term <= "2016-12-01"])
    )
    
    # Calculate accuracy metrics
    accuracy_results <- forecast::accuracy(pre_bsts_es_suicide_rate_tv_sd_female, val_es_suicide_female)
    
    return(data.frame(
      iterations = iter,
      burn_in_percent = burn_percent,
      burn_in = burn,
      MAPE = accuracy_results[5],
      RMSE = accuracy_results[2],
      MAE = accuracy_results[3]
    ))
  })
})

plan(sequential)

# Find the best model based on MAPE
best_model_es_suicide_tv_sd_female <- model_selection_es_model_2_female[which.min(model_selection_es_model_2_female$MAPE), ]

# Fit the final model
model_bsts_es_suicide_tv_sd_female <- bsts(
  formula = train_for_bsts_es_suicide_female$suicide_rate_female_scaled,
  state.specification = ss_es_suicide_tv_sd_female,
  niter = best_model_es_suicide_tv_sd_female$iterations,
  family = "gaussian",
  seed = 42
)

# Plot components
plot(model_bsts_es_suicide_tv_sd_female, "comp") +
  title("ES Model (2) - Female")

# Predict suicide rate
pre_bsts_es_suicide_tv_sd_female <- predict(
  model_bsts_es_suicide_tv_sd_female,
  newdata = test_for_bsts_es_suicide_female,
  niter = best_model_es_suicide_tv_sd_female$iterations,
  burn = best_model_es_suicide_tv_sd_female$burn_in,
  seed = 42
)$mean

pre_bsts_es_suicide_rate_tv_sd_female <- scale_max_min_vector_inverse(
  pre_bsts_es_suicide_tv_sd_female, 
  min(df_main_es$suicide_rate_female[df_main_es$term <= "2016-12-01"]), 
  max(df_main_es$suicide_rate_female[df_main_es$term <= "2016-12-01"])
)

# Predict suicide rate and number for test set
test_es_suicide_female$pre_rate_model_2_female <- pre_bsts_es_suicide_rate_tv_sd_female[(nrow(test_for_bsts_es_suicide_female)-24+1):nrow(test_for_bsts_es_suicide_female)]
test_es_suicide_female$pre_num_model_2_female <- (test_es_suicide_female$pre_rate_model_2_female * test_es_suicide_female$pop) / 100000

# Confirm accuracy for test set
print(accuracy(test_es_suicide_female$pre_rate_model_2_female, test_es_suicide_female$true_rate))
print(accuracy(test_es_suicide_female$pre_num_model_2_female, test_es_suicide_female$true_num))

# model2-2 ------------------------------------------------------------------
# Add local level
ss_es_suicide_tv_all_female <- AddLocalLevel(state.specification = list(), train_for_bsts_es_suicide_female$suicide_rate_female_scaled)
# Add seasonality
ss_es_suicide_tv_all_female <- AddSeasonal(ss_es_suicide_tv_all_female, train_for_bsts_es_suicide_female$suicide_rate_female_scaled, nseasons=12)

# Create scaled query columns
query_columns_scaled_female <- paste0(query_columns, "_scaled")

# Remove constant columns
constant_columns_female <- names(train_for_bsts_es_suicide_female)[sapply(train_for_bsts_es_suicide_female, function(x) length(unique(x)) == 1)]
query_columns_scaled_for_train_female <- setdiff(query_columns_scaled_female, constant_columns_female)

# Add regression with all scaled query columns
ss_es_suicide_tv_all_female <- AddDynamicRegression(
  ss_es_suicide_tv_all_female, 
  as.formula(paste("suicide_rate_female_scaled ~", paste(query_columns_scaled_for_train_female, collapse = " + "))),
  data = train_for_bsts_es_suicide_female
)

# Define grid search parameters
iterations <- c(2000, 4000, 8000, 16000)
burn_in_percentages <- c(0.1, 0.20, 0.30, 0.40)

# Set up parallel processing using furrr
plan(multisession, workers = detectCores() - 1)

# Perform grid search using parallel execution
model_selection_es_model_3_female <- future_map_dfr(iterations, function(iter) {
  future_map_dfr(burn_in_percentages, function(burn_percent) {
    burn <- round(iter * burn_percent)
    
    # Fit the model
    model_bsts_es_suicide_tv_all_female <- bsts(
      formula = train_for_bsts_es_suicide_female$suicide_rate_female_scaled,
      state.specification = ss_es_suicide_tv_all_female,
      niter = iter,
      family = "gaussian",
      seed = 42
    )
    
    # Make predictions
    pre_bsts_es_suicide_tv_all_female <- predict(
      model_bsts_es_suicide_tv_all_female,
      newdata = test_for_bsts_es_suicide_female,
      niter = iter,
      burn = burn,
      seed = 42
    )$mean[(nrow(test_for_bsts_es_suicide_female)-36+1):(nrow(test_for_bsts_es_suicide_female)-24)]
    
    # Inverse scale transformation
    pre_bsts_es_suicide_rate_tv_all_female <- scale_max_min_vector_inverse(
      pre_bsts_es_suicide_tv_all_female, 
      min(df_main_es$suicide_rate_female[df_main_es$term <= "2016-12-01"]),
      max(df_main_es$suicide_rate_female[df_main_es$term <= "2016-12-01"])
    )
    
    # Calculate accuracy metrics
    accuracy_results <- forecast::accuracy(pre_bsts_es_suicide_rate_tv_all_female, val_es_suicide_female)
    
    return(data.frame(
      iterations = iter,
      burn_in_percent = burn_percent,
      burn_in = burn,
      MAPE = accuracy_results[5],
      RMSE = accuracy_results[2],
      MAE = accuracy_results[3]
    ))
  })
})

plan(sequential)

# Find the best model based on MAPE
best_model_es_suicide_tv_all_female <- model_selection_es_model_3_female[which.min(model_selection_es_model_3_female$MAPE), ]

# Fit the final model
model_bsts_es_suicide_tv_all_female <- bsts(
  formula = train_for_bsts_es_suicide_female$suicide_rate_female_scaled,
  state.specification = ss_es_suicide_tv_all_female,
  niter = best_model_es_suicide_tv_all_female$iterations,
  family = "gaussian",
  seed = 42
)

# Plot components
plot(model_bsts_es_suicide_tv_all_female, "comp") +
  title("ES Model (3) - Female")

# Predict suicide rate
pre_bsts_es_suicide_tv_all_female <- predict(
  model_bsts_es_suicide_tv_all_female,
  newdata = test_for_bsts_es_suicide_female,
  niter = best_model_es_suicide_tv_all_female$iterations,
  burn = best_model_es_suicide_tv_all_female$burn_in,
  seed = 42
)$mean

pre_bsts_es_suicide_rate_tv_all_female <- scale_max_min_vector_inverse(
  pre_bsts_es_suicide_tv_all_female, 
  min(df_main_es$suicide_rate_female[df_main_es$term <= "2016-12-01"]), 
  max(df_main_es$suicide_rate_female[df_main_es$term <= "2016-12-01"])
)

# Predict suicide rate and number for test set
test_es_suicide_female$pre_rate_model_3_female <- pre_bsts_es_suicide_rate_tv_all_female[(nrow(test_for_bsts_es_suicide_female)-24+1):nrow(test_for_bsts_es_suicide_female)]
test_es_suicide_female$pre_num_model_3_female <- (test_es_suicide_female$pre_rate_model_3_female * test_es_suicide_female$pop) / 100000

# Confirm accuracy for test set
print(accuracy(test_es_suicide_female$pre_rate_model_3_female, test_es_suicide_female$true_rate))
print(accuracy(test_es_suicide_female$pre_num_model_3_female, test_es_suicide_female$true_num))

# model3 ------------------------------------------------------------------
# Add local level
ss_es_suicide_spike_slab_female <- AddLocalLevel(state.specification = list(), train_for_bsts_es_suicide_female$suicide_rate_female_scaled)

# Add seasonality
ss_es_suicide_spike_slab_female <- AddSeasonal(ss_es_suicide_spike_slab_female, train_for_bsts_es_suicide_female$suicide_rate_female_scaled, nseasons=12)

# Define the formula
formula_es_female <- as.formula(paste("suicide_rate_female_scaled ~", paste(query_columns_scaled_for_train_female, collapse = " + ")))

# Count the number of query
n_predictors_female <- length(query_columns_scaled_for_train_female)

# Define grid search parameters
iterations <- c(2000, 4000, 8000, 16000)
burn_in_percentages <- c(0.1, 0.20, 0.30, 0.40)
pi_values <- c(0.05, 0.1, 0.15, 0.20)

# Set up parallel processing using furrr
plan(multisession, workers = detectCores() - 1)

# Perform grid search using parallel execution
model_selection_es_model_4_female <- future_map_dfr(iterations, function(iter) {
  future_map_dfr(burn_in_percentages, function(burn_percent) {
    future_map_dfr(pi_values, function(pi) {
      burn <- round(iter * burn_percent)
      expected_model_size <- n_predictors_female * pi
      
      # Fit the model
      model_bsts_es_suicide_spike_slab_female <- bsts(formula_es_female,
                                                      data = train_for_bsts_es_suicide_female, 
                                                      state.specification = ss_es_suicide_spike_slab_female, 
                                                      niter = iter,
                                                      expected.model.size = expected_model_size, 
                                                      family = "gaussian", 
                                                      seed = 42)
      
      # Make predictions
      pre_bsts_es_suicide_spike_slab_female <- predict(model_bsts_es_suicide_spike_slab_female,
                                                       newdata = test_for_bsts_es_suicide_female,
                                                       niter = iter,
                                                       burn = burn,
                                                       seed = 42)$mean[(nrow(test_for_bsts_es_suicide_female)-36+1):(nrow(test_for_bsts_es_suicide_female)-24)]
      
      # Inverse scale transformation
      pre_bsts_es_suicide_rate_spike_slab_female <- scale_max_min_vector_inverse(
        pre_bsts_es_suicide_spike_slab_female, 
        min(df_main_es$suicide_rate_female[df_main_es$term <= "2016-12-01"]),
        max(df_main_es$suicide_rate_female[df_main_es$term <= "2016-12-01"])
      )
      
      # Calculate accuracy metrics
      accuracy_results <- forecast::accuracy(pre_bsts_es_suicide_rate_spike_slab_female, val_es_suicide_female)
      
      return(data.frame(
        iterations = iter,
        burn_in_percent = burn_percent,
        pi = pi,
        expected_model_size = expected_model_size,
        burn_in = burn,
        MAPE = accuracy_results[5],
        RMSE = accuracy_results[2],
        MAE = accuracy_results[3]
      ))
    })
  })
})

plan(sequential)

# Find the best model based on MAPE
best_model_es_suicide_spike_slab_female <- model_selection_es_model_4_female[which.min(model_selection_es_model_4_female$MAPE), ]

# Fit the final model
final_model_bsts_es_suicide_spike_slab_female <- bsts(formula_es_female,
                                                      data = train_for_bsts_es_suicide_female, 
                                                      state.specification = ss_es_suicide_spike_slab_female, 
                                                      niter = best_model_es_suicide_spike_slab_female$iterations,
                                                      expected.model.size = best_model_es_suicide_spike_slab_female$expected_model_size, 
                                                      family = "gaussian", 
                                                      seed = 42)

# Plot components
plot(final_model_bsts_es_suicide_spike_slab_female, "comp") +
  title("ES Model (4) - Female")

# Predict suicide rate
pre_bsts_es_suicide_spike_slab_female <- predict(
  final_model_bsts_es_suicide_spike_slab_female,
  newdata = test_for_bsts_es_suicide_female,
  niter = best_model_es_suicide_spike_slab_female$iterations,
  burn = best_model_es_suicide_spike_slab_female$burn_in,
  seed = 42
)$mean

pre_bsts_es_suicide_rate_spike_slab_female <- scale_max_min_vector_inverse(
  pre_bsts_es_suicide_spike_slab_female, 
  min(df_main_es$suicide_rate_female[df_main_es$term <= "2016-12-01"]), 
  max(df_main_es$suicide_rate_female[df_main_es$term <= "2016-12-01"])
)

# Predict suicide rate and number for test set
test_es_suicide_female$pre_rate_model_4_female <- pre_bsts_es_suicide_rate_spike_slab_female[(nrow(test_for_bsts_es_suicide_female)-24+1):nrow(test_for_bsts_es_suicide_female)]
test_es_suicide_female$pre_num_model_4_female <- (test_es_suicide_female$pre_rate_model_4_female * test_es_suicide_female$pop) / 100000

# Confirm accuracy for test set
print(accuracy(test_es_suicide_female$pre_rate_model_4_female, test_es_suicide_female$true_rate))
print(accuracy(test_es_suicide_female$pre_num_model_4_female, test_es_suicide_female$true_num))


# Compara models ----------------------------------------------------------
# all ---------------------------------------------------------------------
# Summary of the ES model
summary_model_bsts_es_suicide_spike_slab <- summary(final_model_bsts_es_suicide_spike_slab)
summary_model_bsts_es_suicide_spike_slab$coefficients[,"inc.prob"]

# Create data frame
coefficients_df_model_bsts_es_suicide_spike_slab <- data.frame(
  variable = rownames(summary_model_bsts_es_suicide_spike_slab$coefficients),
  inc_prob = summary_model_bsts_es_suicide_spike_slab$coefficients[, "inc.prob"],
  mean = summary_model_bsts_es_suicide_spike_slab$coefficients[, "mean"])

# Ordered by inclusion probability
coefficients_df_model_bsts_es_suicide_spike_slab <- coefficients_df_model_bsts_es_suicide_spike_slab %>%
  arrange(desc(inc_prob))
coefficients_df_model_bsts_es_suicide_spike_slab$variable[coefficients_df_model_bsts_es_suicide_spike_slab$inc_prob >= 0.1]

# Edit variable names for visualization
new_variable_names_label_spike_slab_es <- str_remove(str_remove(coefficients_df_model_bsts_es_suicide_spike_slab$variable, "^query_"), "_scaled$")
new_variable_names_label_spike_slab_es <- str_replace_all(new_variable_names_label_spike_slab_es, "_", " ")
coefficients_df_model_bsts_es_suicide_spike_slab$variable <- new_variable_names_label_spike_slab_es

p_coef_spike_slab_es <- ggplot(coefficients_df_model_bsts_es_suicide_spike_slab %>% filter(variable != "(Intercept)"), aes(x = reorder(variable, inc_prob), y = inc_prob, fill = mean < 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("FALSE" = "blue", "TRUE" = "red"),
                    labels = c("Plus", "Minus"),
                    name = "Coefficient Sign") + 
  labs(x = "Variables",
       y = "Inclusion Probability",
       title = "Inclusion Probabilities for Spike-and-Slab Regression in Spain",
       fill = "Coefficient Sign") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(color = "black", size = 12)
  )
plot(p_coef_spike_slab_es)

# Compare final results
# Accuracy
# Rate
accuracy(test_es_suicide$pre_rate_model_1, test_es_suicide$true_rate)
accuracy(test_es_suicide$pre_rate_model_2, test_es_suicide$true_rate)
accuracy(test_es_suicide$pre_rate_model_3, test_es_suicide$true_rate)
accuracy(test_es_suicide$pre_rate_model_4, test_es_suicide$true_rate)

# Number
accuracy(test_es_suicide$pre_num_model_1, test_es_suicide$true_num)
accuracy(test_es_suicide$pre_num_model_2, test_es_suicide$true_num)
accuracy(test_es_suicide$pre_num_model_3, test_es_suicide$true_num)
accuracy(test_es_suicide$pre_num_model_4, test_es_suicide$true_num)

# Cumulative AE for test
# Calculate AE
test_es_suicide$abs_error_model_1 <- abs(test_es_suicide$true_num - test_es_suicide$pre_num_model_1)
test_es_suicide$abs_error_model_2 <- abs(test_es_suicide$true_num - test_es_suicide$pre_num_model_2)
test_es_suicide$abs_error_model_3 <- abs(test_es_suicide$true_num - test_es_suicide$pre_num_model_3)
test_es_suicide$abs_error_model_4 <- abs(test_es_suicide$true_num - test_es_suicide$pre_num_model_4)

# Calculate cumulative AE
test_es_suicide$cumulative_abs_error_model_1 <- cumsum(test_es_suicide$abs_error_model_1)
test_es_suicide$cumulative_abs_error_model_2 <- cumsum(test_es_suicide$abs_error_model_2)
test_es_suicide$cumulative_abs_error_model_3 <- cumsum(test_es_suicide$abs_error_model_3)
test_es_suicide$cumulative_abs_error_model_4 <- cumsum(test_es_suicide$abs_error_model_4)

# Visualizing accuracy
p_pre_accu_es <- ggplot(test_es_suicide, aes(x = term)) +
  geom_line(aes(y = true_num, color = "TRUE", group = 1)) +
  geom_line(aes(y = pre_num_model_1, color = "model(1)", group = 1), linetype = "dashed") +
  geom_line(aes(y = pre_num_model_2, color = "model(2-1)", group = 1), linetype = "dashed") +
  geom_line(aes(y = pre_num_model_3, color = "model(2-2)", group = 1), linetype = "dashed") +
  geom_line(aes(y = pre_num_model_4, color = "model(3)", group = 1), linetype = "dashed") +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title = "Predicted Number of Suicides in Spain",
    x = "Term (Jan 2018 - Dec 2019)",
    y = "The number of Suicides",
    color = ""
  ) +
  scale_color_manual(values = c("black", "purple", "green", "orange", "red"),
                     breaks = c("TRUE", "model(1)", "model(2-1)", "model(2-2)", "model(3)")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")
plot(p_pre_accu_es)

# Visualizing cumulative absolute error
p_cumu_ae_es <- ggplot(test_es_suicide, aes(x = term)) +
  geom_line(aes(y = cumulative_abs_error_model_1,  color = "model(1)"),linetype = "dashed", group = 1) +
  geom_line(aes(y = cumulative_abs_error_model_2 ,  color = "model(2-1)"),linetype = "dashed", group = 1) +
  geom_line(aes(y = cumulative_abs_error_model_3, color = "model(2-2)"),linetype = "dashed", group = 1) +
  geom_line(aes(y = cumulative_abs_error_model_4, color = "model(3)"),linetype = "dashed", group = 1) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title = "(b)Cumulative Absolute Error in Test Set for the Number of Suicides in Spain",
    x = "Term (Jan 2018 - Dec 2019)",
    y = "Cumulative Absolute Error",
    color = ""
  ) +
  scale_color_manual(values = c("purple", "green", "orange", "red"),
                     breaks = c("model(1)", "model(2-1)", "model(2-2)", "model(3)")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")
plot(p_cumu_ae_es)

# Combine the two plots vertically
combined_plot_suicide_es <- p_pre_accu_es / p_cumu_ae_es +
  plot_layout(heights = c(1, 1)) +  # Set equal heights for both plots
  plot_annotation(tag_levels = 'a') &  # Automatically add labels (a), (b)
  theme(plot.tag = element_text(face = "bold"))  # Make labels bold

# Display the combined plot
plot(combined_plot_suicide_es)


# male --------------------------------------------------------------------
# Summary of the ES model (Male)
summary_model_bsts_es_suicide_spike_slab_male <- summary(final_model_bsts_es_suicide_spike_slab_male)
summary_model_bsts_es_suicide_spike_slab_male$coefficients[,"inc.prob"]

# Create data frame
coefficients_df_model_bsts_es_suicide_spike_slab_male <- data.frame(
  variable = rownames(summary_model_bsts_es_suicide_spike_slab_male$coefficients),
  inc_prob = summary_model_bsts_es_suicide_spike_slab_male$coefficients[, "inc.prob"],
  mean = summary_model_bsts_es_suicide_spike_slab_male$coefficients[, "mean"]
)

# Ordered by inclusion probability
coefficients_df_model_bsts_es_suicide_spike_slab_male <- coefficients_df_model_bsts_es_suicide_spike_slab_male %>%
  arrange(desc(inc_prob))

coefficients_df_model_bsts_es_suicide_spike_slab_male$variable[coefficients_df_model_bsts_es_suicide_spike_slab_male$inc_prob >= 0.1]

# Edit variable names for visualization
new_variable_names_label_spike_slab_es_male <- str_remove(str_remove(coefficients_df_model_bsts_es_suicide_spike_slab_male$variable, "^query_"), "_scaled$")
new_variable_names_label_spike_slab_es_male <- str_replace_all(new_variable_names_label_spike_slab_es_male, "_", " ")
coefficients_df_model_bsts_es_suicide_spike_slab_male$variable <- new_variable_names_label_spike_slab_es_male

p_coef_spike_slab_es_male <- ggplot(coefficients_df_model_bsts_es_suicide_spike_slab_male %>% filter(variable != "(Intercept)"), 
                                    aes(x = reorder(variable, inc_prob), y = inc_prob, fill = mean < 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("FALSE" = "blue", "TRUE" = "red"),
                    labels = c("Plus", "Minus"),
                    name = "Coefficient Sign") + 
  labs(x = "Variables",
       y = "Inclusion Probability",
       title = "Inclusion Probabilities for Spike-and-Slab Regression in Spain (Male)",
       fill = "Coefficient Sign") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(color = "black", size = 12)
  )
plot(p_coef_spike_slab_es_male)

# Compare final results
# Accuracy
# Rate
accuracy(test_es_suicide_male$pre_rate_model_1_male, test_es_suicide_male$true_rate)
accuracy(test_es_suicide_male$pre_rate_model_2_male, test_es_suicide_male$true_rate)
accuracy(test_es_suicide_male$pre_rate_model_3_male, test_es_suicide_male$true_rate)
accuracy(test_es_suicide_male$pre_rate_model_4_male, test_es_suicide_male$true_rate)

# Number
accuracy(test_es_suicide_male$pre_num_model_1_male, test_es_suicide_male$true_num)
accuracy(test_es_suicide_male$pre_num_model_2_male, test_es_suicide_male$true_num)
accuracy(test_es_suicide_male$pre_num_model_3_male, test_es_suicide_male$true_num)
accuracy(test_es_suicide_male$pre_num_model_4_male, test_es_suicide_male$true_num)

# Cumulative AE for test
# Calculate AE
test_es_suicide_male$abs_error_model_1_male <- abs(test_es_suicide_male$true_num - test_es_suicide_male$pre_num_model_1_male)
test_es_suicide_male$abs_error_model_2_male <- abs(test_es_suicide_male$true_num - test_es_suicide_male$pre_num_model_2_male)
test_es_suicide_male$abs_error_model_3_male <- abs(test_es_suicide_male$true_num - test_es_suicide_male$pre_num_model_3_male)
test_es_suicide_male$abs_error_model_4_male <- abs(test_es_suicide_male$true_num - test_es_suicide_male$pre_num_model_4_male)

# Calculate cumulative AE
test_es_suicide_male$cumulative_abs_error_model_1_male <- cumsum(test_es_suicide_male$abs_error_model_1_male)
test_es_suicide_male$cumulative_abs_error_model_2_male <- cumsum(test_es_suicide_male$abs_error_model_2_male)
test_es_suicide_male$cumulative_abs_error_model_3_male <- cumsum(test_es_suicide_male$abs_error_model_3_male)
test_es_suicide_male$cumulative_abs_error_model_4_male <- cumsum(test_es_suicide_male$abs_error_model_4_male)

# Visualizing accuracy
p_pre_accu_es_male <- ggplot(test_es_suicide_male, aes(x = term)) +
  geom_line(aes(y = true_num, color = "TRUE", group = 1)) +
  geom_line(aes(y = pre_num_model_1_male, color = "model(1)", group = 1), linetype = "dashed") +
  geom_line(aes(y = pre_num_model_2_male, color = "model(2-1)", group = 1), linetype = "dashed") +
  geom_line(aes(y = pre_num_model_3_male, color = "model(2-2)", group = 1), linetype = "dashed") +
  geom_line(aes(y = pre_num_model_4_male, color = "model(3)", group = 1), linetype = "dashed") +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title = "Predicted Number of Suicides in Spain (Male)",
    x = "Term (Jan 2018 - Dec 2019)",
    y = "The number of Suicides",
    color = ""
  ) +
  scale_color_manual(values = c("black", "purple", "green", "orange", "red"),
                     breaks = c("TRUE", "model(1)", "model(2-1)", "model(2-2)", "model(3)")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")
plot(p_pre_accu_es_male)

# Visualizing cumulative absolute error
p_cumu_ae_es_male <- ggplot(test_es_suicide_male, aes(x = term)) +
  geom_line(aes(y = cumulative_abs_error_model_1_male, color = "model(1)"), linetype = "dashed", group = 1) +
  geom_line(aes(y = cumulative_abs_error_model_2_male, color = "model(2-1)"), linetype = "dashed", group = 1) +
  geom_line(aes(y = cumulative_abs_error_model_3_male, color = "model(2-2)"), linetype = "dashed", group = 1) +
  geom_line(aes(y = cumulative_abs_error_model_4_male, color = "model(3)"), linetype = "dashed", group = 1) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title = "(b)Cumulative Absolute Error in Test Set for the Number of Suicides in Spain (Male)",
    x = "Term (Jan 2018 - Dec 2019)",
    y = "Cumulative Absolute Error",
    color = ""
  ) +
  scale_color_manual(values = c("purple", "green", "orange", "red"),
                     breaks = c("model(1)", "model(2-1)", "model(2-2)", "model(3)")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")
plot(p_cumu_ae_es_male)


# female --------------------------------------------------------------------
# Summary of the ES model (Female)
summary_model_bsts_es_suicide_spike_slab_female <- summary(final_model_bsts_es_suicide_spike_slab_female)
summary_model_bsts_es_suicide_spike_slab_female$coefficients[,"inc.prob"]

# Create data frame
coefficients_df_model_bsts_es_suicide_spike_slab_female <- data.frame(
  variable = rownames(summary_model_bsts_es_suicide_spike_slab_female$coefficients),
  inc_prob = summary_model_bsts_es_suicide_spike_slab_female$coefficients[, "inc.prob"],
  mean = summary_model_bsts_es_suicide_spike_slab_female$coefficients[, "mean"]
)

# Ordered by inclusion probability
coefficients_df_model_bsts_es_suicide_spike_slab_female <- coefficients_df_model_bsts_es_suicide_spike_slab_female %>%
  arrange(desc(inc_prob))

coefficients_df_model_bsts_es_suicide_spike_slab_female$variable[coefficients_df_model_bsts_es_suicide_spike_slab_female$inc_prob >= 0.1]

# Edit variable names for visualization
new_variable_names_label_spike_slab_es_female <- str_remove(str_remove(coefficients_df_model_bsts_es_suicide_spike_slab_female$variable, "^query_"), "_scaled$")
new_variable_names_label_spike_slab_es_female <- str_replace_all(new_variable_names_label_spike_slab_es_female, "_", " ")
coefficients_df_model_bsts_es_suicide_spike_slab_female$variable <- new_variable_names_label_spike_slab_es_female

p_coef_spike_slab_es_female <- ggplot(coefficients_df_model_bsts_es_suicide_spike_slab_female %>% filter(variable != "(Intercept)"), 
                                      aes(x = reorder(variable, inc_prob), y = inc_prob, fill = mean < 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("FALSE" = "blue", "TRUE" = "red"),
                    labels = c("Plus", "Minus"),
                    name = "Coefficient Sign") + 
  labs(x = "Variables",
       y = "Inclusion Probability",
       title = "Inclusion Probabilities for Spike-and-Slab Regression in Spain (Female)",
       fill = "Coefficient Sign") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(color = "black", size = 12)
  )
plot(p_coef_spike_slab_es_female)

# Compare final results
# Accuracy
# Rate
accuracy(test_es_suicide_female$pre_rate_model_1_female, test_es_suicide_female$true_rate)
accuracy(test_es_suicide_female$pre_rate_model_2_female, test_es_suicide_female$true_rate)
accuracy(test_es_suicide_female$pre_rate_model_3_female, test_es_suicide_female$true_rate)
accuracy(test_es_suicide_female$pre_rate_model_4_female, test_es_suicide_female$true_rate)

# Number
accuracy(test_es_suicide_female$pre_num_model_1_female, test_es_suicide_female$true_num)
accuracy(test_es_suicide_female$pre_num_model_2_female, test_es_suicide_female$true_num)
accuracy(test_es_suicide_female$pre_num_model_3_female, test_es_suicide_female$true_num)
accuracy(test_es_suicide_female$pre_num_model_4_female, test_es_suicide_female$true_num)

# Cumulative AE for test
# Calculate AE
test_es_suicide_female$abs_error_model_1_female <- abs(test_es_suicide_female$true_num - test_es_suicide_female$pre_num_model_1_female)
test_es_suicide_female$abs_error_model_2_female <- abs(test_es_suicide_female$true_num - test_es_suicide_female$pre_num_model_2_female)
test_es_suicide_female$abs_error_model_3_female <- abs(test_es_suicide_female$true_num - test_es_suicide_female$pre_num_model_3_female)
test_es_suicide_female$abs_error_model_4_female <- abs(test_es_suicide_female$true_num - test_es_suicide_female$pre_num_model_4_female)

# Calculate cumulative AE
test_es_suicide_female$cumulative_abs_error_model_1_female <- cumsum(test_es_suicide_female$abs_error_model_1_female)
test_es_suicide_female$cumulative_abs_error_model_2_female <- cumsum(test_es_suicide_female$abs_error_model_2_female)
test_es_suicide_female$cumulative_abs_error_model_3_female <- cumsum(test_es_suicide_female$abs_error_model_3_female)
test_es_suicide_female$cumulative_abs_error_model_4_female <- cumsum(test_es_suicide_female$abs_error_model_4_female)

# Visualizing accuracy
p_pre_accu_es_female <- ggplot(test_es_suicide_female, aes(x = term)) +
  geom_line(aes(y = true_num, color = "TRUE", group = 1)) +
  geom_line(aes(y = pre_num_model_1_female, color = "model(1)", group = 1), linetype = "dashed") +
  geom_line(aes(y = pre_num_model_2_female, color = "model(2-1)", group = 1), linetype = "dashed") +
  geom_line(aes(y = pre_num_model_3_female, color = "model(2-2)", group = 1), linetype = "dashed") +
  geom_line(aes(y = pre_num_model_4_female, color = "model(3)", group = 1), linetype = "dashed") +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title = "Predicted Number of Suicides in Spain (Female)",
    x = "Term (Jan 2018 - Dec 2019)",
    y = "The number of Suicides",
    color = ""
  ) +
  scale_color_manual(values = c("black", "purple", "green", "orange", "red"),
                     breaks = c("TRUE", "model(1)", "model(2-1)", "model(2-2)", "model(3)")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")
plot(p_pre_accu_es_female)

# Visualizing cumulative absolute error
p_cumu_ae_es_female <- ggplot(test_es_suicide_female, aes(x = term)) +
  geom_line(aes(y = cumulative_abs_error_model_1_female, color = "model(1)"), linetype = "dashed", group = 1) +
  geom_line(aes(y = cumulative_abs_error_model_2_female, color = "model(2-1)"), linetype = "dashed", group = 1) +
  geom_line(aes(y = cumulative_abs_error_model_3_female, color = "model(2-2)"), linetype = "dashed", group = 1) +
  geom_line(aes(y = cumulative_abs_error_model_4_female, color = "model(3)"), linetype = "dashed", group = 1) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title = "(b)Cumulative Absolute Error in Test Set for the Number of Suicides in Spain (Female)",
    x = "Term (Jan 2018 - Dec 2019)",
    y = "Cumulative Absolute Error",
    color = ""
  ) +
  scale_color_manual(values = c("purple", "green", "orange", "red"),
                     breaks = c("model(1)", "model(2-1)", "model(2-2)", "model(3)")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")
plot(p_cumu_ae_es_female)



# add bzd all -----------------------------------------------------------------
# Add local level component
ss_es_suicide_tv_alc_bzd <- AddLocalLevel(state.specification = list(), train_for_bsts_es_suicide_BZD$suicide_rate_total_scaled)

# Add seasonality component (12-month seasonality)
ss_es_suicide_tv_alc_bzd <- AddSeasonal(ss_es_suicide_tv_alc_bzd, train_for_bsts_es_suicide_BZD$suicide_rate_total_scaled, nseasons = 12)

# Add dynamic regression component using alcohol and BZD queries
ss_es_suicide_tv_alc_bzd <- AddDynamicRegression(ss_es_suicide_tv_alc_bzd, 
                                                 formula(suicide_rate_total_scaled ~   query_alcohol_scaled + query_benzodiacepinas_scaled),
                                                 data = train_for_bsts_es_suicide_BZD)

# Define grid search parameters
iterations <- c(2000, 4000, 8000, 16000)
burn_in_percentages <- c(0.1, 0.20, 0.30, 0.40)

# Set up parallel processing using furrr
plan(multisession, workers = detectCores() - 1)

# Perform grid search using parallel execution
model_selection_es_model_alc_bzd <- future_map_dfr(iterations, function(iter) {
  future_map_dfr(burn_in_percentages, function(burn_percent) {
    burn <- round(iter * burn_percent)
    
    # Fit the BSTS model
    model_bsts_es_suicide_tv_alc_bzd <- bsts(
      formula = suicide_rate_total_scaled ~ query_alcohol_scaled + query_benzodiacepinas_scaled,
      state.specification = ss_es_suicide_tv_alc_bzd,
      niter = iter,
      family = "gaussian",
      data = train_for_bsts_es_suicide_BZD,
      seed = 42
    )
    
    # Make predictions
    pre_bsts_es_suicide_tv_alc_bzd <- predict(
      model_bsts_es_suicide_tv_alc_bzd,
      newdata = test_for_bsts_es_suicide_BZD,
      niter = iter,
      burn = burn,
      seed = 42
    )$mean
    
    # Extract last 12 months of predictions
    pre_bsts_es_suicide_tv_alc_bzd <- pre_bsts_es_suicide_tv_alc_bzd[(nrow(test_for_bsts_es_suicide_BZD)-36+1):(nrow(test_for_bsts_es_suicide_BZD)-24)]
    
    # Inverse scale transformation
    pre_bsts_es_suicide_rate_tv_alc_bzd <- scale_max_min_vector_inverse(
      pre_bsts_es_suicide_tv_alc_bzd, 
      min(df_main_es_add_BZD$suicide_rate_total[df_main_es_add_BZD$term <= "2016-12-01"]),
      max(df_main_es_add_BZD$suicide_rate_total[df_main_es_add_BZD$term <= "2016-12-01"])
    )
    
    # Calculate accuracy metrics
    accuracy_results <- forecast::accuracy(pre_bsts_es_suicide_rate_tv_alc_bzd, val_es_suicide_BZD)
    
    return(data.frame(
      iterations = iter,
      burn_in_percent = burn_percent,
      burn_in = burn,
      MAPE = accuracy_results[5],
      RMSE = accuracy_results[2],
      MAE = accuracy_results[3]
    ))
  })
})

plan(sequential)

# Find the best model based on MAPE
best_model_es_suicide_tv_alc_bzd <- model_selection_es_model_alc_bzd[which.min(model_selection_es_model_alc_bzd$MAPE), ]

# Fit the final model
model_bsts_es_suicide_tv_alc_bzd <- bsts(
  formula = suicide_rate_total_scaled ~ query_alcohol_scaled + query_benzodiacepinas_scaled ,
  state.specification = ss_es_suicide_tv_alc_bzd,
  niter = best_model_es_suicide_tv_alc_bzd$iterations,
  family = "gaussian",
  data = train_for_bsts_es_suicide_BZD,
  seed = 42
)

# Plot model components
plot(model_bsts_es_suicide_tv_alc_bzd, "comp") +
  title("BSTS Model with Alcohol & BZD Queries")

# Predict suicide rate
pre_bsts_es_suicide_tv_alc_bzd <- predict(
  model_bsts_es_suicide_tv_alc_bzd,
  newdata = test_for_bsts_es_suicide_BZD,
  niter = best_model_es_suicide_tv_alc_bzd$iterations,
  burn = best_model_es_suicide_tv_alc_bzd$burn_in,
  seed = 42
)$mean

# Inverse scale transformation
pre_bsts_es_suicide_rate_tv_alc_bzd <- scale_max_min_vector_inverse(
  pre_bsts_es_suicide_tv_alc_bzd, 
  min(df_main_es_add_BZD$suicide_rate_total[df_main_es_add_BZD$term <= "2016-12-01"]),
  max(df_main_es_add_BZD$suicide_rate_total[df_main_es_add_BZD$term <= "2016-12-01"])
)

# Predict suicide rate and number for test set
test_es_suicide$pre_rate_model_alc_bzd <- pre_bsts_es_suicide_rate_tv_alc_bzd[(nrow(test_for_bsts_es_suicide_BZD)-24+1):nrow(test_for_bsts_es_suicide_BZD)]
test_es_suicide$pre_num_model_alc_bzd <- (test_es_suicide$pre_rate_model_alc_bzd * test_es_suicide$pop) / 100000

# Evaluate accuracy of predictions
print(accuracy(test_es_suicide$pre_rate_model_alc_bzd, test_es_suicide$true_rate))
print(accuracy(test_es_suicide$pre_num_model_alc_bzd, test_es_suicide$true_num))


p_pre_accu_es_BZD <- ggplot(test_es_suicide, aes(x = term)) +
  geom_line(aes(y = true_num, color = "TRUE", group = "TRUE"), size = 1) +
  geom_line(aes(y = pre_num_model_2, color = "Optimal Model First Validations", group = "Optimal Model First Validations"), 
            linetype = "dashed", size = 1) +
  geom_line(aes(y = pre_num_model_alc_bzd, color = "Alcohol + BZD", group = "Alcohol + BZD"), 
            linetype = "dashed", size = 1) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title = "Predicted Number of Suicides in Spain (Total)",
    x = "Term (Jan 2018 - Dec 2019)",
    y = "The Number of Suicides",
    color = ""
  ) +
  scale_color_manual(values = c("TRUE" = "black", 
                                "Optimal Model First Validations" = "purple", 
                                "Alcohol + BZD" = "green")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 12)  # 凡例のフォントサイズ調整
  )

p_pre_accu_es_BZD




# add BZD male --------------------------------------------------------------------
# Add local level component
ss_es_suicide_tv_alc_bzd_male <- AddLocalLevel(state.specification = list(), train_for_bsts_es_suicide_male_BZD$suicide_rate_male_scaled)

# Add seasonality component (12-month seasonality)
ss_es_suicide_tv_alc_bzd_male <- AddSeasonal(ss_es_suicide_tv_alc_bzd_male, train_for_bsts_es_suicide_male_BZD$suicide_rate_male_scaled, nseasons = 12)

# Add dynamic regression component using alcohol and BZD queries
ss_es_suicide_tv_alc_bzd_male <- AddDynamicRegression(ss_es_suicide_tv_alc_bzd_male, 
                                                      formula(suicide_rate_male_scaled ~ query_alcohol_scaled + query_benzodiacepinas_scaled),
                                                      data = train_for_bsts_es_suicide_male_BZD)

# Set up parallel processing using furrr
plan(multisession, workers = detectCores() - 1)

# Perform grid search using parallel execution
model_selection_es_model_alc_bzd_male <- future_map_dfr(iterations, function(iter) {
  future_map_dfr(burn_in_percentages, function(burn_percent) {
    burn <- round(iter * burn_percent)
    
    # Fit the BSTS model
    model_bsts_es_suicide_tv_alc_bzd_male <- bsts(
      formula = suicide_rate_male_scaled ~ query_alcohol_scaled + query_benzodiacepinas_scaled,
      state.specification = ss_es_suicide_tv_alc_bzd_male,
      niter = iter,
      family = "gaussian",
      data = train_for_bsts_es_suicide_male_BZD,
      seed = 42
    )
    
    # Make predictions
    pre_bsts_es_suicide_tv_alc_bzd_male <- predict(
      model_bsts_es_suicide_tv_alc_bzd_male,
      newdata = test_for_bsts_es_suicide_male_BZD,
      niter = iter,
      burn = burn,
      seed = 42
    )$mean
    
    # Extract last 12 months of predictions
    pre_bsts_es_suicide_tv_alc_bzd_male <- pre_bsts_es_suicide_tv_alc_bzd_male[(nrow(test_for_bsts_es_suicide_male_BZD)-36+1):(nrow(test_for_bsts_es_suicide_male_BZD)-24)]
    
    # Inverse scale transformation
    pre_bsts_es_suicide_rate_tv_alc_bzd_male <- scale_max_min_vector_inverse(
      pre_bsts_es_suicide_tv_alc_bzd_male, 
      min(df_main_es_add_BZD$suicide_rate_male[df_main_es_add_BZD$term <= "2016-12-01"]),
      max(df_main_es_add_BZD$suicide_rate_male[df_main_es_add_BZD$term <= "2016-12-01"])
    )
    
    # Calculate accuracy metrics
    accuracy_results <- forecast::accuracy(pre_bsts_es_suicide_rate_tv_alc_bzd_male, val_es_suicide_male_BZD)
    
    return(data.frame(
      iterations = iter,
      burn_in_percent = burn_percent,
      burn_in = burn,
      MAPE = accuracy_results[5],
      RMSE = accuracy_results[2],
      MAE = accuracy_results[3]
    ))
  })
})


plan(sequential)
# Fit the final model
best_model_es_suicide_tv_alc_bzd_male <- model_selection_es_model_alc_bzd_male[which.min(model_selection_es_model_alc_bzd_male$MAPE), ]

model_bsts_es_suicide_tv_alc_bzd_male <- bsts(
  formula = suicide_rate_male_scaled ~ query_alcohol_scaled + query_benzodiacepinas_scaled,
  state.specification = ss_es_suicide_tv_alc_bzd_male,
  niter = best_model_es_suicide_tv_alc_bzd_male$iterations,
  family = "gaussian",
  data = train_for_bsts_es_suicide_male_BZD,
  seed = 42
)

# Predict and transform
pre_bsts_es_suicide_tv_alc_bzd_male <- predict(
  model_bsts_es_suicide_tv_alc_bzd_male,
  newdata = test_for_bsts_es_suicide_male_BZD,
  niter = best_model_es_suicide_tv_alc_bzd_male$iterations,
  burn = best_model_es_suicide_tv_alc_bzd_male$burn_in,
  seed = 42
)$mean

# Inverse scale transformation
pre_bsts_es_suicide_rate_tv_alc_bzd_male <- scale_max_min_vector_inverse(
  pre_bsts_es_suicide_tv_alc_bzd_male, 
  min(df_main_es_add_BZD$suicide_rate_male[df_main_es_add_BZD$term <= "2016-12-01"]),
  max(df_main_es_add_BZD$suicide_rate_male[df_main_es_add_BZD$term <= "2016-12-01"])
)

# Predict suicide rate and number for test set
test_es_suicide_male$pre_rate_model_alc_bzd_male <- pre_bsts_es_suicide_rate_tv_alc_bzd_male[(nrow(test_for_bsts_es_suicide_male_BZD)-24+1):nrow(test_for_bsts_es_suicide_male_BZD)]
test_es_suicide_male$pre_num_model_alc_bzd_male <- (test_es_suicide_male$pre_rate_model_alc_bzd_male * test_es_suicide_male$pop) / 100000

# Evaluate accuracy of predictions
print(accuracy(test_es_suicide_male$pre_rate_model_alc_bzd_male, test_es_suicide_male$true_rate))
print(accuracy(test_es_suicide_male$pre_num_model_alc_bzd_male, test_es_suicide_male$true_num))
test_es_suicide_male %>% names()
p_pre_accu_es_BZD_male <- ggplot(test_es_suicide_male, aes(x = term)) +
  geom_line(aes(y = true_num, color = "TRUE", group = "TRUE"), size = 1) +
  geom_line(aes(y = pre_num_model_2_male, color = "Optimal Model First Validations", group = "Optimal Model First Validations"), 
            linetype = "dashed", size = 1) +
  geom_line(aes(y = pre_num_model_alc_bzd_male, color = "Alcohol + BZD", group = "Alcohol + BZD"), 
            linetype = "dashed", size = 1) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title = "Predicted Number of Suicides in Spain (Male)",
    x = "Term (Jan 2018 - Dec 2019)",
    y = "The Number of Suicides",
    color = ""
  ) +
  scale_color_manual(values = c("TRUE" = "black", 
                                "Optimal Model First Validations" = "purple", 
                                "Alcohol + BZD" = "green")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 12)  # 凡例のフォントサイズ調整
  )

p_pre_accu_es_BZD_male


# add BZD female ----------------------------------------------------------
# Add local level component
ss_es_suicide_tv_alc_bzd_female <- AddLocalLevel(state.specification = list(), train_for_bsts_es_suicide_female_BZD$suicide_rate_female_scaled)

# Add seasonality component (12-month seasonality)
ss_es_suicide_tv_alc_bzd_female <- AddSeasonal(ss_es_suicide_tv_alc_bzd_female, train_for_bsts_es_suicide_female_BZD$suicide_rate_female_scaled, nseasons = 12)

# Add dynamic regression component using alcohol and BZD queries
ss_es_suicide_tv_alc_bzd_female <- AddDynamicRegression(ss_es_suicide_tv_alc_bzd_female, 
                                                        formula(suicide_rate_female_scaled ~ query_alcohol_scaled + query_benzodiacepinas_scaled),
                                                        data = train_for_bsts_es_suicide_female_BZD)

# Set up parallel processing using furrr
plan(multisession, workers = detectCores() - 1)

# Perform grid search using parallel execution
model_selection_es_model_alc_bzd_female <- future_map_dfr(iterations, function(iter) {
  future_map_dfr(burn_in_percentages, function(burn_percent) {
    burn <- round(iter * burn_percent)
    
    # Fit the BSTS model
    model_bsts_es_suicide_tv_alc_bzd_female <- bsts(
      formula = suicide_rate_female_scaled ~ query_alcohol_scaled + query_benzodiacepinas_scaled,
      state.specification = ss_es_suicide_tv_alc_bzd_female,
      niter = iter,
      family = "gaussian",
      data = train_for_bsts_es_suicide_female_BZD,
      seed = 42
    )
    
    # Make predictions
    pre_bsts_es_suicide_tv_alc_bzd_female <- predict(
      model_bsts_es_suicide_tv_alc_bzd_female,
      newdata = test_for_bsts_es_suicide_female_BZD,
      niter = iter,
      burn = burn,
      seed = 42
    )$mean
    
    # Extract last 12 months of predictions
    pre_bsts_es_suicide_tv_alc_bzd_female <- pre_bsts_es_suicide_tv_alc_bzd_female[(nrow(test_for_bsts_es_suicide_female_BZD)-36+1):(nrow(test_for_bsts_es_suicide_female_BZD)-24)]
    
    # Inverse scale transformation
    pre_bsts_es_suicide_rate_tv_alc_bzd_female <- scale_max_min_vector_inverse(
      pre_bsts_es_suicide_tv_alc_bzd_female, 
      min(df_main_es_add_BZD$suicide_rate_female[df_main_es_add_BZD$term <= "2016-12-01"]),
      max(df_main_es_add_BZD$suicide_rate_female[df_main_es_add_BZD$term <= "2016-12-01"])
    )
    
    # Calculate accuracy metrics
    accuracy_results <- forecast::accuracy(pre_bsts_es_suicide_rate_tv_alc_bzd_female, val_es_suicide_female_BZD)
    
    return(data.frame(
      iterations = iter,
      burn_in_percent = burn_percent,
      burn_in = burn,
      MAPE = accuracy_results[5],
      RMSE = accuracy_results[2],
      MAE = accuracy_results[3]
    ))
  })
})

plan(sequential)

# Fit the final model
best_model_es_suicide_tv_alc_bzd_female <- model_selection_es_model_alc_bzd_female[which.min(model_selection_es_model_alc_bzd_female$MAPE), ]

model_bsts_es_suicide_tv_alc_bzd_female <- bsts(
  formula = suicide_rate_female_scaled ~ query_alcohol_scaled + query_benzodiacepinas_scaled,
  state.specification = ss_es_suicide_tv_alc_bzd_female,
  niter = best_model_es_suicide_tv_alc_bzd_female$iterations,
  family = "gaussian",
  data = train_for_bsts_es_suicide_female_BZD,
  seed = 42
)

# Predict and transform
pre_bsts_es_suicide_tv_alc_bzd_female <- predict(
  model_bsts_es_suicide_tv_alc_bzd_female,
  newdata = test_for_bsts_es_suicide_female_BZD,
  niter = best_model_es_suicide_tv_alc_bzd_female$iterations,
  burn = best_model_es_suicide_tv_alc_bzd_female$burn_in,
  seed = 42
)$mean

# Inverse scale transformation
pre_bsts_es_suicide_rate_tv_alc_bzd_female <- scale_max_min_vector_inverse(
  pre_bsts_es_suicide_tv_alc_bzd_female, 
  min(df_main_es_add_BZD$suicide_rate_female[df_main_es_add_BZD$term <= "2016-12-01"]),
  max(df_main_es_add_BZD$suicide_rate_female[df_main_es_add_BZD$term <= "2016-12-01"])
)

# Predict suicide rate and number for test set
test_es_suicide_female$pre_rate_model_alc_bzd_female <- pre_bsts_es_suicide_rate_tv_alc_bzd_female[(nrow(test_for_bsts_es_suicide_female_BZD)-24+1):nrow(test_for_bsts_es_suicide_female_BZD)]
test_es_suicide_female$pre_num_model_alc_bzd_female <- (test_es_suicide_female$pre_rate_model_alc_bzd_female * test_es_suicide_female$pop) / 100000

# Evaluate accuracy of predictions
print(accuracy(test_es_suicide_female$pre_rate_model_alc_bzd_female, test_es_suicide_female$true_rate))
print(accuracy(test_es_suicide_female$pre_num_model_alc_bzd_female, test_es_suicide_female$true_num))


p_pre_accu_es_BZD_female <- ggplot(test_es_suicide_female, aes(x = term)) +
  geom_line(aes(y = true_num, color = "TRUE", group = "TRUE"), size = 1) +
  geom_line(aes(y = pre_num_model_1_female, color = "Optimal Model First Validations", group = "Optimal Model First Validations"), 
            linetype = "dashed", size = 1) +
  geom_line(aes(y = pre_num_model_alc_bzd_female, color = "Alcohol + BZD", group = "Alcohol + BZD"), 
            linetype = "dashed", size = 1) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title = "Predicted Number of Suicides in Spain (Female)",
    x = "Term (Jan 2018 - Dec 2019)",
    y = "The Number of Suicides",
    color = ""
  ) +
  scale_color_manual(values = c("TRUE" = "black", 
                                "Optimal Model First Validations" = "purple", 
                                "Alcohol + BZD" = "green")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 12)  # 凡例のフォントサイズ調整
  )

p_pre_accu_es_BZD_female


#combined plot
p_combined <- p_pre_accu_es_BZD | p_pre_accu_es_BZD_male | p_pre_accu_es_BZD_female
p_combined

# season grid all ---------------------------------------------------------
# Define grid search parameters
iterations_nsea <- c(2000, 4000, 8000, 16000)
burn_in_percentages_nsea <- c(0.1, 0.20, 0.30, 0.40)
n_seasonal_nsea <- c(2,4,6,12)

# Set up parallel processing using furrr
plan(multisession, workers = detectCores() - 1)


# Perform grid search including n_seasonal using parallel execution
model_selection_es_model_nsea <- future_map_dfr(iterations_nsea, function(iter) {
  future_map_dfr(burn_in_percentages_nsea, function(burn_percent) {
    future_map_dfr(n_seasonal_nsea, function(nsea) {
      burn_nsea <- round(iter * burn_percent)
      season_duration_nsea <- 12/ nsea
      
      # Define state space model with varying seasonality
      ss_es_suicide_tv_sd_nsea <- AddLocalLevel(state.specification = list(), 
                                                train_for_bsts_es_suicide$suicide_rate_total_scaled)
      ss_es_suicide_tv_sd_nsea <- AddSeasonal(ss_es_suicide_tv_sd_nsea, 
                                              train_for_bsts_es_suicide$suicide_rate_total_scaled, 
                                              nseasons = nsea, 
                                              season.duration = season_duration_nsea)
      ss_es_suicide_tv_sd_nsea <- AddDynamicRegression(ss_es_suicide_tv_sd_nsea, 
                                                       formula(suicide_rate_total_scaled ~ 
                                                                 query_suicide_scaled + query_depression_scaled),
                                                       data = train_for_bsts_es_suicide)
      
      # Fit the model
      model_bsts_es_suicide_tv_sd_nsea <- bsts(
        formula = train_for_bsts_es_suicide$suicide_rate_total_scaled,
        state.specification = ss_es_suicide_tv_sd_nsea,
        niter = iter,
        family = "gaussian",
        seed = 42
      )
      
      # Make predictions
      pre_bsts_es_suicide_tv_sd_nsea <- predict(
        model_bsts_es_suicide_tv_sd_nsea,
        newdata = test_for_bsts_es_suicide,
        niter = iter,
        burn = burn_nsea,
        seed = 42
      )
      
      # Extract predicted values
      pre_bsts_es_suicide_tv_sd_nsea <- pre_bsts_es_suicide_tv_sd_nsea$mean[(nrow(test_for_bsts_es_suicide)-36+1):(nrow(test_for_bsts_es_suicide)-24)]
      
      # Inverse scale transformation
      pre_bsts_es_suicide_rate_tv_sd_nsea <- scale_max_min_vector_inverse(
        pre_bsts_es_suicide_tv_sd_nsea, 
        min(df_main_es$suicide_rate_total[df_main_es$term <= "2016-12-01"]),
        max(df_main_es$suicide_rate_total[df_main_es$term <= "2016-12-01"])
      )
      
      # Calculate accuracy metrics
      accuracy_results_nsea <- forecast::accuracy(pre_bsts_es_suicide_rate_tv_sd_nsea, val_es_suicide)
      
      return(data.frame(
        iterations = iter,
        burn_in_percent = burn_percent,
        burn_in = burn_nsea,
        n_seasonal = nsea,
        season_duration = season_duration_nsea,
        MAPE = accuracy_results_nsea[5],
        RMSE = accuracy_results_nsea[2],
        MAE = accuracy_results_nsea[3]
      ))
    })
  })
})

plan(sequential)

# Find the best model based on MAPE
best_model_es_suicide_tv_sd_nsea <- model_selection_es_model_nsea[which.min(model_selection_es_model_nsea$MAPE), ]

# Fit the final model with the best parameters
ss_es_suicide_tv_sd_best_nsea <- AddLocalLevel(state.specification = list(), 
                                               train_for_bsts_es_suicide$suicide_rate_total_scaled)
ss_es_suicide_tv_sd_best_nsea <- AddSeasonal(ss_es_suicide_tv_sd_best_nsea, 
                                             train_for_bsts_es_suicide$suicide_rate_total_scaled, 
                                             nseasons = best_model_es_suicide_tv_sd_nsea$n_seasonal, 
                                             season.duration = best_model_es_suicide_tv_sd_nsea$season_duration)
ss_es_suicide_tv_sd_best_nsea <- AddDynamicRegression(ss_es_suicide_tv_sd_best_nsea, 
                                                      formula(suicide_rate_total_scaled ~ 
                                                                query_suicide_scaled + query_depression_scaled),
                                                      data = train_for_bsts_es_suicide)

model_bsts_es_suicide_tv_sd_best_nsea <- bsts(
  formula = train_for_bsts_es_suicide$suicide_rate_total_scaled,
  state.specification = ss_es_suicide_tv_sd_best_nsea,
  niter = best_model_es_suicide_tv_sd_nsea$iterations,
  family = "gaussian",
  seed = 42
)

# Plot components
plot(model_bsts_es_suicide_tv_sd_best_nsea, "comp") +
  title("Best ES Model with Seasonality")

# Predict suicide rate
pre_bsts_es_suicide_tv_sd_best_nsea <- predict(
  model_bsts_es_suicide_tv_sd_best_nsea,
  newdata = test_for_bsts_es_suicide,
  niter = best_model_es_suicide_tv_sd_nsea$iterations,
  burn = best_model_es_suicide_tv_sd_nsea$burn_in,
  seed = 42
)$mean

pre_bsts_es_suicide_rate_tv_sd_best_nsea <- scale_max_min_vector_inverse(
  pre_bsts_es_suicide_tv_sd_best_nsea, 
  min(df_main_es$suicide_rate_total[df_main_es$term <= "2016-12-01"]), 
  max(df_main_es$suicide_rate_total[df_main_es$term <= "2016-12-01"])
)

# Predict suicide rate and number for test set
test_es_suicide$pre_rate_model_best_nsea <- pre_bsts_es_suicide_rate_tv_sd_best_nsea[(nrow(test_for_bsts_es_suicide)-24+1):nrow(test_for_bsts_es_suicide)]
test_es_suicide$pre_num_model_best_nsea <- (test_es_suicide$pre_rate_model_best_nsea * test_es_suicide$pop) / 100000

# Confirm accuracy for test set
print(accuracy(test_es_suicide$pre_rate_model_best_nsea, test_es_suicide$true_rate))
print(accuracy(test_es_suicide$pre_num_model_best_nsea, test_es_suicide$true_num))





p_pre_accu_n_seas <- ggplot(test_es_suicide, aes(x = term)) +
  geom_line(aes(y = true_num, color = "TRUE", group = "TRUE"), size = 1) +
  geom_line(aes(y = pre_num_model_2, color = "Seasonal 12-season in year", group = "Seasonal 12-season in year"), 
            linetype = "dashed", size = 1) +
  geom_line(aes(y = pre_num_model_best_nsea, color = "Seasonal 6-season in year", group = "Seasonal 6-season in year"), 
            linetype = "dashed", size = 1) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title = "Predicted Number of Suicides in Spain (Total)",
    x = "Term (Jan 2018 - Dec 2019)",
    y = "The Number of Suicides",
    color = "Seasonality Model"
  ) +
  scale_color_manual(values = c("TRUE" = "black", 
                                "Seasonal 12-season in year" = "purple", 
                                "Seasonal 6-season in year" = "green")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12, face = "bold")
  )

p_pre_accu_n_seas



# season grid male --------------------------------------------------------
# Define grid search parameters
iterations_nsea <- c(2000, 4000, 8000, 16000)
burn_in_percentages_nsea <- c(0.1, 0.20, 0.30, 0.40)
n_seasonal_nsea <- c(2,4,6,12)

# Set up parallel processing using furrr
plan(multisession, workers = detectCores() - 1)

# Perform grid search including n_seasonal using parallel execution for male model
model_selection_es_model_nsea_male <- future_map_dfr(iterations_nsea, function(iter) {
  future_map_dfr(burn_in_percentages_nsea, function(burn_percent) {
    future_map_dfr(n_seasonal_nsea, function(nsea) {
      burn_nsea <- round(iter * burn_percent)
      season_duration_nsea <- floor(12 / nsea)
      
      # Define state space model with varying seasonality for male
      ss_es_suicide_tv_sd_nsea_male <- AddLocalLevel(state.specification = list(), 
                                                     train_for_bsts_es_suicide_male$suicide_rate_male_scaled)
      ss_es_suicide_tv_sd_nsea_male <- AddSeasonal(ss_es_suicide_tv_sd_nsea_male, 
                                                   train_for_bsts_es_suicide_male$suicide_rate_male_scaled, 
                                                   nseasons = nsea, 
                                                   season.duration = season_duration_nsea)
      ss_es_suicide_tv_sd_nsea_male <- AddDynamicRegression(ss_es_suicide_tv_sd_nsea_male, 
                                                            formula(suicide_rate_male_scaled ~ 
                                                                      query_suicide_scaled + query_depression_scaled),
                                                            data = train_for_bsts_es_suicide_male)
      
      # Fit the model for male
      model_bsts_es_suicide_tv_sd_nsea_male <- bsts(
        formula = train_for_bsts_es_suicide_male$suicide_rate_male_scaled,
        state.specification = ss_es_suicide_tv_sd_nsea_male,
        niter = iter,
        family = "gaussian",
        seed = 42
      )
      
      # Make predictions for male
      pre_bsts_es_suicide_tv_sd_nsea_male <- predict(
        model_bsts_es_suicide_tv_sd_nsea_male,
        newdata = test_for_bsts_es_suicide_male,
        niter = iter,
        burn = burn_nsea,
        seed = 42
      )$mean[(nrow(test_for_bsts_es_suicide_male)-36+1):(nrow(test_for_bsts_es_suicide_male)-24)]
      
      # Inverse scale transformation for male
      pre_bsts_es_suicide_rate_tv_sd_nsea_male <- scale_max_min_vector_inverse(
        pre_bsts_es_suicide_tv_sd_nsea_male, 
        min(df_main_es$suicide_rate_male[df_main_es$term <= "2016-12-01"]),
        max(df_main_es$suicide_rate_male[df_main_es$term <= "2016-12-01"])
      )
      
      # Calculate accuracy metrics for male
      accuracy_results_nsea_male <- forecast::accuracy(pre_bsts_es_suicide_rate_tv_sd_nsea_male, val_es_suicide_male)
      
      return(data.frame(
        gender = "male",
        iterations = iter,
        burn_in_percent = burn_percent,
        burn_in = burn_nsea,
        n_seasonal = nsea,
        season_duration = season_duration_nsea,
        MAPE = accuracy_results_nsea_male[5],
        RMSE = accuracy_results_nsea_male[2],
        MAE = accuracy_results_nsea_male[3]
      ))
    })
  })
})

plan(sequential)

# Find the best model based on MAPE
best_model_es_suicide_tv_sd_nsea_male <- model_selection_es_model_nsea_male[which.min(model_selection_es_model_nsea_male$MAPE), ]

# Fit the final model with the best parameters
ss_es_suicide_tv_sd_best_nsea_male <- AddLocalLevel(state.specification = list(), 
                                                    train_for_bsts_es_suicide_male$suicide_rate_male_scaled)
ss_es_suicide_tv_sd_best_nsea_male <- AddSeasonal(ss_es_suicide_tv_sd_best_nsea_male, 
                                                  train_for_bsts_es_suicide_male$suicide_rate_male_scaled, 
                                                  nseasons = best_model_es_suicide_tv_sd_nsea_male$n_seasonal, 
                                                  season.duration = best_model_es_suicide_tv_sd_nsea_male$season_duration)
ss_es_suicide_tv_sd_best_nsea_male <- AddDynamicRegression(ss_es_suicide_tv_sd_best_nsea_male, 
                                                           formula(suicide_rate_male_scaled ~ 
                                                                     query_suicide_scaled + query_depression_scaled),
                                                           data = train_for_bsts_es_suicide_male)

model_bsts_es_suicide_tv_sd_best_nsea_male <- bsts(
  formula = train_for_bsts_es_suicide_male$suicide_rate_male_scaled,
  state.specification = ss_es_suicide_tv_sd_best_nsea_male,
  niter = best_model_es_suicide_tv_sd_nsea_male$iterations,
  family = "gaussian",
  seed = 42
)

# Predict suicide rate
pre_bsts_es_suicide_tv_sd_best_nsea_male <- predict(
  model_bsts_es_suicide_tv_sd_best_nsea_male,
  newdata = test_for_bsts_es_suicide_male,
  niter = best_model_es_suicide_tv_sd_nsea_male$iterations,
  burn = best_model_es_suicide_tv_sd_nsea_male$burn_in,
  seed = 42
)$mean

pre_bsts_es_suicide_rate_tv_sd_best_nsea_male <- scale_max_min_vector_inverse(
  pre_bsts_es_suicide_tv_sd_best_nsea_male, 
  min(df_main_es$suicide_rate_male[df_main_es$term <= "2016-12-01"]), 
  max(df_main_es$suicide_rate_male[df_main_es$term <= "2016-12-01"])
)

# Predict suicide rate and number for test set
test_es_suicide_male$pre_rate_model_best_nsea <- pre_bsts_es_suicide_rate_tv_sd_best_nsea_male[(nrow(test_for_bsts_es_suicide_male)-24+1):nrow(test_for_bsts_es_suicide_male)]
test_es_suicide_male$pre_num_model_best_nsea <- (test_es_suicide_male$pre_rate_model_best_nsea * test_es_suicide_male$pop) / 100000

# Confirm accuracy for test set
print(accuracy(test_es_suicide_male$pre_rate_model_best_nsea, test_es_suicide_male$true_rate))
print(accuracy(test_es_suicide_male$pre_num_model_best_nsea, test_es_suicide_male$true_num))

p_pre_accu_n_seas_male <- ggplot(test_es_suicide_male, aes(x = term)) +
  geom_line(aes(y = true_num, color = "TRUE", group = "TRUE"), size = 1) +
  geom_line(aes(y = pre_num_model_2_male, color = "Seasonal 12-season in year", group = "Seasonal 12-season in year"), 
            linetype = "dashed", size = 1) +
  geom_line(aes(y =pre_num_model_best_nsea, color = "Seasonal 6-season in year", group = "Seasonal 6-season in year"), 
            linetype = "dashed", size = 1) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title = "Predicted Number of Suicides in Spain (Male)",
    x = "Term (Jan 2018 - Dec 2019)",
    y = "The Number of Suicides",
    color = "Seasonality Model"
  ) +
  scale_color_manual(values = c("TRUE" = "black", 
                                "Seasonal 12-season in year" = "purple", 
                                "Seasonal 6-season in year" = "green")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12, face = "bold")
  )


# season grid female ------------------------------------------------------
# Define grid search parameters
iterations_nsea <- c(2000, 4000, 8000, 16000)
burn_in_percentages_nsea <- c(0.1, 0.20, 0.30, 0.40)
n_seasonal_nsea <- c(2,4,6,12)

# Set up parallel processing using furrr
plan(multisession, workers = detectCores() - 1)

# Perform grid search including n_seasonal using parallel execution for female model
model_selection_es_model_nsea_female <- future_map_dfr(iterations_nsea, function(iter) {
  future_map_dfr(burn_in_percentages_nsea, function(burn_percent) {
    future_map_dfr(n_seasonal_nsea, function(nsea) {
      burn_nsea <- round(iter * burn_percent)
      season_duration_nsea <- floor(12 / nsea)
      
      # Define state space model with varying seasonality for female
      ss_es_suicide_tv_sd_nsea_female <- AddLocalLevel(state.specification = list(), 
                                                       train_for_bsts_es_suicide_female$suicide_rate_female_scaled)
      ss_es_suicide_tv_sd_nsea_female <- AddSeasonal(ss_es_suicide_tv_sd_nsea_female, 
                                                     train_for_bsts_es_suicide_female$suicide_rate_female_scaled, 
                                                     nseasons = nsea, 
                                                     season.duration = season_duration_nsea)
      
      # Fit the model for female
      model_bsts_es_suicide_tv_sd_nsea_female <- bsts(
        formula = train_for_bsts_es_suicide_female$suicide_rate_female_scaled,
        state.specification = ss_es_suicide_tv_sd_nsea_female,
        niter = iter,
        family = "gaussian",
        seed = 42
      )
      
      # Make predictions for female
      pre_bsts_es_suicide_tv_sd_nsea_female <- predict(
        model_bsts_es_suicide_tv_sd_nsea_female,
        newdata = test_for_bsts_es_suicide_female,
        horizon = 12,
        niter = iter,
        burn = burn_nsea,
        seed = 42
      )$mean
      
      # Inverse scale transformation for female
      pre_bsts_es_suicide_rate_tv_sd_nsea_female <- scale_max_min_vector_inverse(
        pre_bsts_es_suicide_tv_sd_nsea_female, 
        min(df_main_es$suicide_rate_female[df_main_es$term <= "2016-12-01"]),
        max(df_main_es$suicide_rate_female[df_main_es$term <= "2016-12-01"])
      )
      
      # Calculate accuracy metrics for female
      accuracy_results_nsea_female <- forecast::accuracy(pre_bsts_es_suicide_rate_tv_sd_nsea_female, val_es_suicide_female)
      
      return(data.frame(
        gender = "female",
        iterations = iter,
        burn_in_percent = burn_percent,
        burn_in = burn_nsea,
        n_seasonal = nsea,
        season_duration = season_duration_nsea,
        MAPE = accuracy_results_nsea_female[5],
        RMSE = accuracy_results_nsea_female[2],
        MAE = accuracy_results_nsea_female[3]
      ))
    })
  })
})

plan(sequential)

# Find the best model based on MAPE
best_model_es_suicide_tv_sd_nsea_female <- model_selection_es_model_nsea_female[which.min(model_selection_es_model_nsea_female$MAPE), ]



# Fit the final model with the best parameters
ss_es_suicide_tv_sd_best_nsea_female <- AddLocalLevel(state.specification = list(), 
                                                      train_for_bsts_es_suicide_female$suicide_rate_female_scaled)
ss_es_suicide_tv_sd_best_nsea_female <- AddSeasonal(ss_es_suicide_tv_sd_best_nsea_female, 
                                                    train_for_bsts_es_suicide_female$suicide_rate_female_scaled, 
                                                    nseasons = best_model_es_suicide_tv_sd_nsea_female$n_seasonal, 
                                                    season.duration = best_model_es_suicide_tv_sd_nsea_female$season_duration)



# Fit the final model with the best parameters
model_bsts_es_suicide_tv_sd_best_nsea_female <- bsts(
  formula = train_for_bsts_es_suicide_female$suicide_rate_female_scaled,
  state.specification = ss_es_suicide_tv_sd_best_nsea_female,
  niter = best_model_es_suicide_tv_sd_nsea_female$iterations,
  family = "gaussian",
  seed = 42
)

# Predict suicide rate
pre_bsts_es_suicide_tv_sd_best_nsea_female <- predict(
  model_bsts_es_suicide_tv_sd_best_nsea_female,
  newdata = test_for_bsts_es_suicide_female,
  horizon = 36,
  niter = best_model_es_suicide_tv_sd_nsea_female$iterations,
  burn = best_model_es_suicide_tv_sd_nsea_female$burn_in,
  seed = 42
)$mean

pre_bsts_es_suicide_rate_tv_sd_best_nsea_female <- scale_max_min_vector_inverse(
  pre_bsts_es_suicide_tv_sd_best_nsea_female, 
  min(df_main_es$suicide_rate_female[df_main_es$term <= "2016-12-01"]), 
  max(df_main_es$suicide_rate_female[df_main_es$term <= "2016-12-01"])
)

# Predict suicide rate and number for test set
test_es_suicide_female$pre_rate_model_best_nsea <- pre_bsts_es_suicide_rate_tv_sd_best_nsea_female[13:length(pre_bsts_es_suicide_rate_tv_sd_best_nsea_female)]
test_es_suicide_female$pre_num_model_best_nsea <- (test_es_suicide_female$pre_rate_model_best_nsea * test_es_suicide_female$pop) / 100000

# Confirm accuracy for test set
print(accuracy(test_es_suicide_female$pre_rate_model_best_nsea, test_es_suicide_female$true_rate))
print(accuracy(test_es_suicide_female$pre_num_model_best_nsea, test_es_suicide_female$true_num))



p_pre_accu_n_seas_female <- ggplot(test_es_suicide_female, aes(x = term)) +
  geom_line(aes(y = true_num, color = "TRUE", group = "TRUE"), size = 1) +
  geom_line(aes(y = pre_num_model_1_female, color = "Seasonal 12-season in year", group = "Seasonal 12-season in year"), 
            linetype = "dashed", size = 1) +
  geom_line(aes(y = pre_num_model_best_nsea, color = "Seasonal 6-season in year", group = "Seasonal 6-season in year"), 
            linetype = "dashed", size = 1) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title = "Predicted Number of Suicides in Spain (Female)",
    x = "Term (Jan 2018 - Dec 2019)",
    y = "The Number of Suicides",
    color = "Seasonality Model"
  ) +
  scale_color_manual(values = c("TRUE" = "black", 
                                "Seasonal 12-season in year" = "purple", 
                                "Seasonal 6-season in year" = "green")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12, face = "bold")
  )

p_combined_n_seas <- p_pre_accu_n_seas | p_pre_accu_n_seas_male | p_pre_accu_n_seas_female


p_combined_n_seas




