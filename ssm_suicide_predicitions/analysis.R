# setup----------------------------------------------------------
pacman::p_load(tidyverse,tseries,urca,KFAS,forecast,future,furrr,gridExtra)
load("analysis.RData")
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

# create dataset----------------------------------------------------------
#read data
df_who <- read.csv("data/who_data_std.csv")
es_pop <- read.csv("data/es_pop.csv")
query_es <- read.csv("data/query_es.csv")
suicide_es <- read.csv("data/suicide_es.csv")
es_unemp <- read.csv("data/es_unemp.csv")
es_solar_temp <- read.csv("data/es_solar_temp.csv")

es_pop$term <- as.Date(es_pop$period)
query_es$term <- as.Date(query_es$month)
suicide_es$term <- as.Date(suicide_es$period)
es_unemp$term <- as.Date(es_unemp$period)
es_solar_temp$term <- as.Date(es_solar_temp$term)



es_pop <- es_pop[-1]
query_es <- query_es[-1]
suicide_es <- suicide_es[-1]
es_unemp <- es_unemp[-1]

# unemployment
es_unemp <- es_unemp %>% arrange(term)
for (i in 1:12) {
  es_unemp <- es_unemp %>%
    mutate(across(total_unemp:male_unemp, 
                  ~ lag(., i), 
                  .names = "{.col}_lag{i}"))
}

# Read query_en_es from the data directory
query_en_es <- read.csv("data/query_en_es.csv")
query_en_es$query_en <- paste0('query_', query_en_es$query_en)
query_en_es$query_es <- paste0('query_', query_en_es$query_es)

# Replace spaces with underscores in query_en and query_es columns
query_en_es$query_en <- gsub(" ", "_", query_en_es$query_en)
query_en_es$query_es <- gsub(" ", "_", query_en_es$query_es)

query_en_es$query_es %>% unique()

# Filter query_en_es to keep only rows where query_es is in the column names of query_es
col_query_es <- query_es %>% names()
query_en_es <- query_en_es %>%
  filter(query_es %in% col_query_es) %>%
  mutate_all(~ replace(., is.na(.), 0))
# Extract columns that start with "query_"
query_columns_es <- grep("^query_", names(query_es), value = TRUE)

# Check if all query columns are present in query_en_es
all_queries_present <- all(query_columns_es %in% query_en_es$query_es)
query_columns_es[!(query_columns_es %in% query_en_es$query_es)]

# Output the result
print(all_queries_present)

# Create a named vector for mapping query_es to query_en
query_mapping <- setNames(query_en_es$query_en, query_en_es$query_es)

# Rename the columns in query_es using the query_mapping
names(query_es) <- ifelse(names(query_es) %in% names(query_mapping), query_mapping[names(query_es)], names(query_es))

# Extract query sets based on efficiency flags
total_query <- query_en_es$query_en[query_en_es$total_efficient == 1]
male_query <- query_en_es$query_en[query_en_es$male_efficient == 1]
female_query <- query_en_es$query_en[query_en_es$female_efficient == 1]
# query
query_es <- query_es %>% arrange(term)
# Save original column names starting with "query_" (excluding already lagged ones)
start_with_query <- names(query_es)[startsWith(names(query_es), "query_") & !grepl("_lag", names(query_es))]

# Generate lagged features from lag 1 to 12 for selected columns
for (i in 1:6) {
  query_es <- query_es %>%
    mutate(across(all_of(start_with_query), 
                  ~ lag(., i), 
                  .names = "{.col}_lag{i}"))
}

df <- suicide_es %>%
  filter(term <= "2023-12-01", term >= "2004-01-01") %>%
  left_join(es_pop, by = "term") %>%
  left_join(query_es, by = "term") %>%
  left_join(es_unemp, by = "term") %>%
  left_join(es_solar_temp,by = "term")

# arrange columns
df <- df %>% dplyr::select(term, everything())

#calculate suicide rate
df <- df %>%
 mutate(
 suicide_rate_total = (num_suicide_total / pop_total) * 100000,
  suicide_rate_male = (num_suicide_male / pop_male) * 100000,
  suicide_rate_female = (num_suicide_female / pop_female) * 100000,
  ) 


# who   data
df_filtered_who <- df_who %>%
  select(Period, ParentLocation, Location, Dim1, FactValueNumeric, FactValueNumericLow, FactValueNumericHigh) %>%
  mutate(
    ParentLocation = as.factor(ParentLocation),
    Location = as.factor(Location),
    Dim1 = as.factor(Dim1)
  ) %>%
  rename(
    period = Period,
    parentLocation = ParentLocation, 
    location = Location,
    std_rate = FactValueNumeric,
    std_rate_low = FactValueNumericLow,
    std_rate_high = FactValueNumericHigh,
    sex = Dim1
  ) %>%
  filter(parentLocation == "Europe" & sex == "Both sexes") %>%
  mutate(spain_dummy = ifelse(location == "Spain", 1, 0))

# Calculate the average standardized rate for other European countries (excluding Spain)
other_europe_avg <- df_filtered_who %>%
  filter(location != "Spain") %>%
  group_by(period) %>%
  summarise(other_europe_std_rate = mean(std_rate, na.rm = TRUE))

# Extract data for Spain
spain_data <- df_filtered_who %>%
  filter(location == "Spain") %>%
  select(period, spain_std_rate = std_rate)

# Combine other European average and Spain data
plot_data_who <- other_europe_avg %>%
  left_join(spain_data, by = "period")

# Create the plot
p_who <- ggplot(plot_data_who, aes(x = period)) +
  geom_line(aes(y = other_europe_std_rate, color = "Other European Countries")) +
  geom_line(aes(y = spain_std_rate, color = "Spain")) +
  theme_minimal() +
  labs(
    title = "Suicide Rate Trend (Standardized Rate): Spain vs Other European Countries",
    x = "Period",
    y = "Standardized Rate",
    color = "Country"
  ) +
     theme(
    plot.title = element_text(size = 17, hjust = 0.5),
    axis.title.x = element_text(size = 12),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.title.y = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = "bottom",
    legend.key.size = unit(2, "cm"),
    text = element_text()
  )+
  scale_color_manual(values = c("Other European Countries" = "blue", "Spain" = "red")) +
  expand_limits(y = 0)  # Ensure y-axis starts at 0

windows()
print(p_who)

png("Figure_1.png",
    width = 16, height = 8, units = "in", res = 300) 
print(p_who)
dev.off()

# descriptive statistics----------------------------------------------------------
# Long format data for plotting
  df_long <- df %>%
  select(term, suicide_rate_total, suicide_rate_male, suicide_rate_female) %>%
  tidyr::pivot_longer(
    cols = c(suicide_rate_total, suicide_rate_male, suicide_rate_female),
    names_to = "type",
    values_to = "rate"
  ) %>%
  mutate(type = factor(type, 
                      levels = c("suicide_rate_total", "suicide_rate_male", "suicide_rate_female"),
                      labels = c("Total", "Male", "Female")))
# Line plot 
p_trend  <- df_long %>% ggplot(aes(x = term, y = rate, color = type, group = type)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title = "Monthly Trend of Suicide Rates in Spain (2004-2023)",
    x = "Month",
    y = "Suicide Rate (per 100,000 population)",
    color = "Category"
  )+
    theme(
    plot.title = element_text(size = 17, hjust = 0.5),
    axis.title.x = element_text(size = 12),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.title.y = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = "bottom",
    legend.key.size = unit(2, "cm"),
    text = element_text()
  )

window()
print(p_trend)

png("trend_monthly.png",
    width = 16, height = 8, units = "in", res = 300) 
print(p_trend)
dev.off()
# create train test  set----------------------------------------------------------
#Create a list of candidate month biggining test set
df <- df %>% arrange(term)
candidates <- seq(df$term[floor(nrow(df) * 0.7)], as.Date("2021-12-01"), by = "month")
#set seed
seeds <- c(1, 42, 102, 777, 999)
thresholds<- c()

candidates %>% min()
candidates %>% max()

remaining <- candidates
for (s in seeds) {
  set.seed(s)
  selected <- sample(remaining, size = 4, replace = FALSE)
  thresholds <- c(thresholds, selected)
  remaining <- setdiff(remaining, selected)
}
#sort in ascending order
thresholds <- sort(as.Date(thresholds))

# Create train and test flags for each threshold
for (i in seq_along(thresholds)) {
  df[[paste0("train_", i)]] <- ifelse(df$term < thresholds[i], 1, 0)
  # test data is limited to 24 months from the threshold
  df[[paste0("test_", i)]] <- ifelse(
    df$term >= thresholds[i] & 
    df$term < thresholds[i] + months(24), 
    1, 0
  )
}


# Extract range of columns to be scaled
start_col <- "query_suicide"
end_col <- "suicide_rate_female"

columns_to_scale <- setdiff(
  names(df)[which(names(df) == start_col):which(names(df) == end_col)],
  "quarter"
)

# Max-Min scaling function (determine scale based on train column = 1 range)
scale_max_min_by_train <- function(df, column_name, train_col) {
  filtered_df <- df %>% filter(.data[[train_col]] == 1)
  min_val <- min(filtered_df[[column_name]], na.rm = TRUE)
  max_val <- max(filtered_df[[column_name]], na.rm = TRUE)
  scaled_values <- (df[[column_name]] - min_val) / (max_val - min_val) * 100
  return(scaled_values)
}

# Apply scaling for train_1 ~ train_20 × each variable
for (train_i in 1:20) {
  train_col <- paste0("train_", train_i)
  for (col in columns_to_scale) {
    new_col_name <- paste0(col, "_scaled_", train_i)
    df[[new_col_name]] <- scale_max_min_by_train(df, col, train_col)
  }
}

# Generate _ssm_y_train_ from _scaled_ (only for suicide rate variables)
rate_vars <- c("suicide_rate_total", "suicide_rate_male", "suicide_rate_female")

for (i in 1:20) {
  train_col <- paste0("train_", i)
  for (var in rate_vars) {
    scaled_col <- paste0(var, "_scaled_", i)
    new_col <- paste0(var, "_scaled_", i, "_ssm_y_train_", i)
    df[[new_col]] <- ifelse(df[[train_col]] == 1, df[[scaled_col]], NA)
  }
}

# Overall model----------------------------------------------------------
# Basic structural time series model----------------------------------------------------------
#model name
model_id_bm_overall <- "bm_overall"

#storing scores
mape_vec_bm_overall <- numeric(20)
mae_vec_bm_overall  <- numeric(20)

#trend component
coef_vec_level_bm_overall <- numeric(20)
#seasonal component
coef_matrix_seasonal_bm_overall <- matrix(0, nrow = 20, ncol = 11)
colnames(coef_matrix_seasonal_bm_overall) <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")


#fit model
for (i in 1:20) {
  train_col <- paste0("train_", i)
  test_col <- paste0("test_", i)
  y_col <- paste0("suicide_rate_total_scaled_", i, "_ssm_y_train_", i)
  
  model <- SSModel(
    df[[y_col]] ~ SSMtrend(degree = 1, Q = NA) + SSMseasonal(period = 12, sea.type = "dummy") ,
    H = NA
  )
  fit <- fitSSM(model, inits = c(1, 1))
  kfs <- KFS(fit$model)
  pred <- predict(fit$model, interval = "prediction", level = 0.95)

  n_pred <- sum(df[[test_col]] == 1)
  pred_test <- pred[(nrow(pred) - n_pred + 1):nrow(pred), 1]
  
  min_val <- min(df$suicide_rate_total[df[[train_col]] == 1], na.rm = TRUE)
  max_val <- max(df$suicide_rate_total[df[[train_col]] == 1], na.rm = TRUE)
  pred_inverse <- scale_max_min_vector_inverse(pred_test, min_val, max_val)
  pred_inverse <- pred_inverse * df$pop_total[df[[test_col]] == 1] / 100000
  
  actual <- df$num_suicide_total[df[[test_col]] == 1]
  
  acc <- forecast::accuracy(pred_inverse, actual)
  mape_vec_bm_overall[i] <- acc[5]
  mae_vec_bm_overall[i]  <- acc[3]

  coef_vec_level_bm_overall[i] <- mean(coef(kfs)[,"level"])
  # extract seasonal component matrix
  seasonal_states <- coef(kfs)[, grep("^sea", colnames(coef(kfs)))]
  
  # drop the first row
  seasonal_states <- seasonal_states[-1, , drop = FALSE]

  
  # average every 12 months starting from the 1st row
  month_names <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
  for (j in 1:11) {
  monthly_vals <- seasonal_states[seq(j, nrow(seasonal_states), by = 12), 1]  
  coef_matrix_seasonal_bm_overall[i, month_names[j]] <- mean(monthly_vals, na.rm = TRUE)
}
}


results_overall_bm_mae <- data.frame(
  model_id = model_id_bm_overall,
  rep_id = 1:20,
  test_start = thresholds,
  mae = mae_vec_bm_overall
  )

results_overall_bm_mape <- data.frame(
  model_id = model_id_bm_overall,
  rep_id = 1:20,
  test_start = thresholds,
  mape = mape_vec_bm_overall
)
#summary
print(mean(results_overall_bm_mae$mae))
print(mean(results_overall_bm_mape$mape))
print(mean(coef_vec_level_bm_overall))
print(mean(coef_matrix_seasonal_bm_overall[, "feb"]))
print(mean(coef_matrix_seasonal_bm_overall[, "mar"]))
print(mean(coef_matrix_seasonal_bm_overall[, "apr"]))
print(mean(coef_matrix_seasonal_bm_overall[, "may"]))
print(mean(coef_matrix_seasonal_bm_overall[, "jun"]))
print(mean(coef_matrix_seasonal_bm_overall[, "jul"]))
print(mean(coef_matrix_seasonal_bm_overall[, "aug"]))
print(mean(coef_matrix_seasonal_bm_overall[, "sep"]))
print(mean(coef_matrix_seasonal_bm_overall[, "oct"]))
print(mean(coef_matrix_seasonal_bm_overall[, "nov"]))
print(mean(coef_matrix_seasonal_bm_overall[, "dec"]))
#Economic only model----------------------------------------------------------
#model name
model_id_econ_only <- "econ_only_overall"

#storing score
mape_vec_econ_only_overall <- numeric(20)
mae_vec_econ_only_overall  <- numeric(20)

coef_vec_level_econ_only_overall <- numeric(20)
coef_matrix_seasonal_econ_only_overall <- matrix(0, nrow = 20, ncol = 11)
colnames(coef_matrix_seasonal_econ_only_overall) <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
coef_vec_unemp_econ_only_overall <- numeric(20)


for (i in 1:20) {
  train_col <- paste0("train_", i)
  test_col <- paste0("test_", i)
  y_col <- paste0("suicide_rate_total_scaled_", i, "_ssm_y_train_", i)
  
  model <- SSModel(
    df[[y_col]] ~ SSMtrend(degree = 1, Q = NA) + SSMseasonal(period = 12, sea.type = "dummy") + 
      SSMregression(~ df[[paste0("total_unemp_lag1_scaled_", i)]]),
    H = NA
  )
  fit <- fitSSM(model, inits = c(1, 1))
  kfs <- KFS(fit$model)

  pred <- predict(fit$model, interval = "prediction", level = 0.95)

  n_pred <- sum(df[[test_col]] == 1)
  pred_test <- pred[(nrow(pred) - n_pred + 1):nrow(pred), 1]
  
  min_val <- min(df$suicide_rate_total[df[[train_col]] == 1], na.rm = TRUE)
  max_val <- max(df$suicide_rate_total[df[[train_col]] == 1], na.rm = TRUE)
  pred_inverse <- scale_max_min_vector_inverse(pred_test, min_val, max_val)
  pred_inverse <- pred_inverse * df$pop_total[df[[test_col]] == 1] / 100000
  
  actual <- df$num_suicide_total[df[[test_col]] == 1]
  acc <- forecast::accuracy(pred_inverse, actual)
  mape_vec_econ_only_overall[i] <- acc[5]
  mae_vec_econ_only_overall[i]  <- acc[3]

  coef_vec_level_econ_only_overall[i] <- mean(coef(kfs)[-1,"level"])

  seasonal_states <- coef(kfs)[, grep("^sea", colnames(coef(kfs)))]
  
  # drop the first row
  seasonal_states <- seasonal_states[-1, , drop = FALSE]
  
  # average every 12 months starting from the 1st row
  month_names <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
  for (m in 1:11) {
    monthly_vals <- seasonal_states[seq(m, nrow(seasonal_states), by = 12), 1]
    coef_matrix_seasonal_econ_only_overall[i, month_names[m]] <- mean(monthly_vals, na.rm = TRUE)
  }
  # Extract state name index for the economic regressor
  # Save the estimated coefficient (last filtered value)
  coef_vec_unemp_econ_only_overall[i] <- mean(coef(kfs)[-1, "df[[paste0(\"total_unemp_lag1_scaled_\", i)]]"])
}

results_overall_econ_only_mae <- data.frame(
  model_id = model_id_econ_only,
  rep_id = 1:20,
  test_start = thresholds,
  mae = mae_vec_econ_only_overall
)

results_overall_econ_only_mape <- data.frame(
  model_id = model_id_econ_only,
  rep_id = 1:20,
  test_start = thresholds,
  mape = mape_vec_econ_only_overall
)

print(mean(results_overall_econ_only_mae$mae))
print(mean(results_overall_econ_only_mape$mape))
print(mean(coef_vec_unemp_econ_only_overall))
print(mean(coef_vec_level_econ_only_overall))
print(mean(coef_matrix_seasonal_econ_only_overall[, "feb"]))
print(mean(coef_matrix_seasonal_econ_only_overall[, "mar"]))
print(mean(coef_matrix_seasonal_econ_only_overall[, "apr"]))
print(mean(coef_matrix_seasonal_econ_only_overall[, "may"]))
print(mean(coef_matrix_seasonal_econ_only_overall[, "jun"]))
print(mean(coef_matrix_seasonal_econ_only_overall[, "jul"]))
print(mean(coef_matrix_seasonal_econ_only_overall[, "aug"]))
print(mean(coef_matrix_seasonal_econ_only_overall[, "sep"]))
print(mean(coef_matrix_seasonal_econ_only_overall[, "oct"]))
print(mean(coef_matrix_seasonal_econ_only_overall[, "nov"]))
print(mean(coef_matrix_seasonal_econ_only_overall[, "dec"]))

#natural environment only model----------------------------------------------------------
#model name
model_id_natural_only <- "natural_only_overall"

#storing scores
mape_vec_natural_only_overall <- numeric(20)
mae_vec_natural_only_overall  <- numeric(20)

coef_vec_solar_natural_only_overall <- numeric(20)
coef_vec_temp_natural_only_overall <- numeric(20)
coef_vec_level_natural_only_overall <- numeric(20)
coef_matrix_seasonal_natural_only_overall <- matrix(0, nrow = 20, ncol = 11)
colnames(coef_matrix_seasonal_natural_only_overall) <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")


for (i in 1:20) {
  train_col <- paste0("train_", i)
  test_col <- paste0("test_", i)
  y_col <- paste0("suicide_rate_total_scaled_", i, "_ssm_y_train_", i)

  model <- SSModel(
    df[[y_col]] ~ SSMtrend(degree = 1, Q = NA) + SSMseasonal(period = 12, sea.type = "dummy") + 
      SSMregression(~ df[[paste0("solar_lag1_scaled_", i)]] + df[[paste0("temp_lag1_scaled_", i)]]),
    H = NA
  )
  fit <- fitSSM(model, inits = c(1, 1))
  kfs <- KFS(fit$model)

  pred <- predict(fit$model, interval = "prediction", level = 0.95)

  n_pred <- sum(df[[test_col]] == 1)
  pred_test <- pred[(nrow(pred) - n_pred + 1):nrow(pred), 1]
  
  min_val <- min(df$suicide_rate_total[df[[train_col]] == 1], na.rm = TRUE)
  max_val <- max(df$suicide_rate_total[df[[train_col]] == 1], na.rm = TRUE)
  pred_inverse <- scale_max_min_vector_inverse(pred_test, min_val, max_val)
  pred_inverse <- pred_inverse * df$pop_total[df[[test_col]] == 1] / 100000

  actual <- df$num_suicide_total[df[[test_col]] == 1]
  acc <- forecast::accuracy(pred_inverse, actual)
  mape_vec_natural_only_overall[i] <- acc[5]
  mae_vec_natural_only_overall[i]  <- acc[3]

  coef_vec_solar_natural_only_overall[i] <- mean(coef(kfs)[-1, "df[[paste0(\"solar_lag1_scaled_\", i)]]"])
  coef_vec_temp_natural_only_overall[i] <- mean(coef(kfs)[-1, "df[[paste0(\"temp_lag1_scaled_\", i)]]"])
  coef_vec_level_natural_only_overall[i] <- mean(coef(kfs)[-1,"level"])
  seasonal_states <- coef(kfs)[, grep("^sea", colnames(coef(kfs)))]

  # drop the first row
  seasonal_states <- seasonal_states[-1, , drop = FALSE]
  
  # average every 12 months starting from the 1st row
  month_names <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec") 
  for (m in 1:11) {
    monthly_vals <- seasonal_states[seq(m, nrow(seasonal_states), by = 12), 1]
    coef_matrix_seasonal_natural_only_overall[i, month_names[m]] <- mean(monthly_vals, na.rm = TRUE)
  }
}


results_overall_natural_only_mae <- data.frame(
  model_id = model_id_natural_only,
  rep_id = 1:20,
  test_start = thresholds,
  mae = mae_vec_natural_only_overall
)

results_overall_natural_only_mape <- data.frame(
  model_id = model_id_natural_only,
  rep_id = 1:20,
  test_start = thresholds,
  mape = mape_vec_natural_only_overall
)

print(mean(results_overall_natural_only_mae$mae))
print(mean(results_overall_natural_only_mape$mape))
print(mean(coef_vec_solar_natural_only_overall))
print(mean(coef_vec_temp_natural_only_overall))
print(mean(coef_vec_level_natural_only_overall))
print(mean(coef_matrix_seasonal_natural_only_overall[, "feb"]))
print(mean(coef_matrix_seasonal_natural_only_overall[, "mar"]))
print(mean(coef_matrix_seasonal_natural_only_overall[, "apr"]))
print(mean(coef_matrix_seasonal_natural_only_overall[, "may"]))
print(mean(coef_matrix_seasonal_natural_only_overall[, "jun"]))
print(mean(coef_matrix_seasonal_natural_only_overall[, "jul"]))
print(mean(coef_matrix_seasonal_natural_only_overall[, "aug"]))
print(mean(coef_matrix_seasonal_natural_only_overall[, "sep"]))
print(mean(coef_matrix_seasonal_natural_only_overall[, "oct"]))
print(mean(coef_matrix_seasonal_natural_only_overall[, "nov"]))
print(mean(coef_matrix_seasonal_natural_only_overall[, "dec"]))

#query only model----------------------------------------------------------
#model name
model_id_query_only <- "query_only_overall"

#storing scores
mape_vec_query_only_overall <- numeric(20)
mae_vec_query_only_overall  <- numeric(20)
coef_vec_query_only_overall <- numeric(20)
coef_vec_level_query_only_overall <- numeric(20)
coef_matrix_seasonal_query_only_overall <- matrix(0, nrow = 20, ncol = 11)
colnames(coef_matrix_seasonal_query_only_overall) <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")

# 
coef_matrix_query_only_overall <- matrix(0, nrow = 20, ncol = length(total_query))
colnames(coef_matrix_query_only_overall) <- total_query
coef_vec_query_only_overall <- numeric(20)


for (i in 1:20) {
  train_col <- paste0("train_", i)
  test_col <- paste0("test_", i)
  y_col <- paste0("suicide_rate_total_scaled_", i, "_ssm_y_train_", i)
  
  model <- SSModel(
    df[[y_col]] ~ SSMtrend(degree = 1, Q = NA) + SSMseasonal(period = 12, sea.type = "dummy") + 
      SSMregression(~ df[[paste0("query_suicide_scaled_", i)]] + 
                       df[[paste0("query_depression_scaled_", i)]] + 
                       df[[paste0("query_unemployment_scaled_", i)]] + 
                       df[[paste0("query_relationship_breakup_scaled_", i)]] + 
                       df[[paste0("query_antidepressant_scaled_", i)]] + 
                       df[[paste0("query_allergy_scaled_", i)]] + 
                       df[[paste0("query_pain_scaled_", i)]] + 
                       df[[paste0("query_drunkenness_scaled_", i)]] + 
                       df[[paste0("query_alcohol_abstinence_scaled_", i)]]),
    H = NA
)
  fit <- fitSSM(model, inits = c(1, 1))
  kfs <- KFS(fit$model)

  pred <- predict(fit$model, interval = "prediction", level = 0.95)

  n_pred <- sum(df[[test_col]] == 1)
  pred_test <- pred[(nrow(pred) - n_pred + 1):nrow(pred), 1]
  
  min_val <- min(df$suicide_rate_total[df[[train_col]] == 1], na.rm = TRUE)
  max_val <- max(df$suicide_rate_total[df[[train_col]] == 1], na.rm = TRUE)
  pred_inverse <- scale_max_min_vector_inverse(pred_test, min_val, max_val)
  pred_inverse <- pred_inverse * df$pop_total[df[[test_col]] == 1] / 100000

  actual <- df$num_suicide_total[df[[test_col]] == 1]
  acc <- forecast::accuracy(pred_inverse, actual) 
  mape_vec_query_only_overall[i] <- acc[5]
  mae_vec_query_only_overall[i]  <- acc[3]

  for (j in seq_along(total_query)) {
    query_name <- total_query[j]
    coef_matrix_query_only_overall[i, j] <- mean(coef(kfs)[-1, paste0("df[[paste0(\"", query_name, "_scaled_\", i)]]")])
  }
  coef_vec_query_only_overall[i] <- sum(coef_matrix_query_only_overall[i,])
  coef_vec_level_query_only_overall[i] <- mean(coef(kfs)[-1,"level"])
  seasonal_states <- coef(kfs)[, grep("^sea", colnames(coef(kfs)))]
  
  # drop the first row
  seasonal_states <- seasonal_states[-1, , drop = FALSE]
  
  # average every 12 months starting from the 1st row
  month_names <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
  for (m in 1:11) {
    monthly_vals <- seasonal_states[seq(m, nrow(seasonal_states), by = 12), 1]
    coef_matrix_seasonal_query_only_overall[i, month_names[m]] <- mean(monthly_vals, na.rm = TRUE)
  }
}

results_overall_query_only_mae <- data.frame(
  model_id = model_id_query_only,
  rep_id = 1:20,
  test_start = thresholds,
  mae = mae_vec_query_only_overall
)

results_overall_query_only_mape <- data.frame(
  model_id = model_id_query_only,
  rep_id = 1:20,
  test_start = thresholds,
  mape = mape_vec_query_only_overall
)


coef_matrix_query_only_overall
print(mean(results_overall_query_only_mae$mae))
print(mean(results_overall_query_only_mape$mape))
print(mean(coef_vec_level_query_only_overall))
print(mean(coef_matrix_seasonal_query_only_overall[, "feb"]))
print(mean(coef_matrix_seasonal_query_only_overall[, "mar"]))
print(mean(coef_matrix_seasonal_query_only_overall[, "apr"]))
print(mean(coef_matrix_seasonal_query_only_overall[, "may"]))
print(mean(coef_matrix_seasonal_query_only_overall[, "jun"]))
print(mean(coef_matrix_seasonal_query_only_overall[, "jul"]))
print(mean(coef_matrix_seasonal_query_only_overall[, "aug"]))
print(mean(coef_matrix_seasonal_query_only_overall[, "sep"]))
print(mean(coef_matrix_seasonal_query_only_overall[, "oct"]))
print(mean(coef_matrix_seasonal_query_only_overall[, "nov"]))
print(mean(coef_matrix_seasonal_query_only_overall[, "dec"]))

for (q in total_query) {
  m <- mean(coef_matrix_query_only_overall[, q], na.rm = TRUE)
  cat(q, ":", m, "\n")
}


#economic and natural environment model----------------------------------------------------------
#model name
model_id_econ_natural_overall <- "econ_natural_overall"

#storing scores
mape_vec_econ_natural_overall <- numeric(20)
mae_vec_econ_natural_overall  <- numeric(20)

coef_vec_unemp_econ_natural_overall <- numeric(20)
coef_vec_solar_econ_natural_overall <- numeric(20)
coef_vec_temp_econ_natural_overall <- numeric(20)
coef_vec_level_econ_natural_overall <- numeric(20)
coef_matrix_seasonal_econ_natural_overall <- matrix(0, nrow = 20, ncol = 11)
colnames(coef_matrix_seasonal_econ_natural_overall) <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")


for (i in 1:20) {
  train_col <- paste0("train_", i)
  test_col <- paste0("test_", i)
  y_col <- paste0("suicide_rate_total_scaled_", i, "_ssm_y_train_", i)
  
  model <- SSModel(
    df[[y_col]] ~ SSMtrend(degree = 1, Q = NA) + SSMseasonal(period = 12, sea.type = "dummy") + 
      SSMregression(~ df[[paste0("total_unemp_lag1_scaled_", i)]] + 
                       df[[paste0("solar_lag1_scaled_", i)]] + 
                       df[[paste0("temp_lag1_scaled_", i)]]),
    H = NA
  ) 
  fit <- fitSSM(model, inits = c(1, 1))
  kfs <- KFS(fit$model)

  pred <- predict(fit$model, interval = "prediction", level = 0.95)

  n_pred <- sum(df[[test_col]] == 1)
  pred_test <- pred[(nrow(pred) - n_pred + 1):nrow(pred), 1]  

  min_val <- min(df$suicide_rate_total[df[[train_col]] == 1], na.rm = TRUE)
  max_val <- max(df$suicide_rate_total[df[[train_col]] == 1], na.rm = TRUE)
  pred_inverse <- scale_max_min_vector_inverse(pred_test, min_val, max_val)
  pred_inverse <- pred_inverse * df$pop_total[df[[test_col]] == 1] / 100000

  actual <- df$num_suicide_total[df[[test_col]] == 1] 
  acc <- forecast::accuracy(pred_inverse, actual)
  mape_vec_econ_natural_overall[i] <- acc[5]
  mae_vec_econ_natural_overall[i]  <- acc[3]

  coef_vec_unemp_econ_natural_overall[i] <- mean(coef(kfs)[-1, "df[[paste0(\"total_unemp_lag1_scaled_\", i)]]"])
  coef_vec_solar_econ_natural_overall[i] <- mean(coef(kfs)[-1, "df[[paste0(\"solar_lag1_scaled_\", i)]]"])  
  coef_vec_temp_econ_natural_overall[i] <- mean(coef(kfs)[-1, "df[[paste0(\"temp_lag1_scaled_\", i)]]"])
  coef_vec_level_econ_natural_overall[i] <- mean(coef(kfs)[-1,"level"])
  seasonal_states <- coef(kfs)[, grep("^sea", colnames(coef(kfs)))]
  
  # drop the first row
  seasonal_states <- seasonal_states[-1, , drop = FALSE]
  
  # average every 12 months starting from the 1st row
  month_names <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
  for (m in 1:11) {
    monthly_vals <- seasonal_states[seq(m, nrow(seasonal_states), by = 12), 1]
    coef_matrix_seasonal_econ_natural_overall[i, month_names[m]] <- mean(monthly_vals, na.rm = TRUE)
  }
}

results_overall_econ_natural_mae <- data.frame(
  model_id = model_id_econ_natural_overall,
  rep_id = 1:20,
  test_start = thresholds,
  mae = mae_vec_econ_natural_overall
)

results_overall_econ_natural_mape <- data.frame(
  model_id = model_id_econ_natural_overall,
  rep_id = 1:20,
  test_start = thresholds,
  mape = mape_vec_econ_natural_overall
)

print(mean(results_overall_econ_natural_mae$mae))
print(mean(results_overall_econ_natural_mape$mape))
print(mean(coef_vec_unemp_econ_natural_overall))
print(mean(coef_vec_solar_econ_natural_overall))
print(mean(coef_vec_temp_econ_natural_overall))
print(mean(coef_vec_level_econ_natural_overall))
print(mean(coef_matrix_seasonal_econ_natural_overall[, "feb"]))
print(mean(coef_matrix_seasonal_econ_natural_overall[, "mar"]))
print(mean(coef_matrix_seasonal_econ_natural_overall[, "apr"]))
print(mean(coef_matrix_seasonal_econ_natural_overall[, "may"]))
print(mean(coef_matrix_seasonal_econ_natural_overall[, "jun"]))
print(mean(coef_matrix_seasonal_econ_natural_overall[, "jul"]))
print(mean(coef_matrix_seasonal_econ_natural_overall[, "aug"]))
print(mean(coef_matrix_seasonal_econ_natural_overall[, "sep"]))
print(mean(coef_matrix_seasonal_econ_natural_overall[, "oct"]))
print(mean(coef_matrix_seasonal_econ_natural_overall[, "nov"]))
print(mean(coef_matrix_seasonal_econ_natural_overall[, "dec"]))

#economic and query model----------------------------------------------------------
#model name
model_id_econ_query_overall <- "econ_query_overall"

#storing scores
mape_vec_econ_query_overall <- numeric(20)
mae_vec_econ_query_overall  <- numeric(20)

coef_vec_unemp_econ_query_overall <- numeric(20)
coef_vec_level_econ_query_overall <- numeric(20)
coef_matrix_seasonal_econ_query_overall <- matrix(0, nrow = 20, ncol = 11)
colnames(coef_matrix_seasonal_econ_query_overall) <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")

coef_matrix_query_econ_query_overall <- matrix(0, nrow = 20, ncol = length(total_query))
colnames(coef_matrix_query_econ_query_overall) <- total_query
coef_vec_query_econ_query_overall <- numeric(20)


for (i in 1:20) { 
  train_col <- paste0("train_", i)
  test_col <- paste0("test_", i)
  y_col <- paste0("suicide_rate_total_scaled_", i, "_ssm_y_train_", i)
  
  model <- SSModel(
    df[[y_col]] ~ SSMtrend(degree = 1, Q = NA) + SSMseasonal(period = 12, sea.type = "dummy") + 
      SSMregression(~ df[[paste0("total_unemp_lag1_scaled_", i)]] + 
                       df[[paste0("query_suicide_scaled_", i)]] + 
                       df[[paste0("query_depression_scaled_", i)]] + 
                       df[[paste0("query_unemployment_scaled_", i)]] + 
                       df[[paste0("query_relationship_breakup_scaled_", i)]] + 
                       df[[paste0("query_antidepressant_scaled_", i)]] + 
                       df[[paste0("query_allergy_scaled_", i)]] + 
                       df[[paste0("query_pain_scaled_", i)]] + 
                       df[[paste0("query_drunkenness_scaled_", i)]] + 
                       df[[paste0("query_alcohol_abstinence_scaled_", i)]]),
    H = NA
  )
  fit <- fitSSM(model, inits = c(1, 1))
  kfs <- KFS(fit$model)

  pred <- predict(fit$model, interval = "prediction", level = 0.95) 

  n_pred <- sum(df[[test_col]] == 1)
  pred_test <- pred[(nrow(pred) - n_pred + 1):nrow(pred), 1]

  min_val <- min(df$suicide_rate_total[df[[train_col]] == 1], na.rm = TRUE)
  max_val <- max(df$suicide_rate_total[df[[train_col]] == 1], na.rm = TRUE)
  pred_inverse <- scale_max_min_vector_inverse(pred_test, min_val, max_val)
  pred_inverse <- pred_inverse * df$pop_total[df[[test_col]] == 1] / 100000

  actual <- df$num_suicide_total[df[[test_col]] == 1]
  acc <- forecast::accuracy(pred_inverse, actual)
  mape_vec_econ_query_overall[i] <- acc[5]
  mae_vec_econ_query_overall[i]  <- acc[3]

  coef_vec_unemp_econ_query_overall[i] <- mean(coef(kfs)[-1, "df[[paste0(\"total_unemp_lag1_scaled_\", i)]]"])

  for (j in seq_along(total_query)) {
    query_name <- total_query[j]
    coef_matrix_query_econ_query_overall[i, j] <- mean(coef(kfs)[-1, paste0("df[[paste0(\"", query_name, "_scaled_\", i)]]")])
  }
  coef_vec_query_econ_query_overall[i] <- sum(coef_matrix_query_econ_query_overall[i,])
  coef_vec_level_econ_query_overall[i] <- mean(coef(kfs)[-1,"level"])
  seasonal_states <- coef(kfs)[, grep("^sea", colnames(coef(kfs)))]

  # drop the first row
  seasonal_states <- seasonal_states[-1, , drop = FALSE]
  
  # average every 12 months starting from the 1st row
  month_names <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
  for (m in 1:11) {
    monthly_vals <- seasonal_states[seq(m, nrow(seasonal_states), by = 12), 1]
    coef_matrix_seasonal_econ_query_overall[i, month_names[m]] <- mean(monthly_vals, na.rm = TRUE)
  }
}


results_overall_econ_query_mae <- data.frame(
  model_id = model_id_econ_query_overall,
  rep_id = 1:20,
  test_start = thresholds,
  mae = mae_vec_econ_query_overall
)

results_overall_econ_query_mape <- data.frame(
  model_id = model_id_econ_query_overall,
  rep_id = 1:20,
  test_start = thresholds,
  mape = mape_vec_econ_query_overall
)

print(mean(results_overall_econ_query_mae$mae))
print(mean(results_overall_econ_query_mape$mape))
print(mean(coef_vec_unemp_econ_query_overall))
for (q in total_query) {
  cat(q, ":", mean(coef_matrix_query_econ_query_overall[, q], na.rm = TRUE), "\n")
}

print(mean(coef_vec_level_econ_query_overall))
print(mean(coef_matrix_seasonal_econ_query_overall[, "feb"]))
print(mean(coef_matrix_seasonal_econ_query_overall[, "mar"]))
print(mean(coef_matrix_seasonal_econ_query_overall[, "apr"]))
print(mean(coef_matrix_seasonal_econ_query_overall[, "may"]))
print(mean(coef_matrix_seasonal_econ_query_overall[, "jun"]))
print(mean(coef_matrix_seasonal_econ_query_overall[, "jul"]))
print(mean(coef_matrix_seasonal_econ_query_overall[, "aug"]))
print(mean(coef_matrix_seasonal_econ_query_overall[, "sep"]))
print(mean(coef_matrix_seasonal_econ_query_overall[, "oct"]))
print(mean(coef_matrix_seasonal_econ_query_overall[, "nov"]))
print(mean(coef_matrix_seasonal_econ_query_overall[, "dec"]))

#natural environment and query model----------------------------------------------------------
#model name
model_id_natural_query_overall <- "natural_query_overall"

#storing scores
mape_vec_natural_query_overall <- numeric(20)
mae_vec_natural_query_overall  <- numeric(20)

coef_vec_solar_natural_query_overall <- numeric(20)
coef_vec_temp_natural_query_overall <- numeric(20)
coef_vec_level_natural_query_overall <- numeric(20)
coef_matrix_seasonal_natural_query_overall <- matrix(0, nrow = 20, ncol = 11)
colnames(coef_matrix_seasonal_natural_query_overall) <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")

coef_matrix_query_natural_query_overall <- matrix(0, nrow = 20, ncol = length(total_query))
colnames(coef_matrix_query_natural_query_overall) <- total_query
coef_vec_query_natural_query_overall <- numeric(20)


for (i in 1:20) {
  train_col <- paste0("train_", i)
  test_col <- paste0("test_", i)
  y_col <- paste0("suicide_rate_total_scaled_", i, "_ssm_y_train_", i)

  model <- SSModel(
    df[[y_col]] ~ SSMtrend(degree = 1, Q = NA) + SSMseasonal(period = 12, sea.type = "dummy") + 
      SSMregression(~ df[[paste0("solar_lag1_scaled_", i)]] + df[[paste0("temp_lag1_scaled_", i)]] + 
                       df[[paste0("query_suicide_scaled_", i)]] + df[[paste0("query_depression_scaled_", i)]] + 
                       df[[paste0("query_unemployment_scaled_", i)]] + df[[paste0("query_relationship_breakup_scaled_", i)]] + 
                       df[[paste0("query_antidepressant_scaled_", i)]] + df[[paste0("query_allergy_scaled_", i)]] + 
                       df[[paste0("query_pain_scaled_", i)]] + df[[paste0("query_drunkenness_scaled_", i)]] + 
                       df[[paste0("query_alcohol_abstinence_scaled_", i)]]),
    H = NA
  )
  fit <- fitSSM(model, inits = c(1, 1))
  kfs <- KFS(fit$model)

  pred <- predict(fit$model, interval = "prediction", level = 0.95)

  n_pred <- sum(df[[test_col]] == 1)
  pred_test <- pred[(nrow(pred) - n_pred + 1):nrow(pred), 1]

  min_val <- min(df$suicide_rate_total[df[[train_col]] == 1], na.rm = TRUE)
  max_val <- max(df$suicide_rate_total[df[[train_col]] == 1], na.rm = TRUE)
  pred_inverse <- scale_max_min_vector_inverse(pred_test, min_val, max_val)
  pred_inverse <- pred_inverse * df$pop_total[df[[test_col]] == 1] / 100000

  actual <- df$num_suicide_total[df[[test_col]] == 1]
  acc <- forecast::accuracy(pred_inverse, actual)
  mape_vec_natural_query_overall[i] <- acc[5]
  mae_vec_natural_query_overall[i]  <- acc[3]

  coef_vec_solar_natural_query_overall[i] <- mean(coef(kfs)[-1, "df[[paste0(\"solar_lag1_scaled_\", i)]]"])
  coef_vec_temp_natural_query_overall[i] <- mean(coef(kfs)[-1, "df[[paste0(\"temp_lag1_scaled_\", i)]]"])

  for (j in seq_along(total_query)) {
    query_name <- total_query[j]
    coef_matrix_query_natural_query_overall[i, j] <- mean(coef(kfs)[-1, paste0("df[[paste0(\"", query_name, "_scaled_\", i)]]")])
  }
  coef_vec_query_natural_query_overall[i] <- sum(coef_matrix_query_natural_query_overall[i,])
  coef_vec_level_natural_query_overall[i] <- mean(coef(kfs)[-1,"level"])
  seasonal_states <- coef(kfs)[, grep("^sea", colnames(coef(kfs)))]
  
  # drop the first row
  seasonal_states <- seasonal_states[-1, , drop = FALSE]
  
  # average every 12 months starting from the 1st row 
  month_names <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
  for (m in 1:11) {
    monthly_vals <- seasonal_states[seq(m, nrow(seasonal_states), by = 12), 1]
    coef_matrix_seasonal_natural_query_overall[i, month_names[m]] <- mean(monthly_vals, na.rm = TRUE)
  }
}

results_overall_natural_query_mae <- data.frame(
  model_id = model_id_natural_query_overall,
  rep_id = 1:20,
  test_start = thresholds,
  mae = mae_vec_natural_query_overall
)

results_overall_natural_query_mape <- data.frame(
  model_id = model_id_natural_query_overall,
  rep_id = 1:20,
  test_start = thresholds,
  mape = mape_vec_natural_query_overall
)

print(mean(results_overall_natural_query_mae$mae))
print(mean(results_overall_natural_query_mape$mape))
print(mean(coef_vec_solar_natural_query_overall))
print(mean(coef_vec_temp_natural_query_overall))
for (q in total_query) {
  cat(q, ":", mean(coef_matrix_query_natural_query_overall[, q], na.rm = TRUE), "\n")
}

print(mean(coef_vec_level_natural_query_overall))
print(mean(coef_matrix_seasonal_natural_query_overall[, "feb"]))
print(mean(coef_matrix_seasonal_natural_query_overall[, "mar"]))
print(mean(coef_matrix_seasonal_natural_query_overall[, "apr"]))
print(mean(coef_matrix_seasonal_natural_query_overall[, "may"]))
print(mean(coef_matrix_seasonal_natural_query_overall[, "jun"]))
print(mean(coef_matrix_seasonal_natural_query_overall[, "jul"]))
print(mean(coef_matrix_seasonal_natural_query_overall[, "aug"]))
print(mean(coef_matrix_seasonal_natural_query_overall[, "sep"]))
print(mean(coef_matrix_seasonal_natural_query_overall[, "oct"]))
print(mean(coef_matrix_seasonal_natural_query_overall[, "nov"]))
print(mean(coef_matrix_seasonal_natural_query_overall[, "dec"]))

#economic, natural environment and query model----------------------------------------------------------
#model name
model_id_econ_natural_query_overall <- "econ_natural_query_overall"

#storing scores
mape_vec_econ_natural_query_overall <- numeric(20)
mae_vec_econ_natural_query_overall  <- numeric(20)

coef_vec_unemp_econ_natural_query_overall <- numeric(20)
coef_vec_solar_econ_natural_query_overall <- numeric(20)
coef_vec_temp_econ_natural_query_overall <- numeric(20)
coef_vec_level_econ_natural_query_overall <- numeric(20)
coef_matrix_seasonal_econ_natural_query_overall <- matrix(0, nrow = 20, ncol = 11)
colnames(coef_matrix_seasonal_econ_natural_query_overall) <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")

coef_matrix_query_econ_natural_query_overall <- matrix(0, nrow = 20, ncol = length(total_query))
colnames(coef_matrix_query_econ_natural_query_overall) <- total_query
coef_vec_query_econ_natural_query_overall <- numeric(20)

for (i in 1:20) {
  train_col <- paste0("train_", i)
  test_col <- paste0("test_", i)
  y_col <- paste0("suicide_rate_total_scaled_", i, "_ssm_y_train_", i)

  model <- SSModel(
  df[[y_col]] ~ SSMtrend(degree = 1, Q = NA) + SSMseasonal(period = 12, sea.type = "dummy") + 
    SSMregression(~ df[[paste0("total_unemp_lag1_scaled_", i)]] + 
                     df[[paste0("solar_lag1_scaled_", i)]] + 
                     df[[paste0("temp_lag1_scaled_", i)]] + 
                     df[[paste0("query_suicide_scaled_", i)]] + 
                     df[[paste0("query_depression_scaled_", i)]] + 
                     df[[paste0("query_unemployment_scaled_", i)]] + 
                     df[[paste0("query_relationship_breakup_scaled_", i)]] + 
                     df[[paste0("query_antidepressant_scaled_", i)]] + 
                     df[[paste0("query_allergy_scaled_", i)]] + 
                     df[[paste0("query_pain_scaled_", i)]] + 
                     df[[paste0("query_drunkenness_scaled_", i)]] + 
                     df[[paste0("query_alcohol_abstinence_scaled_", i)]]),
  H = NA
)
  fit <- fitSSM(model, inits = c(1, 1))
  kfs <- KFS(fit$model)

  pred <- predict(fit$model, interval = "prediction", level = 0.95)

  n_pred <- sum(df[[test_col]] == 1)
  pred_test <- pred[(nrow(pred) - n_pred + 1):nrow(pred), 1]  

  min_val <- min(df$suicide_rate_total[df[[train_col]] == 1], na.rm = TRUE)
  max_val <- max(df$suicide_rate_total[df[[train_col]] == 1], na.rm = TRUE)
  pred_inverse <- scale_max_min_vector_inverse(pred_test, min_val, max_val)
  pred_inverse <- pred_inverse * df$pop_total[df[[test_col]] == 1] / 100000 

  actual <- df$num_suicide_total[df[[test_col]] == 1]
  acc <- forecast::accuracy(pred_inverse, actual)
  mape_vec_econ_natural_query_overall[i] <- acc[5]
  mae_vec_econ_natural_query_overall[i]  <- acc[3]    

  coef_vec_unemp_econ_natural_query_overall[i] <- mean(coef(kfs)[-1, "df[[paste0(\"total_unemp_lag1_scaled_\", i)]]"])
  coef_vec_solar_econ_natural_query_overall[i] <- mean(coef(kfs)[-1, "df[[paste0(\"solar_lag1_scaled_\", i)]]"])
  coef_vec_temp_econ_natural_query_overall[i] <- mean(coef(kfs)[-1, "df[[paste0(\"temp_lag1_scaled_\", i)]]"])

  for (j in seq_along(total_query)) {
    query_name <- total_query[j]  
    coef_matrix_query_econ_natural_query_overall[i, j] <- mean(coef(kfs)[-1, paste0("df[[paste0(\"", query_name, "_scaled_\", i)]]")])
  }
  coef_vec_query_econ_natural_query_overall[i] <- sum(coef_matrix_query_econ_natural_query_overall[i,])
  coef_vec_level_econ_natural_query_overall[i] <- mean(coef(kfs)[-1,"level"])
  seasonal_states <- coef(kfs)[, grep("^sea", colnames(coef(kfs)))]

  # drop the first row
  seasonal_states <- seasonal_states[-1, , drop = FALSE]
  
  # average every 12 months starting from the 1st row
  month_names <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
  for (m in 1:11) {
    monthly_vals <- seasonal_states[seq(m, nrow(seasonal_states), by = 12), 1]
    coef_matrix_seasonal_econ_natural_query_overall[i, month_names[m]] <- mean(monthly_vals, na.rm = TRUE)
  }
}

results_overall_econ_natural_query_mae <- data.frame(
  model_id = model_id_econ_natural_query_overall,
  rep_id = 1:20,
  test_start = thresholds,
  mae = mae_vec_econ_natural_query_overall
)

results_overall_econ_natural_query_mape <- data.frame(
  model_id = model_id_econ_natural_query_overall,
  rep_id = 1:20,
  test_start = thresholds,
  mape = mape_vec_econ_natural_query_overall
)

print(mean(results_overall_econ_natural_query_mae$mae))
print(mean(results_overall_econ_natural_query_mape$mape))
print(mean(coef_vec_unemp_econ_natural_query_overall))
print(mean(coef_vec_solar_econ_natural_query_overall))
print(mean(coef_vec_temp_econ_natural_query_overall))
for (q in total_query) {
  cat(q, ":", mean(coef_matrix_query_econ_natural_query_overall[, q], na.rm = TRUE), "\n")
}

print(mean(coef_vec_level_econ_natural_query_overall))
print(mean(coef_matrix_seasonal_econ_natural_query_overall[, "feb"]))
print(mean(coef_matrix_seasonal_econ_natural_query_overall[, "mar"]))
print(mean(coef_matrix_seasonal_econ_natural_query_overall[, "apr"]))
print(mean(coef_matrix_seasonal_econ_natural_query_overall[, "may"]))
print(mean(coef_matrix_seasonal_econ_natural_query_overall[, "jun"]))
print(mean(coef_matrix_seasonal_econ_natural_query_overall[, "jul"]))
print(mean(coef_matrix_seasonal_econ_natural_query_overall[, "aug"]))
print(mean(coef_matrix_seasonal_econ_natural_query_overall[, "sep"]))
print(mean(coef_matrix_seasonal_econ_natural_query_overall[, "oct"]))
print(mean(coef_matrix_seasonal_econ_natural_query_overall[, "nov"]))
print(mean(coef_matrix_seasonal_econ_natural_query_overall[, "dec"]))


# compare Accuracy  ----------------------------------------------------------
# Basic Structural Time Series
results_overall_bm_mae$model_name <- "Basic Structural Time Series"
results_overall_bm_mape$model_name <- "Basic Structural Time Series"

# Econ Only
results_overall_econ_only_mae$model_name <- "Econ Only"
results_overall_econ_only_mape$model_name <- "Econ Only"

# Natural Env Only
results_overall_natural_only_mae$model_name <- "Natural Env Only"
results_overall_natural_only_mape$model_name <- "Natural Env Only"

# Search Query Only
results_overall_query_only_mae$model_name <- "Search Query Only"
results_overall_query_only_mape$model_name <- "Search Query Only"

# Econ + Natural Env
results_overall_econ_natural_mae$model_name <- "Econ + Natural Env"
results_overall_econ_natural_mape$model_name <- "Econ + Natural Env"

# Econ + Search Query
results_overall_econ_query_mae$model_name <- "Econ + Search Query"
results_overall_econ_query_mape$model_name <- "Econ + Search Query"

# Natural Env + Search Query
results_overall_natural_query_mae$model_name <- "Natural Env + Search Query"
results_overall_natural_query_mape$model_name <- "Natural Env + Search Query"

# Econ + Natural Env + Search Query
results_overall_econ_natural_query_mae$model_name <- "Econ + Natural Env + Search Query"
results_overall_econ_natural_query_mape$model_name <- "Econ + Natural Env + Search Query"

#bind all results
result_overall_mae <- rbind(
  results_overall_bm_mae,
  results_overall_econ_only_mae,
  results_overall_natural_only_mae,
  results_overall_query_only_mae,
  results_overall_econ_natural_mae,
  results_overall_econ_query_mae,
  results_overall_natural_query_mae,
  results_overall_econ_natural_query_mae
)

#bind all results
result_overall_mape <- rbind(
  results_overall_bm_mape,
  results_overall_econ_only_mape,
  results_overall_natural_only_mape,
  results_overall_query_only_mape,
  results_overall_econ_natural_mape,
  results_overall_econ_query_mape,
  results_overall_natural_query_mape,
  results_overall_econ_natural_query_mape
)

model_levels <- c(
  "Basic Structural Time Series",
  "Econ Only",
  "Natural Env Only",
  "Search Query Only",
  "Econ + Natural Env",
  "Econ + Search Query",
  "Natural Env + Search Query",
  "Econ + Natural Env + Search Query"
)

result_overall_mae$model_name <- factor(result_overall_mae$model_name, levels = model_levels)
result_overall_mape$model_name <- factor(result_overall_mape$model_name, levels = model_levels)




#box plot for mae
mae_boxplot <- ggplot(result_overall_mae, aes(x = model_name, y = mae)) +
  geom_boxplot() +
  labs(
    title = "MAE across 20 Replicated Hold-Outs for Suicide Deaths (Overall)",
    x = "Model",
    y = "MAE"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 17, hjust = 0.5),
    axis.title.x = element_text(size = 12),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.title.y = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = "bottom",
    legend.key.size = unit(2, "cm"),
    text = element_text()
  )

windows()
print(mae_boxplot)

png("mae_boxplot_overall.png",
    width = 16, height = 8, units = "in", res = 300) 
print(mae_boxplot)
dev.off()


#box plot for mape
mape_boxplot_overall <- ggplot(result_overall_mape, aes(x = model_name, y = mape)) +
  geom_boxplot() +
  labs(
    title = "MAPE across 20 Replicated Hold-Outs for Suicide Deaths (Overall)",
    x = "Model", 
    y = "MAPE"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 17, hjust = 0.5),
    axis.title.x = element_text(size = 12),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.title.y = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = "bottom",
    legend.key.size = unit(2, "cm"),
    text = element_text()
  )

windows()
print(mape_boxplot_overall)

png("mape_boxplot_overall.png",
    width = 16, height = 8, units = "in", res = 300)
print(mape_boxplot_overall )
dev.off()


# Extract MAE and MAPE data and combine them
result_overall_accuracy <- result_overall_mae %>%
  select(model_name, mae) %>%
  group_by(model_name) %>%
  summarise(mae = round(mean(mae), 2)) %>%
  left_join(
    result_overall_mape %>%
      select(model_name, mape) %>%
      group_by(model_name) %>%
      summarise(mape = round(mean(mape), 2)),
    by = "model_name"
  )

# Display the results
print(result_overall_accuracy)
# Save the result
write_csv(result_overall_accuracy, "result_overall_accuracy.csv")

# compara coefficient----------------------------------------------------------
# Create coefficient comparison table  ----------------------------------------------------------
# Create a data frame with model names and coefficients
coef_comparison_overall <- data.frame(
  Model_Name = model_levels,
  # Level coefficients
  Level = c(
    mean(coef_vec_level_bm_overall),
    mean(coef_vec_level_econ_only_overall),
    mean(coef_vec_level_natural_only_overall),
    mean(coef_vec_level_query_only_overall),
    mean(coef_vec_level_econ_natural_overall),
    mean(coef_vec_level_econ_query_overall),
    mean(coef_vec_level_natural_query_overall),
    mean(coef_vec_level_econ_natural_query_overall)
  ),
  # Seasonal coefficients
  Feb = c(
    mean(coef_matrix_seasonal_bm_overall[, "feb"]),
    mean(coef_matrix_seasonal_econ_only_overall[, "feb"]),
    mean(coef_matrix_seasonal_natural_only_overall[, "feb"]),
    mean(coef_matrix_seasonal_query_only_overall[, "feb"]),
    mean(coef_matrix_seasonal_econ_natural_overall[, "feb"]),
    mean(coef_matrix_seasonal_econ_query_overall[, "feb"]),
    mean(coef_matrix_seasonal_natural_query_overall[, "feb"]),
    mean(coef_matrix_seasonal_econ_natural_query_overall[, "feb"])
  ),
  Mar = c(
    mean(coef_matrix_seasonal_bm_overall[, "mar"]),
    mean(coef_matrix_seasonal_econ_only_overall[, "mar"]),
    mean(coef_matrix_seasonal_natural_only_overall[, "mar"]),
    mean(coef_matrix_seasonal_query_only_overall[, "mar"]),
    mean(coef_matrix_seasonal_econ_natural_overall[, "mar"]),
    mean(coef_matrix_seasonal_econ_query_overall[, "mar"]),
    mean(coef_matrix_seasonal_natural_query_overall[, "mar"]),
    mean(coef_matrix_seasonal_econ_natural_query_overall[, "mar"])
  ),
  Apr = c(
    mean(coef_matrix_seasonal_bm_overall[, "apr"]),
    mean(coef_matrix_seasonal_econ_only_overall[, "apr"]),
    mean(coef_matrix_seasonal_natural_only_overall[, "apr"]),
    mean(coef_matrix_seasonal_query_only_overall[, "apr"]),
    mean(coef_matrix_seasonal_econ_natural_overall[, "apr"]),
    mean(coef_matrix_seasonal_econ_query_overall[, "apr"]),
    mean(coef_matrix_seasonal_natural_query_overall[, "apr"]),
    mean(coef_matrix_seasonal_econ_natural_query_overall[, "apr"])
  ),
  May = c(
    mean(coef_matrix_seasonal_bm_overall[, "may"]),
    mean(coef_matrix_seasonal_econ_only_overall[, "may"]),
    mean(coef_matrix_seasonal_natural_only_overall[, "may"]),
    mean(coef_matrix_seasonal_query_only_overall[, "may"]),
    mean(coef_matrix_seasonal_econ_natural_overall[, "may"]),
    mean(coef_matrix_seasonal_econ_query_overall[, "may"]),
    mean(coef_matrix_seasonal_natural_query_overall[, "may"]),
    mean(coef_matrix_seasonal_econ_natural_query_overall[, "may"])
  ),
  Jun = c(
    mean(coef_matrix_seasonal_bm_overall[, "jun"]),
    mean(coef_matrix_seasonal_econ_only_overall[, "jun"]),
    mean(coef_matrix_seasonal_natural_only_overall[, "jun"]),
    mean(coef_matrix_seasonal_query_only_overall[, "jun"]),
    mean(coef_matrix_seasonal_econ_natural_overall[, "jun"]),
    mean(coef_matrix_seasonal_econ_query_overall[, "jun"]),
    mean(coef_matrix_seasonal_natural_query_overall[, "jun"]),
    mean(coef_matrix_seasonal_econ_natural_query_overall[, "jun"])
  ),
  Jul = c(
    mean(coef_matrix_seasonal_bm_overall[, "jul"]),
    mean(coef_matrix_seasonal_econ_only_overall[, "jul"]),
    mean(coef_matrix_seasonal_natural_only_overall[, "jul"]),
    mean(coef_matrix_seasonal_query_only_overall[, "jul"]),
    mean(coef_matrix_seasonal_econ_natural_overall[, "jul"]),
    mean(coef_matrix_seasonal_econ_query_overall[, "jul"]),
    mean(coef_matrix_seasonal_natural_query_overall[, "jul"]),
    mean(coef_matrix_seasonal_econ_natural_query_overall[, "jul"])
  ),
  Aug = c(
    mean(coef_matrix_seasonal_bm_overall[, "aug"]),
    mean(coef_matrix_seasonal_econ_only_overall[, "aug"]),
    mean(coef_matrix_seasonal_natural_only_overall[, "aug"]),
    mean(coef_matrix_seasonal_query_only_overall[, "aug"]),
    mean(coef_matrix_seasonal_econ_natural_overall[, "aug"]),
    mean(coef_matrix_seasonal_econ_query_overall[, "aug"]),
    mean(coef_matrix_seasonal_natural_query_overall[, "aug"]),
    mean(coef_matrix_seasonal_econ_natural_query_overall[, "aug"])
  ),
  Sep = c(
    mean(coef_matrix_seasonal_bm_overall[, "sep"]),
    mean(coef_matrix_seasonal_econ_only_overall[, "sep"]),
    mean(coef_matrix_seasonal_natural_only_overall[, "sep"]),
    mean(coef_matrix_seasonal_query_only_overall[, "sep"]),
    mean(coef_matrix_seasonal_econ_natural_overall[, "sep"]),
    mean(coef_matrix_seasonal_econ_query_overall[, "sep"]),
    mean(coef_matrix_seasonal_natural_query_overall[, "sep"]),
    mean(coef_matrix_seasonal_econ_natural_query_overall[, "sep"])
  ),
  Oct = c(
    mean(coef_matrix_seasonal_bm_overall[, "oct"]),
    mean(coef_matrix_seasonal_econ_only_overall[, "oct"]),
    mean(coef_matrix_seasonal_natural_only_overall[, "oct"]),
    mean(coef_matrix_seasonal_query_only_overall[, "oct"]),
    mean(coef_matrix_seasonal_econ_natural_overall[, "oct"]),
    mean(coef_matrix_seasonal_econ_query_overall[, "oct"]),
    mean(coef_matrix_seasonal_natural_query_overall[, "oct"]),
    mean(coef_matrix_seasonal_econ_natural_query_overall[, "oct"])
  ),
  Nov = c(
    mean(coef_matrix_seasonal_bm_overall[, "nov"]),
    mean(coef_matrix_seasonal_econ_only_overall[, "nov"]),
    mean(coef_matrix_seasonal_natural_only_overall[, "nov"]),
    mean(coef_matrix_seasonal_query_only_overall[, "nov"]),
    mean(coef_matrix_seasonal_econ_natural_overall[, "nov"]),
    mean(coef_matrix_seasonal_econ_query_overall[, "nov"]),
    mean(coef_matrix_seasonal_natural_query_overall[, "nov"]),
    mean(coef_matrix_seasonal_econ_natural_query_overall[, "nov"])
  ),
  Dec = c(
    mean(coef_matrix_seasonal_bm_overall[, "dec"]),
    mean(coef_matrix_seasonal_econ_only_overall[, "dec"]),
    mean(coef_matrix_seasonal_natural_only_overall[, "dec"]),
    mean(coef_matrix_seasonal_query_only_overall[, "dec"]),
    mean(coef_matrix_seasonal_econ_natural_overall[, "dec"]),
    mean(coef_matrix_seasonal_econ_query_overall[, "dec"]),
    mean(coef_matrix_seasonal_natural_query_overall[, "dec"]),
    mean(coef_matrix_seasonal_econ_natural_query_overall[, "dec"])
  ),
  # Economic coefficients
  Unemployment = c(
    NA,
    mean(coef_vec_unemp_econ_only_overall),
    NA,
    NA,
    mean(coef_vec_unemp_econ_natural_overall),
    mean(coef_vec_unemp_econ_query_overall),
    NA,
    mean(coef_vec_unemp_econ_natural_query_overall)
  ),
  # Natural environment coefficients
    Temperature = c(
    NA,
    NA,
    mean(coef_vec_temp_natural_only_overall),
    NA,
    mean(coef_vec_temp_econ_natural_overall),
    NA,
    mean(coef_vec_temp_natural_query_overall),
    mean(coef_vec_temp_econ_natural_query_overall)
  ),
  Solar = c(
    NA,
    NA,
    mean(coef_vec_solar_natural_only_overall),
    NA,
    mean(coef_vec_solar_econ_natural_overall),
    NA,
    mean(coef_vec_solar_natural_query_overall),
    mean(coef_vec_solar_econ_natural_query_overall)
  ),
  # Search query coefficients
`Query suicide` = c(
  NA,
  NA,
  NA,
  mean(coef_matrix_query_only_overall[, "query_suicide"], na.rm = TRUE),
  NA,
  mean(coef_matrix_query_econ_query_overall[, "query_suicide"], na.rm = TRUE),
  mean(coef_matrix_query_natural_query_overall[, "query_suicide"], na.rm = TRUE),
  mean(coef_matrix_query_econ_natural_query_overall[, "query_suicide"], na.rm = TRUE)
),
`Query depression` = c(
  NA,
  NA,
  NA,
  mean(coef_matrix_query_only_overall[, "query_depression"], na.rm = TRUE),
  NA,
  mean(coef_matrix_query_econ_query_overall[, "query_depression"], na.rm = TRUE),
  mean(coef_matrix_query_natural_query_overall[, "query_depression"], na.rm = TRUE),
  mean(coef_matrix_query_econ_natural_query_overall[, "query_depression"], na.rm = TRUE)
),
`Query unemployment` = c(
  NA,
  NA,
  NA,
  mean(coef_matrix_query_only_overall[, "query_unemployment"], na.rm = TRUE),
  NA,
  mean(coef_matrix_query_econ_query_overall[, "query_unemployment"], na.rm = TRUE),
  mean(coef_matrix_query_natural_query_overall[, "query_unemployment"], na.rm = TRUE),
  mean(coef_matrix_query_econ_natural_query_overall[, "query_unemployment"], na.rm = TRUE)
),
`Query relationship breakup` = c(
  NA,
  NA,
  NA,
  mean(coef_matrix_query_only_overall[, "query_relationship_breakup"], na.rm = TRUE),
  NA,
  mean(coef_matrix_query_econ_query_overall[, "query_relationship_breakup"], na.rm = TRUE),
  mean(coef_matrix_query_natural_query_overall[, "query_relationship_breakup"], na.rm = TRUE),
  mean(coef_matrix_query_econ_natural_query_overall[, "query_relationship_breakup"], na.rm = TRUE)
),
`Query antidepressant` = c(
  NA,
  NA,
  NA,
  mean(coef_matrix_query_only_overall[, "query_antidepressant"], na.rm = TRUE),
  NA,
  mean(coef_matrix_query_econ_query_overall[, "query_antidepressant"], na.rm = TRUE),
  mean(coef_matrix_query_natural_query_overall[, "query_antidepressant"], na.rm = TRUE),
  mean(coef_matrix_query_econ_natural_query_overall[, "query_antidepressant"], na.rm = TRUE)
),
`Query allergy` = c(
  NA,
  NA,
  NA,
  mean(coef_matrix_query_only_overall[, "query_allergy"], na.rm = TRUE),
  NA,
  mean(coef_matrix_query_econ_query_overall[, "query_allergy"], na.rm = TRUE),
  mean(coef_matrix_query_natural_query_overall[, "query_allergy"], na.rm = TRUE),
  mean(coef_matrix_query_econ_natural_query_overall[, "query_allergy"], na.rm = TRUE)
),
`Query pain` = c(
  NA,
  NA,
  NA,
  mean(coef_matrix_query_only_overall[, "query_pain"], na.rm = TRUE),
  NA,
  mean(coef_matrix_query_econ_query_overall[, "query_pain"], na.rm = TRUE),
  mean(coef_matrix_query_natural_query_overall[, "query_pain"], na.rm = TRUE),
  mean(coef_matrix_query_econ_natural_query_overall[, "query_pain"], na.rm = TRUE)
),
`Query drunkenness` = c(
  NA,
  NA,
  NA,
  mean(coef_matrix_query_only_overall[, "query_drunkenness"], na.rm = TRUE),
  NA,
  mean(coef_matrix_query_econ_query_overall[, "query_drunkenness"], na.rm = TRUE),
  mean(coef_matrix_query_natural_query_overall[, "query_drunkenness"], na.rm = TRUE),
  mean(coef_matrix_query_econ_natural_query_overall[, "query_drunkenness"], na.rm = TRUE)
),
`Query alcohol abstinence` = c(
  NA,
  NA,
  NA,
  mean(coef_matrix_query_only_overall[, "query_alcohol_abstinence"], na.rm = TRUE),
  NA,
  mean(coef_matrix_query_econ_query_overall[, "query_alcohol_abstinence"], na.rm = TRUE),
  mean(coef_matrix_query_natural_query_overall[, "query_alcohol_abstinence"], na.rm = TRUE),
  mean(coef_matrix_query_econ_natural_query_overall[, "query_alcohol_abstinence"], na.rm = TRUE)
)
)

# Transpose the dataframe and round to 2 decimal places
coef_comparison_overall_t <- data.frame(
  Variable = names(coef_comparison_overall)[-1],  # Get column names except Model_Name
  t(round(coef_comparison_overall[,-1], 2))  # Transpose and round data except Model_Name
)

# Set column names using model names
colnames(coef_comparison_overall_t)[-1] <- coef_comparison_overall$Model_Name

# Replace NA with "-" for better readability
coef_comparison_overall_t[is.na(coef_comparison_overall_t)] <- " "

# Replace Query with Query (e.g. Query suicide with Query suicide)
rn <- rownames(coef_comparison_overall_t) 
rownames(coef_comparison_overall_t) <- ifelse(
  grepl("^Query", rn, ignore.case = TRUE),
  paste("Query", tolower(trimws(gsub("[._]", " ", sub("^Query[[:space:]._]*", "", rn, ignore.case = TRUE))))),
  rn
)

# Save as CSV file for further analysis
write.csv(coef_comparison_overall_t, "coef_comparison_overall_transposed.csv", row.names = FALSE)

# Male model----------------------------------------------------------
# Basic structural time series model----------------------------------------------------------
#model name
model_id_bm_male <- "bm_male"

#storing scores
mape_vec_bm_male <- numeric(20)
mae_vec_bm_male  <- numeric(20)

#trend component
coef_vec_level_bm_male <- numeric(20)
#seasonal component
coef_matrix_seasonal_bm_male <- matrix(0, nrow = 20, ncol = 11)
colnames(coef_matrix_seasonal_bm_male) <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")


#fit model
for (i in 1:20) {
  train_col <- paste0("train_", i)
  test_col <- paste0("test_", i)
  y_col <- paste0("suicide_rate_male_scaled_", i, "_ssm_y_train_", i)
  
  model <- SSModel(
    df[[y_col]] ~ SSMtrend(degree = 1, Q = NA) + SSMseasonal(period = 12, sea.type = "dummy") ,
    H = NA
  )
  fit <- fitSSM(model, inits = c(1, 1))
  kfs <- KFS(fit$model)
  pred <- predict(fit$model, interval = "prediction", level = 0.95)

  n_pred <- sum(df[[test_col]] == 1)
  pred_test <- pred[(nrow(pred) - n_pred + 1):nrow(pred), 1]
  
  min_val <- min(df$suicide_rate_male[df[[train_col]] == 1], na.rm = TRUE)
  max_val <- max(df$suicide_rate_male[df[[train_col]] == 1], na.rm = TRUE)
  pred_inverse <- scale_max_min_vector_inverse(pred_test, min_val, max_val)
  pred_inverse <- pred_inverse * df$pop_male[df[[test_col]] == 1] / 100000
  
  actual <- df$num_suicide_male[df[[test_col]] == 1]
  
  acc <- forecast::accuracy(pred_inverse, actual)
  mape_vec_bm_male[i] <- acc[5]
  mae_vec_bm_male[i]  <- acc[3]

  coef_vec_level_bm_male[i] <- mean(coef(kfs)[,"level"])
  # extract seasonal component matrix
  seasonal_states <- coef(kfs)[, grep("^sea", colnames(coef(kfs)))]
  
  # drop the first row
  seasonal_states <- seasonal_states[-1, , drop = FALSE]

  
  # average every 12 months starting from the 1st row
  month_names <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
  for (j in 1:11) {
  monthly_vals <- seasonal_states[seq(j, nrow(seasonal_states), by = 12), 1]  
  coef_matrix_seasonal_bm_male[i, month_names[j]] <- mean(monthly_vals, na.rm = TRUE)
}
}


results_male_bm_mae <- data.frame(
  model_id = model_id_bm_male,
  rep_id = 1:20,
  test_start = thresholds,
  mae = mae_vec_bm_male
)

results_male_bm_mape <- data.frame(
  model_id = model_id_bm_male,
  rep_id = 1:20,
  test_start = thresholds,
  mape = mape_vec_bm_male
)
#summary
print(mean(results_male_bm_mae$mae))
print(mean(results_male_bm_mape$mape))
print(mean(coef_vec_level_bm_male))
print(mean(coef_matrix_seasonal_bm_male[, "feb"]))
print(mean(coef_matrix_seasonal_bm_male[, "mar"]))
print(mean(coef_matrix_seasonal_bm_male[, "apr"]))
print(mean(coef_matrix_seasonal_bm_male[, "may"]))
print(mean(coef_matrix_seasonal_bm_male[, "jun"]))
print(mean(coef_matrix_seasonal_bm_male[, "jul"]))
print(mean(coef_matrix_seasonal_bm_male[, "aug"]))
print(mean(coef_matrix_seasonal_bm_male[, "sep"]))
print(mean(coef_matrix_seasonal_bm_male[, "oct"]))
print(mean(coef_matrix_seasonal_bm_male[, "nov"]))
print(mean(coef_matrix_seasonal_bm_male[, "dec"]))

#Economic only model----------------------------------------------------------
#model name
model_id_econ_only_male <- "econ_only_male"

#storing score
mape_vec_econ_only_male <- numeric(20)
mae_vec_econ_only_male  <- numeric(20)

coef_vec_level_econ_only_male <- numeric(20)
coef_matrix_seasonal_econ_only_male <- matrix(0, nrow = 20, ncol = 11)
colnames(coef_matrix_seasonal_econ_only_male) <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
coef_vec_unemp_econ_only_male <- numeric(20)


for (i in 1:20) {
  train_col <- paste0("train_", i)
  test_col <- paste0("test_", i)
  y_col <- paste0("suicide_rate_male_scaled_", i, "_ssm_y_train_", i)
  
  model <- SSModel(
    df[[y_col]] ~ SSMtrend(degree = 1, Q = NA) + SSMseasonal(period = 12, sea.type = "dummy") + 
      SSMregression(~ df[[paste0("male_unemp_lag1_scaled_", i)]]),
    H = NA
  )
  fit <- fitSSM(model, inits = c(1, 1))
  kfs <- KFS(fit$model)

  pred <- predict(fit$model, interval = "prediction", level = 0.95)

  n_pred <- sum(df[[test_col]] == 1)
  pred_test <- pred[(nrow(pred) - n_pred + 1):nrow(pred), 1]
  
  min_val <- min(df$suicide_rate_male[df[[train_col]] == 1], na.rm = TRUE)
  max_val <- max(df$suicide_rate_male[df[[train_col]] == 1], na.rm = TRUE)
  pred_inverse <- scale_max_min_vector_inverse(pred_test, min_val, max_val)
  pred_inverse <- pred_inverse * df$pop_male[df[[test_col]] == 1] / 100000
  
  actual <- df$num_suicide_male[df[[test_col]] == 1]
  acc <- forecast::accuracy(pred_inverse, actual)
  mape_vec_econ_only_male[i] <- acc[5]
  mae_vec_econ_only_male[i]  <- acc[3]

  coef_vec_level_econ_only_male[i] <- mean(coef(kfs)[-1,"level"])

  seasonal_states <- coef(kfs)[, grep("^sea", colnames(coef(kfs)))]
  
  # drop the first row
  seasonal_states <- seasonal_states[-1, , drop = FALSE]
  
  # average every 12 months starting from the 1st row
  month_names <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
  for (m in 1:11) {
    monthly_vals <- seasonal_states[seq(m, nrow(seasonal_states), by = 12), 1]
    coef_matrix_seasonal_econ_only_male[i, month_names[m]] <- mean(monthly_vals, na.rm = TRUE)
  }
  # Extract state name index for the economic regressor
  # Save the estimated coefficient (last filtered value)
  coef_vec_unemp_econ_only_male[i] <- mean(coef(kfs)[-1, "df[[paste0(\"male_unemp_lag1_scaled_\", i)]]"])
}

results_male_econ_only_mae <- data.frame(
  model_id = model_id_econ_only_male,
  rep_id = 1:20,
  test_start = thresholds,
  mae = mae_vec_econ_only_male
)

results_male_econ_only_mape <- data.frame(
  model_id = model_id_econ_only_male,
  rep_id = 1:20,
  test_start = thresholds,
  mape = mape_vec_econ_only_male
)

print(mean(results_male_econ_only_mae$mae))
print(mean(results_male_econ_only_mape$mape))
print(mean(coef_vec_unemp_econ_only_male))
print(mean(coef_vec_level_econ_only_male))
print(mean(coef_matrix_seasonal_econ_only_male[, "feb"]))
print(mean(coef_matrix_seasonal_econ_only_male[, "mar"]))
print(mean(coef_matrix_seasonal_econ_only_male[, "apr"]))
print(mean(coef_matrix_seasonal_econ_only_male[, "may"]))
print(mean(coef_matrix_seasonal_econ_only_male[, "jun"]))
print(mean(coef_matrix_seasonal_econ_only_male[, "jul"]))
print(mean(coef_matrix_seasonal_econ_only_male[, "aug"]))
print(mean(coef_matrix_seasonal_econ_only_male[, "sep"]))
print(mean(coef_matrix_seasonal_econ_only_male[, "oct"]))
print(mean(coef_matrix_seasonal_econ_only_male[, "nov"]))
print(mean(coef_matrix_seasonal_econ_only_male[, "dec"]))

# Natural only model----------------------------------------------------------
#model name
model_id_natural_only_male <- "natural_only_male"


#storing score
mape_vec_natural_only_male <- numeric(20)
mae_vec_natural_only_male  <- numeric(20)

coef_vec_level_natural_only_male <- numeric(20)
coef_matrix_seasonal_natural_only_male <- matrix(0, nrow = 20, ncol = 11)
colnames(coef_matrix_seasonal_natural_only_male) <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
coef_vec_solar_natural_only_male <- numeric(20)
coef_vec_temp_natural_only_male <- numeric(20)

for (i in 1:20) {
  train_col <- paste0("train_", i)
  test_col <- paste0("test_", i)
  y_col <- paste0("suicide_rate_male_scaled_", i, "_ssm_y_train_", i)

  model <- SSModel(
    df[[y_col]] ~ SSMtrend(degree = 1, Q = NA) + SSMseasonal(period = 12, sea.type = "dummy") + 
      SSMregression(~ df[[paste0("solar_lag1_scaled_", i)]] + df[[paste0("temp_lag1_scaled_", i)]]),
    H = NA
  )
  fit <- fitSSM(model, inits = c(1, 1))
  kfs <- KFS(fit$model)

  pred <- predict(fit$model, interval = "prediction", level = 0.95)

  n_pred <- sum(df[[test_col]] == 1)
  pred_test <- pred[(nrow(pred) - n_pred + 1):nrow(pred), 1]

  min_val <- min(df$suicide_rate_male[df[[train_col]] == 1], na.rm = TRUE)
  max_val <- max(df$suicide_rate_male[df[[train_col]] == 1], na.rm = TRUE)
  pred_inverse <- scale_max_min_vector_inverse(pred_test, min_val, max_val)
  pred_inverse <- pred_inverse * df$pop_male[df[[test_col]] == 1] / 100000

  actual <- df$num_suicide_male[df[[test_col]] == 1]
  acc <- forecast::accuracy(pred_inverse, actual)
  mape_vec_natural_only_male[i] <- acc[5]
  mae_vec_natural_only_male[i]  <- acc[3]

  coef_vec_level_natural_only_male[i] <- mean(coef(kfs)[-1,"level"])

  seasonal_states <- coef(kfs)[, grep("^sea", colnames(coef(kfs)))]
  seasonal_states <- seasonal_states[-1, , drop = FALSE]

  month_names <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
  for (m in 1:11) {
    monthly_vals <- seasonal_states[seq(m, nrow(seasonal_states), by = 12), 1]
    coef_matrix_seasonal_natural_only_male[i, month_names[m]] <- mean(monthly_vals, na.rm = TRUE)
  }
  coef_vec_solar_natural_only_male[i] <- mean(coef(kfs)[-1, "df[[paste0(\"solar_lag1_scaled_\", i)]]"])
  coef_vec_temp_natural_only_male[i] <- mean(coef(kfs)[-1, "df[[paste0(\"temp_lag1_scaled_\", i)]]"])
}


results_male_natural_only_mae <- data.frame(
  model_id = model_id_natural_only_male,
  rep_id = 1:20,
  test_start = thresholds,
  mae = mae_vec_natural_only_male
)

results_male_natural_only_mape <- data.frame(
  model_id = model_id_natural_only_male,
  rep_id = 1:20,
  test_start = thresholds,
  mape = mape_vec_natural_only_male
)

print(mean(results_male_natural_only_mae$mae))
print(mean(results_male_natural_only_mape$mape))
print(mean(coef_vec_level_natural_only_male))
print(mean(coef_vec_solar_natural_only_male))
print(mean(coef_vec_temp_natural_only_male))
print(mean(coef_matrix_seasonal_natural_only_male[, "feb"]))
print(mean(coef_matrix_seasonal_natural_only_male[, "mar"]))
print(mean(coef_matrix_seasonal_natural_only_male[, "apr"]))
print(mean(coef_matrix_seasonal_natural_only_male[, "may"]))
print(mean(coef_matrix_seasonal_natural_only_male[, "jun"]))
print(mean(coef_matrix_seasonal_natural_only_male[, "jul"]))
print(mean(coef_matrix_seasonal_natural_only_male[, "aug"]))
print(mean(coef_matrix_seasonal_natural_only_male[, "sep"]))
print(mean(coef_matrix_seasonal_natural_only_male[, "oct"]))
print(mean(coef_matrix_seasonal_natural_only_male[, "nov"]))
print(mean(coef_matrix_seasonal_natural_only_male[, "dec"]))

#query only model----------------------------------------------------------
#model name
model_id_query_only_male <- "query_only_male"

#storing scores
mape_vec_query_only_male <- numeric(20)
mae_vec_query_only_male  <- numeric(20)
coef_vec_query_only_male <- numeric(20)
coef_vec_level_query_only_male <- numeric(20)
coef_matrix_seasonal_query_only_male <- matrix(0, nrow = 20, ncol = 11)
colnames(coef_matrix_seasonal_query_only_male) <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")

# 
coef_matrix_query_only_male <- matrix(0, nrow = 20, ncol = length(male_query))
colnames(coef_matrix_query_only_male) <- male_query
coef_vec_query_only_male <- numeric(20)


for (i in 1:20) {
  train_col <- paste0("train_", i)
  test_col <- paste0("test_", i)
  y_col <- paste0("suicide_rate_male_scaled_", i, "_ssm_y_train_", i)
  
  model <- SSModel(
    df[[y_col]] ~ SSMtrend(degree = 1, Q = NA) + SSMseasonal(period = 12, sea.type = "dummy") + 
      SSMregression(~ df[[paste0("query_suicide_scaled_", i)]] + 
                       df[[paste0("query_depression_scaled_", i)]] + 
                       df[[paste0("query_unemployment_scaled_", i)]] + 
                       df[[paste0("query_relationship_breakup_scaled_", i)]] + 
                       df[[paste0("query_antidepressant_scaled_", i)]] + 
                       df[[paste0("query_allergy_scaled_", i)]] + 
                       df[[paste0("query_pain_scaled_", i)]] + 
                       df[[paste0("query_alcohol_abstinence_scaled_", i)]]),
    H = NA
)
  fit <- fitSSM(model, inits = c(1, 1))
  kfs <- KFS(fit$model)

  pred <- predict(fit$model, interval = "prediction", level = 0.95)

  n_pred <- sum(df[[test_col]] == 1)
  pred_test <- pred[(nrow(pred) - n_pred + 1):nrow(pred), 1]
  
  min_val <- min(df$suicide_rate_male[df[[train_col]] == 1], na.rm = TRUE)
  max_val <- max(df$suicide_rate_male[df[[train_col]] == 1], na.rm = TRUE)
  pred_inverse <- scale_max_min_vector_inverse(pred_test, min_val, max_val)
  pred_inverse <- pred_inverse * df$pop_male[df[[test_col]] == 1] / 100000

  actual <- df$num_suicide_male[df[[test_col]] == 1]
  acc <- forecast::accuracy(pred_inverse, actual) 
  mape_vec_query_only_male[i] <- acc[5]
  mae_vec_query_only_male[i]  <- acc[3]

  for (j in seq_along(male_query)) {
    query_name <- male_query[j]
    coef_matrix_query_only_male[i, j] <- mean(coef(kfs)[-1, paste0("df[[paste0(\"", query_name, "_scaled_\", i)]]")])
  }
  coef_vec_query_only_male[i] <- sum(coef_matrix_query_only_male[i,])
  coef_vec_level_query_only_male[i] <- mean(coef(kfs)[-1,"level"])
  seasonal_states <- coef(kfs)[, grep("^sea", colnames(coef(kfs)))]
  
  # drop the first row
  seasonal_states <- seasonal_states[-1, , drop = FALSE]
  
  # average every 12 months starting from the 1st row
  month_names <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
  for (m in 1:11) {
    monthly_vals <- seasonal_states[seq(m, nrow(seasonal_states), by = 12), 1]
    coef_matrix_seasonal_query_only_male[i, month_names[m]] <- mean(monthly_vals, na.rm = TRUE)
  }
}

results_male_query_only_mae <- data.frame(
  model_id = model_id_query_only_male,
  rep_id = 1:20,
  test_start = thresholds,
  mae = mae_vec_query_only_male
)

results_male_query_only_mape <- data.frame(
  model_id = model_id_query_only_male,
  rep_id = 1:20,
  test_start = thresholds,
  mape = mape_vec_query_only_male
)

print(mean(results_male_query_only_mae$mae))
print(mean(results_male_query_only_mape$mape))
print(mean(coef_vec_level_query_only_male))
print(mean(coef_matrix_seasonal_query_only_male[, "feb"]))
print(mean(coef_matrix_seasonal_query_only_male[, "mar"]))
print(mean(coef_matrix_seasonal_query_only_male[, "apr"]))
print(mean(coef_matrix_seasonal_query_only_male[, "may"]))
print(mean(coef_matrix_seasonal_query_only_male[, "jun"]))
print(mean(coef_matrix_seasonal_query_only_male[, "jul"]))
print(mean(coef_matrix_seasonal_query_only_male[, "aug"]))
print(mean(coef_matrix_seasonal_query_only_male[, "sep"]))
print(mean(coef_matrix_seasonal_query_only_male[, "oct"]))
print(mean(coef_matrix_seasonal_query_only_male[, "nov"]))
print(mean(coef_matrix_seasonal_query_only_male[, "dec"]))
for (q in male_query) {
  cat(q, ":", mean(coef_matrix_query_only_male[, q], na.rm = TRUE), "\n")
}


# economic and natural model----------------------------------------------------------
#model name
model_id_econ_natural_male <- "econ_natural_male"

#storing score
mape_vec_econ_natural_male <- numeric(20)
mae_vec_econ_natural_male  <- numeric(20)

coef_vec_level_econ_natural_male <- numeric(20)
coef_matrix_seasonal_econ_natural_male <- matrix(0, nrow = 20, ncol = 11)
colnames(coef_matrix_seasonal_econ_natural_male) <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
coef_vec_unemp_econ_natural_male <- numeric(20)
coef_vec_solar_econ_natural_male <- numeric(20)
coef_vec_temp_econ_natural_male <- numeric(20)


for (i in 1:20) {
  train_col <- paste0("train_", i)
  test_col <- paste0("test_", i)
  y_col <- paste0("suicide_rate_male_scaled_", i, "_ssm_y_train_", i)

  model <- SSModel(
    df[[y_col]] ~ SSMtrend(degree = 1, Q = NA) + SSMseasonal(period = 12, sea.type = "dummy") + 
      SSMregression(~ df[[paste0("male_unemp_lag1_scaled_", i)]] + df[[paste0("solar_lag1_scaled_", i)]] + df[[paste0("temp_lag1_scaled_", i)]]),
    H = NA
  )
  fit <- fitSSM(model, inits = c(1, 1))
  kfs <- KFS(fit$model)

  pred <- predict(fit$model, interval = "prediction", level = 0.95)

  n_pred <- sum(df[[test_col]] == 1)
  pred_test <- pred[(nrow(pred) - n_pred + 1):nrow(pred), 1]
  
  min_val <- min(df$suicide_rate_male[df[[train_col]] == 1], na.rm = TRUE)
  max_val <- max(df$suicide_rate_male[df[[train_col]] == 1], na.rm = TRUE)
  pred_inverse <- scale_max_min_vector_inverse(pred_test, min_val, max_val)
  pred_inverse <- pred_inverse * df$pop_male[df[[test_col]] == 1] / 100000

  actual <- df$num_suicide_male[df[[test_col]] == 1]
  acc <- forecast::accuracy(pred_inverse, actual)
  mape_vec_econ_natural_male[i] <- acc[5]
  mae_vec_econ_natural_male[i]  <- acc[3]

  coef_vec_level_econ_natural_male[i] <- mean(coef(kfs)[-1,"level"])

  seasonal_states <- coef(kfs)[, grep("^sea", colnames(coef(kfs)))]
  seasonal_states <- seasonal_states[-1, , drop = FALSE]

  month_names <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
  for (m in 1:11) {
    monthly_vals <- seasonal_states[seq(m, nrow(seasonal_states), by = 12), 1]
    coef_matrix_seasonal_econ_natural_male[i, month_names[m]] <- mean(monthly_vals, na.rm = TRUE)
  }
  coef_vec_unemp_econ_natural_male[i] <- mean(coef(kfs)[-1, "df[[paste0(\"male_unemp_lag1_scaled_\", i)]]"])
  coef_vec_solar_econ_natural_male[i] <- mean(coef(kfs)[-1, "df[[paste0(\"solar_lag1_scaled_\", i)]]"])
  coef_vec_temp_econ_natural_male[i] <- mean(coef(kfs)[-1, "df[[paste0(\"temp_lag1_scaled_\", i)]]"])
}

results_male_econ_natural_mae <- data.frame(
  model_id = model_id_econ_natural_male,
  rep_id = 1:20,
  test_start = thresholds,
  mae = mae_vec_econ_natural_male
)

results_male_econ_natural_mape <- data.frame(
  model_id = model_id_econ_natural_male,
  rep_id = 1:20,
  test_start = thresholds,
  mape = mape_vec_econ_natural_male
)

print(mean(results_male_econ_natural_mae$mae))
print(mean(results_male_econ_natural_mape$mape))
print(mean(coef_vec_level_econ_natural_male))
print(mean(coef_vec_unemp_econ_natural_male))
print(mean(coef_vec_solar_econ_natural_male))
print(mean(coef_vec_temp_econ_natural_male))
print(mean(coef_matrix_seasonal_econ_natural_male[, "feb"]))
print(mean(coef_matrix_seasonal_econ_natural_male[, "mar"]))
print(mean(coef_matrix_seasonal_econ_natural_male[, "apr"]))
print(mean(coef_matrix_seasonal_econ_natural_male[, "may"]))
print(mean(coef_matrix_seasonal_econ_natural_male[, "jun"]))
print(mean(coef_matrix_seasonal_econ_natural_male[, "jul"]))
print(mean(coef_matrix_seasonal_econ_natural_male[, "aug"]))
print(mean(coef_matrix_seasonal_econ_natural_male[, "sep"]))
print(mean(coef_matrix_seasonal_econ_natural_male[, "oct"]))
print(mean(coef_matrix_seasonal_econ_natural_male[, "nov"]))
print(mean(coef_matrix_seasonal_econ_natural_male[, "dec"]))

# economic and query model----------------------------------------------------------
#model name
model_id_econ_query_male <- "econ_query_male"

#storing score
mape_vec_econ_query_male <- numeric(20)
mae_vec_econ_query_male  <- numeric(20)

coef_vec_level_econ_query_male <- numeric(20)
coef_matrix_seasonal_econ_query_male <- matrix(0, nrow = 20, ncol = 11)
colnames(coef_matrix_seasonal_econ_query_male) <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
coef_vec_unemp_econ_query_male <- numeric(20)

coef_matrix_query_econ_query_male <- matrix(0, nrow = 20, ncol = length(male_query))
colnames(coef_matrix_query_econ_query_male) <- male_query
coef_vec_query_econ_query_male <- numeric(20)

for (i in 1:20) {
  train_col <- paste0("train_", i)
  test_col <- paste0("test_", i)
  y_col <- paste0("suicide_rate_male_scaled_", i, "_ssm_y_train_", i)


  model <- SSModel(
    df[[y_col]] ~ SSMtrend(degree = 1, Q = NA) + SSMseasonal(period = 12, sea.type = "dummy") + 
      SSMregression(~ df[[paste0("male_unemp_lag1_scaled_", i)]] + df[[paste0("query_suicide_scaled_", i)]] + df[[paste0("query_depression_scaled_", i)]] + df[[paste0("query_unemployment_scaled_", i)]] + df[[paste0("query_relationship_breakup_scaled_", i)]] + df[[paste0("query_antidepressant_scaled_", i)]] + df[[paste0("query_allergy_scaled_", i)]] + df[[paste0("query_pain_scaled_", i)]] + df[[paste0("query_alcohol_abstinence_scaled_", i)]]),
    H = NA
  )
  fit <- fitSSM(model, inits = c(1, 1))
  kfs <- KFS(fit$model)

  pred <- predict(fit$model, interval = "prediction", level = 0.95)

  n_pred <- sum(df[[test_col]] == 1)
  pred_test <- pred[(nrow(pred) - n_pred + 1):nrow(pred), 1]

  min_val <- min(df$suicide_rate_male[df[[train_col]] == 1], na.rm = TRUE)
  max_val <- max(df$suicide_rate_male[df[[train_col]] == 1], na.rm = TRUE)
  pred_inverse <- scale_max_min_vector_inverse(pred_test, min_val, max_val)
  pred_inverse <- pred_inverse * df$pop_male[df[[test_col]] == 1] / 100000

  actual <- df$num_suicide_male[df[[test_col]] == 1]
  acc <- forecast::accuracy(pred_inverse, actual)
  mape_vec_econ_query_male[i] <- acc[5]
  mae_vec_econ_query_male[i]  <- acc[3]

  coef_vec_unemp_econ_query_male[i] <- mean(coef(kfs)[-1, "df[[paste0(\"male_unemp_lag1_scaled_\", i)]]"])
  coef_vec_level_econ_query_male[i] <- mean(coef(kfs)[-1,"level"])

  for (j in seq_along(male_query)) {
    query_name <- male_query[j]
    coef_matrix_query_econ_query_male[i, query_name] <- mean(coef(kfs)[-1, paste0("df[[paste0(\"", query_name, "_scaled_\", i)]]")])
  }
  coef_vec_query_econ_query_male[i] <- sum(coef_matrix_query_econ_query_male[i,])

  seasonal_states <- coef(kfs)[, grep("^sea", colnames(coef(kfs)))]
  seasonal_states <- seasonal_states[-1, , drop = FALSE]

  month_names <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
  for (m in 1:11) {
    monthly_vals <- seasonal_states[seq(m, nrow(seasonal_states), by = 12), 1]
    coef_matrix_seasonal_econ_query_male[i, month_names[m]] <- mean(monthly_vals, na.rm = TRUE)
  }
}

results_male_econ_query_mae <- data.frame(
  model_id = model_id_econ_query_male,
  rep_id = 1:20,
  test_start = thresholds,
  mae = mae_vec_econ_query_male
)

results_male_econ_query_mape <- data.frame(
  model_id = model_id_econ_query_male,
  rep_id = 1:20,
  test_start = thresholds,
  mape = mape_vec_econ_query_male
)

print(mean(results_male_econ_query_mae$mae))
print(mean(results_male_econ_query_mape$mape))
print(mean(coef_vec_level_econ_query_male))
print(mean(coef_vec_unemp_econ_query_male))
for (q in male_query) {
  cat(q, ":", mean(coef_matrix_query_econ_query_male[, q], na.rm = TRUE), "\n")
}

print(mean(coef_matrix_seasonal_econ_query_male[, "feb"]))
print(mean(coef_matrix_seasonal_econ_query_male[, "mar"]))
print(mean(coef_matrix_seasonal_econ_query_male[, "apr"]))
print(mean(coef_matrix_seasonal_econ_query_male[, "may"]))
print(mean(coef_matrix_seasonal_econ_query_male[, "jun"]))
print(mean(coef_matrix_seasonal_econ_query_male[, "jul"]))
print(mean(coef_matrix_seasonal_econ_query_male[, "aug"]))
print(mean(coef_matrix_seasonal_econ_query_male[, "sep"]))
print(mean(coef_matrix_seasonal_econ_query_male[, "oct"]))
print(mean(coef_matrix_seasonal_econ_query_male[, "nov"]))
print(mean(coef_matrix_seasonal_econ_query_male[, "dec"]))

# natural and query model----------------------------------------------------------
#model name
model_id_natural_query_male <- "natural_query_male"

#storing score
mape_vec_natural_query_male <- numeric(20)
mae_vec_natural_query_male  <- numeric(20)

coef_vec_level_natural_query_male <- numeric(20)
coef_matrix_seasonal_natural_query_male <- matrix(0, nrow = 20, ncol = 11)
colnames(coef_matrix_seasonal_natural_query_male) <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
coef_vec_solar_natural_query_male <- numeric(20)
coef_vec_temp_natural_query_male <- numeric(20)

coef_matrix_query_natural_query_male <- matrix(0, nrow = 20, ncol = length(male_query))
colnames(coef_matrix_query_natural_query_male) <- male_query
coef_vec_query_natural_query_male <- numeric(20)

for (i in 1:20) {
  train_col <- paste0("train_", i)
  test_col <- paste0("test_", i)
  y_col <- paste0("suicide_rate_male_scaled_", i, "_ssm_y_train_", i)

  model <- SSModel(
    df[[y_col]] ~ SSMtrend(degree = 1, Q = NA) + SSMseasonal(period = 12, sea.type = "dummy") + 
      SSMregression(~ df[[paste0("solar_lag1_scaled_", i)]] + df[[paste0("temp_lag1_scaled_", i)]] + df[[paste0("query_suicide_scaled_", i)]] + df[[paste0("query_depression_scaled_", i)]] + df[[paste0("query_unemployment_scaled_", i)]] + df[[paste0("query_relationship_breakup_scaled_", i)]] + df[[paste0("query_antidepressant_scaled_", i)]] + df[[paste0("query_allergy_scaled_", i)]] + df[[paste0("query_pain_scaled_", i)]] + df[[paste0("query_alcohol_abstinence_scaled_", i)]]),
    H = NA
  )
  fit <- fitSSM(model, inits = c(1, 1))
  kfs <- KFS(fit$model)

  pred <- predict(fit$model, interval = "prediction", level = 0.95)
  n_pred <- sum(df[[test_col]] == 1)
  pred_test <- pred[(nrow(pred) - n_pred + 1):nrow(pred), 1]

  min_val <- min(df$suicide_rate_male[df[[train_col]] == 1], na.rm = TRUE)
  max_val <- max(df$suicide_rate_male[df[[train_col]] == 1], na.rm = TRUE)
  pred_inverse <- scale_max_min_vector_inverse(pred_test, min_val, max_val)
  pred_inverse <- pred_inverse * df$pop_male[df[[test_col]] == 1] / 100000

  actual <- df$num_suicide_male[df[[test_col]] == 1]
  acc <- forecast::accuracy(pred_inverse, actual)
  mape_vec_natural_query_male[i] <- acc[5]
  mae_vec_natural_query_male[i]  <- acc[3]

  coef_vec_solar_natural_query_male[i] <- mean(coef(kfs)[-1, "df[[paste0(\"solar_lag1_scaled_\", i)]]"])
  coef_vec_temp_natural_query_male[i] <- mean(coef(kfs)[-1, "df[[paste0(\"temp_lag1_scaled_\", i)]]"])
  coef_vec_level_natural_query_male[i] <- mean(coef(kfs)[-1,"level"])

  for (j in seq_along(male_query)) {
    query_name <- male_query[j]
    coef_matrix_query_natural_query_male[i, query_name] <- mean(coef(kfs)[-1, paste0("df[[paste0(\"", query_name, "_scaled_\", i)]]")])
  }
  coef_vec_query_natural_query_male[i] <- sum(coef_matrix_query_natural_query_male[i,])

  seasonal_states <- coef(kfs)[, grep("^sea", colnames(coef(kfs)))]
  seasonal_states <- seasonal_states[-1, , drop = FALSE]

  month_names <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
  for (m in 1:11) {
    monthly_vals <- seasonal_states[seq(m, nrow(seasonal_states), by = 12), 1]
    coef_matrix_seasonal_natural_query_male[i, month_names[m]] <- mean(monthly_vals, na.rm = TRUE)
  }
}

results_male_natural_query_mae <- data.frame(
  model_id = model_id_natural_query_male,
  rep_id = 1:20,
  test_start = thresholds,
  mae = mae_vec_natural_query_male
)

results_male_natural_query_mape <- data.frame(
  model_id = model_id_natural_query_male,
  rep_id = 1:20,
  test_start = thresholds,
  mape = mape_vec_natural_query_male
)

print(mean(results_male_natural_query_mae$mae))
print(mean(results_male_natural_query_mape$mape))
print(mean(coef_vec_level_natural_query_male))
print(mean(coef_vec_solar_natural_query_male))
print(mean(coef_vec_temp_natural_query_male))
for (q in male_query) {
  cat(q, ":", mean(coef_matrix_query_natural_query_male[, q], na.rm = TRUE), "\n")
}
print(mean(coef_matrix_seasonal_natural_query_male[, "feb"]))
print(mean(coef_matrix_seasonal_natural_query_male[, "mar"]))
print(mean(coef_matrix_seasonal_natural_query_male[, "apr"]))
print(mean(coef_matrix_seasonal_natural_query_male[, "may"]))
print(mean(coef_matrix_seasonal_natural_query_male[, "jun"]))
print(mean(coef_matrix_seasonal_natural_query_male[, "jul"]))
print(mean(coef_matrix_seasonal_natural_query_male[, "aug"]))
print(mean(coef_matrix_seasonal_natural_query_male[, "sep"]))
print(mean(coef_matrix_seasonal_natural_query_male[, "oct"]))
print(mean(coef_matrix_seasonal_natural_query_male[, "nov"]))
print(mean(coef_matrix_seasonal_natural_query_male[, "dec"]))

# economic and natural and query model----------------------------------------------------------
#model name
model_id_econ_natural_query_male <- "econ_natural_query_male"

#storing score
mape_vec_econ_natural_query_male <- numeric(20)
mae_vec_econ_natural_query_male  <- numeric(20)

coef_vec_level_econ_natural_query_male <- numeric(20)
coef_matrix_seasonal_econ_natural_query_male <- matrix(0, nrow = 20, ncol = 11)
colnames(coef_matrix_seasonal_econ_natural_query_male) <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
coef_vec_unemp_econ_natural_query_male <- numeric(20)
coef_vec_solar_econ_natural_query_male <- numeric(20)
coef_vec_temp_econ_natural_query_male <- numeric(20)
coef_matrix_query_econ_natural_query_male <- matrix(0, nrow = 20, ncol = length(male_query))
colnames(coef_matrix_query_econ_natural_query_male) <- male_query
coef_vec_query_econ_natural_query_male <- numeric(20)

for (i in 1:20) {
  train_col <- paste0("train_", i)
  test_col <- paste0("test_", i)
  y_col <- paste0("suicide_rate_male_scaled_", i, "_ssm_y_train_", i)

  model <- SSModel(
    df[[y_col]] ~ SSMtrend(degree = 1, Q = NA) + SSMseasonal(period = 12, sea.type = "dummy") + 
      SSMregression(~ df[[paste0("male_unemp_lag1_scaled_", i)]] + df[[paste0("solar_lag1_scaled_", i)]] + df[[paste0("temp_lag1_scaled_", i)]] + df[[paste0("query_suicide_scaled_", i)]] + df[[paste0("query_depression_scaled_", i)]] + df[[paste0("query_unemployment_scaled_", i)]] + df[[paste0("query_relationship_breakup_scaled_", i)]] + df[[paste0("query_antidepressant_scaled_", i)]] + df[[paste0("query_allergy_scaled_", i)]] + df[[paste0("query_pain_scaled_", i)]] + df[[paste0("query_drunkenness_scaled_", i)]] + df[[paste0("query_alcohol_abstinence_scaled_", i)]]),
    H = NA
  )
  fit <- fitSSM(model, inits = c(1, 1))
  kfs <- KFS(fit$model)

  pred <- predict(fit$model, interval = "prediction", level = 0.95)
  n_pred <- sum(df[[test_col]] == 1)
  pred_test <- pred[(nrow(pred) - n_pred + 1):nrow(pred), 1]

  min_val <- min(df$suicide_rate_male[df[[train_col]] == 1], na.rm = TRUE)
  max_val <- max(df$suicide_rate_male[df[[train_col]] == 1], na.rm = TRUE)
  pred_inverse <- scale_max_min_vector_inverse(pred_test, min_val, max_val)
  pred_inverse <- pred_inverse * df$pop_male[df[[test_col]] == 1] / 100000

  actual <- df$num_suicide_male[df[[test_col]] == 1]
  acc <- forecast::accuracy(pred_inverse, actual)
  mape_vec_econ_natural_query_male[i] <- acc[5]
  mae_vec_econ_natural_query_male[i]  <- acc[3]

  coef_vec_unemp_econ_natural_query_male[i] <- mean(coef(kfs)[-1, "df[[paste0(\"male_unemp_lag1_scaled_\", i)]]"])
  coef_vec_solar_econ_natural_query_male[i] <- mean(coef(kfs)[-1, "df[[paste0(\"solar_lag1_scaled_\", i)]]"])
  coef_vec_temp_econ_natural_query_male[i] <- mean(coef(kfs)[-1, "df[[paste0(\"temp_lag1_scaled_\", i)]]"])
  coef_vec_level_econ_natural_query_male[i] <- mean(coef(kfs)[-1,"level"])

  for (j in seq_along(male_query)) {
    query_name <- male_query[j]
    coef_matrix_query_econ_natural_query_male[i, query_name] <- mean(coef(kfs)[-1, paste0("df[[paste0(\"", query_name, "_scaled_\", i)]]")])
  }
  coef_vec_query_econ_natural_query_male[i] <- sum(coef_matrix_query_econ_natural_query_male[i,])

  seasonal_states <- coef(kfs)[, grep("^sea", colnames(coef(kfs)))]
  seasonal_states <- seasonal_states[-1, , drop = FALSE]

  month_names <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
  for (m in 1:11) {
    monthly_vals <- seasonal_states[seq(m, nrow(seasonal_states), by = 12), 1]
    coef_matrix_seasonal_econ_natural_query_male[i, month_names[m]] <- mean(monthly_vals, na.rm = TRUE)
  }
}

results_male_econ_natural_query_mae <- data.frame(
  model_id = model_id_econ_natural_query_male,
  rep_id = 1:20,
  test_start = thresholds,
  mae = mae_vec_econ_natural_query_male
)

results_male_econ_natural_query_mape <- data.frame(
  model_id = model_id_econ_natural_query_male,
  rep_id = 1:20,
  test_start = thresholds,
  mape = mape_vec_econ_natural_query_male
)

print(mean(results_male_econ_natural_query_mae$mae))
print(mean(results_male_econ_natural_query_mape$mape))
print(mean(coef_vec_level_econ_natural_query_male))
print(mean(coef_vec_unemp_econ_natural_query_male))
print(mean(coef_vec_solar_econ_natural_query_male))
print(mean(coef_vec_temp_econ_natural_query_male))
for (q in male_query) {
  cat(q, ":", mean(coef_matrix_query_econ_natural_query_male[, q], na.rm = TRUE), "\n")
}
print(mean(coef_matrix_seasonal_econ_natural_query_male[, "feb"]))
print(mean(coef_matrix_seasonal_econ_natural_query_male[, "mar"]))
print(mean(coef_matrix_seasonal_econ_natural_query_male[, "apr"]))
print(mean(coef_matrix_seasonal_econ_natural_query_male[, "may"]))
print(mean(coef_matrix_seasonal_econ_natural_query_male[, "jun"]))
print(mean(coef_matrix_seasonal_econ_natural_query_male[, "jul"]))
print(mean(coef_matrix_seasonal_econ_natural_query_male[, "aug"]))
print(mean(coef_matrix_seasonal_econ_natural_query_male[, "sep"]))
print(mean(coef_matrix_seasonal_econ_natural_query_male[, "oct"]))
print(mean(coef_matrix_seasonal_econ_natural_query_male[, "nov"]))
print(mean(coef_matrix_seasonal_econ_natural_query_male[, "dec"]))

# Compare Accuracy  ----------------------------------------------------------
# Basic Structural Time Series
results_male_bm_mae$model_name <- "Basic Structural Time Series"
results_male_bm_mape$model_name <- "Basic Structural Time Series"

# Econ Only
results_male_econ_only_mae$model_name <- "Econ Only"
results_male_econ_only_mape$model_name <- "Econ Only"

# Natural Env Only
results_male_natural_only_mae$model_name <- "Natural Env Only"
results_male_natural_only_mape$model_name <- "Natural Env Only"

# Search Query Only
results_male_query_only_mae$model_name <- "Search Query Only"
results_male_query_only_mape$model_name <- "Search Query Only"

# Econ + Natural Env
results_male_econ_natural_mae$model_name <- "Econ + Natural Env"
results_male_econ_natural_mape$model_name <- "Econ + Natural Env"

# Econ + Search Query
results_male_econ_query_mae$model_name <- "Econ + Search Query"
results_male_econ_query_mape$model_name <- "Econ + Search Query"

# Natural Env + Search Query
results_male_natural_query_mae$model_name <- "Natural Env + Search Query"
results_male_natural_query_mape$model_name <- "Natural Env + Search Query"

# Econ + Natural Env + Search Query
results_male_econ_natural_query_mae$model_name <- "Econ + Natural Env + Search Query"
results_male_econ_natural_query_mape$model_name <- "Econ + Natural Env + Search Query"

# Bind all results
result_male_mae <- rbind(
  results_male_bm_mae,
  results_male_econ_only_mae,
  results_male_natural_only_mae,
  results_male_query_only_mae,
  results_male_econ_natural_mae,
  results_male_econ_query_mae,
  results_male_natural_query_mae,
  results_male_econ_natural_query_mae
)

# Bind all results
result_male_mape <- rbind(
  results_male_bm_mape,
  results_male_econ_only_mape,
  results_male_natural_only_mape,
  results_male_query_only_mape,
  results_male_econ_natural_mape,
  results_male_econ_query_mape,
  results_male_natural_query_mape,
  results_male_econ_natural_query_mape
)

model_levels <- c(
  "Basic Structural Time Series",
  "Econ Only",
  "Natural Env Only",
  "Search Query Only",
  "Econ + Natural Env",
  "Econ + Search Query",
  "Natural Env + Search Query",
  "Econ + Natural Env + Search Query"
)

result_male_mae$model_name <- factor(result_male_mae$model_name, levels = model_levels)
result_male_mape$model_name <- factor(result_male_mape$model_name, levels = model_levels)

# Box plot for mae
mae_boxplot <- ggplot(result_male_mae, aes(x = model_name, y = mae)) +
  geom_boxplot() +
  labs(
    title = "MAE across 20 Replicated Hold-Outs for Suicide Deaths (Male)",
    x = "Model",
    y = "MAE"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 17, hjust = 0.5),
    axis.title.x = element_text(size = 12),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.title.y = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = "bottom",
    legend.key.size = unit(2, "cm"),
    text = element_text()
  )

# Save MAE plot
png("mae_boxplot_male.png",
    width = 16, height = 8.0, units = "in", res = 300)
print(mae_boxplot)
dev.off()

# Box plot for mape
mape_boxplot_male <- ggplot(result_male_mape, aes(x = model_name, y = mape)) +
  geom_boxplot() +
  labs(
    title = "MAPE across 20 Replicated Hold-Outs for Suicide Deaths (Male)",
    x = "Model",
    y = "MAPE"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 17, hjust = 0.5),
    axis.title.x = element_text(size = 12),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.title.y = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = "bottom",
    legend.key.size = unit(2, "cm"),
    text = element_text()
  )

# Save MAPE plot
png("mape_boxplot_male.png",
    width = 16, height = 8.0, units = "in", res = 300)
print(mape_boxplot_male)
dev.off()

# Extract MAE and MAPE data and combine them
result_male_accuracy <- result_male_mae %>%
  select(model_name, mae) %>%
  group_by(model_name) %>%
  summarise(mae = round(mean(mae), 2)) %>%
  left_join(
    result_male_mape %>%
      select(model_name, mape) %>%
      group_by(model_name) %>%
      summarise(mape = round(mean(mape), 2)),
    by = "model_name"
  )

# Display the results
print(result_male_accuracy)
# Save the result
write_csv(result_male_accuracy, "result_male_accuracy.csv")

# Create coefficient comparison table for male models ----------------------------------------------------------
# Create a data frame with model names and coefficients
coef_comparison_male <- data.frame(
  Model_Name = model_levels,
  # Level coefficients
  Level = c(
    mean(coef_vec_level_bm_male),
    mean(coef_vec_level_econ_only_male),
    mean(coef_vec_level_natural_only_male),
    mean(coef_vec_level_query_only_male),
    mean(coef_vec_level_econ_natural_male),
    mean(coef_vec_level_econ_query_male),
    mean(coef_vec_level_natural_query_male),
    mean(coef_vec_level_econ_natural_query_male)
  ),
  # Seasonal coefficients
  Feb = c(
    mean(coef_matrix_seasonal_bm_male[, "feb"]),
    mean(coef_matrix_seasonal_econ_only_male[, "feb"]),
    mean(coef_matrix_seasonal_natural_only_male[, "feb"]),
    mean(coef_matrix_seasonal_query_only_male[, "feb"]),
    mean(coef_matrix_seasonal_econ_natural_male[, "feb"]),
    mean(coef_matrix_seasonal_econ_query_male[, "feb"]),
    mean(coef_matrix_seasonal_natural_query_male[, "feb"]),
    mean(coef_matrix_seasonal_econ_natural_query_male[, "feb"])
  ),
  Mar = c(
    mean(coef_matrix_seasonal_bm_male[, "mar"]),
    mean(coef_matrix_seasonal_econ_only_male[, "mar"]),
    mean(coef_matrix_seasonal_natural_only_male[, "mar"]),
    mean(coef_matrix_seasonal_query_only_male[, "mar"]),
    mean(coef_matrix_seasonal_econ_natural_male[, "mar"]),
    mean(coef_matrix_seasonal_econ_query_male[, "mar"]),
    mean(coef_matrix_seasonal_natural_query_male[, "mar"]),
    mean(coef_matrix_seasonal_econ_natural_query_male[, "mar"])
  ),
  Apr = c(
    mean(coef_matrix_seasonal_bm_male[, "apr"]),
    mean(coef_matrix_seasonal_econ_only_male[, "apr"]),
    mean(coef_matrix_seasonal_natural_only_male[, "apr"]),
    mean(coef_matrix_seasonal_query_only_male[, "apr"]),
    mean(coef_matrix_seasonal_econ_natural_male[, "apr"]),
    mean(coef_matrix_seasonal_econ_query_male[, "apr"]),
    mean(coef_matrix_seasonal_natural_query_male[, "apr"]),
    mean(coef_matrix_seasonal_econ_natural_query_male[, "apr"])
  ),
  May = c(
    mean(coef_matrix_seasonal_bm_male[, "may"]),
    mean(coef_matrix_seasonal_econ_only_male[, "may"]),
    mean(coef_matrix_seasonal_natural_only_male[, "may"]),
    mean(coef_matrix_seasonal_query_only_male[, "may"]),
    mean(coef_matrix_seasonal_econ_natural_male[, "may"]),
    mean(coef_matrix_seasonal_econ_query_male[, "may"]),
    mean(coef_matrix_seasonal_natural_query_male[, "may"]),
    mean(coef_matrix_seasonal_econ_natural_query_male[, "may"])
  ),
  Jun = c(
    mean(coef_matrix_seasonal_bm_male[, "jun"]),
    mean(coef_matrix_seasonal_econ_only_male[, "jun"]),
    mean(coef_matrix_seasonal_natural_only_male[, "jun"]),
    mean(coef_matrix_seasonal_query_only_male[, "jun"]),
    mean(coef_matrix_seasonal_econ_natural_male[, "jun"]),
    mean(coef_matrix_seasonal_econ_query_male[, "jun"]),
    mean(coef_matrix_seasonal_natural_query_male[, "jun"]),
    mean(coef_matrix_seasonal_econ_natural_query_male[, "jun"])
  ),
  Jul = c(
    mean(coef_matrix_seasonal_bm_male[, "jul"]),
    mean(coef_matrix_seasonal_econ_only_male[, "jul"]),
    mean(coef_matrix_seasonal_natural_only_male[, "jul"]),
    mean(coef_matrix_seasonal_query_only_male[, "jul"]),
    mean(coef_matrix_seasonal_econ_natural_male[, "jul"]),
    mean(coef_matrix_seasonal_econ_query_male[, "jul"]),
    mean(coef_matrix_seasonal_natural_query_male[, "jul"]),
    mean(coef_matrix_seasonal_econ_natural_query_male[, "jul"])
  ),
  Aug = c(
    mean(coef_matrix_seasonal_bm_male[, "aug"]),
    mean(coef_matrix_seasonal_econ_only_male[, "aug"]),
    mean(coef_matrix_seasonal_natural_only_male[, "aug"]),
    mean(coef_matrix_seasonal_query_only_male[, "aug"]),
    mean(coef_matrix_seasonal_econ_natural_male[, "aug"]),
    mean(coef_matrix_seasonal_econ_query_male[, "aug"]),
    mean(coef_matrix_seasonal_natural_query_male[, "aug"]),
    mean(coef_matrix_seasonal_econ_natural_query_male[, "aug"])
  ),
  Sep = c(
    mean(coef_matrix_seasonal_bm_male[, "sep"]),
    mean(coef_matrix_seasonal_econ_only_male[, "sep"]),
    mean(coef_matrix_seasonal_natural_only_male[, "sep"]),
    mean(coef_matrix_seasonal_query_only_male[, "sep"]),
    mean(coef_matrix_seasonal_econ_natural_male[, "sep"]),
    mean(coef_matrix_seasonal_econ_query_male[, "sep"]),
    mean(coef_matrix_seasonal_natural_query_male[, "sep"]),
    mean(coef_matrix_seasonal_econ_natural_query_male[, "sep"])
  ),
  Oct = c(
    mean(coef_matrix_seasonal_bm_male[, "oct"]),
    mean(coef_matrix_seasonal_econ_only_male[, "oct"]),
    mean(coef_matrix_seasonal_natural_only_male[, "oct"]),
    mean(coef_matrix_seasonal_query_only_male[, "oct"]),
    mean(coef_matrix_seasonal_econ_natural_male[, "oct"]),
    mean(coef_matrix_seasonal_econ_query_male[, "oct"]),
    mean(coef_matrix_seasonal_natural_query_male[, "oct"]),
    mean(coef_matrix_seasonal_econ_natural_query_male[, "oct"])
  ),
  Nov = c(
    mean(coef_matrix_seasonal_bm_male[, "nov"]),
    mean(coef_matrix_seasonal_econ_only_male[, "nov"]),
    mean(coef_matrix_seasonal_natural_only_male[, "nov"]),
    mean(coef_matrix_seasonal_query_only_male[, "nov"]),
    mean(coef_matrix_seasonal_econ_natural_male[, "nov"]),
    mean(coef_matrix_seasonal_econ_query_male[, "nov"]),
    mean(coef_matrix_seasonal_natural_query_male[, "nov"]),
    mean(coef_matrix_seasonal_econ_natural_query_male[, "nov"])
  ),
  Dec = c(
    mean(coef_matrix_seasonal_bm_male[, "dec"]),
    mean(coef_matrix_seasonal_econ_only_male[, "dec"]),
    mean(coef_matrix_seasonal_natural_only_male[, "dec"]),
    mean(coef_matrix_seasonal_query_only_male[, "dec"]),
    mean(coef_matrix_seasonal_econ_natural_male[, "dec"]),
    mean(coef_matrix_seasonal_econ_query_male[, "dec"]),
    mean(coef_matrix_seasonal_natural_query_male[, "dec"]),
    mean(coef_matrix_seasonal_econ_natural_query_male[, "dec"])
  ),
  # Economic coefficients
  Unemployment = c(
    NA,
    mean(coef_vec_unemp_econ_only_male),
    NA,
    NA,
    mean(coef_vec_unemp_econ_natural_male),
    mean(coef_vec_unemp_econ_query_male),
    NA,
    mean(coef_vec_unemp_econ_natural_query_male)
  ),
  # Natural environment coefficients
   Temperature = c(
    NA,
    NA,
    mean(coef_vec_temp_natural_only_male),
    NA,
    mean(coef_vec_temp_econ_natural_male),
    NA,
    mean(coef_vec_temp_natural_query_male),
    mean(coef_vec_temp_econ_natural_query_male)
  ),
  Solar = c(
    NA,
    NA,
    mean(coef_vec_solar_natural_only_male),
    NA,
    mean(coef_vec_solar_econ_natural_male),
    NA,
    mean(coef_vec_solar_natural_query_male),
    mean(coef_vec_solar_econ_natural_query_male)
  ),
  # Search query coefficients

`Query suicide` = c(
  NA,
  NA,
  NA,
  mean(coef_matrix_query_only_male[, "query_suicide"], na.rm = TRUE),
  NA,
  mean(coef_matrix_query_econ_query_male[, "query_suicide"], na.rm = TRUE),
  mean(coef_matrix_query_natural_query_male[, "query_suicide"], na.rm = TRUE),
  mean(coef_matrix_query_econ_natural_query_male[, "query_suicide"], na.rm = TRUE)
),
`Query depression` = c(
  NA,
  NA,
  NA,
  mean(coef_matrix_query_only_male[, "query_depression"], na.rm = TRUE),
  NA,
  mean(coef_matrix_query_econ_query_male[, "query_depression"], na.rm = TRUE),
  mean(coef_matrix_query_natural_query_male[, "query_depression"], na.rm = TRUE),
  mean(coef_matrix_query_econ_natural_query_male[, "query_depression"], na.rm = TRUE)
),
`Query unemployment` = c(
  NA,
  NA,
  NA,
  mean(coef_matrix_query_only_male[, "query_unemployment"], na.rm = TRUE),
  NA,
  mean(coef_matrix_query_econ_query_male[, "query_unemployment"], na.rm = TRUE),
  mean(coef_matrix_query_natural_query_male[, "query_unemployment"], na.rm = TRUE),
  mean(coef_matrix_query_econ_natural_query_male[, "query_unemployment"], na.rm = TRUE)
),
`Query relationship breakup` = c(
  NA,
  NA,
  NA,
  mean(coef_matrix_query_only_male[, "query_relationship_breakup"], na.rm = TRUE),
  NA,
  mean(coef_matrix_query_econ_query_male[, "query_relationship_breakup"], na.rm = TRUE),
  mean(coef_matrix_query_natural_query_male[, "query_relationship_breakup"], na.rm = TRUE),
  mean(coef_matrix_query_econ_natural_query_male[, "query_relationship_breakup"], na.rm = TRUE)
),
`Query antidepressant` = c(
  NA,
  NA,
  NA,
  mean(coef_matrix_query_only_male[, "query_antidepressant"], na.rm = TRUE),
  NA,
  mean(coef_matrix_query_econ_query_male[, "query_antidepressant"], na.rm = TRUE),
  mean(coef_matrix_query_natural_query_male[, "query_antidepressant"], na.rm = TRUE),
  mean(coef_matrix_query_econ_natural_query_male[, "query_antidepressant"], na.rm = TRUE)
),
`Query allergy` = c(
  NA,
  NA,
  NA,
  mean(coef_matrix_query_only_male[, "query_allergy"], na.rm = TRUE),
  NA,
  mean(coef_matrix_query_econ_query_male[, "query_allergy"], na.rm = TRUE),
  mean(coef_matrix_query_natural_query_male[, "query_allergy"], na.rm = TRUE),
  mean(coef_matrix_query_econ_natural_query_male[, "query_allergy"], na.rm = TRUE)
),
`Query pain` = c(
  NA,
  NA,
  NA,
  mean(coef_matrix_query_only_male[, "query_pain"], na.rm = TRUE),
  NA,
  mean(coef_matrix_query_econ_query_male[, "query_pain"], na.rm = TRUE),
  mean(coef_matrix_query_natural_query_male[, "query_pain"], na.rm = TRUE),
  mean(coef_matrix_query_econ_natural_query_male[, "query_pain"], na.rm = TRUE)
),
`Query alcohol abstinence` = c(
  NA,
  NA,
  NA,
  mean(coef_matrix_query_only_male[, "query_alcohol_abstinence"], na.rm = TRUE),
  NA,
  mean(coef_matrix_query_econ_query_male[, "query_alcohol_abstinence"], na.rm = TRUE),
  mean(coef_matrix_query_natural_query_male[, "query_alcohol_abstinence"], na.rm = TRUE),
  mean(coef_matrix_query_econ_natural_query_male[, "query_alcohol_abstinence"], na.rm = TRUE)
)
)

# Transpose the dataframe and round to 2 decimal places
coef_comparison_male_t <- data.frame(
  Variable = names(coef_comparison_male)[-1],  # Get column names except Model_Name
  t(round(coef_comparison_male[,-1], 2))       # Transpose and round data except Model_Name
)

# Set column names using model names
colnames(coef_comparison_male_t)[-1] <- coef_comparison_male$Model_Name

# Replace NA with "-" for better readability
coef_comparison_male_t[is.na(coef_comparison_male_t)] <- " "

# Replace Query with Query (e.g. Query suicide with Query suicide)
rn <- rownames(coef_comparison_male_t) 
rownames(coef_comparison_male_t) <- ifelse(
  grepl("^Query", rn, ignore.case = TRUE),
  paste("Query", tolower(trimws(gsub("[._]", " ", sub("^Query[[:space:]._]*", "", rn, ignore.case = TRUE))))),
  rn
)

# Save as CSV file for further analysis
write.csv(coef_comparison_male_t, "coef_comparison_male_transposed.csv", row.names = FALSE)


# Female model----------------------------------------------------------
# Basic structural time series model----------------------------------------------------------
#model name
model_id_bm_female <- "bm_female"

#storing score
mape_vec_bm_female <- numeric(20)
mae_vec_bm_female  <- numeric(20)

coef_vec_level_bm_female <- numeric(20)
coef_matrix_seasonal_bm_female <- matrix(0, nrow = 20, ncol = 11)
colnames(coef_matrix_seasonal_bm_female) <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")


for (i in 1:20) {
  train_col <- paste0("train_", i)
  test_col <- paste0("test_", i)
  y_col <- paste0("suicide_rate_female_scaled_", i, "_ssm_y_train_", i)

  model <- SSModel(
    df[[y_col]] ~ SSMtrend(degree = 1, Q = NA) + SSMseasonal(period = 12, sea.type = "dummy"),
    H = NA
  )
  fit <- fitSSM(model, inits = c(1, 1))
  kfs <- KFS(fit$model)

  pred <- predict(fit$model, interval = "prediction", level = 0.95)
  n_pred <- sum(df[[test_col]] == 1)
  pred_test <- pred[(nrow(pred) - n_pred + 1):nrow(pred), 1]

  min_val <- min(df$suicide_rate_female[df[[train_col]] == 1], na.rm = TRUE)
  max_val <- max(df$suicide_rate_female[df[[train_col]] == 1], na.rm = TRUE)
  pred_inverse <- scale_max_min_vector_inverse(pred_test, min_val, max_val)
  pred_inverse <- pred_inverse * df$pop_female[df[[test_col]] == 1] / 100000

  actual <- df$num_suicide_female[df[[test_col]] == 1]
  acc <- forecast::accuracy(pred_inverse, actual)
  mape_vec_bm_female[i] <- acc[5]
  mae_vec_bm_female[i]  <- acc[3]

  coef_vec_level_bm_female[i] <- mean(coef(kfs)[-1,"level"])

  seasonal_states <- coef(kfs)[, grep("^sea", colnames(coef(kfs)))]
  seasonal_states <- seasonal_states[-1, , drop = FALSE]
  month_names <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
  for (m in 1:11) {
    monthly_vals <- seasonal_states[seq(m, nrow(seasonal_states), by = 12), 1]
    coef_matrix_seasonal_bm_female[i, month_names[m]] <- mean(monthly_vals, na.rm = TRUE)
  }
}

results_female_bm_mae <- data.frame(
  model_id = model_id_bm_female,
  rep_id = 1:20,
  test_start = thresholds,
  mae = mae_vec_bm_female
)

results_female_bm_mape <- data.frame(
  model_id = model_id_bm_female,
  rep_id = 1:20,
  test_start = thresholds,
  mape = mape_vec_bm_female
)

print(mean(results_female_bm_mae$mae))
print(mean(results_female_bm_mape$mape))
print(mean(coef_vec_level_bm_female))
print(mean(coef_matrix_seasonal_bm_female[, "feb"]))
print(mean(coef_matrix_seasonal_bm_female[, "mar"]))
print(mean(coef_matrix_seasonal_bm_female[, "apr"]))
print(mean(coef_matrix_seasonal_bm_female[, "may"]))
print(mean(coef_matrix_seasonal_bm_female[, "jun"]))
print(mean(coef_matrix_seasonal_bm_female[, "jul"]))
print(mean(coef_matrix_seasonal_bm_female[, "aug"]))
print(mean(coef_matrix_seasonal_bm_female[, "sep"]))
print(mean(coef_matrix_seasonal_bm_female[, "oct"]))
print(mean(coef_matrix_seasonal_bm_female[, "nov"]))
print(mean(coef_matrix_seasonal_bm_female[, "dec"]))

# Economic only model----------------------------------------------------------
#model name
model_id_econ_only_female <- "econ_only_female"

#storing score
mape_vec_econ_only_female <- numeric(20)
mae_vec_econ_only_female  <- numeric(20)

coef_vec_level_econ_only_female <- numeric(20)
coef_matrix_seasonal_econ_only_female <- matrix(0, nrow = 20, ncol = 11)
colnames(coef_matrix_seasonal_econ_only_female) <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
coef_vec_unemp_econ_only_female <- numeric(20)

for (i in 1:20) {
  train_col <- paste0("train_", i)
  test_col <- paste0("test_", i)
  y_col <- paste0("suicide_rate_female_scaled_", i, "_ssm_y_train_", i)

  model <- SSModel(
    df[[y_col]] ~ SSMtrend(degree = 1, Q = NA) + SSMseasonal(period = 12, sea.type = "dummy") + 
      SSMregression(~ df[[paste0("female_unemp_lag1_scaled_", i)]]),
    H = NA
  )
  fit <- fitSSM(model, inits = c(1, 1))
  kfs <- KFS(fit$model)

  pred <- predict(fit$model, interval = "prediction", level = 0.95)
  n_pred <- sum(df[[test_col]] == 1)
  pred_test <- pred[(nrow(pred) - n_pred + 1):nrow(pred), 1]

  min_val <- min(df$suicide_rate_female[df[[train_col]] == 1], na.rm = TRUE)
  max_val <- max(df$suicide_rate_female[df[[train_col]] == 1], na.rm = TRUE)
  pred_inverse <- scale_max_min_vector_inverse(pred_test, min_val, max_val)
  pred_inverse <- pred_inverse * df$pop_female[df[[test_col]] == 1] / 100000

  actual <- df$num_suicide_female[df[[test_col]] == 1]
  acc <- forecast::accuracy(pred_inverse, actual)
  mape_vec_econ_only_female[i] <- acc[5]
  mae_vec_econ_only_female[i]  <- acc[3]

  coef_vec_level_econ_only_female[i] <- mean(coef(kfs)[-1,"level"])
  coef_vec_unemp_econ_only_female[i] <- mean(coef(kfs)[-1, "df[[paste0(\"female_unemp_lag1_scaled_\", i)]]"])

  seasonal_states <- coef(kfs)[, grep("^sea", colnames(coef(kfs)))]
  seasonal_states <- seasonal_states[-1, , drop = FALSE]
  month_names <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
  for (m in 1:11) {
    monthly_vals <- seasonal_states[seq(m, nrow(seasonal_states), by = 12), 1]
    coef_matrix_seasonal_econ_only_female[i, month_names[m]] <- mean(monthly_vals, na.rm = TRUE)
  }
}

results_female_econ_only_mae <- data.frame(
  model_id = model_id_econ_only_female,
  rep_id = 1:20,
  test_start = thresholds,
  mae = mae_vec_econ_only_female
)
results_female_econ_only_mape <- data.frame(
  model_id = model_id_econ_only_female,
  rep_id = 1:20,
  test_start = thresholds,
  mape = mape_vec_econ_only_female
)

print(mean(results_female_econ_only_mae$mae))
print(mean(results_female_econ_only_mape$mape))
print(mean(coef_vec_level_econ_only_female))
print(mean(coef_vec_unemp_econ_only_female))
print(mean(coef_matrix_seasonal_econ_only_female[, "feb"]))
print(mean(coef_matrix_seasonal_econ_only_female[, "mar"]))
print(mean(coef_matrix_seasonal_econ_only_female[, "apr"]))
print(mean(coef_matrix_seasonal_econ_only_female[, "may"]))
print(mean(coef_matrix_seasonal_econ_only_female[, "jun"]))
print(mean(coef_matrix_seasonal_econ_only_female[, "jul"]))
print(mean(coef_matrix_seasonal_econ_only_female[, "aug"]))
print(mean(coef_matrix_seasonal_econ_only_female[, "sep"]))
print(mean(coef_matrix_seasonal_econ_only_female[, "oct"]))
print(mean(coef_matrix_seasonal_econ_only_female[, "nov"]))
print(mean(coef_matrix_seasonal_econ_only_female[, "dec"]))

# natural only model----------------------------------------------------------
#model name
model_id_natural_only_female <- "natural_only_female"
#storing score
mape_vec_natural_only_female <- numeric(20)
mae_vec_natural_only_female  <- numeric(20)
coef_vec_level_natural_only_female <- numeric(20)
coef_matrix_seasonal_natural_only_female <- matrix(0, nrow = 20, ncol = 11)
colnames(coef_matrix_seasonal_natural_only_female) <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
coef_vec_solar_natural_only_female <- numeric(20)
coef_vec_temp_natural_only_female <- numeric(20)

for (i in 1:20) {
  train_col <- paste0("train_", i)
  test_col <- paste0("test_", i)
  y_col <- paste0("suicide_rate_female_scaled_", i, "_ssm_y_train_", i)

  model <- SSModel(
    df[[y_col]] ~ SSMtrend(degree = 1, Q = NA) + SSMseasonal(period = 12, sea.type = "dummy") + 
      SSMregression(~ df[[paste0("solar_lag1_scaled_", i)]] + df[[paste0("temp_lag1_scaled_", i)]]),
    H = NA
  )
  fit <- fitSSM(model, inits = c(1, 1))
  kfs <- KFS(fit$model)

  pred <- predict(fit$model, interval = "prediction", level = 0.95)
  n_pred <- sum(df[[test_col]] == 1)
  pred_test <- pred[(nrow(pred) - n_pred + 1):nrow(pred), 1]

  min_val <- min(df$suicide_rate_female[df[[train_col]] == 1], na.rm = TRUE)
  max_val <- max(df$suicide_rate_female[df[[train_col]] == 1], na.rm = TRUE)
  pred_inverse <- scale_max_min_vector_inverse(pred_test, min_val, max_val)
  pred_inverse <- pred_inverse * df$pop_female[df[[test_col]] == 1] / 100000

  actual <- df$num_suicide_female[df[[test_col]] == 1]
  acc <- forecast::accuracy(pred_inverse, actual)
  mape_vec_natural_only_female[i] <- acc[5]
  mae_vec_natural_only_female[i]  <- acc[3]

  coef_vec_level_natural_only_female[i] <- mean(coef(kfs)[-1,"level"])
  coef_vec_solar_natural_only_female[i] <- mean(coef(kfs)[-1, "df[[paste0(\"solar_lag1_scaled_\", i)]]"])
  coef_vec_temp_natural_only_female[i] <- mean(coef(kfs)[-1, "df[[paste0(\"temp_lag1_scaled_\", i)]]"])

  seasonal_states <- coef(kfs)[, grep("^sea", colnames(coef(kfs)))]
  seasonal_states <- seasonal_states[-1, , drop = FALSE]
  month_names <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
  for (m in 1:11) {
    monthly_vals <- seasonal_states[seq(m, nrow(seasonal_states), by = 12), 1]
    coef_matrix_seasonal_natural_only_female[i, month_names[m]] <- mean(monthly_vals, na.rm = TRUE)
  }
}

results_female_natural_only_mae <- data.frame(
  model_id = model_id_natural_only_female,
  rep_id = 1:20,
  test_start = thresholds,
  mae = mae_vec_natural_only_female
)
results_female_natural_only_mape <- data.frame(
  model_id = model_id_natural_only_female,
  rep_id = 1:20,
  test_start = thresholds,
  mape = mape_vec_natural_only_female
)
print(mean(results_female_natural_only_mae$mae))
print(mean(results_female_natural_only_mape$mape))
print(mean(coef_vec_level_natural_only_female))
print(mean(coef_vec_solar_natural_only_female))
print(mean(coef_vec_temp_natural_only_female))
print(mean(coef_matrix_seasonal_natural_only_female[, "feb"]))
print(mean(coef_matrix_seasonal_natural_only_female[, "mar"]))
print(mean(coef_matrix_seasonal_natural_only_female[, "apr"]))
print(mean(coef_matrix_seasonal_natural_only_female[, "may"]))
print(mean(coef_matrix_seasonal_natural_only_female[, "jun"]))
print(mean(coef_matrix_seasonal_natural_only_female[, "jul"]))
print(mean(coef_matrix_seasonal_natural_only_female[, "aug"]))
print(mean(coef_matrix_seasonal_natural_only_female[, "sep"]))
print(mean(coef_matrix_seasonal_natural_only_female[, "oct"]))
print(mean(coef_matrix_seasonal_natural_only_female[, "nov"]))
print(mean(coef_matrix_seasonal_natural_only_female[, "dec"]))

# query only model----------------------------------------------------------
#model name
model_id_query_only_female <- "query_only_female"

#storing scores
mape_vec_query_only_female <- numeric(20)
mae_vec_query_only_female  <- numeric(20)
coef_vec_query_only_female <- numeric(20)
coef_vec_level_query_only_female <- numeric(20)
coef_matrix_seasonal_query_only_female <- matrix(0, nrow = 20, ncol = 11)
colnames(coef_matrix_seasonal_query_only_female) <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")

# storing query coefficients
coef_matrix_query_only_female <- matrix(0, nrow = 20, ncol = length(female_query))
colnames(coef_matrix_query_only_female) <- female_query
coef_vec_query_only_female <- numeric(20)

for (i in 1:20) {
  train_col <- paste0("train_", i)
  test_col <- paste0("test_", i)
  y_col <- paste0("suicide_rate_female_scaled_", i, "_ssm_y_train_", i)
  
  model <- SSModel(
    df[[y_col]] ~ SSMtrend(degree = 1, Q = NA) + SSMseasonal(period = 12, sea.type = "dummy") + 
      SSMregression(~ df[[paste0("query_suicide_scaled_", i)]] + 
                       df[[paste0("query_depression_scaled_", i)]] + 
                       df[[paste0("query_unemployment_scaled_", i)]] + 
                       df[[paste0("query_major_depression_scaled_", i)]] + 
                       df[[paste0("query_insomnia_scaled_", i)]] + 
                       df[[paste0("query_marriage_scaled_", i)]] + 
                       df[[paste0("query_relationship_breakup_scaled_", i)]] + 
                       df[[paste0("query_antidepressant_scaled_", i)]] + 
                       df[[paste0("query_allergy_scaled_", i)]] + 
                       df[[paste0("query_pain_scaled_", i)]] + 
                       df[[paste0("query_drunkenness_scaled_", i)]] + 
                       df[[paste0("query_alcohol_abstinence_scaled_", i)]]),
    H = NA
)
  fit <- fitSSM(model, inits = c(1, 1))
  kfs <- KFS(fit$model)

  pred <- predict(fit$model, interval = "prediction", level = 0.95)

  n_pred <- sum(df[[test_col]] == 1)
  pred_test <- pred[(nrow(pred) - n_pred + 1):nrow(pred), 1]
  
  min_val <- min(df$suicide_rate_female[df[[train_col]] == 1], na.rm = TRUE)
  max_val <- max(df$suicide_rate_female[df[[train_col]] == 1], na.rm = TRUE)
  pred_inverse <- scale_max_min_vector_inverse(pred_test, min_val, max_val)
  pred_inverse <- pred_inverse * df$pop_female[df[[test_col]] == 1] / 100000

  actual <- df$num_suicide_female[df[[test_col]] == 1]
  acc <- forecast::accuracy(pred_inverse, actual) 
  mape_vec_query_only_female[i] <- acc[5]
  mae_vec_query_only_female[i]  <- acc[3]

  for (j in seq_along(female_query)) {
    query_name <- female_query[j]
    coef_matrix_query_only_female[i, j] <- mean(coef(kfs)[-1, paste0("df[[paste0(\"", query_name, "_scaled_\", i)]]")])
  }
  coef_vec_query_only_female[i] <- sum(coef_matrix_query_only_female[i,])
  coef_vec_level_query_only_female[i] <- mean(coef(kfs)[-1,"level"])
  seasonal_states <- coef(kfs)[, grep("^sea", colnames(coef(kfs)))]
  
  # drop the first row
  seasonal_states <- seasonal_states[-1, , drop = FALSE]
  
  # average every 12 months starting from the 1st row
  month_names <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
  for (m in 1:11) {
    monthly_vals <- seasonal_states[seq(m, nrow(seasonal_states), by = 12), 1]
    coef_matrix_seasonal_query_only_female[i, month_names[m]] <- mean(monthly_vals, na.rm = TRUE)
  }
}

results_female_query_only_mae <- data.frame(
  model_id = model_id_query_only_female,
  rep_id = 1:20,
  test_start = thresholds,
  mae = mae_vec_query_only_female
)

results_female_query_only_mape <- data.frame(
  model_id = model_id_query_only_female,
  rep_id = 1:20,
  test_start = thresholds,
  mape = mape_vec_query_only_female
)

print(mean(results_female_query_only_mae$mae))
print(mean(results_female_query_only_mape$mape))
for (q in female_query) {
  cat(q, ":", mean(coef_matrix_query_only_female[, q], na.rm = TRUE), "\n")
}
print(mean(coef_vec_level_query_only_female))
print(mean(coef_matrix_seasonal_query_only_female[, "feb"]))
print(mean(coef_matrix_seasonal_query_only_female[, "mar"]))
print(mean(coef_matrix_seasonal_query_only_female[, "apr"]))
print(mean(coef_matrix_seasonal_query_only_female[, "may"]))
print(mean(coef_matrix_seasonal_query_only_female[, "jun"]))
print(mean(coef_matrix_seasonal_query_only_female[, "jul"]))
print(mean(coef_matrix_seasonal_query_only_female[, "aug"]))
print(mean(coef_matrix_seasonal_query_only_female[, "sep"]))
print(mean(coef_matrix_seasonal_query_only_female[, "oct"]))
print(mean(coef_matrix_seasonal_query_only_female[, "nov"]))
print(mean(coef_matrix_seasonal_query_only_female[, "dec"]))

# economic and natural model---------------------------------------------------------
#model name
model_id_econ_natural_female <- "econ_natural_female"
#storing score
mape_vec_econ_natural_female <- numeric(20)
mae_vec_econ_natural_female  <- numeric(20)
coef_vec_level_econ_natural_female <- numeric(20)
coef_matrix_seasonal_econ_natural_female <- matrix(0, nrow = 20, ncol = 11)
colnames(coef_matrix_seasonal_econ_natural_female) <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
coef_vec_unemp_econ_natural_female <- numeric(20)
coef_vec_solar_econ_natural_female <- numeric(20)
coef_vec_temp_econ_natural_female <- numeric(20)


for (i in 1:20) {
  train_col <- paste0("train_", i)
  test_col <- paste0("test_", i)
  y_col <- paste0("suicide_rate_female_scaled_", i, "_ssm_y_train_", i)

  model <- SSModel(
    df[[y_col]] ~ SSMtrend(degree = 1, Q = NA) + SSMseasonal(period = 12, sea.type = "dummy") + 
      SSMregression(~ df[[paste0("female_unemp_lag1_scaled_", i)]] + df[[paste0("solar_lag1_scaled_", i)]] + df[[paste0("temp_lag1_scaled_", i)]]),
    H = NA
  )
  fit <- fitSSM(model, inits = c(1, 1))
  kfs <- KFS(fit$model)

  pred <- predict(fit$model, interval = "prediction", level = 0.95)
  n_pred <- sum(df[[test_col]] == 1)
  pred_test <- pred[(nrow(pred) - n_pred + 1):nrow(pred), 1]

  min_val <- min(df$suicide_rate_female[df[[train_col]] == 1], na.rm = TRUE)
  max_val <- max(df$suicide_rate_female[df[[train_col]] == 1], na.rm = TRUE)
  pred_inverse <- scale_max_min_vector_inverse(pred_test, min_val, max_val)
  pred_inverse <- pred_inverse * df$pop_female[df[[test_col]] == 1] / 100000

  actual <- df$num_suicide_female[df[[test_col]] == 1]
  acc <- forecast::accuracy(pred_inverse, actual)
  mape_vec_econ_natural_female[i] <- acc[5]
  mae_vec_econ_natural_female[i]  <- acc[3]

  coef_vec_level_econ_natural_female[i] <- mean(coef(kfs)[-1,"level"])
  coef_vec_unemp_econ_natural_female[i] <- mean(coef(kfs)[-1, "df[[paste0(\"female_unemp_lag1_scaled_\", i)]]"])
  coef_vec_solar_econ_natural_female[i] <- mean(coef(kfs)[-1, "df[[paste0(\"solar_lag1_scaled_\", i)]]"])
  coef_vec_temp_econ_natural_female[i] <- mean(coef(kfs)[-1, "df[[paste0(\"temp_lag1_scaled_\", i)]]"])

  seasonal_states <- coef(kfs)[, grep("^sea", colnames(coef(kfs)))]
  seasonal_states <- seasonal_states[-1, , drop = FALSE]
  month_names <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
  for (m in 1:11) {
    monthly_vals <- seasonal_states[seq(m, nrow(seasonal_states), by = 12), 1]
    coef_matrix_seasonal_econ_natural_female[i, month_names[m]] <- mean(monthly_vals, na.rm = TRUE)
  }
}

results_female_econ_natural_mae <- data.frame(
  model_id = model_id_econ_natural_female,
  rep_id = 1:20,
  test_start = thresholds,
  mae = mae_vec_econ_natural_female
)
results_female_econ_natural_mape <- data.frame(
  model_id = model_id_econ_natural_female,
  rep_id = 1:20,
  test_start = thresholds,
  mape = mape_vec_econ_natural_female
)

print(mean(results_female_econ_natural_mae$mae))
print(mean(results_female_econ_natural_mape$mape))
print(mean(coef_vec_level_econ_natural_female))
print(mean(coef_vec_unemp_econ_natural_female))
print(mean(coef_vec_solar_econ_natural_female))
print(mean(coef_vec_temp_econ_natural_female))
print(mean(coef_matrix_seasonal_econ_natural_female[, "feb"]))
print(mean(coef_matrix_seasonal_econ_natural_female[, "mar"]))
print(mean(coef_matrix_seasonal_econ_natural_female[, "apr"]))
print(mean(coef_matrix_seasonal_econ_natural_female[, "may"]))
print(mean(coef_matrix_seasonal_econ_natural_female[, "jun"]))
print(mean(coef_matrix_seasonal_econ_natural_female[, "jul"]))
print(mean(coef_matrix_seasonal_econ_natural_female[, "aug"]))
print(mean(coef_matrix_seasonal_econ_natural_female[, "sep"]))
print(mean(coef_matrix_seasonal_econ_natural_female[, "oct"]))
print(mean(coef_matrix_seasonal_econ_natural_female[, "nov"]))
print(mean(coef_matrix_seasonal_econ_natural_female[, "dec"]))

# economic and query model---------------------------------------------------------
#model name
model_id_econ_query_female <- "econ_query_female"
#storing score
mape_vec_econ_query_female <- numeric(20)
mae_vec_econ_query_female  <- numeric(20)
coef_vec_level_econ_query_female <- numeric(20)
coef_matrix_seasonal_econ_query_female <- matrix(0, nrow = 20, ncol = 11)
colnames(coef_matrix_seasonal_econ_query_female) <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
coef_vec_unemp_econ_query_female <- numeric(20)
coef_matrix_query_econ_query_female <- matrix(0, nrow = 20, ncol = length(female_query))
colnames(coef_matrix_query_econ_query_female) <- female_query
coef_vec_query_econ_query_female <- numeric(20)


for (i in 1:20) {
  train_col <- paste0("train_", i)
  test_col <- paste0("test_", i)
  y_col <- paste0("suicide_rate_female_scaled_", i, "_ssm_y_train_", i)

  model <- SSModel(
    df[[y_col]] ~ SSMtrend(degree = 1, Q = NA) + SSMseasonal(period = 12, sea.type = "dummy") + 
      SSMregression(~ df[[paste0("female_unemp_lag1_scaled_", i)]] + 
      df[[paste0("query_suicide_scaled_", i)]] + 
                       df[[paste0("query_depression_scaled_", i)]] + 
                       df[[paste0("query_unemployment_scaled_", i)]] + 
                       df[[paste0("query_major_depression_scaled_", i)]] + 
                       df[[paste0("query_insomnia_scaled_", i)]] + 
                       df[[paste0("query_marriage_scaled_", i)]] + 
                       df[[paste0("query_relationship_breakup_scaled_", i)]] + 
                       df[[paste0("query_antidepressant_scaled_", i)]] + 
                       df[[paste0("query_allergy_scaled_", i)]] + 
                       df[[paste0("query_pain_scaled_", i)]] + 
                       df[[paste0("query_drunkenness_scaled_", i)]] + 
                       df[[paste0("query_alcohol_abstinence_scaled_", i)]]),
    H = NA
  )
  fit <- fitSSM(model, inits = c(1, 1))
  kfs <- KFS(fit$model)

  pred <- predict(fit$model, interval = "prediction", level = 0.95)
  n_pred <- sum(df[[test_col]] == 1)
  pred_test <- pred[(nrow(pred) - n_pred + 1):nrow(pred), 1]

  min_val <- min(df$suicide_rate_female[df[[train_col]] == 1], na.rm = TRUE)
  max_val <- max(df$suicide_rate_female[df[[train_col]] == 1], na.rm = TRUE)
  pred_inverse <- scale_max_min_vector_inverse(pred_test, min_val, max_val)
  pred_inverse <- pred_inverse * df$pop_female[df[[test_col]] == 1] / 100000

  actual <- df$num_suicide_female[df[[test_col]] == 1]
  acc <- forecast::accuracy(pred_inverse, actual)
  mape_vec_econ_query_female[i] <- acc[5]
  mae_vec_econ_query_female[i]  <- acc[3]

  coef_vec_level_econ_query_female[i] <- mean(coef(kfs)[-1,"level"])
  coef_vec_unemp_econ_query_female[i] <- mean(coef(kfs)[-1, "df[[paste0(\"female_unemp_lag1_scaled_\", i)]]"])

  for (j in seq_along(female_query)) {
    query_name <- female_query[j]
    coef_matrix_query_econ_query_female[i, j] <- mean(coef(kfs)[-1, paste0("df[[paste0(\"", query_name, "_scaled_\", i)]]")])
  }
  coef_vec_query_econ_query_female[i] <- sum(coef_matrix_query_econ_query_female[i,])

  seasonal_states <- coef(kfs)[, grep("^sea", colnames(coef(kfs)))]
  seasonal_states <- seasonal_states[-1, , drop = FALSE]
  month_names <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
  for (m in 1:11) {
    monthly_vals <- seasonal_states[seq(m, nrow(seasonal_states), by = 12), 1]
    coef_matrix_seasonal_econ_query_female[i, month_names[m]] <- mean(monthly_vals, na.rm = TRUE)
}
}

results_female_econ_query_mae <- data.frame(
  model_id = model_id_econ_query_female,
  rep_id = 1:20,
  test_start = thresholds,
  mae = mae_vec_econ_query_female
)
results_female_econ_query_mape <- data.frame(
  model_id = model_id_econ_query_female,
  rep_id = 1:20,
  test_start = thresholds,
  mape = mape_vec_econ_query_female
)

print(mean(results_female_econ_query_mae$mae))
print(mean(results_female_econ_query_mape$mape))
print(mean(coef_vec_level_econ_query_female))
print(mean(coef_vec_unemp_econ_query_female))
for (q in female_query) {
  cat(q, ":", mean(coef_matrix_query_econ_query_female[, q], na.rm = TRUE), "\n")
}
print(mean(coef_matrix_seasonal_econ_query_female[, "feb"]))
print(mean(coef_matrix_seasonal_econ_query_female[, "mar"]))
print(mean(coef_matrix_seasonal_econ_query_female[, "apr"]))
print(mean(coef_matrix_seasonal_econ_query_female[, "may"]))
print(mean(coef_matrix_seasonal_econ_query_female[, "jun"]))
print(mean(coef_matrix_seasonal_econ_query_female[, "jul"]))
print(mean(coef_matrix_seasonal_econ_query_female[, "aug"]))
print(mean(coef_matrix_seasonal_econ_query_female[, "sep"]))
print(mean(coef_matrix_seasonal_econ_query_female[, "oct"]))
print(mean(coef_matrix_seasonal_econ_query_female[, "nov"]))
print(mean(coef_matrix_seasonal_econ_query_female[, "dec"]))

# natural and query model---------------------------------------------------------
#model name
model_id_natural_query_female <- "natural_query_female"
#storing score
mape_vec_natural_query_female <- numeric(20)
mae_vec_natural_query_female  <- numeric(20)
coef_vec_level_natural_query_female <- numeric(20)
coef_matrix_seasonal_natural_query_female <- matrix(0, nrow = 20, ncol = 11)
colnames(coef_matrix_seasonal_natural_query_female) <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
coef_vec_solar_natural_query_female <- numeric(20)
coef_vec_temp_natural_query_female <- numeric(20)
coef_matrix_query_natural_query_female <- matrix(0, nrow = 20, ncol = length(female_query))
colnames(coef_matrix_query_natural_query_female) <- female_query
coef_vec_query_natural_query_female <- numeric(20)

for (i in 1:20) {
  train_col <- paste0("train_", i)
  test_col <- paste0("test_", i)
  y_col <- paste0("suicide_rate_female_scaled_", i, "_ssm_y_train_", i)
  
  model <- SSModel(
    df[[y_col]] ~ SSMtrend(degree = 1, Q = NA) + SSMseasonal(period = 12, sea.type = "dummy") + 
      SSMregression(~ df[[paste0("solar_lag1_scaled_", i)]] + df[[paste0("temp_lag1_scaled_", i)]] + 
      df[[paste0("query_suicide_scaled_", i)]] + 
                       df[[paste0("query_depression_scaled_", i)]] + 
                       df[[paste0("query_unemployment_scaled_", i)]] + 
                       df[[paste0("query_major_depression_scaled_", i)]] +  
                       df[[paste0("query_insomnia_scaled_", i)]] + 
                       df[[paste0("query_marriage_scaled_", i)]] + 
                       df[[paste0("query_relationship_breakup_scaled_", i)]] + 
                       df[[paste0("query_antidepressant_scaled_", i)]] + 
                       df[[paste0("query_allergy_scaled_", i)]] + 
                       df[[paste0("query_pain_scaled_", i)]] + 
                       df[[paste0("query_drunkenness_scaled_", i)]] +
                       df[[paste0("query_alcohol_abstinence_scaled_", i)]]),
    H = NA
  )
  fit <- fitSSM(model, inits = c(1, 1))
  kfs <- KFS(fit$model)

  pred <- predict(fit$model, interval = "prediction", level = 0.95)
  n_pred <- sum(df[[test_col]] == 1)
  pred_test <- pred[(nrow(pred) - n_pred + 1):nrow(pred), 1]

  min_val <- min(df$suicide_rate_female[df[[train_col]] == 1], na.rm = TRUE)
  max_val <- max(df$suicide_rate_female[df[[train_col]] == 1], na.rm = TRUE)
  pred_inverse <- scale_max_min_vector_inverse(pred_test, min_val, max_val)
  pred_inverse <- pred_inverse * df$pop_female[df[[test_col]] == 1] / 100000

  actual <- df$num_suicide_female[df[[test_col]] == 1]
  acc <- forecast::accuracy(pred_inverse, actual)
  mape_vec_natural_query_female[i] <- acc[5]
  mae_vec_natural_query_female[i]  <- acc[3]

  coef_vec_level_natural_query_female[i] <- mean(coef(kfs)[-1,"level"])
  coef_vec_solar_natural_query_female[i] <- mean(coef(kfs)[-1, "df[[paste0(\"solar_lag1_scaled_\", i)]]"])
  coef_vec_temp_natural_query_female[i] <- mean(coef(kfs)[-1, "df[[paste0(\"temp_lag1_scaled_\", i)]]"])

  for (j in seq_along(female_query)) {
    query_name <- female_query[j]
    coef_matrix_query_natural_query_female[i, query_name] <- mean(coef(kfs)[-1, paste0("df[[paste0(\"", query_name, "_scaled_\", i)]]")])
  }
  coef_vec_query_natural_query_female[i] <- sum(coef_matrix_query_natural_query_female[i,])
  
  seasonal_states <- coef(kfs)[, grep("^sea", colnames(coef(kfs)))]
  seasonal_states <- seasonal_states[-1, , drop = FALSE]
  month_names <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
  for (m in 1:11) {
    monthly_vals <- seasonal_states[seq(m, nrow(seasonal_states), by = 12), 1]
    coef_matrix_seasonal_natural_query_female[i, month_names[m]] <- mean(monthly_vals, na.rm = TRUE)
  }
}

results_female_natural_query_mae <- data.frame(
  model_id = model_id_natural_query_female,
  rep_id = 1:20,
  test_start = thresholds,
  mae = mae_vec_natural_query_female
)

results_female_natural_query_mape <- data.frame(
  model_id = model_id_natural_query_female,
  rep_id = 1:20,
  test_start = thresholds,
  mape = mape_vec_natural_query_female
)
print(mean(results_female_natural_query_mae$mae))
print(mean(results_female_natural_query_mape$mape))
print(mean(coef_vec_level_natural_query_female))
print(mean(coef_vec_solar_natural_query_female))
print(mean(coef_vec_temp_natural_query_female))
for (q in female_query) {
  cat(q, ":", mean(coef_matrix_query_natural_query_female[, q], na.rm = TRUE), "\n")
}
print(mean(coef_matrix_seasonal_natural_query_female[, "feb"]))
print(mean(coef_matrix_seasonal_natural_query_female[, "mar"]))
print(mean(coef_matrix_seasonal_natural_query_female[, "apr"]))
print(mean(coef_matrix_seasonal_natural_query_female[, "may"]))
print(mean(coef_matrix_seasonal_natural_query_female[, "jun"]))
print(mean(coef_matrix_seasonal_natural_query_female[, "jul"]))
print(mean(coef_matrix_seasonal_natural_query_female[, "aug"]))
print(mean(coef_matrix_seasonal_natural_query_female[, "sep"]))
print(mean(coef_matrix_seasonal_natural_query_female[, "oct"]))
print(mean(coef_matrix_seasonal_natural_query_female[, "nov"]))
print(mean(coef_matrix_seasonal_natural_query_female[, "dec"]))

# economic and natural and query model---------------------------------------------------------
#model name
model_id_econ_natural_query_female <- "econ_natural_query_female"
#storing score
mape_vec_econ_natural_query_female <- numeric(20)
mae_vec_econ_natural_query_female  <- numeric(20)
coef_vec_level_econ_natural_query_female <- numeric(20)
coef_matrix_seasonal_econ_natural_query_female <- matrix(0, nrow = 20, ncol = 11)
colnames(coef_matrix_seasonal_econ_natural_query_female) <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
coef_vec_unemp_econ_natural_query_female <- numeric(20)
coef_vec_solar_econ_natural_query_female <- numeric(20)
coef_vec_temp_econ_natural_query_female <- numeric(20)
coef_matrix_query_econ_natural_query_female <- matrix(0, nrow = 20, ncol = length(female_query))
colnames(coef_matrix_query_econ_natural_query_female) <- female_query
coef_vec_query_econ_natural_query_female <- numeric(20)

for (i in 1:20) {
  train_col <- paste0("train_", i)
  test_col <- paste0("test_", i)
  y_col <- paste0("suicide_rate_female_scaled_", i, "_ssm_y_train_", i)

  model <- SSModel(
    df[[y_col]] ~ SSMtrend(degree = 1, Q = NA) + SSMseasonal(period = 12, sea.type = "dummy") + 
      SSMregression(~ df[[paste0("female_unemp_lag1_scaled_", i)]] + df[[paste0("solar_lag1_scaled_", i)]] + df[[paste0("temp_lag1_scaled_", i)]] + 
      df[[paste0("query_suicide_scaled_", i)]] + 
                       df[[paste0("query_depression_scaled_", i)]] + 
                       df[[paste0("query_unemployment_scaled_", i)]] + 
                       df[[paste0("query_major_depression_scaled_", i)]] +  
                       df[[paste0("query_insomnia_scaled_", i)]] + 
                       df[[paste0("query_marriage_scaled_", i)]] + 
                       df[[paste0("query_relationship_breakup_scaled_", i)]] + 
                       df[[paste0("query_antidepressant_scaled_", i)]] + 
                       df[[paste0("query_allergy_scaled_", i)]] + 
                       df[[paste0("query_pain_scaled_", i)]] + 
                       df[[paste0("query_drunkenness_scaled_", i)]] +
                       df[[paste0("query_alcohol_abstinence_scaled_", i)]]),
    H = NA
  )
  fit <- fitSSM(model, inits = c(1, 1))
  kfs <- KFS(fit$model)

  pred <- predict(fit$model, interval = "prediction", level = 0.95)
  n_pred <- sum(df[[test_col]] == 1)
  pred_test <- pred[(nrow(pred) - n_pred + 1):nrow(pred), 1]

  min_val <- min(df$suicide_rate_female[df[[train_col]] == 1], na.rm = TRUE)
  max_val <- max(df$suicide_rate_female[df[[train_col]] == 1], na.rm = TRUE)
  pred_inverse <- scale_max_min_vector_inverse(pred_test, min_val, max_val)
  pred_inverse <- pred_inverse * df$pop_female[df[[test_col]] == 1] / 100000

  actual <- df$num_suicide_female[df[[test_col]] == 1]
  acc <- forecast::accuracy(pred_inverse, actual)
  mape_vec_econ_natural_query_female[i] <- acc[5]
  mae_vec_econ_natural_query_female[i]  <- acc[3]

  coef_vec_level_econ_natural_query_female[i] <- mean(coef(kfs)[-1,"level"])
  coef_vec_unemp_econ_natural_query_female[i] <- mean(coef(kfs)[-1, "df[[paste0(\"female_unemp_lag1_scaled_\", i)]]"])
  coef_vec_solar_econ_natural_query_female[i] <- mean(coef(kfs)[-1, "df[[paste0(\"solar_lag1_scaled_\", i)]]"])
  coef_vec_temp_econ_natural_query_female[i] <- mean(coef(kfs)[-1, "df[[paste0(\"temp_lag1_scaled_\", i)]]"])

  for (j in seq_along(female_query)) {
    query_name <- female_query[j]
    coef_matrix_query_econ_natural_query_female[i, query_name] <- mean(coef(kfs)[-1, paste0("df[[paste0(\"", query_name, "_scaled_\", i)]]")])
  }
  coef_vec_query_econ_natural_query_female[i] <- sum(coef_matrix_query_econ_natural_query_female[i,])

  seasonal_states <- coef(kfs)[, grep("^sea", colnames(coef(kfs)))]
  seasonal_states <- seasonal_states[-1, , drop = FALSE]
  month_names <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
  for (m in 1:11) {
    monthly_vals <- seasonal_states[seq(m, nrow(seasonal_states), by = 12), 1]
    coef_matrix_seasonal_econ_natural_query_female[i, month_names[m]] <- mean(monthly_vals, na.rm = TRUE)
  }
}

results_female_econ_natural_query_mae <- data.frame(
  model_id = model_id_econ_natural_query_female,
  rep_id = 1:20,
  test_start = thresholds,
  mae = mae_vec_econ_natural_query_female
)
results_female_econ_natural_query_mape <- data.frame(
  model_id = model_id_econ_natural_query_female,
  rep_id = 1:20,
  test_start = thresholds,
  mape = mape_vec_econ_natural_query_female
)
print(mean(results_female_econ_natural_query_mae$mae))
print(mean(results_female_econ_natural_query_mape$mape))
print(mean(coef_vec_level_econ_natural_query_female))
print(mean(coef_vec_unemp_econ_natural_query_female))
print(mean(coef_vec_solar_econ_natural_query_female))
print(mean(coef_vec_temp_econ_natural_query_female))
for (q in female_query) {
  cat(q, ":", mean(coef_matrix_query_econ_natural_query_female[, q], na.rm = TRUE), "\n")
}
print(mean(coef_matrix_seasonal_econ_natural_query_female[, "feb"]))
print(mean(coef_matrix_seasonal_econ_natural_query_female[, "mar"]))
print(mean(coef_matrix_seasonal_econ_natural_query_female[, "apr"]))
print(mean(coef_matrix_seasonal_econ_natural_query_female[, "may"]))
print(mean(coef_matrix_seasonal_econ_natural_query_female[, "jun"]))
print(mean(coef_matrix_seasonal_econ_natural_query_female[, "jul"]))
print(mean(coef_matrix_seasonal_econ_natural_query_female[, "aug"]))
print(mean(coef_matrix_seasonal_econ_natural_query_female[, "sep"]))
print(mean(coef_matrix_seasonal_econ_natural_query_female[, "oct"]))
print(mean(coef_matrix_seasonal_econ_natural_query_female[, "nov"]))
print(mean(coef_matrix_seasonal_econ_natural_query_female[, "dec"]))

# Compare Accuracy  ----------------------------------------------------------
# Basic Structural Time Series
results_female_bm_mae$model_name <- "Basic Structural Time Series"
results_female_bm_mape$model_name <- "Basic Structural Time Series"

# Econ Only
results_female_econ_only_mae$model_name <- "Econ Only"
results_female_econ_only_mape$model_name <- "Econ Only"

# Natural Env Only
results_female_natural_only_mae$model_name <- "Natural Env Only"
results_female_natural_only_mape$model_name <- "Natural Env Only"

# Search Query Only
results_female_query_only_mae$model_name <- "Search Query Only"
results_female_query_only_mape$model_name <- "Search Query Only"

# Econ + Natural Env
results_female_econ_natural_mae$model_name <- "Econ + Natural Env"
results_female_econ_natural_mape$model_name <- "Econ + Natural Env"

# Econ + Search Query
results_female_econ_query_mae$model_name <- "Econ + Search Query"
results_female_econ_query_mape$model_name <- "Econ + Search Query"

# Natural Env + Search Query
results_female_natural_query_mae$model_name <- "Natural Env + Search Query"
results_female_natural_query_mape$model_name <- "Natural Env + Search Query"

# Econ + Natural Env + Search Query
results_female_econ_natural_query_mae$model_name <- "Econ + Natural Env + Search Query"
results_female_econ_natural_query_mape$model_name <- "Econ + Natural Env + Search Query"

# Bind all results
result_female_mae <- rbind(
  results_female_bm_mae,
  results_female_econ_only_mae,
  results_female_natural_only_mae,
  results_female_query_only_mae,
  results_female_econ_natural_mae,
  results_female_econ_query_mae,
  results_female_natural_query_mae,
  results_female_econ_natural_query_mae
)

# Bind all results
result_female_mape <- rbind(
  results_female_bm_mape,
  results_female_econ_only_mape,
  results_female_natural_only_mape,
  results_female_query_only_mape,
  results_female_econ_natural_mape,
  results_female_econ_query_mape,
  results_female_natural_query_mape,
  results_female_econ_natural_query_mape
)

model_levels <- c(
  "Basic Structural Time Series",
  "Econ Only",
  "Natural Env Only",
  "Search Query Only",
  "Econ + Natural Env",
  "Econ + Search Query",
  "Natural Env + Search Query",
  "Econ + Natural Env + Search Query"
)

result_female_mae$model_name <- factor(result_female_mae$model_name, levels = model_levels)
result_female_mape$model_name <- factor(result_female_mape$model_name, levels = model_levels)
# Box plot for mae
mae_boxplot <- ggplot(result_female_mae, aes(x = model_name, y = mae)) +
  geom_boxplot() +
  labs(
    title = "Results of 20 rep-hold-out (Female)",
    x = "Model",
    y = "MAE"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 17, hjust = 0.5),
    axis.title.x = element_text(size = 12),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.title.y = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = "bottom",
    legend.key.size = unit(2, "cm"),
    text = element_text()
  )

# Save mae plot
png("mae_boxplot_female.png",
    width = 16, height = 8.0, units = "in", res = 300)
print(mae_boxplot)
dev.off()

# Box plot for mape
mape_boxplot_female <- ggplot(result_female_mape, aes(x = model_name, y = mape)) +
  geom_boxplot() +
  labs(
    title = "Results of 20 rep-hold-out (Female)",
    x = "Model",
    y = "MAPE"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 17, hjust = 0.5),
    axis.title.x = element_text(size = 12),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.title.y = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = "bottom",
    legend.key.size = unit(2, "cm"),
    text = element_text()
  )

# Save mape plot
png("mape_boxplot_female.png",
    width = 16, height = 8.0, units = "in", res = 300)
print(mape_boxplot_female)
dev.off()

# Extract MAE and MAPE data and combine them
result_female_accuracy <- result_female_mae %>%
  select(model_name, mae) %>%
  group_by(model_name) %>%
  summarise(mae = round(mean(mae), 2)) %>%
  left_join(
    result_female_mape %>%
      select(model_name, mape) %>%
      group_by(model_name) %>%
      summarise(mape = round(mean(mape), 2)),
    by = "model_name"
  )

# Display the results
print(result_female_accuracy)
# Save the result
write_csv(result_female_accuracy, "result_female_accuracy.csv")


# Create coefficient comparison table for female models ----------------------------------------------------------
# Create a data frame with model names and coefficients
coef_comparison_female <- data.frame(
  Model_Name = model_levels,
  # Level coefficients
  Level = c(
    mean(coef_vec_level_bm_female),
    mean(coef_vec_level_econ_only_female),
    mean(coef_vec_level_natural_only_female),
    mean(coef_vec_level_query_only_female),
    mean(coef_vec_level_econ_natural_female),
    mean(coef_vec_level_econ_query_female),
    mean(coef_vec_level_natural_query_female),
    mean(coef_vec_level_econ_natural_query_female)
  ),
  # Seasonal coefficients
  Feb = c(
    mean(coef_matrix_seasonal_bm_female[, "feb"]),
    mean(coef_matrix_seasonal_econ_only_female[, "feb"]),
    mean(coef_matrix_seasonal_natural_only_female[, "feb"]),
    mean(coef_matrix_seasonal_query_only_female[, "feb"]),
    mean(coef_matrix_seasonal_econ_natural_female[, "feb"]),
    mean(coef_matrix_seasonal_econ_query_female[, "feb"]),
    mean(coef_matrix_seasonal_natural_query_female[, "feb"]),
    mean(coef_matrix_seasonal_econ_natural_query_female[, "feb"])
  ),
  Mar = c(
    mean(coef_matrix_seasonal_bm_female[, "mar"]),
    mean(coef_matrix_seasonal_econ_only_female[, "mar"]),
    mean(coef_matrix_seasonal_natural_only_female[, "mar"]),
    mean(coef_matrix_seasonal_query_only_female[, "mar"]),
    mean(coef_matrix_seasonal_econ_natural_female[, "mar"]),
    mean(coef_matrix_seasonal_econ_query_female[, "mar"]),
    mean(coef_matrix_seasonal_natural_query_female[, "mar"]),
    mean(coef_matrix_seasonal_econ_natural_query_female[, "mar"])
  ),
  Apr = c(
    mean(coef_matrix_seasonal_bm_female[, "apr"]),
    mean(coef_matrix_seasonal_econ_only_female[, "apr"]),
    mean(coef_matrix_seasonal_natural_only_female[, "apr"]),
    mean(coef_matrix_seasonal_query_only_female[, "apr"]),
    mean(coef_matrix_seasonal_econ_natural_female[, "apr"]),
    mean(coef_matrix_seasonal_econ_query_female[, "apr"]),
    mean(coef_matrix_seasonal_natural_query_female[, "apr"]),
    mean(coef_matrix_seasonal_econ_natural_query_female[, "apr"])
  ),
  May = c(
    mean(coef_matrix_seasonal_bm_female[, "may"]),
    mean(coef_matrix_seasonal_econ_only_female[, "may"]),
    mean(coef_matrix_seasonal_natural_only_female[, "may"]),
    mean(coef_matrix_seasonal_query_only_female[, "may"]),
    mean(coef_matrix_seasonal_econ_natural_female[, "may"]),
    mean(coef_matrix_seasonal_econ_query_female[, "may"]),
    mean(coef_matrix_seasonal_natural_query_female[, "may"]),
    mean(coef_matrix_seasonal_econ_natural_query_female[, "may"])
  ),
  Jun = c(
    mean(coef_matrix_seasonal_bm_female[, "jun"]),
    mean(coef_matrix_seasonal_econ_only_female[, "jun"]),
    mean(coef_matrix_seasonal_natural_only_female[, "jun"]),
    mean(coef_matrix_seasonal_query_only_female[, "jun"]),
    mean(coef_matrix_seasonal_econ_natural_female[, "jun"]),
    mean(coef_matrix_seasonal_econ_query_female[, "jun"]),
    mean(coef_matrix_seasonal_natural_query_female[, "jun"]),
    mean(coef_matrix_seasonal_econ_natural_query_female[, "jun"])
  ),
  Jul = c(
    mean(coef_matrix_seasonal_bm_female[, "jul"]),
    mean(coef_matrix_seasonal_econ_only_female[, "jul"]),
    mean(coef_matrix_seasonal_natural_only_female[, "jul"]),
    mean(coef_matrix_seasonal_query_only_female[, "jul"]),
    mean(coef_matrix_seasonal_econ_natural_female[, "jul"]),
    mean(coef_matrix_seasonal_econ_query_female[, "jul"]),
    mean(coef_matrix_seasonal_natural_query_female[, "jul"]),
    mean(coef_matrix_seasonal_econ_natural_query_female[, "jul"])
  ),
  Aug = c(
    mean(coef_matrix_seasonal_bm_female[, "aug"]),
    mean(coef_matrix_seasonal_econ_only_female[, "aug"]),
    mean(coef_matrix_seasonal_natural_only_female[, "aug"]),
    mean(coef_matrix_seasonal_query_only_female[, "aug"]),
    mean(coef_matrix_seasonal_econ_natural_female[, "aug"]),
    mean(coef_matrix_seasonal_econ_query_female[, "aug"]),
    mean(coef_matrix_seasonal_natural_query_female[, "aug"]),
    mean(coef_matrix_seasonal_econ_natural_query_female[, "aug"])
  ),
  Sep = c(
    mean(coef_matrix_seasonal_bm_female[, "sep"]),
    mean(coef_matrix_seasonal_econ_only_female[, "sep"]),
    mean(coef_matrix_seasonal_natural_only_female[, "sep"]),
    mean(coef_matrix_seasonal_query_only_female[, "sep"]),
    mean(coef_matrix_seasonal_econ_natural_female[, "sep"]),
    mean(coef_matrix_seasonal_econ_query_female[, "sep"]),
    mean(coef_matrix_seasonal_natural_query_female[, "sep"]),
    mean(coef_matrix_seasonal_econ_natural_query_female[, "sep"])
  ),
  Oct = c(
    mean(coef_matrix_seasonal_bm_female[, "oct"]),
    mean(coef_matrix_seasonal_econ_only_female[, "oct"]),
    mean(coef_matrix_seasonal_natural_only_female[, "oct"]),
    mean(coef_matrix_seasonal_query_only_female[, "oct"]),
    mean(coef_matrix_seasonal_econ_natural_female[, "oct"]),
    mean(coef_matrix_seasonal_econ_query_female[, "oct"]),
    mean(coef_matrix_seasonal_natural_query_female[, "oct"]),
    mean(coef_matrix_seasonal_econ_natural_query_female[, "oct"])
  ),
  Nov = c(
    mean(coef_matrix_seasonal_bm_female[, "nov"]),
    mean(coef_matrix_seasonal_econ_only_female[, "nov"]),
    mean(coef_matrix_seasonal_natural_only_female[, "nov"]),
    mean(coef_matrix_seasonal_query_only_female[, "nov"]),
    mean(coef_matrix_seasonal_econ_natural_female[, "nov"]),
    mean(coef_matrix_seasonal_econ_query_female[, "nov"]),
    mean(coef_matrix_seasonal_natural_query_female[, "nov"]),
    mean(coef_matrix_seasonal_econ_natural_query_female[, "nov"])
  ),
  Dec = c(
    mean(coef_matrix_seasonal_bm_female[, "dec"]),
    mean(coef_matrix_seasonal_econ_only_female[, "dec"]),
    mean(coef_matrix_seasonal_natural_only_female[, "dec"]),
    mean(coef_matrix_seasonal_query_only_female[, "dec"]),
    mean(coef_matrix_seasonal_econ_natural_female[, "dec"]),
    mean(coef_matrix_seasonal_econ_query_female[, "dec"]),
    mean(coef_matrix_seasonal_natural_query_female[, "dec"]),
    mean(coef_matrix_seasonal_econ_natural_query_female[, "dec"])
  ),
  # Economic coefficients
  Unemployment = c(
    NA,
    mean(coef_vec_unemp_econ_only_female),
    NA,
    NA,
    mean(coef_vec_unemp_econ_natural_female),
    mean(coef_vec_unemp_econ_query_female),
    NA,
    mean(coef_vec_unemp_econ_natural_query_female)
  ),
  # Natural environment coefficients
   Temperature = c(
    NA,
    NA,
    mean(coef_vec_temp_natural_only_female),
    NA,
    mean(coef_vec_temp_econ_natural_female),
    NA,
    mean(coef_vec_temp_natural_query_female),
    mean(coef_vec_temp_econ_natural_query_female)
  ),
  Solar = c(
    NA,
    NA,
    mean(coef_vec_solar_natural_only_female),
    NA,
    mean(coef_vec_solar_econ_natural_female),
    NA,
    mean(coef_vec_solar_natural_query_female),
    mean(coef_vec_solar_econ_natural_query_female)
  ),
  # Search query coefficients
  # --- replace ONLY the "Query = c(...)" part with the block below ---

`Query suicide` = c(
  NA, NA, NA,
  mean(coef_matrix_query_only_female[,"query_suicide"], na.rm=TRUE),
  NA,
  mean(coef_matrix_query_econ_query_female[,"query_suicide"], na.rm=TRUE),
  mean(coef_matrix_query_natural_query_female[,"query_suicide"], na.rm=TRUE),
  mean(coef_matrix_query_econ_natural_query_female[,"query_suicide"], na.rm=TRUE)
),
`Query depression` = c(
  NA, NA, NA,
  mean(coef_matrix_query_only_female[,"query_depression"], na.rm=TRUE),
  NA,
  mean(coef_matrix_query_econ_query_female[,"query_depression"], na.rm=TRUE),
  mean(coef_matrix_query_natural_query_female[,"query_depression"], na.rm=TRUE),
  mean(coef_matrix_query_econ_natural_query_female[,"query_depression"], na.rm=TRUE)
),
`Query unemployment` = c(
  NA, NA, NA,
  mean(coef_matrix_query_only_female[,"query_unemployment"], na.rm=TRUE),
  NA,
  mean(coef_matrix_query_econ_query_female[,"query_unemployment"], na.rm=TRUE),
  mean(coef_matrix_query_natural_query_female[,"query_unemployment"], na.rm=TRUE),
  mean(coef_matrix_query_econ_natural_query_female[,"query_unemployment"], na.rm=TRUE)
),
`Query major depression` = c(
  NA, NA, NA,
  mean(coef_matrix_query_only_female[,"query_major_depression"], na.rm=TRUE),
  NA,
  mean(coef_matrix_query_econ_query_female[,"query_major_depression"], na.rm=TRUE),
  mean(coef_matrix_query_natural_query_female[,"query_major_depression"], na.rm=TRUE),
  mean(coef_matrix_query_econ_natural_query_female[,"query_major_depression"], na.rm=TRUE)
),
`Query insomnia` = c(
  NA, NA, NA,
  mean(coef_matrix_query_only_female[,"query_insomnia"], na.rm=TRUE),
  NA,
  mean(coef_matrix_query_econ_query_female[,"query_insomnia"], na.rm=TRUE),
  mean(coef_matrix_query_natural_query_female[,"query_insomnia"], na.rm=TRUE),
  mean(coef_matrix_query_econ_natural_query_female[,"query_insomnia"], na.rm=TRUE)
),
`Query marriage` = c(
  NA, NA, NA,
  mean(coef_matrix_query_only_female[,"query_marriage"], na.rm=TRUE),
  NA,
  mean(coef_matrix_query_econ_query_female[,"query_marriage"], na.rm=TRUE),
  mean(coef_matrix_query_natural_query_female[,"query_marriage"], na.rm=TRUE),
  mean(coef_matrix_query_econ_natural_query_female[,"query_marriage"], na.rm=TRUE)
),
`Query relationship breakup` = c(
  NA, NA, NA,
  mean(coef_matrix_query_only_female[,"query_relationship_breakup"], na.rm=TRUE),
  NA,
  mean(coef_matrix_query_econ_query_female[,"query_relationship_breakup"], na.rm=TRUE),
  mean(coef_matrix_query_natural_query_female[,"query_relationship_breakup"], na.rm=TRUE),
  mean(coef_matrix_query_econ_natural_query_female[,"query_relationship_breakup"], na.rm=TRUE)
),
`Query antidepressant` = c(
  NA, NA, NA,
  mean(coef_matrix_query_only_female[,"query_antidepressant"], na.rm=TRUE),
  NA,
  mean(coef_matrix_query_econ_query_female[,"query_antidepressant"], na.rm=TRUE),
  mean(coef_matrix_query_natural_query_female[,"query_antidepressant"], na.rm=TRUE),
  mean(coef_matrix_query_econ_natural_query_female[,"query_antidepressant"], na.rm=TRUE)
),
`Query allergy` = c(
  NA, NA, NA,
  mean(coef_matrix_query_only_female[,"query_allergy"], na.rm=TRUE),
  NA,
  mean(coef_matrix_query_econ_query_female[,"query_allergy"], na.rm=TRUE),
  mean(coef_matrix_query_natural_query_female[,"query_allergy"], na.rm=TRUE),
  mean(coef_matrix_query_econ_natural_query_female[,"query_allergy"], na.rm=TRUE)
),
`Query pain` = c(
  NA, NA, NA,
  mean(coef_matrix_query_only_female[,"query_pain"], na.rm=TRUE),
  NA,
  mean(coef_matrix_query_econ_query_female[,"query_pain"], na.rm=TRUE),
  mean(coef_matrix_query_natural_query_female[,"query_pain"], na.rm=TRUE),
  mean(coef_matrix_query_econ_natural_query_female[,"query_pain"], na.rm=TRUE)
),
`Query drunkenness` = c(
  NA, NA, NA,
  mean(coef_matrix_query_only_female[,"query_drunkenness"], na.rm=TRUE),
  NA,
  mean(coef_matrix_query_econ_query_female[,"query_drunkenness"], na.rm=TRUE),
  mean(coef_matrix_query_natural_query_female[,"query_drunkenness"], na.rm=TRUE),
  mean(coef_matrix_query_econ_natural_query_female[,"query_drunkenness"], na.rm=TRUE)
),
`Query alcohol abstinence` = c(
  NA, NA, NA,
  mean(coef_matrix_query_only_female[,"query_alcohol_abstinence"], na.rm=TRUE),
  NA,
  mean(coef_matrix_query_econ_query_female[,"query_alcohol_abstinence"], na.rm=TRUE),
  mean(coef_matrix_query_natural_query_female[,"query_alcohol_abstinence"], na.rm=TRUE),
  mean(coef_matrix_query_econ_natural_query_female[,"query_alcohol_abstinence"], na.rm=TRUE)
)
)

# Transpose the dataframe and round to 2 decimals (keep spaces)
coef_comparison_female_t <- data.frame(
  Variable = names(coef_comparison_female)[-1],
  t(round(coef_comparison_female[,-1], 2)),
  check.names = FALSE
)

# Set model names as column headers
colnames(coef_comparison_female_t)[-1] <- coef_comparison_female$Model_Name

# Replace NA with blank for readability
coef_comparison_female_t[is.na(coef_comparison_female_t)] <- " "

# Normalize "Query ..." row labels like male (lowercase terms, no underscores/dots)
rn <- rownames(coef_comparison_female_t)
rownames(coef_comparison_female_t) <- ifelse(
  grepl("^Query", rn, ignore.case = TRUE),
  paste("Query", tolower(trimws(gsub("[._]", " ", sub("^Query[[:space:]._]*", "", rn, ignore.case = TRUE))))),
  rn
)

# Save as CSV
write.csv(coef_comparison_female_t, "coef_comparison_female_transposed.csv", row.names = FALSE)

# combined graph---------------------------------------------------------
# Create the combined plot object
windows()
grid.arrange(
  mape_boxplot_overall,
  mape_boxplot_male,
  mape_boxplot_female,
  ncol = 1
)

# Save the combined plot
png("Figure_2.png",
    width = 16, height = 10, units = "in", res = 300)
grid.arrange(
  mape_boxplot_overall,
  mape_boxplot_male,
  mape_boxplot_female,
  ncol = 2
)
dev.off()

# save----------------------------------------------------------# 
save.image("analysis.RData")




