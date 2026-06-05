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


#Spain vs other EU
# EU member countries (excluding Spain)
eu_countries <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus",
  "Czechia", "Denmark", "Estonia", "Finland", "France",
  "Germany", "Greece", "Hungary", "Ireland", "Italy",
  "Latvia", "Lithuania", "Luxembourg", "Malta",
  "Netherlands (Kingdom of the)", "Poland", "Portugal",
  "Romania", "Slovakia", "Slovenia", "Sweden"
)

# Calculate the average standardized rate for other EU countries (excluding Spain)
other_eu_avg <- df_filtered_who %>%
  filter(location %in% eu_countries) %>%
  group_by(period) %>%
  summarise(other_europe_std_rate = mean(std_rate, na.rm = TRUE))

# Combine EU average and Spain data
plot_data_who_eu <- other_eu_avg %>%
  left_join(spain_data, by = "period")

# Create the plot
p_who_eu <- ggplot(plot_data_who_eu, aes(x = period)) +
  geom_line(aes(y = other_europe_std_rate, color = "Other EU Countries")) +
  geom_line(aes(y = spain_std_rate, color = "Spain")) +
  theme_minimal() +
  labs(
    title = "Suicide Rate Trend (Standardized Rate): Spain vs Other EU Countries",
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
  ) +
  scale_color_manual(values = c("Other EU Countries" = "blue", "Spain" = "red")) +
  expand_limits(y = 0)

windows()
print(p_who_eu)

png("Figure_1_EU.png",
    width = 16, height = 8, units = "in", res = 300)
print(p_who_eu)
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

total_query
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
month_names <- c("feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")

unemp_covs   <- "total_unemp_lag1"
natural_covs <- c("solar_lag1", "temp_lag1")
query_covs   <-total_query

covariate_display <- c(
  total_unemp_lag1           = "Unemployment",
  male_unemp_lag1            = "Unemployment",
  female_unemp_lag1          = "Unemployment",
  solar_lag1                 = "Solar",
  temp_lag1                  = "Temperature",
  query_suicide              = "Query suicide",
  query_depression           = "Query depression",
  query_unemployment         = "Query unemployment",
  query_relationship_breakup = "Query relationship breakup",
  query_antidepressant       = "Query antidepressant",
  query_allergy              = "Query allergy",
  query_pain                 = "Query pain",
  query_drunkenness          = "Query drunkenness",
  query_alcohol_abstinence   = "Query alcohol abstinence"
)

# Fit a single SSM fold for a given outcome and covariate set
fit_ssm_fold <- function(df, i, outcome, pop_col, actual_col, covariate_base_names) {
  train_col   <- paste0("train_", i)
  test_col    <- paste0("test_", i)
  y_col       <- paste0(outcome, "_scaled_", i, "_ssm_y_train_", i)
  active_covs <- if (length(covariate_base_names) > 0)
    covariate_base_names[nzchar(covariate_base_names)] else character(0)
  scaled_covs <- if (length(active_covs) > 0)
    paste0(active_covs, "_scaled_", i) else character(0)
  y           <- df[[y_col]]

  if (length(scaled_covs) > 0) {
    local_env <- environment()
    for (cn in scaled_covs) local_env[[cn]] <- df[[cn]]
    reg_formula <- as.formula(paste("~", paste(scaled_covs, collapse = " + ")),
                               env = local_env)
    model <- SSModel(
      y ~ SSMtrend(degree = 1, Q = NA) +
        SSMseasonal(period = 12, sea.type = "dummy") +
        SSMregression(reg_formula),
      H = NA
    )
  } else {
    model <- SSModel(
      y ~ SSMtrend(degree = 1, Q = NA) +
        SSMseasonal(period = 12, sea.type = "dummy"),
      H = NA
    )
  }

  fit  <- fitSSM(model, inits = c(1, 1))
  kfs  <- KFS(fit$model)
  pred <- predict(fit$model, interval = "prediction", level = 0.95)

  n_pred   <- sum(df[[test_col]] == 1)
  pred_raw <- pred[(nrow(pred) - n_pred + 1):nrow(pred), 1]
  min_val  <- min(df[[outcome]][df[[train_col]] == 1], na.rm = TRUE)
  max_val  <- max(df[[outcome]][df[[train_col]] == 1], na.rm = TRUE)
  pred_inv <- scale_max_min_vector_inverse(pred_raw, min_val, max_val)
  pred_inv <- pred_inv * df[[pop_col]][df[[test_col]] == 1] / 100000

  actual <- df[[actual_col]][df[[test_col]] == 1]
  acc    <- forecast::accuracy(pred_inv, actual)

  kfs_coef <- coef(kfs)[-1, , drop = FALSE]
  sea_mat  <- kfs_coef[, grep("^sea", colnames(kfs_coef)), drop = FALSE]
  seasonal <- setNames(
    sapply(seq_along(month_names), function(j)
      mean(sea_mat[seq(j, nrow(sea_mat), by = 12), 1], na.rm = TRUE)),
    month_names
  )
  reg_coef <- if (length(scaled_covs) > 0)
    setNames(sapply(scaled_covs, function(cn) mean(kfs_coef[, cn], na.rm = TRUE)),
             active_covs)
  else numeric(0)

  list(mae = acc[3], mape = acc[5],
       level    = mean(kfs_coef[, "level"], na.rm = TRUE),
       seasonal = seasonal,
       reg_coef = reg_coef)
}

# Run model across all 20 folds
run_ssm_model <- function(df, model_id, outcome, pop_col, actual_col,
                           covariate_base_names, thresholds, n_rep = 20) {
  folds        <- lapply(seq_len(n_rep), function(i)
    fit_ssm_fold(df, i, outcome, pop_col, actual_col, covariate_base_names))
  mae_vec      <- sapply(folds, `[[`, "mae")
  mape_vec     <- sapply(folds, `[[`, "mape")
  level_vec    <- sapply(folds, `[[`, "level")
  seasonal_mat <- do.call(rbind, lapply(folds, `[[`, "seasonal"))
  reg_mat      <- if (length(covariate_base_names) > 0)
    do.call(rbind, lapply(folds, `[[`, "reg_coef")) else NULL

  list(model_id        = model_id,
       covariate_names = covariate_base_names,
       mae_df  = data.frame(model_id   = model_id, rep_id = seq_len(n_rep),
                             test_start = thresholds, mae  = mae_vec),
       mape_df = data.frame(model_id   = model_id, rep_id = seq_len(n_rep),
                             test_start = thresholds, mape = mape_vec),
       level_vec    = level_vec,
       seasonal_mat = seasonal_mat,
       reg_mat      = reg_mat)
}

# Build transposed coefficient comparison table
build_coef_table <- function(models, model_levels, query_col_order, covariate_display) {
  month_cap <- c("Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  display_col_order <- c(
    "Level", month_cap,
    "Unemployment", "Temperature", "Solar",
    paste("Query", query_col_order)
  )

  coef_rows <- lapply(models, function(m) {
    row <- c(
      Level = mean(m$level_vec),
      setNames(colMeans(m$seasonal_mat, na.rm = TRUE), month_cap)
    )
    if (!is.null(m$reg_mat)) {
      reg_means  <- colMeans(m$reg_mat, na.rm = TRUE)
      disp       <- covariate_display[names(reg_means)]
      reg_means  <- reg_means[!is.na(disp)]
      names(reg_means) <- disp[!is.na(disp)]
      row        <- c(row, reg_means)
    }
    row
  })

  comparison <- data.frame(
    Model_Name = model_levels,
    do.call(rbind, lapply(coef_rows, function(r) {
      out        <- setNames(rep(NA_real_, length(display_col_order)), display_col_order)
      out[names(r)] <- r
      out
    })),
    check.names = FALSE
  )

  t_df <- data.frame(
    Variable = names(comparison)[-1],
    t(round(comparison[, -1, drop = FALSE], 2)),
    check.names = FALSE
  )
  colnames(t_df)[-1] <- comparison$Model_Name
  t_df[is.na(t_df)] <- " "
  t_df
}

# Model specifications
model_specs_overall <- list(
  list(id = "bm_overall",                 covs = character(0)),
  list(id = "econ_only_overall",          covs = unemp_covs),
  list(id = "natural_only_overall",       covs = natural_covs),
  list(id = "query_only_overall",         covs = query_covs),
  list(id = "econ_natural_overall",       covs = c(unemp_covs, natural_covs)),
  list(id = "econ_query_overall",         covs = c(unemp_covs, query_covs)),
  list(id = "natural_query_overall",      covs = c(natural_covs, query_covs)),
  list(id = "econ_natural_query_overall", covs = c(unemp_covs, natural_covs, query_covs))
)

model_levels_overall <- c(
  "Basic Structural Time Series",
  "Econ Only",
  "Natural Env Only",
  "Search Query Only",
  "Econ + Natural Env",
  "Econ + Search Query",
  "Natural Env + Search Query",
  "Econ + Natural Env + Search Query"
)


# Run all 8 models x 20 folds
models_overall <- lapply(model_specs_overall, function(spec) {
  run_ssm_model(
    df = df, 
    model_id = spec$id, 
    outcome = "suicide_rate_total", 
    pop_col = "pop_total", 
    actual_col = "num_suicide_total",
    covariate_base_names = spec$covs, 
    thresholds = thresholds
  )
})

names(models_overall) <- sapply(model_specs_overall, `[[`, "id")

model_display_overall <- setNames(model_levels_overall,
                                   sapply(model_specs_overall, `[[`, "id"))

# Build accuracy result dataframes
result_overall_mae <- do.call(rbind, lapply(models_overall, function(m) {
  d <- m$mae_df; d$model_name <- model_display_overall[m$model_id]; d
}))
result_overall_mape <- do.call(rbind, lapply(models_overall, function(m) {
  d <- m$mape_df; d$model_name <- model_display_overall[m$model_id]; d
}))
result_overall_mae$model_name  <- factor(result_overall_mae$model_name,
                                          levels = model_levels_overall)
result_overall_mape$model_name <- factor(result_overall_mape$model_name,
                                          levels = model_levels_overall)

# Shared plot theme
plot_theme_ssm <- theme_minimal() + theme(
  plot.title      = element_text(size = 17, hjust = 0.5),
  axis.title      = element_text(size = 12),
  axis.text.x     = element_text(size = 12, angle = 45, hjust = 1),
  axis.text.y     = element_text(size = 12),
  legend.title    = element_text(size = 12),
  legend.text     = element_text(size = 12),
  legend.position = "bottom",
  legend.key.size = unit(2, "cm")
)

mae_boxplot <- ggplot(result_overall_mae, aes(x = model_name, y = mae)) +
  geom_boxplot() +
  labs(title = "MAE across 20 Replicated Hold-Outs for Suicide Deaths (Overall)",
       x = "Model", y = "MAE") +
  plot_theme_ssm
windows()
print(mae_boxplot)
png("mae_boxplot_overall.png", width = 16, height = 8, units = "in", res = 300)
print(mae_boxplot)
dev.off()

mape_boxplot_overall <- ggplot(result_overall_mape, aes(x = model_name, y = mape)) +
  geom_boxplot() +
  labs(title = "MAPE across 20 Replicated Hold-Outs for Suicide Deaths (Overall)",
       x = "Model", y = "MAPE") +
  plot_theme_ssm
windows()
print(mape_boxplot_overall)
png("mape_boxplot_overall.png", width = 16, height = 8, units = "in", res = 300)
print(mape_boxplot_overall)
dev.off()

# Accuracy summary
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
  ) %>%
  arrange(model_levels_overall )
print(result_overall_accuracy)
write_csv(result_overall_accuracy, "result_overall_accuracy.csv")

# Coefficient comparison table
query_col_order_overall <- c(
  "suicide", "depression", "unemployment", "relationship breakup",
  "antidepressant", "allergy", "pain", "drunkenness", "alcohol abstinence"
)
coef_comparison_overall_t <- build_coef_table(
  models_overall, model_levels_overall, query_col_order_overall, covariate_display
)
write.csv(coef_comparison_overall_t, "coef_comparison_overall_transposed.csv",
          row.names = FALSE)

# Male model----------------------------------------------------------
unemp_covs_male <- "male_unemp_lag1"

model_specs_male <- list(
  list(id = "bm_male",                 covs = character(0)),
  list(id = "econ_only_male",          covs = unemp_covs_male),
  list(id = "natural_only_male",       covs = natural_covs),
  list(id = "query_only_male",         covs = male_query),
  list(id = "econ_natural_male",       covs = c(unemp_covs_male, natural_covs)),
  list(id = "econ_query_male",         covs = c(unemp_covs_male, male_query)),
  list(id = "natural_query_male",      covs = c(natural_covs, male_query)),
  list(id = "econ_natural_query_male", covs = c(unemp_covs_male, natural_covs, male_query))
)

model_levels_male <- c(
  "Basic Structural Time Series",
  "Econ Only",
  "Natural Env Only",
  "Search Query Only",
  "Econ + Natural Env",
  "Econ + Search Query",
  "Natural Env + Search Query",
  "Econ + Natural Env + Search Query"
)
# Run all 8 models x 20 folds
models_male <- lapply(model_specs_male, function(spec)
  run_ssm_model(df, spec$id, "suicide_rate_male", "pop_male", "num_suicide_male",
                spec$covs, thresholds))
names(models_male) <- sapply(model_specs_male, `[[`, "id")


model_display_male <- setNames(model_levels_male, sapply(model_specs_male, `[[`, "id"))

result_male_mae <- do.call(rbind, lapply(models_male, function(m) {
  d <- m$mae_df; d$model_name <- model_display_male[m$model_id]; d
}))
result_male_mape <- do.call(rbind, lapply(models_male, function(m) {
  d <- m$mape_df; d$model_name <- model_display_male[m$model_id]; d
}))
result_male_mae$model_name  <- factor(result_male_mae$model_name,  levels = model_levels_male)
result_male_mape$model_name <- factor(result_male_mape$model_name, levels = model_levels_male)

#plot results
mae_boxplot <- ggplot(result_male_mae, aes(x = model_name, y = mae)) +
  geom_boxplot() +
  labs(title = "MAE across 20 Replicated Hold-Outs for Suicide Deaths (Male)",
       x = "Model", y = "MAE") +
  plot_theme_ssm
png("mae_boxplot_male.png", width = 16, height = 8, units = "in", res = 300)
print(mae_boxplot)
dev.off()

mape_boxplot_male <- ggplot(result_male_mape, aes(x = model_name, y = mape)) +
  geom_boxplot() +
  labs(title = "MAPE across 20 Replicated Hold-Outs for Suicide Deaths (Male)",
       x = "Model", y = "MAPE") +
  plot_theme_ssm
png("mape_boxplot_male.png", width = 16, height = 8, units = "in", res = 300)
print(mape_boxplot_male)
dev.off()

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
print(result_male_accuracy)
write_csv(result_male_accuracy, "result_male_accuracy.csv")

query_col_order_male <- c(
  "suicide", "depression", "unemployment", "relationship breakup",
  "antidepressant", "allergy", "pain", "drunkenness", "alcohol abstinence"
)
coef_comparison_male_t <- build_coef_table(
  models_male, model_levels_male, query_col_order_male, covariate_display
)
write.csv(coef_comparison_male_t, "coef_comparison_male_transposed.csv", row.names = FALSE)

# Female model----------------------------------------------------------
unemp_covs_female <- "female_unemp_lag1"

model_specs_female <- list(
  list(id = "bm_female",                 covs = character(0)),
  list(id = "econ_only_female",          covs = unemp_covs_female),
  list(id = "natural_only_female",       covs = natural_covs),
  list(id = "query_only_female",         covs = female_query),
  list(id = "econ_natural_female",       covs = c(unemp_covs_female, natural_covs)),
  list(id = "econ_query_female",         covs = c(unemp_covs_female, female_query)),
  list(id = "natural_query_female",      covs = c(natural_covs, female_query)),
  list(id = "econ_natural_query_female", covs = c(unemp_covs_female, natural_covs, female_query))
)

model_levels_female <- c(
  "Basic Structural Time Series",
  "Econ Only",
  "Natural Env Only",
  "Search Query Only",
  "Econ + Natural Env",
  "Econ + Search Query",
  "Natural Env + Search Query",
  "Econ + Natural Env + Search Query"
)


# Run all 8 models x 20 folds
models_female <- lapply(model_specs_female, function(spec)
  run_ssm_model(df, spec$id, "suicide_rate_female", "pop_female", "num_suicide_female",
                spec$covs, thresholds))
names(models_female) <- sapply(model_specs_female, `[[`, "id")

model_display_female <- setNames(model_levels_female, sapply(model_specs_female, `[[`, "id"))

result_female_mae <- do.call(rbind, lapply(models_female, function(m) {
  d <- m$mae_df; d$model_name <- model_display_female[m$model_id]; d
}))
result_female_mape <- do.call(rbind, lapply(models_female, function(m) {
  d <- m$mape_df; d$model_name <- model_display_female[m$model_id]; d
}))
result_female_mae$model_name  <- factor(result_female_mae$model_name,  levels = model_levels_female)
result_female_mape$model_name <- factor(result_female_mape$model_name, levels = model_levels_female)

#plot results
mae_boxplot <- ggplot(result_female_mae, aes(x = model_name, y = mae)) +
  geom_boxplot() +
  labs(title = "MAE across 20 Replicated Hold-Outs for Suicide Deaths (Female)",
       x = "Model", y = "MAE") +
  plot_theme_ssm
png("mae_boxplot_female.png", width = 16, height = 8, units = "in", res = 300)
print(mae_boxplot)
dev.off()

mape_boxplot_female <- ggplot(result_female_mape, aes(x = model_name, y = mape)) +
  geom_boxplot() +
  labs(title = "MAPE across 20 Replicated Hold-Outs for Suicide Deaths (Female)",
       x = "Model", y = "MAPE") +
  plot_theme_ssm
png("mape_boxplot_female.png", width = 16, height = 8, units = "in", res = 300)
print(mape_boxplot_female)
dev.off()

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
print(result_female_accuracy)
write_csv(result_female_accuracy, "result_female_accuracy.csv")

query_col_order_female <- c(
  "suicide", "depression", "unemployment", "relationship breakup",
  "antidepressant", "allergy", "pain", "drunkenness", "alcohol abstinence"
)
coef_comparison_female_t <- build_coef_table(
  models_female, model_levels_female, query_col_order_female, covariate_display
)
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

# predicion like monitoring----------------------------------------------------------
monitoring_start<- "2022-01-01"
monitoring_end<- "2023-12-01"
#create dataframe for prediction
df_final_pred <- df %>% select(
  term,
  num_suicide_total,
  num_suicide_male,
  num_suicide_female,
  pop_female,
  pop_male,
  pop_total,
  query_suicide,
  suicide_rate_total,
  suicide_rate_male,
  suicide_rate_female,
)

df_final_pred <- df_final_pred %>% mutate(
  suicide_rate_total_y   = if_else(term >= as.Date(monitoring_start) & term <= as.Date(monitoring_end), NA_real_, suicide_rate_total),
  suicide_rate_male_y    = if_else(term >= as.Date(monitoring_start) & term <= as.Date(monitoring_end), NA_real_, suicide_rate_male),
  suicide_rate_female_y  = if_else(term >= as.Date(monitoring_start) & term <= as.Date(monitoring_end), NA_real_, suicide_rate_female)
)

# Add scaled versions using monitoring_start as cutoff date
df_final_pred <- df_final_pred %>% mutate(
  suicide_rate_total_y_scaled  = scale_max_min_vector(df_final_pred, "suicide_rate_total_y", as.Date(monitoring_start)),
  suicide_rate_male_y_scaled   = scale_max_min_vector(df_final_pred, "suicide_rate_male_y", as.Date(monitoring_start)),
  suicide_rate_female_y_scaled = scale_max_min_vector(df_final_pred, "suicide_rate_female_y", as.Date(monitoring_start))
)


# Store min/max values for inverse scaling later
suicide_rate_total_min_for_mnt <- min(df_final_pred$suicide_rate_total[df_final_pred$term < as.Date(monitoring_start)], na.rm = TRUE)
suicide_rate_total_max_for_mnt <- max(df_final_pred$suicide_rate_total[df_final_pred$term < as.Date(monitoring_start)], na.rm = TRUE)
suicide_rate_male_min_for_mnt <- min(df_final_pred$suicide_rate_male[df_final_pred$term < as.Date(monitoring_start)], na.rm = TRUE)
suicide_rate_male_max_for_mnt <- max(df_final_pred$suicide_rate_male[df_final_pred$term < as.Date(monitoring_start)], na.rm = TRUE)
suicide_rate_female_min_for_mnt <- min(df_final_pred$suicide_rate_female[df_final_pred$term < as.Date(monitoring_start)], na.rm = TRUE)
suicide_rate_female_max_for_mnt <- max(df_final_pred$suicide_rate_female[df_final_pred$term < as.Date(monitoring_start)], na.rm = TRUE)

# Overall----------------------------------------------------------
model_for_mnt_overall <- SSModel(
    df_final_pred$suicide_rate_total_y_scaled ~ SSMtrend(degree = 1, Q = NA) + SSMseasonal(period = 12, sea.type = "dummy") ,
    H = NA
  )
  fit_for_mnt_overall <- fitSSM(model_for_mnt_overall, inits = c(1, 1))
  kfs_for_mnt_overall <- KFS(fit_for_mnt_overall$model)
  pred_for_mnt_overall <- predict(fit_for_mnt_overall$model, interval = "prediction", level = 0.50)

  # base value
  df_final_pred$num_suicide_total_pred_mnt <- scale_max_min_vector_inverse(
    pred_for_mnt_overall[1:nrow(pred_for_mnt_overall), 1],
    suicide_rate_total_min_for_mnt,
    suicide_rate_total_max_for_mnt
  ) * df_final_pred$pop_total / 100000

  # lower value
  df_final_pred$num_suicide_total_pred_mnt_lwr <- scale_max_min_vector_inverse(
    pred_for_mnt_overall[1:nrow(pred_for_mnt_overall), 2],
    suicide_rate_total_min_for_mnt,
    suicide_rate_total_max_for_mnt
  ) * df_final_pred$pop_total / 100000

  # upper value
  df_final_pred$num_suicide_total_pred_mnt_upr <- scale_max_min_vector_inverse(
    pred_for_mnt_overall[1:nrow(pred_for_mnt_overall), 3],
    suicide_rate_total_min_for_mnt,
    suicide_rate_total_max_for_mnt
  )*df_final_pred$pop_total/100000

#confirtm accuracy
acc_monitoring_overall <- accuracy(df_final_pred$num_suicide_total_pred_mnt[df_final_pred$term >= as.Date(monitoring_start) & df_final_pred$term <= as.Date(monitoring_end)], df_final_pred$num_suicide_total[df_final_pred$term >= as.Date(monitoring_start) & df_final_pred$term <= as.Date(monitoring_end)])
print(acc_monitoring_overall)
mape_monitoring_overall <- acc_monitoring_overall[5]
# Male----------------------------------------------------------
model_for_mnt_male <- SSModel(
    df_final_pred$suicide_rate_male_y_scaled ~ SSMtrend(degree = 1, Q = NA) + SSMseasonal(period = 12, sea.type = "dummy") ,
    H = NA
  )
  fit_for_mnt_male <- fitSSM(model_for_mnt_male, inits = c(1, 1))
  kfs_for_mnt_male <- KFS(fit_for_mnt_male$model)
  pred_for_mnt_male <- predict(fit_for_mnt_male$model, interval = "prediction", level = 0.50)


  # base value
  df_final_pred$num_suicide_male_pred_mnt <- scale_max_min_vector_inverse(
    pred_for_mnt_male[1:nrow(pred_for_mnt_male), 1],
    suicide_rate_male_min_for_mnt,
    suicide_rate_male_max_for_mnt
  ) * df_final_pred$pop_male / 100000

  # lower value
  df_final_pred$num_suicide_male_pred_mnt_lwr <- scale_max_min_vector_inverse(
    pred_for_mnt_male[1:nrow(pred_for_mnt_male), 2],
    suicide_rate_male_min_for_mnt,
    suicide_rate_male_max_for_mnt
  ) * df_final_pred$pop_male / 100000

  # upper value
  df_final_pred$num_suicide_male_pred_mnt_upr <- scale_max_min_vector_inverse(
    pred_for_mnt_male[1:nrow(pred_for_mnt_male), 3],
    suicide_rate_male_min_for_mnt,
    suicide_rate_male_max_for_mnt
  ) * df_final_pred$pop_male / 100000

#confirm accuracy
acc_monitoring_male <- accuracy(df_final_pred$num_suicide_male_pred_mnt[df_final_pred$term >= as.Date(monitoring_start) & df_final_pred$term <= as.Date(monitoring_end)], df_final_pred$num_suicide_male[df_final_pred$term >= as.Date(monitoring_start) & df_final_pred$term <= as.Date(monitoring_end)])
print(acc_monitoring_male)
mape_monitoring_male <- acc_monitoring_male[5]

# Female----------------------------------------------------------
model_for_mnt_female <- SSModel(
    df_final_pred$suicide_rate_female_y_scaled ~ SSMtrend(degree = 1, Q = NA) + SSMseasonal(period = 12, sea.type = "dummy") ,
    H = NA
  )
  fit_for_mnt_female <- fitSSM(model_for_mnt_female, inits = c(1, 1))
  kfs_for_mnt_female <- KFS(fit_for_mnt_female$model)
  pred_for_mnt_female <- predict(fit_for_mnt_female$model, interval = "prediction", level = 0.50)

  # base value
  df_final_pred$num_suicide_female_pred_mnt <- scale_max_min_vector_inverse(
    pred_for_mnt_female[1:nrow(pred_for_mnt_female), 1],
    suicide_rate_female_min_for_mnt,
    suicide_rate_female_max_for_mnt
  ) * df_final_pred$pop_female / 100000

  # lower value
  df_final_pred$num_suicide_female_pred_mnt_lwr <- scale_max_min_vector_inverse(
    pred_for_mnt_female[1:nrow(pred_for_mnt_female), 2],
    suicide_rate_female_min_for_mnt,
    suicide_rate_female_max_for_mnt
  ) * df_final_pred$pop_female / 100000

  # upper value
  df_final_pred$num_suicide_female_pred_mnt_upr <- scale_max_min_vector_inverse(
    pred_for_mnt_female[1:nrow(pred_for_mnt_female), 3],
    suicide_rate_female_min_for_mnt,
    suicide_rate_female_max_for_mnt
  ) * df_final_pred$pop_female / 100000

#confirm accuracy
acc_monitoring_female <- accuracy(df_final_pred$num_suicide_female_pred_mnt[df_final_pred$term >= as.Date(monitoring_start) & df_final_pred$term <= as.Date(monitoring_end)], df_final_pred$num_suicide_female[df_final_pred$term >= as.Date(monitoring_start) & df_final_pred$term <= as.Date(monitoring_end)])
print(acc_monitoring_female)
mape_monitoring_female <- acc_monitoring_female[5]

# Comparison plots for monitoring period----------------------------------------------------------
# Filter data for monitoring period
monitoring_data <- df_final_pred %>%
  filter(term >= as.Date(monitoring_start) & term <= as.Date(monitoring_end))

# Overall comparison plot
plot_overall_mnt <- monitoring_data %>%
  ggplot(aes(x = term)) +
  geom_line(aes(y = num_suicide_total, color = "Actual"), size = 1) +
  geom_line(aes(y = num_suicide_total_pred_mnt, color = "Predicted by BSTS"), size = 1) +
  geom_ribbon(aes(ymin = num_suicide_total_pred_mnt_lwr, ymax = num_suicide_total_pred_mnt_upr), 
              alpha = 0.2, fill = "lightblue") +
  labs(title = "Overall Suicide Deaths: Actual vs Predicted by BSTS with 50% CI (Jan 2022-Dec 2023)",
       x = "Date", y = "Suicide Deaths", color = "Value") +
  scale_y_continuous(limits = c(0, NA)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.title.x = element_text(size = 10),
    axis.text.x = element_text(size = 10, angle = 60, hjust = 1),
    axis.title.y = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12)
  ) +
  annotate("text", x = as.Date("2023-01-01"), y = 150, 
           label = paste("MAPE =", round(mape_monitoring_overall, 2), "%"), 
           hjust = 0.5, vjust = 0.5, size = 4, fontface = "bold")

# Male comparison plot
plot_male_mnt <- monitoring_data %>%
  ggplot(aes(x = term)) +
  geom_line(aes(y = num_suicide_male, color = "Actual"), size = 1) +
  geom_line(aes(y = num_suicide_male_pred_mnt, color = "Predicted by BSTS"), size = 1) +
  geom_ribbon(aes(ymin = num_suicide_male_pred_mnt_lwr, ymax = num_suicide_male_pred_mnt_upr), 
              alpha = 0.2, fill = "lightblue") +
  labs(title = "Male Suicide Deaths: Actual vs Predicted by BSTS with 50% CI (Jan 2022-Dec 2023)",
       x = "Date", y = "Suicide Deaths", color = "Value") +
  scale_y_continuous(limits = c(0, NA)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.title.x = element_text(size = 10),
    axis.text.x = element_text(size = 10, angle = 60, hjust = 1),
    axis.title.y = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12)
  ) +
  annotate("text", x = as.Date("2023-01-01"), y = 150, 
           label = paste("MAPE =", round(mape_monitoring_male, 2), "%"), 
           hjust = 0.5, vjust = 0.5, size = 4, fontface = "bold")

# Female comparison plot
plot_female_mnt <- monitoring_data %>%
  ggplot(aes(x = term)) +
  geom_line(aes(y = num_suicide_female, color = "Actual"), size = 1) +
  geom_line(aes(y = num_suicide_female_pred_mnt, color = "Predicted by BSTS"), size = 1) +
  geom_ribbon(aes(ymin = num_suicide_female_pred_mnt_lwr, ymax = num_suicide_female_pred_mnt_upr), 
              alpha = 0.2, fill = "lightblue") +
  labs(title = "Female Suicide Deaths: Actual vs Predicted by BSTS with 50% CI (Jan 2022-Dec 2023)",
       x = "Date", y = "Suicide Deaths", color = "Value") +
  scale_y_continuous(limits = c(0, NA)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.title.x = element_text(size = 10),
    axis.text.x = element_text(size = 10, angle = 60, hjust = 1),
    axis.title.y = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12)
  ) +
  annotate("text", x = as.Date("2023-01-01"), y = 45, 
           label = paste("MAPE =", round(mape_monitoring_female, 2), "%"), 
           hjust = 0.5, vjust = 0.5, size = 4, fontface = "bold")
# Save the combined plot
png("Figure_3.png",
    width = 16.3, height = 10, units = "in", res = 300)
grid.arrange(
  plot_overall_mnt,
  plot_male_mnt,
  plot_female_mnt,
  ncol = 2
)
dev.off()

# save----------------------------------------------------------# 
save.image("analysis.RData")




