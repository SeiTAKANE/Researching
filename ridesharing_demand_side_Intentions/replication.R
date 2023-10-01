#Author's Development Environment ----------------------------------------
#R version: R version 4.2.3 (2023-03-15)
#RStudio version: 2023.3.0.386 
#tidyverse version: 1.3.2 
#lmtest version: 0.9.40 
#sandwich version: 3.0.2 
#margins version: 0.3.26 
#BalanceR: https://github.com/JaehyunSong/BalanceR

# read packages -----------------------------------------------------------
pacman::p_load(tidyverse,lmtest,sandwich,margins)
library(BalanceR)

# Preprocedure ------------------------------------------------------------
#read data
#rs group
rs_data_1 <- read.csv("data/rs_group_data.csv")
#taxi group
taxi_data_1 <- read.csv("data/taxi_group_data.csv")

#setting the attribution levels
# Price
price_levels <- c('¥1500','¥1200')
rs_data_1$Price <- factor(rs_data_1$Price, levels = price_levels,ordered = TRUE)
taxi_data_1$Price <- factor(taxi_data_1$Price, levels = price_levels,ordered = TRUE)

# Accident
safety_levels <- c('3%','1%')
rs_data_1$Safety <- factor(rs_data_1$Safety, levels = safety_levels,ordered = TRUE)
taxi_data_1$Safety <- factor(taxi_data_1$Safety, levels = safety_levels,ordered = TRUE)

# Dispatch
dispatch_levels <- c('Phone', 'App')
rs_data_1$Dispatch <- factor(rs_data_1$Dispatch, levels = dispatch_levels)
taxi_data_1$Dispatch <- factor(taxi_data_1$Dispatch, levels = dispatch_levels)

# C2License
license_levels <- c('No', 'Yes')
rs_data_1$License <- factor(rs_data_1$License, levels = license_levels)
taxi_data_1$License <- factor(taxi_data_1$License, levels = license_levels)

# Time
time_levels <- c('10 min','7 min')
rs_data_1$Time <- factor(rs_data_1$Time , levels = time_levels,ordered = TRUE)
taxi_data_1$Time <- factor(taxi_data_1$Time , levels = time_levels,ordered = TRUE)

# Service
service_levels <- c('No', 'Yes')
rs_data_1$Service <- factor(rs_data_1$Service, levels = service_levels)
taxi_data_1$Service <- factor(taxi_data_1$Service,  levels = service_levels)



#create dummy variables for covariates
#rs
rs_data_1  <- rs_data_1  %>% mutate(
  age_over40 = ifelse(age %in% c("40s","50s","60s","70s"), 1, 0),
  sex_female = ifelse(sex == "Female", 1, 0),
  occupation_Full_time_employee = ifelse(occupation == "Full-time employee (mainly private companies)", 1, 0),
  income_over_50M = ifelse(income %in% c("¥5M or more and less than ¥6M ","¥6M or more and less than ¥7M","7M or more and less than ¥8M","¥8M or more and less than ¥9M",
                                         "¥9M or more and less than ¥10M","¥10M or more"), 1, 0),
  region_more_than_500k = ifelse(region == "Population of 500K or more", 1, 0),
  Taxi_frequency_more_than_several_time_a_year = ifelse(Taxi_frequency %in% c("Several times a year","Several times a month ","About once a week","Several times a week"," Almost every day"), 1, 0),
  Taxi_aim_laisure = ifelse(Taxi_aim == "Leisure", 1, 0),
  RS_experience_yes = ifelse(RS_experience == "Yes", 1, 0)
)

#taxi
taxi_data_1 <- taxi_data_1 %>% mutate(
  age_over40 = ifelse(age %in% c("40s","50s","60s","70s"), 1, 0),
  sex_female = ifelse(sex == "Female", 1, 0),
  occupation_Full_time_employee = ifelse(occupation == "Full-time employee (mainly private companies)", 1, 0),
  income_over_50M = ifelse(income %in% c("¥5M or more and less than ¥6M ","¥6M or more and less than ¥7M","7M or more and less than ¥8M","¥8M or more and less than ¥9M",
                                         "¥9M or more and less than ¥10M","¥10M or more"), 1, 0),
  region_more_than_500k = ifelse(region == "Population of 500K or more", 1, 0),
  Taxi_frequency_more_than_several_time_a_year = ifelse(Taxi_frequency %in% c("Several times a year","Several times a month ","About once a week","Several times a week"," Almost every day"), 1, 0),
  Taxi_aim_laisure = ifelse(Taxi_aim == "Leisure", 1, 0),
  RS_experience_yes = ifelse(RS_experience == "Yes", 1, 0)
)


# Balance Check -----------------------------------------------------------
#reading package
#https://github.com/JaehyunSong/BalanceR
devtools::install_github("JaehyunSong/BalanceR")
library(BalanceR)

#create the dataset for balance check
rs_data_blc <- rs_data_1 %>% select(c("id","age_over40", "sex_female", "occupation_Full_time_employee", "income_over_50M", 
                                      "region_more_than_500k", "Taxi_frequency_more_than_several_time_a_year", 
                                      "Taxi_aim_laisure", "RS_experience_yes")) 
taxi_data_blc <- taxi_data_1  %>% select(c("id","age_over40", "sex_female", "occupation_Full_time_employee", "income_over_50M", 
                                          "region_more_than_500k", "Taxi_frequency_more_than_several_time_a_year", 
                                          "Taxi_aim_laisure", "RS_experience_yes"))  

# drop duplicate for id
rs_data_blc  <- rs_data_blc[!duplicated(rs_data_blc $id), ]
taxi_data_blc  <- taxi_data_blc[!duplicated(taxi_data_blc$id), ]

#pasting the group name 
rs_data_blc$group <- "RS"
taxi_data_blc$group <- "Taxi"

#bind  two groups data
all_data_blc <- rbind(rs_data_blc,taxi_data_blc)

#setting group variable as a factor
all_data_blc$group <- as.factor(all_data_blc$group)

#change categorical variables to numeric
#setting changing columns
cols_to_change <- c("age_over40", "sex_female", "occupation_Full_time_employee", 
                    "income_over_50M", "region_more_than_500k", 
                    "Taxi_frequency_more_than_several_time_a_year", "Taxi_aim_laisure", 
                    "RS_experience_yes")

#changing
all_data_blc[cols_to_change] <- lapply(all_data_blc[cols_to_change], as.numeric)

#balacance check
all_data_blc_2 <- BalanceR(data = all_data_blc, group = group ,
                   cov  = c(Age_more_than_40S = age_over40, 
                            Sex_Female = sex_female, 
                            Full_time_employee = occupation_Full_time_employee, 
                            Income_more_than_50M_JPY = income_over_50M,
                            Population_for_living_region_more_than_500k = region_more_than_500k,
                            Taxi_frequency_more_than_several_time_a_year = Taxi_frequency_more_than_several_time_a_year,
                            Taxi_purpose_leisure = Taxi_aim_laisure,
                            RS_experience_Yes = RS_experience_yes))


#changing BalanceR obeject to dataframe
all_data_blc_3 <- data_frame(
  Covariate = all_data_blc_2$SB$Covariate,
  StandardizedBiases = abs(all_data_blc_2$SB$`SB:RS-Taxi`)
)


#setting the covariates name for visualization
formatted_covariates <- c("Age: More Than 40S",
                          "Sex: Female",
                          "Occupation: Full Time Employee",
                          "Income: More Than 50M JPY",
                          "Population for Living Region: More Than 500k",
                          "Taxi Frequency: More Than Several Time a Year",
                          "Taxi Purpose: Leisure",
                          "RS Experience: Yes")

#formatting
all_data_blc_3$Covariate <- formatted_covariates

#changing covariates columns to factor
all_data_blc_3$Covariate <- factor(all_data_blc_3$Covariate,
                              levels = rev(c("Age: More Than 40S",
                                             "Sex: Female",
                                             "Occupation: Full Time Employee",
                                             "Income: More Than 50M JPY",
                                             "Population for Living Region: More Than 500k",
                                             "Taxi Frequency: More Than Several Time a Year",
                                             "Taxi Purpose: Leisure",
                                             "RS Experience: Yes")))


#visualization
fig_3 <- all_data_blc_3　%>% 
  ggplot(aes(x = StandardizedBiases, y = Covariate)) +
  geom_point() +
  geom_vline(xintercept = 0,color = "black") +
  geom_vline(xintercept = 10, linetype="dashed", color = "black") +
  scale_x_continuous(limits = c(0, 10))+
  theme_minimal() +
  labs(title = "Checking the balance between the RS group and the Taxi group", x = "Standardized Biases (Absolute)", y = "Covariate")+
  theme(
    plot.title = element_text(size = 17),  
    axis.title.x = element_text(size = 17), 
    axis.title.y = element_text(size = 17), 
    legend.position = "bottom",
    legend.text = element_text(size = 17),  
    axis.text.x = element_text(face = "bold", size = 17), 
    axis.text.y = element_text(face = "bold", size = 17)  
  ) 

plot(fig_3)


# Comprehensive results ---------------------------------------------------
# RS Group ------------------------------------------------------------------
#create model
base_logit_1_rs <- glm(choice ~ Price + Safety + Dispatch + License + Time + Service+ age_over40 + sex_female + occupation_Full_time_employee + income_over_50M + region_more_than_500k
                       + Taxi_frequency_more_than_several_time_a_year + Taxi_aim_laisure + RS_experience_yes,data = rs_data_1,family = binomial)
#calculate CR standard error
cr_se_base_logit_1_rs <- sqrt(diag(vcovCL(base_logit_1_rs, cluster = rs_data_1$id)))[1:7]

# predict probability
predicted_probs_base_logit_1_rs  <- predict(base_logit_1_rs, type = "response")

# calculate marginal effects
coef_me_base_logit_1_rs <- c(rep(0,7))
se_coef_me_base_logit_1_rs <- c(rep(0,7))
me_base_logit_1_rs <- matrix(0, nrow(rs_data_1), length(coef(base_logit_1_rs)[1:7]))
se_me_base_logit_1_rs <- matrix(0, nrow(rs_data_1), length(coef(base_logit_1_rs)[1:7]))
for (i in 1:length(coef(base_logit_1_rs)[1:7])) {
  me_base_logit_1_rs[,i] <- coef(base_logit_1_rs)[i] * predicted_probs_base_logit_1_rs * (1 - predicted_probs_base_logit_1_rs)
  se_me_base_logit_1_rs[,i] <- cr_se_base_logit_1_rs[i]* predicted_probs_base_logit_1_rs * (1 - predicted_probs_base_logit_1_rs)
  coef_me_base_logit_1_rs[i] <- mean(me_base_logit_1_rs[,i])
  se_coef_me_base_logit_1_rs[i] <- mean(se_me_base_logit_1_rs[,i])
}
#results
cbind(names(base_logit_1_rs$coefficients)[1:7],coef_me_base_logit_1_rs,se_coef_me_base_logit_1_rs)


# Taxi Group --------------------------------------------------------------------
#create model
base_logit_1_taxi <- glm(choice ~ Price + Safety + Dispatch + License + Time + Service+ age_over40 + sex_female + occupation_Full_time_employee + income_over_50M + region_more_than_500k
                         + Taxi_frequency_more_than_several_time_a_year + Taxi_aim_laisure + RS_experience_yes, data = taxi_data_1,family = binomial)

#calculate CR standard error
cr_se_base_logit_1_taxi <- sqrt(diag(vcovCL(base_logit_1_taxi, cluster = taxi_data_1$id)))[1:7]

# predict probability
predicted_probs_base_logit_1_taxi  <- predict(base_logit_1_taxi, type = "response")

# calculate marginal effects
coef_me_base_logit_1_taxi <- c(rep(0,7))
se_coef_me_base_logit_1_taxi <- c(rep(0,7))
me_base_logit_1_taxi <- matrix(0, nrow(taxi_data_1), length(coef(base_logit_1_taxi)[1:7]))
se_me_base_logit_1_taxi <- matrix(0, nrow(taxi_data_1), length(coef(base_logit_1_taxi)[1:7]))
for (i in 1:length(coef(base_logit_1_taxi)[1:7])) {
  me_base_logit_1_taxi[,i] <- coef(base_logit_1_taxi)[i] * predicted_probs_base_logit_1_taxi * (1 - predicted_probs_base_logit_1_taxi)
  se_me_base_logit_1_taxi[,i] <- cr_se_base_logit_1_taxi[i]* predicted_probs_base_logit_1_taxi * (1 - predicted_probs_base_logit_1_taxi)
  coef_me_base_logit_1_taxi[i] <- mean(me_base_logit_1_taxi[,i])
  se_coef_me_base_logit_1_taxi[i] <- mean(se_me_base_logit_1_taxi[,i])
}
#results
cbind(coef_me_base_logit_1_taxi,se_coef_me_base_logit_1_taxi)


# Visualization ------------------------------------------------------------------
#aggregate results
#rs group
result_rs <- data.frame(Variable =  rev(c("ServiceYes(vs No)",  "Time7min(vs 10min)", "C2LicenseYes(vs NO)","DispatchApp (vs PhoneCall)", "Accident1% (vs 3%)", "Price1200 (vs1500)")),
                        Coefficient = coef_me_base_logit_1_rs[2:7],
                        SE = se_coef_me_base_logit_1_rs[2:7])

#taxi group
result_taxi <- data.frame(Variable = rev(c("ServiceYes(vs No)",  "Time7min(vs 10min)", "C2LicenseYes(vs NO)","DispatchApp (vs PhoneCall)", "Accident1% (vs 3%)", "Price1200 (vs1500)")),
                          Coefficient = coef_me_base_logit_1_taxi[2:7],
                          SE = se_coef_me_base_logit_1_taxi[2:7])


# calculate 95%CI
#rs group
result_rs$CI_low <- result$Coefficient - 1.96 * result_rs$SE
result_rs$CI_high <- result$Coefficient + 1.96 * result_rs$SE

#taxi group
result_taxi$CI_low <- result_taxi$Coefficient - 1.96 * result_taxi$SE
result_taxi$CI_high <- result_taxi$Coefficient + 1.96 * result_taxi$SE


# create labels
result_rs$Label <- paste0(round(result_rs$Coefficient, 3), " (", round(result_rs$CI_low, 3), ", ", round(result_rs$CI_high, 3), ")")
result_taxi$Label <- paste0(round(result_taxi$Coefficient, 3), " (", round(result_taxi$CI_low, 3), ", ", round(result_taxi$CI_high, 3), ")")

# set group names
result_rs$Group <- "RS"
result_taxi$Group <- "Taxi"

# combine results
combined_results <- rbind(result_rs, result_taxi)

# sort a variables
combined_results$Variable <- factor(combined_results$Variable, levels = c("ServiceYes(vs No)",  "Time7min(vs 10min)", "C2LicenseYes(vs NO)","DispatchApp (vs PhoneCall)", "Accident1% (vs 3%)", "Price1200 (vs1500)"))

# visualization
fig_4 <- combined_results %>% 
  ggplot(aes(x = Variable, y = Coefficient, color = Group)) +
  geom_point(position = position_dodge(-0.5)) +  
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.2, position = position_dodge(-0.5)) +  
  geom_text(aes(label = Label), vjust = -0.5, hjust = 0.5, size = 3, position = position_dodge(-0.5)) +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(x = "Attributions", y = "Coefficient (95%CI)", title = "The Impact of Changes in Attribution Levels on the Probability of Service Choice") +
  theme_minimal() +
  scale_color_manual(values = c("RS" = "blue", "Taxi" = "red")) +  
  theme(
    plot.title = element_text(size = 20),  
    axis.title.x = element_text(size = 17), 
    axis.title.y = element_text(size = 17), 
    legend.position = "bottom",
    legend.text = element_text(size = 17),  
    axis.text.x = element_text(face = "bold", size = 17), 
    axis.text.y = element_text(face = "bold", size = 17)  
  ) 

plot(fig_4)

# Sub Group-Living Region -------------------------------------------------
# rs group metropolitan ---------------------------------------------------
#create dataset
rs_data_metro <- rs_data_1 %>% filter(region != "Prefer not to answer" & region_more_than_500k == 1)
# create model
base_logit_1_rs_metro <- glm(choice ~ Price + Safety + Dispatch + License + Time + Service + age_over40 + occupation_Full_time_employee + income_over_50M + sex_female
                             + Taxi_frequency_more_than_several_time_a_year + Taxi_aim_laisure, data = rs_data_metro, family = binomial)

#calculate CR standard error
cr_se_base_logit_1_rs_metro <- sqrt(diag(vcovCL(base_logit_1_rs_metro, cluster = rs_data_metro$id)))[1:7]

# predict probability
predicted_probs_base_logit_1_rs_metro  <- predict(base_logit_1_rs_metro, type = "response")


# calculate marginal effect
coef_me_base_logit_1_rs_metro <- c(rep(0,7))
se_coef_me_base_logit_1_rs_metro <- c(rep(0,7))
me_base_logit_1_rs_metro <- matrix(0, nrow(rs_data_metro), length(coef(base_logit_1_rs_metro)[1:7]))
se_me_base_logit_1_rs_metro <- matrix(0, nrow(rs_data_metro), length(coef(base_logit_1_rs_metro)[1:7]))

for (i in 1:length(coef(base_logit_1_rs_metro)[1:7])) {
  me_base_logit_1_rs_metro[,i] <- coef(base_logit_1_rs_metro)[i] * predicted_probs_base_logit_1_rs_metro * (1 - predicted_probs_base_logit_1_rs_metro)
  se_me_base_logit_1_rs_metro[,i] <- cr_se_base_logit_1_rs_metro[i]* predicted_probs_base_logit_1_rs_metro * (1 - predicted_probs_base_logit_1_rs_metro)
  coef_me_base_logit_1_rs_metro[i] <- mean(me_base_logit_1_rs_metro[,i])
  se_coef_me_base_logit_1_rs_metro[i] <- mean(se_me_base_logit_1_rs_metro[,i])
}

#results
cbind(coef_me_base_logit_1_rs_metro,se_coef_me_base_logit_1_rs_metro)

# rs group other ---------------------------------------------------
rs_data_non_metro <- rs_data_1 %>% filter(region != "Prefer not to answer" & region_more_than_500k == 0)

# create model
base_logit_1_rs_non_metro <- glm(choice ~ Price + Safety + Dispatch + License + Time + Service + age_over40 + occupation_Full_time_employee + income_over_50M + sex_female
                                 + Taxi_frequency_more_than_several_time_a_year + Taxi_aim_laisure, data = rs_data_non_metro , family = binomial)

# calculate CR standard error
cr_se_base_logit_1_rs_non_metro <- sqrt(diag(vcovCL(base_logit_1_rs_non_metro, cluster = rs_data_non_metro$id)))[1:7]

# predict probability
predicted_probs_base_logit_1_rs_non_metro  <- predict(base_logit_1_rs_non_metro, type = "response")

# calculate marginal effect
coef_me_base_logit_1_rs_non_metro <- c(rep(0,7))
se_coef_me_base_logit_1_rs_non_metro <- c(rep(0,7))
me_base_logit_1_rs_non_metro <- matrix(0, nrow(rs_data_non_metro), length(coef(base_logit_1_rs_non_metro)[1:7]))
se_me_base_logit_1_rs_non_metro <- matrix(0, nrow(rs_data_non_metro), length(coef(base_logit_1_rs_non_metro)[1:7]))

for (i in 1:length(coef(base_logit_1_rs_non_metro)[1:7])) {
  me_base_logit_1_rs_non_metro[,i] <- coef(base_logit_1_rs_non_metro)[i] * predicted_probs_base_logit_1_rs_non_metro * (1 - predicted_probs_base_logit_1_rs_non_metro)
  se_me_base_logit_1_rs_non_metro[,i] <- cr_se_base_logit_1_rs_non_metro[i]* predicted_probs_base_logit_1_rs_non_metro * (1 - predicted_probs_base_logit_1_rs_non_metro)
  coef_me_base_logit_1_rs_non_metro[i] <- mean(me_base_logit_1_rs_non_metro[,i])
  se_coef_me_base_logit_1_rs_non_metro[i] <- mean(se_me_base_logit_1_rs_non_metro[,i])
}

#result
cbind(coef_me_base_logit_1_rs_non_metro,se_coef_me_base_logit_1_rs_non_metro)

# taxi group metropolitan --------------------------------------------------------------------
#create dataset
taxi_data_metro <- taxi_data_1 %>% filter(region != "Prefer not to answer" & region_more_than_500k == 1)

# create model
base_logit_1_taxi_metro <- glm(choice ~ Price + Safety + Dispatch + License + Time + Service + age_over40 + occupation_Full_time_employee + income_over_50M + sex_female
                               + Taxi_frequency_more_than_several_time_a_year + Taxi_aim_laisure, data = taxi_data_metro, family = binomial)

# calcurate CR standard error
cr_se_base_logit_1_taxi_metro <- sqrt(diag(vcovCL(base_logit_1_taxi_metro, cluster = taxi_data_metro$id)))[1:7]

# predict probability
predicted_probs_base_logit_1_taxi_metro  <- predict(base_logit_1_taxi_metro, type = "response")


# calculate marginal effect
coef_me_base_logit_1_taxi_metro <- c(rep(0,7))
se_coef_me_base_logit_1_taxi_metro <- c(rep(0,7))
me_base_logit_1_taxi_metro <- matrix(0, nrow(taxi_data_metro ), length(coef(base_logit_1_taxi_metro)[1:7]))
se_me_base_logit_1_taxi_metro <- matrix(0, nrow(taxi_data_metro ), length(coef(base_logit_1_taxi_metro)[1:7]))

for (i in 1:length(coef(base_logit_1_taxi_metro)[1:7])) {
  me_base_logit_1_taxi_metro[,i] <- coef(base_logit_1_taxi_metro)[i] * predicted_probs_base_logit_1_taxi_metro * (1 - predicted_probs_base_logit_1_taxi_metro)
  se_me_base_logit_1_taxi_metro[,i] <- cr_se_base_logit_1_taxi_metro[i]* predicted_probs_base_logit_1_taxi_metro * (1 - predicted_probs_base_logit_1_taxi_metro)
  coef_me_base_logit_1_taxi_metro[i] <- mean(me_base_logit_1_taxi_metro[,i])
  se_coef_me_base_logit_1_taxi_metro[i] <- mean(se_me_base_logit_1_taxi_metro[,i])
}

#result
cbind(coef_me_base_logit_1_taxi_metro,se_coef_me_base_logit_1_taxi_metro)

# rs group other ---------------------------------------------------
#create dataset
taxi_data_non_metro <- taxi_data_1  %>% filter(region != "Prefer not to answer" & region_more_than_500k == 0)

# create a model
base_logit_1_taxi_non_metro <- glm(choice ~ Price + Safety + Dispatch + License + Time + Service + age_over40 + occupation_Full_time_employee + income_over_50M + sex_female
                                   + Taxi_frequency_more_than_several_time_a_year + Taxi_aim_laisure, data = taxi_data_non_metro, family = binomial)

# calculate CR standard error
cr_se_base_logit_1_taxi_non_metro <- sqrt(diag(vcovCL(base_logit_1_taxi_non_metro, cluster = taxi_data_non_metro$id)))[1:7]

# predict probability
predicted_probs_base_logit_1_taxi_non_metro  <- predict(base_logit_1_taxi_non_metro, type = "response")


# calculate marginal effects
coef_me_base_logit_1_taxi_non_metro <- c(rep(0,7))
se_coef_me_base_logit_1_taxi_non_metro <- c(rep(0,7))
me_base_logit_1_taxi_non_metro <- matrix(0, nrow(taxi_data_non_metro), length(coef(base_logit_1_taxi_non_metro)[1:7]))
se_me_base_logit_1_taxi_non_metro <- matrix(0, nrow(taxi_data_non_metro), length(coef(base_logit_1_taxi_non_metro)[1:7]))

for (i in 1:length(coef(base_logit_1_taxi_non_metro)[1:7])) {
  me_base_logit_1_taxi_non_metro[,i] <- coef(base_logit_1_taxi_non_metro)[i] * predicted_probs_base_logit_1_taxi_non_metro * (1 - predicted_probs_base_logit_1_taxi_non_metro)
  se_me_base_logit_1_taxi_non_metro[,i] <- cr_se_base_logit_1_taxi_non_metro[i]* predicted_probs_base_logit_1_taxi_non_metro * (1 - predicted_probs_base_logit_1_taxi_non_metro)
  coef_me_base_logit_1_taxi_non_metro[i] <- mean(me_base_logit_1_taxi_non_metro[,i])
  se_coef_me_base_logit_1_taxi_non_metro[i] <- mean(se_me_base_logit_1_taxi_non_metro[,i])
}

#結果
cbind(coef_me_base_logit_1_taxi_non_metro,se_coef_me_base_logit_1_taxi_non_metro)


# Visualization ----------------------------------------------------------------
# aggregate results
#rs metro
result_metro_rs <- data.frame(Variable = rev(c("ServiceYes(vs No)",  "Time7min(vs 10min)", "C2LicenseYes(vs NO)","DispatchApp (vs PhoneCall)", "Accident1% (vs 3%)", "Price1200 (vs1500)")),
                              Coefficient = coef_me_base_logit_1_rs_metro[2:7],
                              SE = se_coef_me_base_logit_1_rs_metro[2:7])

result_metro_rs$CI_low <- result_metro_rs$Coefficient - 1.96 * result_metro_rs$SE
result_metro_rs$CI_high <- result_metro_rs$Coefficient + 1.96 * result_metro_rs$SE
result_metro_rs$Label <- paste0(round(result_metro_rs$Coefficient, 3), " (", round(result_metro_rs$CI_low, 3), ", ", round(result_metro_rs$CI_high, 3), ")")

#rs other
result_non_metro_rs <- data.frame(Variable = rev(c("ServiceYes(vs No)",  "Time7min(vs 10min)", "C2LicenseYes(vs NO)","DispatchApp (vs PhoneCall)", "Accident1% (vs 3%)", "Price1200 (vs1500)")),
                                  Coefficient = coef_me_base_logit_1_rs_non_metro[2:7],
                                  SE = se_coef_me_base_logit_1_rs_non_metro[2:7])

result_non_metro_rs$CI_low <- result_non_metro_rs$Coefficient - 1.96 * result_non_metro_rs$SE
result_non_metro_rs$CI_high <- result_non_metro_rs$Coefficient + 1.96 * result_non_metro_rs$SE
result_non_metro_rs$Label <- paste0(round(result_non_metro_rs$Coefficient, 3), " (", round(result_non_metro_rs$CI_low, 3), ", ", round(result_non_metro_rs$CI_high, 3), ")")

# taxi metro
result_metro_taxi <- data.frame(Variable = rev(c("ServiceYes(vs No)",  "Time7min(vs 10min)", "C2LicenseYes(vs NO)","DispatchApp (vs PhoneCall)", "Accident1% (vs 3%)", "Price1200 (vs1500)")),
                                Coefficient = coef_me_base_logit_1_taxi_metro[2:7],
                                SE = se_coef_me_base_logit_1_taxi_metro[2:7])

result_metro_taxi$CI_low <- result_metro_taxi$Coefficient - 1.96 * result_metro_taxi$SE
result_metro_taxi$CI_high <- result_metro_taxi$Coefficient + 1.96 * result_metro_taxi$SE
result_metro_taxi$Label <- paste0(round(result_metro_taxi$Coefficient, 3), " (", round(result_metro_taxi$CI_low, 3), ", ", round(result_metro_taxi$CI_high, 3), ")")

#taxi other
result_non_metro_taxi <- data.frame(Variable = rev(c("ServiceYes(vs No)",  "Time7min(vs 10min)", "C2LicenseYes(vs NO)","DispatchApp (vs PhoneCall)", "Accident1% (vs 3%)", "Price1200 (vs1500)")),
                                    Coefficient = coef_me_base_logit_1_taxi_non_metro[2:7],
                                    SE = se_coef_me_base_logit_1_taxi_non_metro[2:7])

result_non_metro_taxi$CI_low <- result_non_metro_taxi$Coefficient - 1.96 * result_non_metro_taxi$SE
result_non_metro_taxi$CI_high <- result_non_metro_taxi$Coefficient + 1.96 * result_non_metro_taxi$SE
result_non_metro_taxi$Label <- paste0(round(result_non_metro_taxi$Coefficient, 3), " (", round(result_non_metro_taxi$CI_low, 3), ", ", round(result_non_metro_taxi$CI_high, 3), ")")


# set a group name
result_metro_rs$Group <- "RS,Region more than 500k"
result_non_metro_rs$Group <- "RS,Other"
result_metro_taxi$Group <- "Taxi,Region more than 500k"
result_non_metro_taxi$Group <- "Taxi,Other"

# combined results
combined_results_region <- rbind(result_metro_rs, result_non_metro_rs, result_metro_taxi, result_non_metro_taxi)


#sort a group
combined_results_region$Group <- factor(combined_results_region$Group, 
                                        levels = c( "Taxi,Other","Taxi,Region more than 500k","RS,Other","RS,Region more than 500k"))

#sort a variables
combined_results_region$Variable <- factor(combined_results_region$Variable, levels = c("ServiceYes(vs No)",  "Time7min(vs 10min)", "C2LicenseYes(vs NO)","DispatchApp (vs PhoneCall)", "Accident1% (vs 3%)", "Price1200 (vs1500)"))

#reset a group
combined_results_region$Groups <- ifelse(grepl("RS", combined_results_region$Group), "RS", "Taxi")

# visualization
fig_5 <- ggplot(combined_results_region, aes(x = Variable, y = Coefficient, color = Group)) +
  geom_point(position = position_dodge(0.9)) +  
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.2, position = position_dodge(0.9)) + 
  geom_text(aes(label = Label), vjust = -1.2, hjust = 0.5, size = 3, position = position_dodge(0.9)) +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(x = "Attributions", y = "Coefficient",title = "The Impact of Changes in Attribution Levels on the Probability of Service Choice, \nConditioned on Living Region") +
  theme_minimal() +
  scale_color_manual(values = c("RS,Region more than 500k"  = "blue", "RS,Other" = "darkblue", "Taxi,Region more than 500k" = "red", "Taxi,Other" = "darkred"),
                     breaks = c("RS,Region more than 500k" , "RS,Other", "Taxi,Region more than 500k" , "Taxi,Other"))+  
  facet_grid(. ~ Groups, scales = "free_x", space = "free_x")+
  theme(
    plot.title = element_text(size = 17),  
    axis.title.x = element_text(size = 17), 
    axis.title.y = element_text(size = 17),  
    legend.position = "bottom",
    legend.text = element_text(size = 17),
    axis.text.x = element_text(face = "bold", size = 17),  
    axis.text.y = element_text(face = "bold", size = 17) 
  ) 

plot(fig_5)
# Sub Group-Income -------------------------------------------------
# rs group high-income ---------------------------------------------------
#create dataset
rs_data_high_income <- rs_data_1 %>% filter(income != "Prefer not to answer" & income_over_50M == 1) 

# create model
base_logit_1_rs_high_income <- glm(choice ~ Price + Safety + Dispatch + License + Time + Service + age_over40 + occupation_Full_time_employee + sex_female + region_more_than_500k
                                   + Taxi_frequency_more_than_several_time_a_year + Taxi_aim_laisure, data = rs_data_high_income , family = binomial)

# calculate CR standard error
cr_se_base_logit_1_rs_high_income <- sqrt(diag(vcovCL(base_logit_1_rs_high_income, cluster = rs_data_high_income $id)))[1:7]

# predict probability
predicted_probs_base_logit_1_rs_high_income  <- predict(base_logit_1_rs_high_income, type = "response")

# calculate marginal effects
coef_me_base_logit_1_rs_high_income <- c(rep(0,7))
se_coef_me_base_logit_1_rs_high_income <- c(rep(0,7))
me_base_logit_1_rs_high_income <- matrix(0, nrow(rs_data_high_income), length(coef(base_logit_1_rs_high_income)[1:7]))
se_me_base_logit_1_rs_high_income <- matrix(0, nrow(rs_data_high_income), length(coef(base_logit_1_rs_high_income)[1:7]))

for (i in 1:length(coef(base_logit_1_rs_high_income)[1:7])) {
  me_base_logit_1_rs_high_income[,i] <- coef(base_logit_1_rs_high_income)[i] * predicted_probs_base_logit_1_rs_high_income * (1 - predicted_probs_base_logit_1_rs_high_income)
  se_me_base_logit_1_rs_high_income[,i] <- cr_se_base_logit_1_rs_high_income[i]* predicted_probs_base_logit_1_rs_high_income * (1 - predicted_probs_base_logit_1_rs_high_income)
  coef_me_base_logit_1_rs_high_income[i] <- mean(me_base_logit_1_rs_high_income[,i])
  se_coef_me_base_logit_1_rs_high_income[i] <- mean(se_me_base_logit_1_rs_high_income[,i])
}

#result
cbind(coef_me_base_logit_1_rs_high_income,se_coef_me_base_logit_1_rs_high_income)

# rs group other ---------------------------------------------------
#create dataset
rs_data_low_income <- rs_data_1 %>% filter(income != "Prefer not to answer" & income_over_50M == 0) 

# create model
base_logit_1_rs_low_income <- glm(choice ~ Price + Safety + Dispatch + License + Time + Service + age_over40 + occupation_Full_time_employee + sex_female + region_more_than_500k
                                  + Taxi_frequency_more_than_several_time_a_year + Taxi_aim_laisure, data = rs_data_low_income , family = binomial)

# calculate CR standard error
cr_se_base_logit_1_rs_low_income <- sqrt(diag(vcovCL(base_logit_1_rs_low_income, cluster = rs_data_low_income$id)))[1:7]

# predict probability
predicted_probs_base_logit_1_rs_low_income  <- predict(base_logit_1_rs_low_income, type = "response")


# calculate marginal effects
coef_me_base_logit_1_rs_low_income <- c(rep(0,7))
se_coef_me_base_logit_1_rs_low_income <- c(rep(0,7))
me_base_logit_1_rs_low_income <- matrix(0, nrow(rs_data_low_income), length(coef(base_logit_1_rs_low_income)[1:7]))
se_me_base_logit_1_rs_low_income <- matrix(0, nrow(rs_data_low_income), length(coef(base_logit_1_rs_low_income)[1:7]))

for (i in 1:length(coef(base_logit_1_rs_low_income)[1:7])) {
  me_base_logit_1_rs_low_income[,i] <- coef(base_logit_1_rs_low_income)[i] * predicted_probs_base_logit_1_rs_low_income * (1 - predicted_probs_base_logit_1_rs_low_income)
  se_me_base_logit_1_rs_low_income[,i] <- cr_se_base_logit_1_rs_low_income[i]* predicted_probs_base_logit_1_rs_low_income * (1 - predicted_probs_base_logit_1_rs_low_income)
  coef_me_base_logit_1_rs_low_income[i] <- mean(me_base_logit_1_rs_low_income[,i])
  se_coef_me_base_logit_1_rs_low_income[i] <- mean(se_me_base_logit_1_rs_low_income[,i])
}

#result
cbind(coef_me_base_logit_1_rs_low_income,se_coef_me_base_logit_1_rs_low_income)


# taxi group high-income --------------------------------------------------
#create dataset
taxi_data_high_income <- taxi_data_1 %>% filter(income != "Prefer not to answer" & income_over_50M == 1) 

#create model
base_logit_1_taxi_high_income <- glm(choice ~ Price + Safety + Dispatch + License + Time + Service + age_over40 + occupation_Full_time_employee + sex_female + region_more_than_500k
                                     + Taxi_frequency_more_than_several_time_a_year + Taxi_aim_laisure, data = taxi_data_high_income , family = binomial)

#calculate CR standard error
cr_se_base_logit_1_taxi_high_income <- sqrt(diag(vcovCL(base_logit_1_taxi_high_income, cluster = taxi_data_high_income$id)))[1:7]

# predict probability
predicted_probs_base_logit_1_taxi_high_income  <- predict(base_logit_1_taxi_high_income, type = "response")

# calculate marginal effects
coef_me_base_logit_1_taxi_high_income <- c(rep(0,7))
se_coef_me_base_logit_1_taxi_high_income <- c(rep(0,7))
me_base_logit_1_taxi_high_income <- matrix(0, nrow(taxi_data_high_income), length(coef(base_logit_1_taxi_high_income)[1:7]))
se_me_base_logit_1_taxi_high_income <- matrix(0, nrow(taxi_data_high_income), length(coef(base_logit_1_taxi_high_income)[1:7]))

for (i in 1:length(coef(base_logit_1_taxi_high_income)[1:7])) {
  me_base_logit_1_taxi_high_income[,i] <- coef(base_logit_1_taxi_high_income)[i] * predicted_probs_base_logit_1_taxi_high_income * (1 - predicted_probs_base_logit_1_taxi_high_income)
  se_me_base_logit_1_taxi_high_income[,i] <- cr_se_base_logit_1_taxi_high_income[i]* predicted_probs_base_logit_1_taxi_high_income * (1 - predicted_probs_base_logit_1_taxi_high_income)
  coef_me_base_logit_1_taxi_high_income[i] <- mean(me_base_logit_1_taxi_high_income[,i])
  se_coef_me_base_logit_1_taxi_high_income[i] <- mean(se_me_base_logit_1_taxi_high_income[,i])
}

#結果
cbind(coef_me_base_logit_1_taxi_high_income,se_coef_me_base_logit_1_taxi_high_income)

# taxi group other --------------------------------------------------
#create dataset
taxi_data_low_income <- taxi_data_1 %>% filter(income != "Prefer not to answer" & income_over_50M == 0) 

# create model
base_logit_1_taxi_low_income <- glm(choice ~ Price + Safety + Dispatch + License + Time + Service + age_over40 + occupation_Full_time_employee + sex_female + region_more_than_500k
                                    + Taxi_frequency_more_than_several_time_a_year + Taxi_aim_laisure, data = taxi_data_low_income, family = binomial)

# calculate CR standard error
cr_se_base_logit_1_taxi_low_income <- sqrt(diag(vcovCL(base_logit_1_taxi_low_income, cluster = taxi_data_low_income$id)))[1:7]

# predict probability
predicted_probs_base_logit_1_taxi_low_income  <- predict(base_logit_1_taxi_low_income, type = "response")


# calculate marginal effects
coef_me_base_logit_1_taxi_low_income <- c(rep(0,7))
se_coef_me_base_logit_1_taxi_low_income <- c(rep(0,7))
me_base_logit_1_taxi_low_income <- matrix(0, nrow(taxi_data_low_income), length(coef(base_logit_1_taxi_low_income)[1:7]))
se_me_base_logit_1_taxi_low_income <- matrix(0, nrow(taxi_data_low_income), length(coef(base_logit_1_taxi_low_income)[1:7]))

for (i in 1:length(coef(base_logit_1_taxi_low_income)[1:7])) {
  me_base_logit_1_taxi_low_income[,i] <- coef(base_logit_1_taxi_low_income)[i] * predicted_probs_base_logit_1_taxi_low_income * (1 - predicted_probs_base_logit_1_taxi_low_income)
  se_me_base_logit_1_taxi_low_income[,i] <- cr_se_base_logit_1_taxi_low_income[i]* predicted_probs_base_logit_1_taxi_low_income * (1 - predicted_probs_base_logit_1_taxi_low_income)
  coef_me_base_logit_1_taxi_low_income[i] <- mean(me_base_logit_1_taxi_low_income[,i])
  se_coef_me_base_logit_1_taxi_low_income[i] <- mean(se_me_base_logit_1_taxi_low_income[,i])
}

#result 
cbind(coef_me_base_logit_1_taxi_low_income,se_coef_me_base_logit_1_taxi_low_income)


# Visualization -----------------------------------------------------------
# aggregate results
#rs group high-income
result_high_income_rs <- data.frame(Variable = rev(c("ServiceYes(vs No)",  "Time7min(vs 10min)", "C2LicenseYes(vs NO)","DispatchApp (vs PhoneCall)", "Accident1% (vs 3%)", "Price1200 (vs1500)")),
                                    Coefficient = coef_me_base_logit_1_rs_high_income[2:7],
                                    SE = se_coef_me_base_logit_1_rs_high_income[2:7])

#rs group other
result_low_income_rs <- data.frame(Variable = rev(c("ServiceYes(vs No)",  "Time7min(vs 10min)", "C2LicenseYes(vs NO)","DispatchApp (vs PhoneCall)", "Accident1% (vs 3%)", "Price1200 (vs1500)")),
                                   Coefficient = coef_me_base_logit_1_rs_low_income[2:7],
                                   SE = se_coef_me_base_logit_1_rs_low_income[2:7])


# taxi group high income
result_high_income_taxi <- data.frame(Variable = rev(c("ServiceYes(vs No)",  "Time7min(vs 10min)", "C2LicenseYes(vs NO)","DispatchApp (vs PhoneCall)", "Accident1% (vs 3%)", "Price1200 (vs1500)")),
                                      Coefficient = coef_me_base_logit_1_taxi_high_income[2:7],
                                      SE = se_coef_me_base_logit_1_taxi_high_income[2:7])

# taxi group other
result_low_income_taxi <- data.frame(Variable = rev(c("ServiceYes(vs No)",  "Time7min(vs 10min)", "C2LicenseYes(vs NO)","DispatchApp (vs PhoneCall)", "Accident1% (vs 3%)", "Price1200 (vs1500)")),
                                     Coefficient = coef_me_base_logit_1_taxi_low_income[2:7],
                                     SE = se_coef_me_base_logit_1_taxi_low_income[2:7])

result_low_income_taxi$CI_low <- result_low_income_taxi$Coefficient - 1.96 * result_low_income_taxi$SE
result_low_income_taxi$CI_high <- result_low_income_taxi$Coefficient + 1.96 * result_low_income_taxi$SE
result_low_income_taxi$Label <- paste0(round(result_low_income_taxi$Coefficient, 3), " (", round(result_low_income_taxi$CI_low, 3), ", ", round(result_low_income_taxi$CI_high, 3), ")")


#calculate 95% CI and
#rs group high-income
result_high_income_rs$CI_low <- result_high_income_rs$Coefficient - 1.96 * result_high_income_rs$SE
result_high_income_rs$CI_high <- result_high_income_rs$Coefficient + 1.96 * result_high_income_rs$SE
result_high_income_rs$Label <- paste0(round(result_high_income_rs$Coefficient, 3), " (", round(result_high_income_rs$CI_low, 3), ", ", round(result_high_income_rs$CI_high, 3), ")")

#rs group other
result_low_income_rs$CI_low <- result_low_income_rs$Coefficient - 1.96 * result_low_income_rs$SE
result_low_income_rs$CI_high <- result_low_income_rs$Coefficient + 1.96 * result_low_income_rs$SE
result_low_income_rs$Label <- paste0(round(result_low_income_rs$Coefficient, 3), " (", round(result_low_income_rs$CI_low, 3), ", ", round(result_low_income_rs$CI_high, 3), ")")

#taxi  group high-income
result_high_income_taxi$CI_low <- result_high_income_taxi$Coefficient - 1.96 * result_high_income_taxi$SE
result_high_income_taxi$CI_high <- result_high_income_taxi$Coefficient + 1.96 * result_high_income_taxi$SE
result_high_income_taxi$Label <- paste0(round(result_high_income_taxi$Coefficient, 3), " (", round(result_high_income_taxi$CI_low, 3), ", ", round(result_high_income_taxi$CI_high, 3), ")")

#taxi  group hother
result_low_income_taxi$CI_low <- result_low_income_taxi$Coefficient - 1.96 * result_low_income_taxi$SE
result_low_income_taxi$CI_high <- result_low_income_taxi$Coefficient + 1.96 * result_low_income_taxi$SE
result_low_income_taxi$Label <- paste0(round(result_low_income_taxi$Coefficient, 3), " (", round(result_low_income_taxi$CI_low, 3), ", ", round(result_low_income_taxi$CI_high, 3), ")")

# set the group
result_high_income_rs$Group <- "RS, Income 5M or more JPY"
result_low_income_rs$Group <- "RS,Other"
result_high_income_taxi$Group <- "Taxi,Income 5M or more JPY"
result_low_income_taxi$Group <- "Taxi,Other"

#combine results
combined_results_income <- rbind(result_high_income_taxi, result_low_income_taxi, result_high_income_rs, result_low_income_rs)

#sort group
combined_results_income$Group <- factor(combined_results_income$Group, 
                                        levels = c("Taxi,Other", "Taxi,Income 5M or more JPY", "RS,Other", "RS, Income 5M or more JPY"))
#sort variables
combined_results_income$Variable <- factor(combined_results_income$Variable, levels = c("ServiceYes(vs No)",  "Time7min(vs 10min)", "C2LicenseYes(vs NO)","DispatchApp (vs PhoneCall)", "Accident1% (vs 3%)", "Price1200 (vs1500)"))

#reset group
combined_results_income$Groups <- ifelse(grepl("RS", combined_results_income$Group), "RS", "Taxi")

# visualization
fig_6 <- ggplot(combined_results_income, aes(x = Variable, y = Coefficient, color = Group)) +
  geom_point(position = position_dodge(0.9)) +  
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.2, position = position_dodge(0.9)) +  
  geom_text(aes(label = Label), vjust = -1.2, hjust = 0.5, size = 3, position = position_dodge(0.9)) +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(x = "Attributions", y = "Coefficient",title = "The Impact of Changes in Attribution Levels on the Probability of Service Choice, \nConditioned on Income") +
  theme_minimal() +
  scale_color_manual(values = c("RS, Income 5M or more JPY" = "blue", "RS,Other" = "darkblue", 
                                "Taxi,Income 5M or more JPY" = "red", "Taxi,Other" = "darkred"),
                     breaks = c("RS, Income 5M or more JPY" , "RS,Other", "Taxi,Income 5M or more JPY" , "Taxi,Other")) +  
  facet_grid(. ~ Groups, scales = "free_x", space = "free_x")+
  theme(
    plot.title = element_text(size = 20),  
    axis.title.x = element_text(size = 17),  
    axis.title.y = element_text(size = 17), 
    legend.position = "bottom",
    legend.text = element_text(size = 17),
    axis.text.x = element_text(face = "bold", size = 17),  
    axis.text.y = element_text(face = "bold", size = 17) 
  ) 

plot(fig_6)

# The Trade-Off Between Price and Safety -------------------------------------------------
# rs group high-price ------------------------------------------------------------------
# create dataset
rs_data_high_price <- subset(rs_data_1, Price == "¥1500")
#create model
base_logit_1_rs_high_price <- glm(choice ~ Safety + Dispatch + License + Time + Service + age_over40 + occupation_Full_time_employee + sex_female + region_more_than_500k
                                  + Taxi_frequency_more_than_several_time_a_year + Taxi_aim_laisure, data = rs_data_high_price, family = binomial)


#calculate CR standard error
cr_se_base_logit_1_rs_high_price <- sqrt(diag(vcovCL(base_logit_1_rs_high_price , cluster = rs_data_high_price$id)))[1:2]

# predict probability
predicted_probs_base_logit_1_rs_high_price <- predict(base_logit_1_rs_high_price, type = "response")

# calculate marginal effects
me_base_logit_1_rs_high_price<- matrix(0, nrow(rs_data_high_price), length(coef(base_logit_1_rs_high_price)[1:2]))
se_me_base_logit_1_rs_high_price <- matrix(0, nrow(rs_data_high_price), length(coef(base_logit_1_rs_high_price)[1:2]))
coef_me_base_logit_1_rs_high_price<- c(rep(0,2))
se_coef_me_base_logit_1_rs_high_price <- c(rep(0,2))

for (i in 1:length(coef(base_logit_1_rs_high_price )[1:2])) {
  me_base_logit_1_rs_high_price[,i] <- coef(base_logit_1_rs_high_price)[i] * predicted_probs_base_logit_1_rs_high_price * (1 - predicted_probs_base_logit_1_rs_high_price)
  se_me_base_logit_1_rs_high_price[,i] <- cr_se_base_logit_1_rs_high_price[i]* predicted_probs_base_logit_1_rs_high_price * (1 - predicted_probs_base_logit_1_rs_high_price)
  coef_me_base_logit_1_rs_high_price[i] <- mean(me_base_logit_1_rs_high_price[,i])
  se_coef_me_base_logit_1_rs_high_price[i] <- mean(se_me_base_logit_1_rs_high_price[,i])
}

#results
cbind(coef_me_base_logit_1_rs_high_price,se_coef_me_base_logit_1_rs_high_price)


# rs group low-price ------------------------------------------------------
#create dataset
rs_data_low_price <- subset(rs_data_1, Price == "¥1200")

# create model
base_logit_1_rs_low_price <- glm(choice ~ Safety + Dispatch + License + Time + Service + age_over40 + occupation_Full_time_employee + sex_female + region_more_than_500k
                                 + Taxi_frequency_more_than_several_time_a_year + Taxi_aim_laisure, data = rs_data_low_price, family = binomial)

#calculate CR standar error
cr_se_base_logit_1_rs_low_price <- sqrt(diag(vcovCL(base_logit_1_rs_low_price , cluster = rs_data_low_price $id)))[1:2]

# predict probability
predicted_probs_base_logit_1_rs_low_price <- predict(base_logit_1_rs_low_price, type = "response")

coef_me_base_logit_1_rs_low_price<- c(rep(0,2))
se_coef_me_base_logit_1_rs_low_price <- c(rep(0,2))

# calculate marginal effects
me_base_logit_1_rs_low_price<- matrix(0, nrow(rs_data_low_price), length(coef(base_logit_1_rs_low_price)[1:2]))
se_me_base_logit_1_rs_low_price <- matrix(0, nrow(rs_data_low_price), length(coef(base_logit_1_rs_low_price)[1:2]))

for (i in 1:length(coef(base_logit_1_rs_low_price )[1:2])) {
  me_base_logit_1_rs_low_price[,i] <- coef(base_logit_1_rs_low_price)[i] * predicted_probs_base_logit_1_rs_low_price * (1 - predicted_probs_base_logit_1_rs_low_price)
  se_me_base_logit_1_rs_low_price[,i] <- cr_se_base_logit_1_rs_low_price[i]* predicted_probs_base_logit_1_rs_low_price * (1 - predicted_probs_base_logit_1_rs_low_price)
  coef_me_base_logit_1_rs_low_price[i] <- mean(me_base_logit_1_rs_low_price[,i])
  se_coef_me_base_logit_1_rs_low_price[i] <- mean(se_me_base_logit_1_rs_low_price[,i])
}

#結果
cbind(coef_me_base_logit_1_rs_low_price,se_coef_me_base_logit_1_rs_low_price)

# taxi group high-price --------------------------------------------------------------------
# create dataset
taxi_data_high_price <- subset(taxi_data_1, Price == "¥1500")

# create model
base_logit_1_taxi_high_price <- glm(choice ~ Safety + Dispatch + License + Time + Service + age_over40 + occupation_Full_time_employee + sex_female + region_more_than_500k
                                    + Taxi_frequency_more_than_several_time_a_year + Taxi_aim_laisure, data = taxi_data_high_price , family = binomial)

# calculate CR standard error
cr_se_base_logit_1_taxi_high_price <- sqrt(diag(vcovCL(base_logit_1_taxi_high_price , cluster = taxi_data_high_price$id)))[1:2]

# predict probability
predicted_probs_base_logit_1_taxi_high_price <- predict(base_logit_1_taxi_high_price, type = "response")

# calculate marginal effects
coef_me_base_logit_1_taxi_high_price<- c(rep(0,2))
se_coef_me_base_logit_1_taxi_high_price <- c(rep(0,2))
me_base_logit_1_taxi_high_price<- matrix(0, nrow(taxi_data_high_price), length(coef(base_logit_1_taxi_high_price)[1:2]))
se_me_base_logit_1_taxi_high_price <- matrix(0, nrow(taxi_data_high_price), length(coef(base_logit_1_taxi_high_price)[1:2]))

for (i in 1:length(coef(base_logit_1_taxi_high_price )[1:2])) {
  me_base_logit_1_taxi_high_price[,i] <- coef(base_logit_1_taxi_high_price)[i] * predicted_probs_base_logit_1_taxi_high_price * (1 - predicted_probs_base_logit_1_taxi_high_price)
  se_me_base_logit_1_taxi_high_price[,i] <- cr_se_base_logit_1_taxi_high_price[i]* predicted_probs_base_logit_1_taxi_high_price * (1 - predicted_probs_base_logit_1_taxi_high_price)
  coef_me_base_logit_1_taxi_high_price[i] <- mean(me_base_logit_1_taxi_high_price[,i])
  se_coef_me_base_logit_1_taxi_high_price[i] <- mean(se_me_base_logit_1_taxi_high_price[,i])
}

#result
cbind(coef_me_base_logit_1_taxi_high_price,se_coef_me_base_logit_1_taxi_high_price)


# taxi group low-price ----------------------------------------------------
#create dataset
taxi_data_low_price <- subset(taxi_data_1, Price == "¥1200")

# create model
base_logit_1_taxi_low_price <- glm(choice ~ Safety + Dispatch + License + Time + Service + age_over40 + occupation_Full_time_employee + sex_female + region_more_than_500k
                                   + Taxi_frequency_more_than_several_time_a_year + Taxi_aim_laisure, data = taxi_data_low_price, family = binomial)

#calculate CR standard error
cr_se_base_logit_1_taxi_low_price <- sqrt(diag(vcovCL(base_logit_1_taxi_low_price , cluster = taxi_data_low_price$id)))[1:2]

# predict probability
predicted_probs_base_logit_1_taxi_low_price <- predict(base_logit_1_taxi_low_price, type = "response")

# calculate marginal effect
me_base_logit_1_taxi_low_price<- matrix(0, nrow(taxi_data_low_price), length(coef(base_logit_1_taxi_low_price)[1:2]))
se_me_base_logit_1_taxi_low_price <- matrix(0, nrow(taxi_data_low_price), length(coef(base_logit_1_taxi_low_price)[1:2]))
coef_me_base_logit_1_taxi_low_price<- c(rep(0,2))
se_coef_me_base_logit_1_taxi_low_price <- c(rep(0,2))

for (i in 1:length(coef(base_logit_1_taxi_low_price )[1:2])) {
  me_base_logit_1_taxi_low_price[,i] <- coef(base_logit_1_taxi_low_price)[i] * predicted_probs_base_logit_1_taxi_low_price * (1 - predicted_probs_base_logit_1_taxi_low_price)
  se_me_base_logit_1_taxi_low_price[,i] <- cr_se_base_logit_1_taxi_low_price[i]* predicted_probs_base_logit_1_taxi_low_price * (1 - predicted_probs_base_logit_1_taxi_low_price)
  coef_me_base_logit_1_taxi_low_price[i] <- mean(me_base_logit_1_taxi_low_price[,i])
  se_coef_me_base_logit_1_taxi_low_price[i] <- mean(se_me_base_logit_1_taxi_low_price[,i])
}

#結果
cbind(coef_me_base_logit_1_taxi_low_price,se_coef_me_base_logit_1_taxi_low_price)

# Visualization ----------------------------------------------------------------
# aggregate results
#rs group high-price
result_high_price_rs <- data.frame(Variable = rev(c("ServiceYes(vs No)",  "Time7min(vs 10min)", "C2LicenseYes(vs NO)","DispatchApp (vs PhoneCall)", "Accident1% (vs 3%)")),
                                   Coefficient = coef_me_base_logit_1_rs_high_price[2:6],
                                   SE = se_coef_me_base_logit_1_rs_high_price[2:6])

#rs group low-price
result_low_price_rs <- data.frame(Variable = rev(c("ServiceYes(vs No)",  "Time7min(vs 10min)", "C2LicenseYes(vs NO)","DispatchApp (vs PhoneCall)", "Accident1% (vs 3%)")),
                                  Coefficient = coef_me_base_logit_1_rs_low_price[2:6],
                                  SE = se_coef_me_base_logit_1_rs_low_price[2:6])

#taxi group high-price
result_high_price_taxi <- data.frame(Variable = rev(c("ServiceYes(vs No)",  "Time7min(vs 10min)", "C2LicenseYes(vs NO)","DispatchApp (vs PhoneCall)", "Accident1% (vs 3%)")),
                                     Coefficient = coef_me_base_logit_1_taxi_high_price[2:6],
                                     SE = se_coef_me_base_logit_1_taxi_high_price[2:6])

#taxi group low-price
result_low_price_taxi <- data.frame(Variable = rev(c("ServiceYes(vs No)",  "Time7min(vs 10min)", "C2LicenseYes(vs NO)","DispatchApp (vs PhoneCall)", "Accident1% (vs 3%)")),
                                    Coefficient = coef_me_base_logit_1_taxi_low_price[2:6],
                                    SE = se_coef_me_base_logit_1_taxi_low_price[2:6])


#calculate 95%CI
#rs group high-price
result_high_price_rs $CI_low <- result_high_price_rs $Coefficient - 1.96 * result_high_price_rs$SE
result_high_price_rs$CI_high <- result_high_price_rs$Coefficient + 1.96 * result_high_price_rs$SE
result_high_price_rs$Label <- paste0(round(result_high_price_rs$Coefficient, 3), " (", round(result_high_price_rs$CI_low, 3), ", ", round(result_high_price_rs$CI_high, 3), ")")

#rs group low-price
result_low_price_rs$CI_low <- result_low_price_rs$Coefficient - 1.96 * result_low_price_rs$SE
result_low_price_rs$CI_high <- result_low_price_rs$Coefficient + 1.96 * result_low_price_rs$SE
result_low_price_rs$Label <- paste0(round(result_low_price_rs$Coefficient, 3), " (", round(result_low_price_rs$CI_low, 3), ", ", round(result_low_price_rs$CI_high, 3), ")")

#taxi group high-price
result_high_price_taxi$CI_low <- result_high_price_taxi$Coefficient - 1.96 * result_high_price_taxi$SE
result_high_price_taxi$CI_high <- result_high_price_taxi$Coefficient + 1.96 * result_high_price_taxi$SE
result_high_price_taxi$Label <- paste0(round(result_high_price_taxi$Coefficient, 3), " (", round(result_high_price_taxi$CI_low, 3), ", ", round(result_high_price_taxi$CI_high, 3), ")")

#taxi group low-price
result_low_price_taxi$CI_low <- result_low_price_taxi$Coefficient - 1.96 * result_low_price_taxi$SE
result_low_price_taxi$CI_high <- result_low_price_taxi$Coefficient + 1.96 * result_low_price_taxi$SE
result_low_price_taxi$Label <- paste0(round(result_low_price_taxi$Coefficient, 3), " (", round(result_low_price_taxi$CI_low, 3), ", ", round(result_low_price_taxi$CI_high, 3), ")")

# set group
result_high_price_rs$Group <- "RS, Price 1500 JPY"
result_low_price_rs$Group <- "RS, Price 1200 JPY"
result_high_price_taxi$Group <- "Taxi, Price 1500 JPY"
result_low_price_taxi$Group <- "Taxi, Price 1200 JPY"

# combine results
combined_results_price <- rbind(result_high_price_taxi, result_low_price_taxi, result_high_price_rs, result_low_price_rs)

# sort group
combined_results_price$Group <- factor(combined_results_price$Group, 
                                       levels = c("Taxi, Price 1200 JPY", "Taxi, Price 1500 JPY", "RS, Price 1200 JPY", "RS, Price 1500 JPY"))

# sort variables
combined_results_price$Variable <- factor(combined_results_price$Variable, levels = c("ServiceYes(vs No)",  "Time7min(vs 10min)", "C2LicenseYes(vs NO)","DispatchApp (vs PhoneCall)", "Accident1% (vs 3%)"))

# reset group
combined_results_price$Groups <- ifelse(grepl("RS", combined_results_price$Group), "RS", "Taxi")

#visualization
fig_7 <- combined_results_price %>% filter(Variable %in% c("Accident1% (vs 3%)")) %>%
  ggplot(aes(x = Variable, y = Coefficient, color = Group)) +
  geom_point(position = position_dodge(0.9)) + 
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.2, position = position_dodge(0.9)) + 
  geom_text(aes(label = Label), vjust = -1.2, hjust = 0.5, size = 3, position = position_dodge(0.9)) +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(x = "", y = "Coefficient",title = "The Impact of Changes in Safety Levels on the Probability of Service Choice, \nConditioned on Price") +
  theme_minimal() +
  scale_color_manual(values = c("RS, Price 1500 JPY" = "blue", "RS, Price 1200 JPY" = "darkblue", 
                                "Taxi, Price 1500 JPY" = "red", "Taxi, Price 1200 JPY" = "darkred"),
                     breaks = c("RS, Price 1500 JPY" , "RS, Price 1200 JPY" , "Taxi, Price 1500 JPY" , "Taxi, Price 1200 JPY")) +  
  facet_grid(. ~ Groups, scales = "free_x", space = "free_x")+
  theme(
    plot.title = element_text(size = 20),  
    axis.title.x = element_text(size = 17), 
    axis.title.y = element_text(size = 17),  
    legend.position = "bottom",
    legend.text = element_text(size = 17),  
    axis.text.x = element_text(face = "bold", size = 17),  
    axis.text.y = element_text(face = "bold", size = 17)  
  ) 

plot(fig_7)


