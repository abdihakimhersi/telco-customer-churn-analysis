```r
#checking data structure and missing values
glimpse(telco_data)
summary(telco_data)
# any missing values?
# removing na's from total charges
telco_data_clean <- telco_data %>% 
  filter(!is.na(TotalCharges))
# convert churn from boolean to binary 1-yes 0-no
telco_data_clean <- telco_data_clean %>% 
  mutate(
    Churn_binary = as.numeric(Churn=='Yes')
  )
# check to see whether conversion has worked
telco_data_clean %>% 
  select(Churn, Churn_binary) %>% 
  head(10)
# first model
churn_model <- glm(Churn_binary ~ Contract + PaymentMethod + InternetService + tenure,
                   data = telco_data_clean,
                   family = 'binomial')
summary(churn_model)
# predicted probabilities
telco_data_clean <- telco_data_clean %>% 
  mutate(predicted_churn_prob = predict(churn_model, type = 'response'))
# range of probabilities
summary(telco_data_clean$predicted_churn_prob)
# looking at the distribution
hist(telco_data_clean$predicted_churn_prob,
     main = 'Distribution of Churn Probabilities',
     xlab = 'Predicted Churn Probabilities')
# How many customers are high risk
high_risk_count <- telco_data_clean %>% 
  filter(predicted_churn_prob > 0.5) %>% 
  nrow()

print(paste('Customers with >50% churn risk:', high_risk_count))

# what is the potential loss in reveneue
revenue_at_risk <- telco_data_clean %>% 
  filter(predicted_churn_prob > 0.5) %>% 
  summarise(
    total_monthly_revenue = sum(MonthlyCharges),
    avg_monthly_revenue = mean(MonthlyCharges),
    customers_at_risk = n()
  )
print(revenue_at_risk)
# Top 10 most at risk
telco_data_clean %>% 
  arrange(desc(predicted_churn_prob)) %>% 
  select(customerID, predicted_churn_prob, Contract, PaymentMethod, InternetService,
         tenure) %>% 
  head(10)
# highest risk customers: month-to-month contract, electronic check, fiber optic
# low tenure
# how many customers have this combination "perfect storm"
perfect_storm_count <- telco_data_clean %>% 
  filter(Contract == "Month-to-month",
         PaymentMethod == 'Electronic check',
         InternetService == 'Fiber optic') %>% 
  nrow()
print(paste("Customers with the 'Perfect Storm' profile:", perfect_storm_count))
# quantify the business impact
business_impact <- telco_data_clean %>% 
  filter(Contract == "Month-to-month",
         PaymentMethod == 'Electronic check',
         InternetService == 'Fiber optic') %>% 
  summarise(
    customer_risk = n(),
    monthly_revenue_at_risk = sum(MonthlyCharges),
    avg_tenure = mean(tenure)
  )
print(business_impact)
