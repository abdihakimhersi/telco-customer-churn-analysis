```r
# calculating percentage churn rate for each method of payment
telco_data %>%
  group_by(PaymentMethod) %>% 
  summarise(churn_rate = paste0(round(mean(Churn=='Yes')*100, 1), "%"))
# Electronic check showed the highest percentage of churn
# Finding the most dangerous combinations: contract type & payment method
risk_combination1 <- telco_data %>% 
  group_by(Contract, PaymentMethod) %>% 
  summarise(
    customers = n(),
    churn_rate = paste0(round(mean(Churn =='Yes')*100, 1), "%"),
    .groups = 'drop'
    ) %>% 
  arrange(desc(churn_rate))
# exploring other potential factors affecting churn: internet services
internet_churn <- telco_data %>% 
  group_by(InternetService) %>% 
  summarise(
    customers = n(),
    churn_rate = paste0(round(mean(Churn == 'Yes')*100,1), '%')
    ) %>% 
  arrange(desc(churn_rate))
# Fiber optic customer are more likely to churn
# How does internet service interact with contract type
risk_combination2 <- telco_data %>% 
  group_by(Contract, InternetService) %>% 
  summarise(
    customer = n(),
    churn_rate = paste0(round(mean(Churn =='Yes')*100, 1), "%"),
    .groups = 'drop'
  ) %>% 
  arrange(desc(churn_rate))
