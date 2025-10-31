```r
# Loading package
library(tidyverse)
# reading data
telco_data <- read_csv("data/WA_Fn-UseC_-Telco-Customer-Churn.csv")
# checking if data has loaded correctly
glimpse(telco_data)
# total number of customers
total_number_customers <- nrow(telco_data)
# created a summary table that counts the number customers churned grouped by
# contract type
contract_summary <- telco_data %>%
  group_by(Contract) %>% 
  summarise(total_number_customers = n(),
            churned_customers = sum(Churn=='Yes'),
            percentage_churned = (churned_customers/total_number_customers)*100)
# plotted the data in to a bar chart
plot1 <- ggplot(contract_summary, aes(x = Contract, y = percentage_churned)) +  # Fixed parenthesis
  geom_col(aes(fill=percentage_churned)) +
  scale_fill_gradient(low = 'green', high = 'red', guide = 'none') +
  labs(title = "Churn Rate by Contract Type",
    x = 'Contract Type',
    y = 'Percentage Churned (%)',
    subtitle = 'Month-to-month customers are 15x more likely to leave',
    caption = 'Source: Telco Customer Data') +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40")
  ) +
  geom_text(aes(label = paste0(round(percentage_churned, 1), "%")),
            vjust = -0.5,
            size = 4,
            fontface = "bold") +
  ylim(0, 50)
# printing plot
print(plot1)
# saving plot
ggsave('telco_churn_by_contract.png',plot1)
# creating a contigency table
contingency_table <- table(telco_data$Contract, telco_data$Churn)
# checking to see whether contract type and churn are related
result <- chisq.test(contingency_table)
# install and load effectsize to understand the impact of this realtionship
install.packages("effectsize")
library(effectsize)
# calcualte Cramer's V
effect_size <- cramers_v(contingency_table)
# printing results
print(effect_size)
# now exploring monthly charges
