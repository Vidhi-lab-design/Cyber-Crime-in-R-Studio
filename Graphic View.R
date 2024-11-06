setwd("C:/Users/ghadi/OneDrive/data analysis and visualization")
cc<-read.csv("C:/Users/ghadi/OneDrive/data analysis and visualization/Rdis - Sheet1.csv")

install.packages(c("ggplot2", "ggcorrplot", "dplyr", "tidyr", "scales", "lubridate", "ggthemes"))
install.packages(c("gganimate","gifski", "transformr"))
library(ggplot2)
library(ggcorrplot)
library(dplyr)
library(tidyr)
library(scales)
library(lubridate)
library(ggthemes)
library(gganimate)
library(gifski)
library(transformr)

View(cc)

#ggcorrplot
correlation_matrix <- cor(cc[, c("amount", "customer_age")], use = "complete.obs")
ggcorrplot(correlation_matrix, lab = TRUE) +
  theme_minimal() +  
  labs(title = "Correlation Matrix")

#facetgrid
ggplot(cc, aes(amount, customer_age, color=location))+
  geom_point()+facet_grid(.~fraud_type)+
  labs(x= "amount", y ="customer_age", title = "Age wise fraud")+
  theme(plot.title = element_text(hjust=0.5))


#geom_bar
ggplot(cc, aes(fraud_type, fill = location))+
  geom_bar()+
  labs(title = "Fraud type city wise")
  theme(plot.title = element_text(hjust = 0.5))

#geom_jitter
cc$is_fraudulent <- as.factor(cc$is_fraudulent)
ggplot(cc, aes(x = card_type, y = amount, color = is_fraudulent)) +
  geom_jitter(width = 0.2) +
  labs(title = "Transaction Amount Distribution by Card Type with Jitter", 
       x = "Card Type", 
       y = "Amount") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "lightgrey")  
  ) +
  scale_color_manual(values = c("0" = "blue", "1" = "red"))

#geom_boxplot
ggplot(cc, aes(x = card_type, y = amount, fill = card_type)) +
  geom_boxplot() +
  labs(title = "Transaction Amount Distribution by Card Type", x = "Card Type", y = "Amount") +
  theme_minimal() +
  scale_fill_manual(values = c("Visa" = "blue", "MasterCard" = "green", "Rupay" = "red"))

#geom_line 
filtered_data <- filter(cc, location == "Surat" | location == "Chennai")
ggplot(filtered_data, aes(x = amount, y = card_type, color = location, group = location)) +
  geom_line() +
  labs(title = "City with highest crime rates to lowest", 
       x = "card type", 
       y = "Amount") +
  scale_color_manual(values = c("Surat" = "purple", "Chennai" = "darkgreen"))

#geom_point
ggplot(cc, aes(customer_age, fraud_type)) +
  geom_point(aes(color = purchase_category)) +
  theme_tufte()

#gganimate
ggplot(cc, aes(x = fraud_type, y = amount, fill = fraud_type)) +
  geom_bar(stat = "identity") +
  transition_states(transaction_time, transition_length = 2, state_length = 1) +
  labs(title = 'Fraud Type Over Time', y = 'Transaction Amount', x = 'Fraud Type')


