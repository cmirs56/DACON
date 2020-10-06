library(readr)
train <- read_csv("DAT/train.csv")

head(train)
structure(train)
summary(train)
View(train)

library(moonBook)
mytable(voted ~ ., data = train) # Descriptive Statistics by 'voted' 

library(PerformanceAnalytics)
chart.Correlation(df_narm[,c(00:00)],pch=19) # Correlation of Qs

library(ggplot2)
colnames(train) # Select the EDA_features: age_group, education, engnat, familysize, gender, hand, married, race, religion

ggplot(train, aes(x = voted)) +
  geom_bar(position = "dodge")
  