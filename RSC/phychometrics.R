library(readr)
train <- read_csv("DAT/train.csv")

head(train)
structure(train)
summary(train)
View(train)

library(moonBook)
mytable(voted ~ ., data = train) # Descriptive Statistics by 'voted' 


# data cleansing -----------------------------------------------------------

# erase the index column
train <- train[-1]
train

# Visualization -----------------------------------------------------------

library(ggplot2)
colnames(train) # Select the possible features: age_group, education, engnat, familysize, gender, hand, married, race, religion

ggplot(train, aes(x = voted)) +
  geom_bar(position = "dodge")

# Q_A_machiavellianism test score: boxplot
library(dplyr)

Q_A <- train %>% 
  select(matches('A$'))

plot(Q_A)

library(PerformanceAnalytics)
chart.Correlation(Q_A, histogram[,c(00:00)],pch=19) # Correlation of Qs_positive vs. negative : grouping the Qs

# Q_E_consumed time for each Qs: boxplot 

Q_E <- train %>% 
  select(matches('E$'), -familysize, -race)





# age_group

# education: 0 means no answer 

# engnat: 1=Yes, 2=No, 0=n/a

# familysize: 

# gender 

# hand: 1=Right, 2=Left, 3=Both, 0=n/a

# married: 1=Never married, 2=Currently married, 3=Previously married, 0=Other

# race: Asian, Arab, Black, Indigenous Australian, Native American, White, Other

# religion: Agnostic, Atheist, Buddhist, Christian_Catholic, Christian_Mormon, Christian_Protestant, Christian_Other, Hindu, Jewish, Muslim, Sikh, Othe

# tp01~10: grouping the items based on the correlation 

chart.Correlation()

# urban: 1=Rural, 2=Suburban, 3=Urban, 0=n/a

# wr01~13: know the definition of real things: 1=Yes, 0=No

# wf01~03: know the definition of fictitious things: 1=Yes, 0=No

# voted: 1=Yes, 2=No 

