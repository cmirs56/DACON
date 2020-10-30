library(readr)
train <- read_csv("DAT/train.csv")

head(train)
str(train)
summary(train)
View(train)

library(moonBook)
mytable(voted ~ ., data = train) # Descriptive Statistics by 'voted' 

# data cleansing -----------------------------------------------------------

train <- train[-1] # erase the index column
train

sum(is.na(train)) # Missing data 
colSums(is.na(train))

# Visualization -----------------------------------------------------------

library(ggplot2)
colnames(train) # Select the possible features: age_group, education, engnat, familysize, gender, hand, married, race, religion, urban, tp__, wr, wf 

ggplot(train, aes(x = voted)) +
  geom_bar(position = "dodge")

# Q_A_machiavellianism test score: heatmap  
library(dplyr)

Q_A <- train %>% 
  select(matches('A$')) # select the question columns_end with 'A' 
str(Q_A)
head(Q_A)

# Correlation of Qs_positive vs. negative : grouping the Qs

cor_Q_A <- cor(Q_A) # calculate the correlation matrix 
cor_Q_A

pairs(Q_A, panel = panel.smooth) # scatter matrix plot (loading time!!!)

library(PerformanceAnalytics)
chart.Correlation(Q_A, histogram = TRUE,pch=19) # need to check again!!! 

library(corrplot)
corrplot(cor_Q_A, method="number")
corrplot(cor_Q_A, method="circle")
corrplot(cor_Q_A, method="ellipse")
corrplot(cor_Q_A, method="color")
corrplot(cor_Q_A, method="shade", addshade="all", shade.col=NA, 
         tl.col="red", tl.srt=30, diag=FALSE, addCoef.col="black", order="FPC")

negative <- c('QeA', 'QdA', 'QgA', 'QaA', 'QrA', 'QfA', 'QqA', 'QiA', 'QnA', 'QkA') # select the Qs with negative correlations

library(tidyverse)
train[negative] <- map(train[negative], ~-.x+6) # repeat_purrr : score = 6 - score  

Q_A <- train %>% 
  select(matches('A$'))
cor_Q_A <- cor(Q_A)
  
corrplot(cor_Q_A, method="shade", addshade="all", shade.col=NA, 
         tl.col="red", tl.srt=30, diag=FALSE, addCoef.col="black", order="FPC")

# derived variable_machi_score_generate a new column 

dim(train)
train <- train %>% 
  mutate(machi_score = (QaA + QbA + QcA + QdA + QeA + QfA + QgA + QhA + QiA + QjA + QkA + QlA + QmA + QnA + QoA + QpA + QqA + QrA + QsA + QtA)/20)
dim(train) 
         
# Q_E_consumed time for each Qs

Q_E <- train %>% 
  select(matches('E$'), -familysize, -race)

# voted: 1=Yes, 2=No

str(train$voted)
train$voted <- as.factor(train$voted)

# age_group

ggplot(train, aes(x = age_group, fill = voted)) +
  geom_bar(position = "dodge")

# education: 0 means no answer 

ggplot(train, aes(x = education, fill = voted)) +
  geom_bar(position = "dodge")

# engnat: 1=Yes, 2=No, 0=n/a

ggplot(train, aes(x = engnat, fill = voted)) +
  geom_bar(position = "dodge")

# familysize: 

ggplot(train, aes(x = familysize)) +
  geom_boxplot()

train$familysize <- ifelse(train$familysize > 10, NA, train$familysize)

ggplot(train, aes(x = familysize)) +
  geom_histogram(binwidth = 1) +
  facet_grid(voted ~ .)

# gender 

ggplot(train, aes(x = gender, fill = voted)) +
  geom_bar(position = "dodge")

# hand: 1=Right, 2=Left, 3=Both, 0=n/a

ggplot(train, aes(x = hand, fill = voted)) +
  geom_bar(position = "dodge")

# married: 1=Never married, 2=Currently married, 3=Previously married, 0=Other

ggplot(train, aes(x = married, fill = voted)) +
  geom_bar(position = "dodge")

# race: Asian, Arab, Black, Indigenous Australian, Native American, White, Other

ggplot(train, aes(x = race, fill = voted)) +
  geom_bar(position = "dodge")

# religion: Agnostic, Atheist, Buddhist, Christian_Catholic, Christian_Mormon, Christian_Protestant, Christian_Other, Hindu, Jewish, Muslim, Sikh, Othe

ggplot(train, aes(x = religion, fill = voted)) +
  geom_bar(position = "dodge")

# urban: 1=Rural, 2=Suburban, 3=Urban, 0=n/a

ggplot(train, aes(x = urban, fill = voted)) +
  geom_bar(position = "dodge")

# tp01~10: grouping the items based on the correlation, 7=no response  



# wr01~13: know the definition of real things: 1=Yes, 0=No

# wf01~03: know the definition of fictitious things: 1=Yes, 0=No


# Feature selection -------------------------------------------------------


# Modeling ----------------------------------------------------------------

x_train <- 
  
y_train <- 

# Evaluation --------------------------------------------------------------
