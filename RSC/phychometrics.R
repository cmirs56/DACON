
# Data Loading ------------------------------------------------------------
library(readr)
train <- read_csv("DAT/train.csv")

head(train)
str(train)
summary(train)
Hmisc::describe(train)
table(train$familysize)
View(train)

library(moonBook)
mytable(voted ~ ., data = train) # Descriptive Statistics by 'voted' 

# Preprocessing & EDA -----------------------------------------------------------
#train <- train[-1] # erase the index column
train
sum(is.na(train)) # Missing data 
colSums(is.na(train))

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
#pairs(Q_A, panel = panel.smooth) # scatter matrix plot (loading time!!!)

#library(PerformanceAnalytics)
#chart.Correlation(Q_A, histogram = TRUE,pch=19) # need to check again!!! 

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
corrplot::corrplot(cor_Q_A, method="shade", addshade="all", shade.col=NA, 
         tl.col="red", tl.srt=30, diag=FALSE, addCoef.col="black", order="FPC")

# derived variable_machi_score_generate a new column 
dim(train)
train <- train %>% 
  mutate(machi_score = (QaA + QbA + QcA + QdA + QeA + QfA + QgA + QhA + QiA + QjA + QkA + QlA + QmA + QnA + QoA + QpA + QqA + QrA + QsA + QtA)/20)
dim(train) 
         
# Q_E_consumed time for each Qs
Q_E <- train %>% 
  select(matches('E$'), -familysize, -race -machi_score)

# voted: 1=Yes, 2=No
str(train$voted)
train$voted <- as.factor(train$voted)

# age_group
ggplot(train, aes(x = age_group, fill = voted)) +
  geom_bar(position = "dodge")

# education: 0 means no answer 
ggplot(train, aes(x = education, fill = voted)) +
  geom_bar(position = "dodge")
ggplot(train, aes(education, fill = voted)) +
  geom_bar(position = "fill") +
  theme(axis.title.x = element_text(angle = 45, vjust = 0.6)) +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Education", y = "Rate", 
       title = "Ratio bar plot by education")

# engnat: 1=Yes, 2=No, 0=n/a
ggplot(train, aes(x = engnat, fill = voted)) +
  geom_bar(position = "dodge")

# familysize: 
ggplot(train, aes(x = familysize, color = voted)) +
  geom_boxplot()
train$familysize <- ifelse(train$familysize > 7, NA, train$familysize)
table(train$familysize)
ggplot(train, aes(x = familysize)) +
  geom_histogram(binwidth = 1) +
  facet_grid(voted ~ .)
train %>% 
  ggplot(aes(familysize, fill = voted)) +
  geom_density(alpha = .7) +
  scale_fill_brewer(palette = "Set1")

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

# tp01~10: grouping the items based on the correlation, 7=no response (!!!!!!!!!!!!)  
tp <- c('tp01', 'tp02', 'tp03', 'tp04', 'tp05', 'tp06', 'tp07', 'tp08', 'tp09', 'tp10')
train[tp] <- ifelse(train[tp] == 7, NA, train[tp]) 

# wr01~13: know the definition of real things: 1=Yes, 0=No
# wf01~03: know the definition of fictitious things: 1=Yes, 0=No
train <- train %>% 
  mutate(wr = (wr_01 + wr_02 + wr_03 + wr_04 + wr_05 + wr_06 + wr_07 + wr_08 + wr_09 + wr_10 + wr_11 + wr_12 + wr_13))
train$wr
train <- train %>% 
  mutate(wf = (wf_01 + wf_02 + wf_03))
train$wf

ggplot(train, aes(wr, wf, color = education)) +
  geom_point()
ggplot(train, aes(wr, wf, fill = education)) +
  geom_tile()
ggplot(train, aes(x = wr, y = wf)) +
  geom_point(shape = 19, size = 3) +
  facet_grid(voted ~.)

# Modeling ----------------------------------------------------------------
df <- subset(train, select = c(age_group, gender, race, religion, education, engnat, familysize, married, urban, machi_score, wr, wf, voted)) 
dim(df)
str(df)

y_name <- "voted"
x_name <- names(df)[1:12]

tr_ratio <- 0.7
y1_index <- which(df[ , y_name] == 1)
y2_index <- which(df[ , y_name] == 2)

set.seed(123)
sample_y1_tr_id <- sample(y1_index, length(y1_index) * tr_ratio, replace = FALSE)
sample_y2_tr_id <- sample(y2_index, length(y2_index) * tr_ratio, replace = FALSE)

sample_tr_id <- c(sample_y1_tr_id, sample_y2_tr_id)
data_tr_df <- df[sample_tr_id, ]
data_te_df <- df[-sample_tr_id, ]

table(data_tr_df$voted)
table(data_te_df$voted)


#Feature selection: age_group, education, engnat, familysize, gender, hand, married, race, religion, urban, machi_score, wr, wf 


# Assessment_AUC --------------------------------------------------------------
