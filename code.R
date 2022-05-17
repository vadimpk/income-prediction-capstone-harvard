###############################################
# Importing libraries and reading the data set
###############################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(ggpubr)
library(caret)
library(randomForest)

# read the data set from .csv file
data <- read_csv("https://raw.githubusercontent.com/vadimpk/income-prediction-capstone-harvard/master/adult.csv")

# set the seed for consistent result
set.seed(1)


###############################################
# Cleaning the data
###############################################


# structure of a data set
str(data, give.attr=F)

# check for NA
sum(colSums(is.na(data))) # no NA's

# check unique values in every column
sapply(data, function(x){length(unique(x))})

# make income feature binary and transform it to factor
data$income[data$income == "<=50K"] <- 0
data$income[data$income == ">50K"] <- 1
data$income <- as.numeric(data$income)
data$income_factor <- factor(data$income, levels = c(0,1))

# make sex feature binary and transform it to factor
data$sex[data$sex == "Female"] <- 0
data$sex[data$sex == "Male"] <- 1
data$sex <- as.numeric(data$sex)
data$sex_factor <- factor(data$sex, levels = c(0,1))

# here we can see that some rows have "?" values in following features
unique(data$workclass)
unique(data$occupation)
unique(data$native.country)
# let's change them to "Other"
data$workclass[data$workclass == "?"] <- "Other"
data$occupation[data$occupation == "?"] <- "Other"
data$native.country[data$native.country == "?"] <- "Other"

# drop the fnlwgt and education.num columns (useless ones)
data <- select(data, -fnlwgt, -education.num)

###############################################
# Exploratory analysis
###############################################


# the proportion of income (0 - means less than 50K, 1 - >50K)
table(data$income)
prop.table(table(data$income)) # we can see that there are more people with income less that 50K

# check correlation between income and other numerical variables
cor(data[,sapply(data, is.numeric)])[,"income"]

# let's make some plots to better visualize the data

# AGE
# age importance
data %>% 
  ggplot(aes(age, fill=income_factor)) + 
  geom_histogram(stat="count", binwidth = 10, show.legend = F) +
  labs(title = "Age Importance on Income", subtitle = "red < 50K income; blue > 50K", 
       x = "Age", y = "Count") +
  theme_minimal()

# age importance with position fill
data %>% 
  ggplot(aes(age, fill=income_factor)) + 
  geom_histogram(stat="count", binwidth = 10, show.legend = F, position = "fill") +
  labs(title = "Age Importance on Income", subtitle = "red < 50K income; blue > 50K", 
       x = "Age", y = "Count") +
  theme_minimal()

# results of previous plot can help us group the age feature better
# factoring age feature
data <- data %>% add_column(age_factor = NA)
data$age_factor[data$age <= 21] <- "younger than 22" 
data$age_factor[data$age > 21 & data$age <= 31] <- "22 - 31" 
data$age_factor[data$age > 31 & data$age <= 61] <- "32 - 61" 
data$age_factor[data$age > 61 & data$age <= 75] <- "62 - 75"
data$age_factor[data$age > 75] <- "older than 75"
data$age_factor <- factor(data$age_factor)
# relevel to make younger than 22 first
data$age_factor <- relevel(data$age_factor, "younger than 22")

# age importance grouped
grouped_age_plot_1 <- data %>% 
  ggplot(aes(age_factor, fill=income_factor)) + 
  geom_histogram(stat="count", binwidth = 1, show.legend = F) +
  labs(title = "Age Importance Grouped on Income", subtitle = "red < 50K income; blue > 50K",
       x = "Age Group", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=80, vjust=0.6))

grouped_age_plot_2 <- data %>% 
  ggplot(aes(age_factor, fill=income_factor)) + 
  geom_histogram(stat="count", binwidth = 1, show.legend = F, position = "fill") +
  labs(title = "Age Importance Grouped on Income", subtitle = "red < 50K income; blue > 50K",
       x = "Age Group", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=80, vjust=0.6))

ggarrange(grouped_age_plot_1, grouped_age_plot_2, nrow=1,ncol=2)


# WORK CLASS
# workclass importance grouped
workclass_grouped_plot_1 <- data %>% 
  ggplot(aes(workclass, fill=income_factor)) + 
  geom_histogram(stat="count", binwidth = 10, show.legend = F) +
  labs(title = "Work Class Importance on Income", subtitle = "red < 50K income; blue > 50K", 
       x = "Work Class", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=80, vjust=0.6))

workclass_grouped_plot_2 <- data %>% 
  ggplot(aes(workclass, fill=income_factor)) + 
  geom_histogram(stat="count", binwidth = 10, show.legend = F, position = "fill") +
  labs(x = "Work Class", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=80, vjust=0.6))

# arranging plots
ggarrange(workclass_grouped_plot_1, workclass_grouped_plot_2, ncol = 2, nrow = 1)

data$workclass[data$workclass == "Without-pay"] <- "Never-worked"
data$workclass <- factor(data$workclass)

# EDUCATION
# education importance plot
data %>% 
  ggplot(aes(education, fill=income_factor)) + 
  geom_histogram(stat="count", binwidth = 10, show.legend = F) +
  labs(title = "Education Importance on Income", subtitle = "red < 50K income; blue > 50K", 
       x = "Education Level", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=80, vjust=0.6))

# education importance plot with position filled 
data %>% 
  ggplot(aes(education, fill=income_factor)) + 
  geom_histogram(stat="count", binwidth = 10, show.legend = F, position = "fill") +
  labs(title = "Education Importance on Income", subtitle = "red < 50K income; blue > 50K", 
       x = "Education Level", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=80, vjust=0.6))

# results of previous plot can help us group the education feature better
# factoring age feature
undergrad <- c("10th","11th","12th","1st-4th","5th-6th","7th-8th","9th","Preschool")
data$education[data$education %in% undergrad] <- "Undergraduate" 
data$education[data$education %in% c("Assoc-acdm","Assoc-voc")] <- "Associate"
data$education <- factor(data$education)

# education importance grouped
education_grouped_1 <- data %>% 
  ggplot(aes(education, fill=income_factor)) + 
  geom_histogram(stat="count", binwidth = 10, show.legend = F) +
  labs(title = "Education Importance on Income", subtitle = "red < 50K income; blue > 50K", 
       x = "Education Level", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=80, vjust=0.6))

education_grouped_2 <- data %>% 
  ggplot(aes(education, fill=income_factor)) + 
  geom_histogram(stat="count", binwidth = 10, show.legend = F, position = "fill") +
  labs(title = "Education Importance on Income", subtitle = "red < 50K income; blue > 50K", 
       x = "Education Level", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=80, vjust=0.6))

ggarrange(education_grouped_1, education_grouped_2, nrow=1,ncol=2)


# MARITAL STATUS
# marital status importance 
data %>% 
  ggplot(aes(marital.status, fill=income_factor)) + 
  geom_histogram(stat="count", binwidth = 10, show.legend = F) +
  labs(title = "Marital Status Importance on Income", subtitle = "red < 50K income; blue > 50K", 
       x = "Marital Status", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=80, vjust=0.6))

data %>% 
  ggplot(aes(marital.status, fill=income_factor)) + 
  geom_histogram(stat="count", binwidth = 10, show.legend = F, position="fill") +
  labs(title = "Marital Status Importance on Income", subtitle = "red < 50K income; blue > 50K", 
       x = "Marital Status", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=80, vjust=0.6))

# grouping marital.status by data from plot making it binary
data$marital.status[data$marital.status %in% c("Married-civ-spouse", "Married-AF-spouse")] <- 1
data$marital.status[data$marital.status != 1] <- 0
data$marital.status <- factor(as.numeric(data$marital.status))

# marital status importance grouped
data %>% 
  ggplot(aes(marital.status, fill=income_factor)) + 
  geom_histogram(stat="count", binwidth = 10, show.legend = F, position = "fill") +
  labs(title = "Marital Status Importance on Income", subtitle = "red < 50K income; blue > 50K", 
       x = "Marital Status", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=80, vjust=0.6))



# OCCUPATION
# occupation importance
data %>% 
  ggplot(aes(occupation, fill=income_factor)) + 
  geom_histogram(stat="count", binwidth = 10, show.legend = F) +
  labs(title = "Occupation Importance on Income", subtitle = "red < 50K income; blue > 50K", 
       x = "Occupation", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=80, vjust=0.6))

data %>% 
  ggplot(aes(occupation, fill=income_factor)) + 
  geom_histogram(stat="count", binwidth = 10, show.legend = F,position = "fill") +
  labs(title = "Occupation Importance on Income", subtitle = "red < 50K income; blue > 50K", 
       x = "Occupation", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=80, vjust=0.6))

# nothing to group here
data$occupation <- factor(data$occupation)

# RELATIONSHIP
# relationship importance grouped
data %>% 
  ggplot(aes(relationship, fill=income_factor)) + 
  geom_histogram(stat="count", binwidth = 10, show.legend = F) +
  labs(title = "Relationship Importance on Income", subtitle = "red < 50K income; blue > 50K", 
       x = "Relationship Status", y = "Count") +
  theme_minimal() 

data %>% 
  ggplot(aes(relationship, fill=income_factor)) + 
  geom_histogram(stat="count", binwidth = 10, show.legend = F, position = "fill") +
  labs(title = "Relationship Importance on Income", subtitle = "red < 50K income; blue > 50K", 
       x = "Relationship Status", y = "Count") +
  theme_minimal() 

# nothing to group here
data$relationship <- factor(data$relationship)


# RACE
# race importance 
data %>% 
  ggplot(aes(race, fill=income_factor)) + 
  geom_histogram(stat="count", binwidth = 10, show.legend = F) +
  labs(title = "Race Importance on Income", subtitle = "red < 50K income; blue > 50K", 
       x = "Race", y = "Count") +
  theme_minimal()

data %>% 
  ggplot(aes(race, fill=income_factor)) + 
  geom_histogram(stat="count", binwidth = 10, show.legend = F, position="fill") +
  labs(title = "Race Importance on Income", subtitle = "red < 50K income; blue > 50K", 
       x = "Race", y = "Count") +
  theme_minimal()

# group based on plot data
data <- select(data, -race)


#SEX
# sex importance grouped
data %>% 
  ggplot(aes(sex_factor, fill=income_factor)) + 
  geom_histogram(stat="count", binwidth = 10, show.legend = F) +
  labs(title = "Sex Importance on Income", subtitle = "red < 50K income; blue > 50K\n0 - Female, 1 - Male", x = "Sex", y = "Count") +
  theme_minimal()

data %>% 
  ggplot(aes(sex_factor, fill=income_factor)) + 
  geom_histogram(stat="count", binwidth = 10, show.legend = F, position="fill") +
  labs(title = "Sex Importance on Income", subtitle = "red < 50K income; blue > 50K\n0 - Female, 1 - Male", x = "Sex", y = "Count") +
  theme_minimal()


# NATIVE COUNTRY
# native country importance 
data %>% 
  ggplot(aes(native.country, fill=income_factor)) + 
  geom_histogram(stat="count", binwidth = 10, show.legend = F) +
  labs(title = "Native Country Importance on Income", subtitle = "red < 50K income; blue > 50K", 
       x = "Native Country", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=80, vjust=0.6))

data %>% 
  ggplot(aes(native.country, fill=income_factor)) + 
  geom_histogram(stat="count", binwidth = 10, show.legend = F, position = "fill") +
  labs(title = "Native Country Importance on Income", subtitle = "red < 50K income; blue > 50K", 
       x = "Native Country", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=80, vjust=0.6))

# droup native country
data <- select(data, -native.country)



# CAPITAL GAIN
# capital gain distribution
data %>%
  ggplot(aes(capital.gain)) + 
  geom_histogram() +
  labs(title = "Distribution of Capital Gain", x = "Capital Gain", y = "Count") +
  theme_minimal()

# people with capital gain of 0
data %>% filter(capital.gain != 0) %>% nrow
data %>% filter(capital.gain == 99999) %>% pull(income) %>% mean()


# capital gain distribution without outliers
capital_gain_plot_1 <- data %>% filter(capital.gain != 0 & capital.gain != 99999) %>%
  ggplot(aes(capital.gain, fill = income_factor)) + 
  geom_histogram(show.legend = F) +
  labs(x = "Capital Gain", y = "Count") +
  theme_minimal()

# capital gain distribution from 1 to 20000
capital_gain_plot_2 <- data %>% filter(capital.gain != 0 & capital.gain <= 20000) %>%
  ggplot(aes(capital.gain, fill = income_factor)) + 
  geom_histogram(show.legend = F) +
  labs(x = "Capital Gain", y = "Count") +
  theme_minimal()

# capital gain distribution from 1 to 10000
capital_gain_plot_3 <- data %>% filter(capital.gain != 0 & capital.gain <= 10000) %>%
  ggplot(aes(capital.gain, fill = income_factor)) + 
  geom_histogram(show.legend = F) +
  labs(x = "Capital Gain", y = "Count") +
  theme_minimal()

# arranging plots
ggarrange(capital_gain_plot_1, capital_gain_plot_2, capital_gain_plot_3, 
          labels = c("No Outliers", "1 - 20000", "1 - 10000"),
          ncol = 2, nrow = 2)


# results of previous plots can help us group the capital gain feature
# factoring capital gain feature
data <- data %>% add_column(capital.gain_factor = NA)
data$capital.gain_factor[data$capital.gain == 0] <- "0" 
data$capital.gain_factor[data$capital.gain > 0 & data$age <= 2500] <- "1 - 2500" 
data$capital.gain_factor[data$capital.gain > 2500 & data$age <= 7000] <- "2500 - 7000" 
data$capital.gain_factor[data$capital.gain > 7000] <- "more than 7000"
data$capital.gain_factor <- factor(data$capital.gain_factor)

# capital gain importance grouped
capital_gain_grouped_plot_1 <- data %>% 
  ggplot(aes(capital.gain_factor,fill=income_factor)) + 
  geom_histogram(stat="count", show.legend = F) +
  labs(title = "Capital Gain", subtitle = "red < 50K income; blue > 50K", x = "Capital Gain", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=80, vjust=0.6))

capital_gain_grouped_plot_2 <- data %>% ggplot(aes(capital.gain_factor, fill=income_factor)) + 
  geom_histogram(stat="count", show.legend = F, position = "fill") +
  labs(title = "Capital Gain", subtitle = "red < 50K income; blue > 50K", x = "Capital Gain", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=80, vjust=0.6))

ggarrange(capital_gain_grouped_plot_1, capital_gain_grouped_plot_2, ncol=2, nrow=1)


# CAPITAL LOSS
# capital loss distribution
data %>%
  ggplot(aes(capital.loss)) + 
  geom_histogram() +
  labs(title = "Distribution of Capital Loss", subtitle = "red < 50K income; blue > 50K",
       x = "Capital Loss", y = "Count") +
  theme_minimal()

# capital loss importance 
data %>% 
  ggplot(aes(capital.loss, fill = income_factor)) + 
  geom_histogram(show.legend = F, position = "fill") +
  labs(title = "Capital Loss Importance on Income", subtitle = "red < 50K income; blue > 50K",
       x = "Capital Loss", y = "Count") +
  theme_minimal()

# results of previous plots show us that capital loss is not that important feature so we can drop it
# using capital.gain would be much better
data <- select(data, -capital.loss)


# WORKING HOURS
# working hours per week distribution
data %>% 
  ggplot(aes(hours.per.week, fill=income_factor)) + 
  geom_histogram(binwidth = 3, show.legend = F) +
  labs(title = "Working Hours per Week Importance on Income", subtitle = "red < 50K income; blue > 50K",
       x = "Working Hours per Week", y = "Count") +
  theme_minimal()

data %>%
  ggplot(aes(hours.per.week, fill=income_factor)) + 
  geom_histogram(binwidth = 3, show.legend = F, position="fill") +
  labs(title = "Working Hours per Week Importance on Income", subtitle = "red < 50K income; blue > 50K", x = "Working Hours per Week", y = "Count") +
  theme_minimal()

# results of previous plots can help us group the working hours per week feature
# factoring working hours per week feature
data <- data %>% add_column(hours.per.week_factor = NA)
data$hours.per.week_factor[data$hours.per.week < 35] <- "0-35" 
data$hours.per.week_factor[data$hours.per.week >= 35 & data$hours.per.week < 50] <- "35-49" 
data$hours.per.week_factor[data$hours.per.week >= 50 & data$hours.per.week <= 60] <- "50-60"
data$hours.per.week_factor[data$hours.per.week > 60] <- "61-100"
data$hours.per.week_factor <- factor(data$hours.per.week_factor)

# working hours per week importance grouped
hours_grouped_plot_1 <- data %>%
  ggplot(aes(hours.per.week_factor, fill=income_factor)) + 
  geom_histogram(stat="count", binwidth = 3, show.legend = F) +
  labs(title = "Working Hours per Week", 
       subtitle = "red < 50K income; blue > 50K",
       x = "Working Hours per Week", y = "Count") +
  theme_minimal()

hours_grouped_plot_2 <- data %>%
  ggplot(aes(hours.per.week_factor, fill=income_factor)) + 
  geom_histogram(stat="count", binwidth = 3, show.legend = F, position = "fill") +
  labs(title = "Working Hours per Week", 
       subtitle = "red < 50K income; blue > 50K",
       x = "Working Hours per Week", y = "Count") +
  theme_minimal()

ggarrange(hours_grouped_plot_1, hours_grouped_plot_2, ncol=2, nrow=1)


# RESULTS
# drop non-factor columns
columns_to_drop <- c("age","capital.gain","sex","income","hours.per.week")
data <- data %>% select(-all_of(columns_to_drop))

# structure after exploratory analysis
str(data, give.attr=F)


###############################################
# Model building
###############################################


# split the data into train and test sets
test_index <- createDataPartition(y = data$income_factor, times = 1, p = 0.1, list = FALSE)
train <- data[-test_index,]
test <- data[test_index,]

# Logistic Regression Model
fitglm <- glm(income_factor~., data=train, family="binomial")
p_hat_logit <- predict(fitglm, newdata = test, type = "response")
y_hat_logit <- ifelse(p_hat_logit > 0.5, 1, 0) %>% factor
# accuracy
glm_accuracy <- confusionMatrix(y_hat_logit, test$income_factor)$overall[["Accuracy"]]

# Decision Tree Model
fitrpart <- train(income_factor ~ ., method = "rpart", data = train)
# accuracy
rpart_accuracy <- confusionMatrix(predict(fitrpart, test), test$income_factor)$overall["Accuracy"]

# Random Forest Model
fitrf <- randomForest(income_factor~., data=train)
# accuracy
rf_accuracy <- confusionMatrix(predict(fitrf, test), test$income_factor)$overall["Accuracy"]

# KNN Model
#fitknn <- train(income_factor~., data=train, method="knn")

#accuracy <- confusionMatrix(predict(fitknn, test), test$income_factor)$overall["Accuracy"]

# saving the confusion matrix of the best model (which is random forest)
matrix <- confusionMatrix(predict(fitrf, test), test$income_factor)

# some parameters of the best predictive model
rf_sensitivity <- matrix$byClass["Sensitivity"]
rf_specificity <- matrix$byClass["Specificity"]
rf_balanced_accuracy <- matrix$byClass["Balanced Accuracy"]
rf_kappa <- matrix$overall["Kappa"]

# plot the most important features 
varImpPlot(fitrf, sort = T, main = "Features' Importance")





