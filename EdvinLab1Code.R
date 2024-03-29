#loading needed libraries
library(pscl)      
library(ordinal) 
library(sjPlot) 
library(lmtest)       
library(tidyverse)  
library(gtsummary) 
library(readr) 
library(viridis)
library(pROC)
library(patchwork)
library(wesanderson)

setwd("~/Lab1")#setting working directory

#Loading data 
dt <- read_csv("StudentsPerformance2.1.csv")
set.seed(sum(as.integer(charToRaw("EDVIN"))))
mySample <- sample_n(dt, 200)


theme_set(theme_minimal()) #Setting ggplot aesthetic 


## The following variables are included in the dataset:	 
#female dummy: True if someone is female 
#race.ethnicity: ethnic background of the student (group A - E) 
#parental.level.of.education: parents’ highest level of education (high school - university) 
#lunch: was there support in paying for school lunch (free/reduced or standard) 
#test.preparation.course: participation in a preparation course (completed or none) 
#math.score: grade in a math test (’A’ is the highest grade and F the lowest) 
#reading.score: whether a student passed the reading test (passed or failed) 
#writing.score: whether a student passed the writing test (passed or failed)


## Data management	 


#first, looking at dataset 
view(mySample) 
str(mySample) 

#looking at individual variables 
table(mySample$female) 
table(mySample$race.ethnicity) 
table(mySample$parental.level.of.education) 
table(mySample$lunch) 
table(mySample$test.preparation.course) 
table(mySample$math.score) 
table(mySample$reading.score) 
table(mySample$writing.score) 

#recoding variblels
mySample_tidy <- mySample %>%
  mutate(
    writing.score = case_when(writing.score == "failed" ~ 0, writing.score == "passed" ~ 1),
    reading.score = case_when(reading.score == "failed" ~ 0, reading.score == "passed" ~ 1),
    test.preparation.course = case_when(test.preparation.course == "none" ~ 0, test.preparation.course == "completed" ~ 1),
    lunch = case_when(lunch == "standard" ~ 0, lunch == "free/reduced" ~ 1))


#assigning correct class
mySample_tidy$reading.score <- as.factor(mySample_tidy$reading.score)
mySample_tidy$writing.score <- as.factor(mySample_tidy$writing.score)

mySample_tidy$female <- as.factor(mySample_tidy$female) 

# Re-code 'math.score' for Ordinal Regression (ordered factor)
mySample_tidy$math.score <- factor(mySample_tidy$math.score, ordered = TRUE, levels = c("F", "E", "D", "C", "B", "A"))



#verifying the changes 
str(mySample_tidy) 



#Detecting and dealing with NA:s
NAs <- sapply(mySample_tidy, function(x) sum(is.na(x)))#using sapply() to detect NA:s in the data frame  

print(NAs) #Viewing NAs 
#math.score=5, reading.score=2, writing score=3
#low amount of NA:s distributed among fairly equal around all focal dependent variables
#hence NA:S are removed. However this could potently pose a problem for writing model since very few students failed, 
#hence the model for writing risks being imbalanced and only predicting students passing. However missing values are only 3 and the model will unbalanced either way.

mySample_tidy <- na.omit(mySample_tidy) #removing missing values 



#Overview of demographics 

wes_palette <- wes_palette("Rushmore1") #setting color palette 

# Plot for 'race.ethnicity' (assumed to be a character variable)
race_plot <- ggplot(mySample_tidy, aes(x = race.ethnicity)) +
  geom_bar(aes(fill = race.ethnicity), color = "black", show.legend = FALSE) +
  scale_fill_manual(values = wes_palette) +
  ggtitle("Race/Ethnicity") +
  xlab("Race/Ethnicity") +
  ylab("Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
print(race_plot)

#calculating percentages 
table(mySample_tidy$race.ethnicity)
(70/190) * 100#36.84211 group C
(13/190) * 100#6.842105 group a

# Plot for 'race.ethnicity' (assumed to be a character variable)
parental_education_plot <- ggplot(mySample_tidy, aes(x = parental.level.of.education)) +
  geom_bar(aes(fill = race.ethnicity), color = "black", show.legend = FALSE) +
  scale_fill_manual(values = wes_palette) +
  ggtitle("Parental level education") +
  xlab("Parental Education") +
  ylab("Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
print(parental_education_plot)

table(mySample_tidy$parental.level.of.education)
(11/190) * 100 # 5.789474
41+37+24+11 #some collage and higher = 113
(113/190) * 100 #percentage with at least some collage

# Plot for 'race.ethnicity' (assumed to be a character variable)
Female_plot <- ggplot(mySample_tidy, aes(x = female)) +
  geom_bar(aes(fill = female), color = "black", show.legend = FALSE) +
  scale_fill_manual(values = wes_palette) +
  ggtitle("Female") +
  xlab("Female") +
  ylab("Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
print(Female_plot)

#calculating percentages
table(mySample_tidy$female)
(93 /190) * 100 #female = 48.94737

combined_plots <- race_plot + parental_education_plot + Female_plot # Combining the plots
combined_plots # printing the combined plot



##logistic and ordinal models

#We now move on to build a logistic regression models, where we predict writing score and reading score
#the variables "writing.score" and "reading.score" have binary outcomes, that is "passed" or "failed".
#since the variables have binary outcomes the logistic regression model is of advantage. 
#the variables test.preparation.course and lunch is the independent variables that we are mostly interested in 
#we will also control for the variables: female, race.ethnicity & parental.level.of.education 



# added after comments: 
#renaming categories to be more informative
mySample_info <- mySample_tidy %>%
  mutate(
    test.preparation.course = ifelse(test.preparation.course == 0, "No Preparation", "Preparation"),
    lunch = ifelse(lunch == 0, "No Lunch", "Lunch"))

mySample_info$lunch <- factor(mySample_info$lunch, levels = c("No Lunch", "Lunch"))

# changing refrence to be more meningful (biggest category from each varible) 
mySample_info$female <- factor(mySample_info$female, levels = c("FALSE", "TRUE")) #recoding to factor to set FALSE as refrerence category

mySample_info$race.ethnicity <- factor(mySample_info$race.ethnicity) # Converting race.ethnicity to a factor
mySample_info$race.ethnicity <- relevel(mySample_info$race.ethnicity, ref = "group C") # Setting "group C" as the reference category 

mySample_info$parental.level.of.education <- factor(mySample_info$parental.level.of.education)# Converting parental.level.of.education to a factor
mySample_info$parental.level.of.education <- relevel(mySample_info$parental.level.of.education, ref = "associate's degree") # Setting "associate's degree" as the reference category 


#we begin by building our writing model
writing_model0 <- glm(writing.score ~ test.preparation.course + lunch, 
                     family = binomial(), data = mySample_info) 

summary(writing_model0) 

#writing model including control varibles
writing_model <- glm(writing.score ~ test.preparation.course + lunch + female + race.ethnicity + parental.level.of.education, 
                     family = binomial(), data = mySample_info) 

summary(writing_model) 

pR2(writing_model0) #McFadden 0.17
pR2(writing_model)  #McFadden 0.67

table(mySample_tidy$writing.score)#185 students passed and 5 failed
#(data is unbalanced which could explain the Warning message: "glm.fit: fitted probabilities numerically 0 or 1 occurred"


#creating a table for writing model
#table has been updated after comments
table_writing <- tbl_regression(
  writing_model,
  exponentiate = FALSE, 
  intercept = TRUE,
  label = list(writing.score = "Writing Score")) %>% 
  add_significance_stars(hide_ci = FALSE, hide_p = FALSE) %>% 
  modify_header(label = "**Variable**", p.value = "**P**") %>%
  modify_caption("**Logistic regression: writing scores predicted by test.preparation.course and lunch** (N = {N})") %>%
  as_gt() %>%
  gt::fmt_number(
    columns = vars(estimate, conf.low, conf.high),
    decimals = 3) %>%
  gt::fmt_number(
    columns = vars(p.value),
    decimals = 4) 

print(table_writing)#printing writing model


#building the reading model
reading_model0 <-  glm(reading.score ~ test.preparation.course + lunch, 	 
                       family = binomial(), data = mySample_info)	

summary(reading_model0)

#reading model including control varibles
reading_model <-  glm(reading.score ~ test.preparation.course + lunch + race.ethnicity + female + parental.level.of.education, 	 
                      family = binomial(), data = mySample_info)	
summary(reading_model) 

#creating a table for reading model
table_reading <- tbl_regression(
  reading_model,
  exponentiate = FALSE, 
  intercept = TRUE,
  label = list(reading.score = "reading.score")) %>% 
  add_significance_stars(hide_ci = FALSE, hide_p = FALSE) %>% 
  modify_header(label = "**Variable**", p.value = "**P**") %>%
  modify_caption("**Logistic regression: reading scores predicted by test.preparation.course and lunch** (N = {N})") %>%
  as_gt() %>%
  gt::fmt_number(
    columns = vars(estimate, conf.low, conf.high),
    decimals = 3) %>%
  gt::fmt_number(
    columns = vars(p.value),
    decimals = 4)

print(table_reading)#printing reading model

#model performance
pR2(reading_model0) #McFadden 0.12
pR2(reading_model)  #McFadden 0.34


#taking a look at the accuracy of the predictions for the writing model

#step1: predicted log-odds for each observation in the dataset 
predict(writing_model)	 


#step1: here we make a function that transforms log-odds to probabillites 
logit2prob <- function(logit){ 
  odds <- exp(logit) 
  prob <- odds / (1 + odds) 
  return(prob) 
} 

#step 3: Predicted probabilities
#combing predicted probabilities with the original dataset in order to determine correct prediction
predictedProbability1 <- logit2prob(predict(writing_model))

mySample_tidy <- cbind(mySample_tidy, predictedProbability1) # Combing predicted probabilities with the original dataset in order to determine correct prediction

#step4: tunning thershold to achive sensetivity of approximately 90%
mySample_tidy_predict <- mySample_tidy %>% 
  mutate(pred_writing_model = predict(writing_model)) %>% 
  mutate(
    pred_writing_model = ifelse(pred_writing_model <= 5, 0, 1), #setting threshold to 5 
    correct_prediction = ifelse(pred_writing_model == writing.score, "correct", "incorrect"))



#step5: analysing accuracy of predictions
#viewing correct categorization overall
mySample_tidy_predict %>%	
  group_by(correct_prediction) %>%	
  summarise(count = n()) %>%	
  mutate(freq = count / sum(count))	
#False negative        17 0.0895
#True negative          5 0.0263
#True positive        168 0.884 

#correctly categorized as passing (sensitivity)
mySample_tidy_predict %>%	
  filter(writing.score == "1") %>% 	
  group_by(correct_prediction) %>%	
  summarise(count = n()) %>%	
  mutate(freq = count / sum(count))	
#true positve = 0.908, sensitivity of 91% in predicting students passing

#correctly categorized as failing (specificity)
mySample_tidy_predict %>%	
  filter(writing.score == "0") %>% 	
  group_by(correct_prediction) %>%	
  summarise(count = n()) %>%	
  mutate(freq = count / sum(count))
#true negative = 1 =specificity of 100% in predicting students failing 

#creating a confusion matrix for summary
mySample_tidy_predict = mySample_tidy_predict %>%	
  mutate(correct_prediction = case_when(writing.score=="0" & pred_writing_model == "0"  ~ "True negative",	
                                        writing.score=="1" & pred_writing_model == "1"  ~ "True positive",
                                        writing.score=="0" & pred_writing_model == "1" ~ "False positive",
                                        writing.score=="1" & pred_writing_model == "0" ~ "False negative"))	

# Lotta: false negatives means that you wrongly predict someone has failed even though they have actually passed; I think you maybe mixed up false positives and false negatives here? 
# Edvin: Corrected false negative and false postive after comment 

conf_matrix <- table(mySample_tidy_predict$correct_prediction, mySample_tidy_predict$writing.score)#confusion matrix
print(conf_matrix) #printing matrix


#step5.1: tunning thershold to achive perfect sensetivity (100%)
mySample_tidy_predict1 <- mySample_tidy_predict %>% 
  mutate(pred_writing_model = predict(writing_model)) %>% 
  mutate(
    pred_writing_model = ifelse(pred_writing_model <= -1, 0, 1),  
    correct_prediction = ifelse(pred_writing_model == writing.score, "correct", "incorrect"))

#step5.1: analysing accuray of predictions
#viewing correct categorization overall
mySample_tidy_predict1 %>%	
  group_by(correct_prediction) %>%	
  summarise(count = n()) %>%	
  mutate(freq = count / sum(count))	
#98%
#False positive         3 0.0158
#True negative          2 0.0105
#True positive        185 0.974 

#correctly categorized as passing (sensitivity)
mySample_tidy_predict1 %>%	
  filter(writing.score == "1") %>% 	
  group_by(correct_prediction) %>%	
  summarise(count = n()) %>% 	
  mutate(freq = count / sum(count))	
#-1 =100% correctly categorized as passing
#correctly categorized as failing

  
#correctly categorized as failing (specificity)
  mySample_tidy_predict1 %>%	
    filter(writing.score == "0") %>% 	
    group_by(correct_prediction) %>%	
    summarise(count = n()) %>%	
    mutate(freq = count / sum(count))
#true negative = 0.4 =specificity of 40% in predicting students failing 


##creating a confusion matrix for summary
  mySample_tidy_predict1 = mySample_tidy_predict1 %>%	
  mutate(correct_prediction = case_when(writing.score=="0" & pred_writing_model == "0"  ~ "True negative",	
                                        writing.score=="1" & pred_writing_model == "1"  ~ "True positive",
                                        writing.score=="0" & pred_writing_model == "1" ~ "False positive",
                                        writing.score=="1" & pred_writing_model == "0" ~ "False negative"))	
# Lotta: s. comment on false negatives and false positives above 
# Edvin: Corrected false negative and false postive after comment 

conf_matrix1 <- table(mySample_tidy_predict1$correct_prediction, mySample_tidy_predict1$writing.score) #confusion matrix
print(conf_matrix1) #printing matrix

# Plotting the confusion matrix for writing model 90% sensitivity
plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
title('CONFUSION MATRIX', cex.main=2)
rect(150, 430, 240, 370, col='darkslategray')
text(195, 435, 'Passed', cex=1.2)
rect(250, 430, 340, 370, col='darkgoldenrod3')
text(295, 435, 'Failed', cex=1.2)

text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
text(245, 450, 'Actual', cex=1.3, font=2)
rect(150, 305, 240, 365, col='darkgoldenrod3')
rect(250, 305, 340, 365, col='darkslategray')
text(140, 400, 'Passed', cex=1.2, srt=90)
text(140, 335, 'Failed', cex=1.2, srt=90)

#Manually adding confusion matrix values
text(195, 400, paste("(TN) =", 5), cex=1.6, font=2, col='black')
text(195, 335, paste("(FN) =", 17), cex=1.6, font=2, col='black')
text(295, 400, paste("(FP) =", 0), cex=1.6, font=2, col='black')
text(295, 335, paste("(TP) =", 168), cex=1.6, font=2, col='black')



#Plotting the confusion matrix with perfect sensitivity (writing model)
plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
title('CONFUSION MATRIX', cex.main=2)
rect(150, 430, 240, 370, col='darkslategray')
text(195, 435, 'Passed', cex=1.2)
rect(250, 430, 340, 370, col='darkgoldenrod3')
text(295, 435, 'Failed', cex=1.2)

text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
text(245, 450, 'Actual', cex=1.3, font=2)
rect(150, 305, 240, 365, col='darkgoldenrod3')
rect(250, 305, 340, 365, col='darkslategray')
text(140, 400, 'Passed', cex=1.2, srt=90)
text(140, 335, 'Failed', cex=1.2, srt=90)

# Manually adding confusion matrix values
text(195, 400, paste("(TN) =", 2), cex=1.6, font=2, col='black')
text(195, 335, paste("(FN) =", 0), cex=1.6, font=2, col='black')
text(295, 400, paste("(FP) =", 3), cex=1.6, font=2, col='black')
text(295, 335, paste("(TP) =", 185), cex=1.6, font=2, col='black')




#We now move on to the ordinal model. Here we will predict math score which is an ordinal variable with ranging from F - A. 
#the variables test.preparation.course and lunch is the independent variables that we are mostly interested in 
#we will also control for the variables: female, race.ethnicity & parental.level.of.education 

#exploratory analysis
mySample_tidy %>% 	 
  ggplot() +	 
  aes(x = math.score, fill = math.score) +	 
  geom_bar() 

table(mySample_tidy$math.score)#grade distribution in numbers

mySample_tidy %>% 	 
  ggplot() +	 
  aes(x = test.preparation.course, fill = math.score) +	 
  geom_bar() 
#students that completed the prep course seem to have performed better than does who did not

mySample_tidy %>% 	 
  ggplot() +	 
  aes(x = lunch, fill = math.score) +	 
  geom_bar() 
#seems like students who recived free/reduced lunch performed worse compared to stundets not reciving free/reduced lunch 

#ordinal model
math_model0 <- clm(math.score ~  test.preparation.course + lunch, data = mySample_info) 

summary(math_model0)
AIC(math_model0)#608.9174

#lunch is significant but prep course is not

plot_model(math_model0)#plotting model

#ordinal model including control variables
math_model <- clm(math.score ~  test.preparation.course + lunch + race.ethnicity + female + parental.level.of.education, data = mySample_info) 
#lunch remains highly significant after controlling for other variables, hence indicating a casual relationship
summary(math_model) 
plot_model(math_model)#plotting model
#grades are not so significantly different from each other 
AIC(math_model)#605.7947, slightly lower indicating that model with control is better


#creating a table for math model
# Lotta: s. comment on labels for table 
table_math <- tbl_regression(
  math_model,
  exponentiate = FALSE, 
  intercept = TRUE,
  label = list(math.score = "reading.score")) %>% 
  add_significance_stars(hide_ci = FALSE, hide_p = FALSE) %>% 
  modify_header(label = "**Variable**", p.value = "**P**") %>%
  modify_caption("**Ordinal regression: math scores predicted by test.preparation.course and lunch** (N = {N})") %>%
  as_gt() %>%
  gt::fmt_number(
    columns = vars(estimate, conf.low, conf.high),
    decimals = 3) %>%
  gt::fmt_number(
    columns = vars(p.value),
    decimals = 4)

print(table_math)

#Making predictions using our math_model 

# Calculating probability for getting higher grades for student with free/reduced lunch
probability <- exp(-1.85380) / (1 + exp(-1.85380))
cat("Probability:", probability, "\n")#print probability
#Probability: 0.1354274 = 13%


#the following is added for the final turn in

#creating a data frame of a typical student that has received lunch
new_data <- data.frame(
  test.preparation.course = "No Preparation",  
  lunch = "Lunch",  
  race.ethnicity = "group C", 
  female = "FALSE",  
  parental.level.of.education = "some college")

# Predicting using model
predicted_probabilities <- predict(math_model, newdata = new_data, type = "prob")
print(predicted_probabilities)
#probability when reciving lunch
#F           E         D         C          B          A
#1 0.2231006 0.241776 0.2935999 0.1801696 0.05066612 0.01068782


#creating a data frame of a typical student that has not recived lunch
new_data1 <- data.frame(
  test.preparation.course = "No Preparation", 
  lunch = "No Lunch",  
  race.ethnicity = "group C",  
  female = "FALSE",  
  parental.level.of.education = "some college")

# Predicting using model
predicted_probabilities1 <- predict(math_model, newdata = new_data1, type = "prob")
print(predicted_probabilities1)
#probability when reciving no lunch
#   F          E         D         C        B          A
#1 0.04304597 0.07673311 0.2099406 0.3758537 0.229908 0.06451868

# visulizing grade prediction for lunch and no lunch
categories <- c('F', 'E', 'D', 'C', 'B', 'A')
probabilities1 <- c(0.04304597, 0.07673311, 0.2099406, 0.3758537, 0.229908, 0.06451868)
probabilities2 <- c(0.2231006, 0.241776, 0.2935999, 0.1801696, 0.05066612, 0.01068782)

#bar plot for predicted_probabilities
par(mfrow = c(1, 2))  
barplot(probabilities1, names.arg = categories, col = 'darkred', main = 'No Lunch', xlab = 'Math Grades', ylab = 'Probabilities')
barplot(probabilities2, names.arg = categories, col = 'aquamarine4', main = 'Lunch', xlab = 'Math Grades', ylab = 'Probabilities')



