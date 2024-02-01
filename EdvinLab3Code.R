#installing needed libraries
install.packages(c("tidyverse", "readr", "wesanderson", "cowplot", "patchwork", "ordinal", "sjPlot",
                   "lme4", "car", "MASS", "pscl", "stargazer", "glmmTMB"))

#loading needed libraries
library(tidyverse)   
library(readr) 
library(wesanderson)
library(corrplot)
library(cowplot)
library(patchwork)
library(ordinal)
library(sjPlot)
library(lme4)
library(car)
library(MASS) 
library(pscl) 
library(stargazer) 
library(glmmTMB)


theme_set(theme_minimal()) #Setting ggplot aesthetic 


setwd("~/lab3")#Setting working directory
setwd("C:/Users/Huawei/Downloads/Lab3")

student_data <- read_csv("studentLab3-1.csv") #Loading dataset

##This dataset includes the following varibles:
# • The grade that a student received at the end of the course. VG: passed with
# distinction, G: passed, U: not passed.
# • The number of homeworks that a student did not hand in. There were 3
# homeworks in total during the course.
# • An identifier for the school at which the student took the course.
# • Studytime: The number of hours that a student spend each day to practice
# for the course.
# • Traveltime: The time in minutes that the student had to spend to travel to
# the course location.
# • Paid: A binary variable coding whether a student payed extra money to have
# an additional private teacher.
#• other controls: age (in years), sex (male vs other)


#loking at dataset
view(student_data)
str(student_data)
head(student_data)
tail(student_data)     
summary(student_data)  


#Looking at individual varibles

#grade
table(student_data$grade) #three leveles: (u,vg, vg) #NA identifed
sort(student_data$grade) 
class(student_data$grade) #character

#homeworks
table(student_data$homeworks) #four ctegories: 0,1,2,3 #high number for 0 homworks (451)
sort(student_data$homeworks)
class(student_data$homeworks) #numeric

#school
table(student_data$school) ##three categories: AD, GP, MS
sort(student_data$school)
class(student_data$school) #character

#sex
table(student_data$sex) #three categories: F, M, O
class(student_data$sex) #character


#age
table(student_data$age) #rangning from 15-22, also 2 cases of age 0 (this will have to be dealt with)
summary(student_data$age) #median age is 17
sort(student_data$age) 
class(student_data$age) #numeric


#studytime
table(student_data$studytime) #four categories: 1,2,3,4
summary(student_data$studytime) #median study time is 2 hours
sort(student_data$studytime)
class(student_data$studytime) #numeric


#traveltime
table(student_data$traveltime) #four categories: 1,2,3,4
summary(student_data$traveltime) #median studytime 1 hour
sort(student_data$traveltime)
class(student_data$traveltime) #numeric

#paid
table(student_data$paid) #two categories: no, yes 
sort(student_data$paid)
class(student_data$paid) #character

#Detecting NA:s
NAs <- sapply(student_data, function(x) sum(is.na(x)))
print(NAs) #NAs detected for: grade:1, school:2, sex:1, study time:1, travel time:1, paid1

student_data <- na.omit(student_data) #NAs are removed due to it being spread out evenly across the data and not having a major impact


#cleaning data
student_data.1 <- student_data %>%
  filter(age != 0) %>% #dealing with observations of age 0
  mutate(sex = fct_collapse(sex, other = c("F", "O"), male = c("M"))) %>%  #collapsing sex to two categories due given information
  mutate(grade = fct_collapse(grade, "1" = "u", "2" = "g", "3" = "vg")) %>%  #re-coding to simplify interpretation 
  mutate(grade = as.factor(grade)) %>% #setting as factor
  mutate(grade = relevel(grade, ref = "1")) #setting reference category for grade to 1


#controling clean data
NAs <- sapply(student_data.1, function(x) sum(is.na(x)))
print(NAs) #0 NAs

str(student_data.1)
summary(student_data.1)


# Task 1: Present the controls of your sample per school

# Age-School plot with median age 

median_age_by_school <- aggregate(age ~ school, data = student_data.1, FUN = median) # Calculate median age by school

age_school_plot <- ggplot(student_data.1, aes(x = age, fill = school)) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.7) +
  geom_vline(data = median_age_by_school, aes(xintercept = age, color = school),
             linetype = "dashed", size = 1) +
  geom_text(data = median_age_by_school, aes(x = age, y = 0, label = sprintf("%.1f", age),
                                             color = school), vjust = -43, hjust = -0.4, size = 3) +  
  labs(title = "Students Age by School",
       x = "Age",
       y = "Frequency") +
  scale_fill_manual(values = c("darkslategrey", "darkred", "darkgoldenrod3")) +
  scale_color_manual(values = c("darkslategrey", "darkred", "darkgoldenrod3")) +  
  facet_wrap(~school, scales = "free")

print(age_school_plot)

# Plot for school and sex with percentages
school_plot <- ggplot(student_data.1, aes(x = school)) +
  geom_bar(aes(fill = sex), position = "dodge", show.legend = TRUE, alpha = 0.7) +
  geom_text(stat = "count", aes(label = scales::percent(..prop..), group = sex),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("darkolivegreen4", "darkslateblue")) +
  ggtitle("Students Sex by School") +
  xlab("School") +
  ylab("Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the plot
print(school_plot)

combined_plot <- age_school_plot + school_plot # Combining the plots
print(combined_plot) #printing combined plots


# Task 2: With the help of graphs, explore whether there is a relationship between study-
# time and grade.

#jitter plot 
jitter_plot <- ggplot(student_data.1, aes(x = grade, y = studytime, color = grade)) +
  geom_jitter(position = position_jitter(width = 0.2), size = 3, alpha = 0.7) +
  labs(title = "Relationship Between Study Time and Grade",
       x = "Grade",
       y = "Study Time") +
  scale_color_manual(values = c("darkblue", "darkred", "darkorange4")) +
  theme_minimal() +
  theme(legend.position = "none") 

#mean study time for each grade
means_data <- student_data.1 %>%
  group_by(grade) %>%
  summarise(mean_studytime = mean(studytime))

#barplot
bar_plot <- ggplot(means_data, aes(x = factor(grade), y = mean_studytime, fill = factor(grade))) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
  labs(title = "Mean Study Time Differences Between Grades",
       x = "Grade",
       y = "Mean Study Time") +
  scale_fill_manual(values = c("darkblue", "darkred", "darkorange4")) +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_text(aes(label = sprintf("%.2f", mean_studytime), y = mean_studytime + 0.1), vjust = 0)

# Combine plots
combined_plot2 <- jitter_plot + bar_plot

# Print or save the combined plot
print(combined_plot2)



# Task 3: With the help of graphs, explore whether there is a relationship between paying
# for an additional teacher and grade. (5 points)

# Create a data frame with proportions
proportions_data <- student_data.1 %>%
  group_by(paid, grade) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))

# Grouped bar plot with proportions
ggplot(proportions_data, aes(x = factor(paid), y = proportion, fill = grade)) +
  geom_bar(position = "dodge", stat = "identity", alpha = 0.7) +
  labs(title = "Relationship Between Paying for Additional Teacher and Grade",
       x = "Paying for Additional Teacher",
       y = "Proportion") +
  scale_fill_manual(values = c("darkblue", "darkred", "darkorange4")) +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank()) +
  geom_text(aes(label = scales::percent(proportion), group = grade),
            position = position_dodge(width = 0.9), vjust = -0.5)

#keep in mind that this is proportions. Only 39 students paid for additional teacher.

# Task 5: Present adequate (one or multiple) statistical models 
#analyzing the relationship between study time and grade.

# First we standardize study time and travel time
student_data.1$studytimeSt = scale(student_data.1$studytime, center = TRUE, scale = TRUE)
student_data.1$traveltimeSt = scale(student_data.1$traveltime, center = TRUE, scale = TRUE)

#seeing if there are corelations between numrical varible, hence if multicolinarity can be a problem
numerical_data <- student_data.1 %>%
  select_if(is.numeric)

cor(numerical_data)#looks all good

#ordinal model including controls
ordinal_model1 <- clm(grade ~ studytimeSt + age + sex + traveltimeSt + paid + school , data = student_data.1)
summary(ordinal_model1) #paid not significant
AIC(ordinal_model1) #1089.741

#parismonius ordinal model excluding paid
ordinal_model2 <- clm(grade ~ studytimeSt + age + sex + traveltimeSt + school , data = student_data.1)
summary(ordinal_model2)
AIC(ordinal_model2) #1087.749 - indcating model is sligthly better


#ordinal model, we also include random intercept for school because of the 
ordinal_model3 <- clmm(grade ~ studytimeSt + age + sex + traveltimeSt + (1|school) , data = student_data.1)
summary(ordinal_model3)
anova(ordinal_model2, ordinal_model3) 
#model with random intercept (ordinal_model3) 
#has a significantly better fit than ordinal_model2

anova(ordinal_model2, ordinal_model3)
#since both the independence assumption and fit of the model speak in favor of a mixed regression we continue with model3

#we think that traveltime might be interacting with study time
#we therefore add and interaction term
ordinal_model4 <- clmm(grade ~ studytimeSt * traveltimeSt + age + sex + (1 | school), data = student_data.1)
summary(ordinal_model4) #interaction not significant, therefore we chose ordinal_model3 as our final model


#checking whether the assumption of normally distributed random effects is valid
plot_model(ordinal_model3, type = "re") 

qqnorm(ranef(ordinal_model3)$school[, "(Intercept)"])
qqline(ranef(ordinal_model3)$school[, "(Intercept)"]) 
#deviates at the tails which is problematic but few numbers of schools makes it hard to asses 

#Table and plot for chosen model
sjPlot::tab_model(ordinal_model3)

plot_model(ordinal_model3, type = "pred", terms = c("studytimeSt")) #plotting probabilites


#task 8: making prediction on grade for male student age 20

#coefficients
coef_studytime <- 0.41341
coef_age <- -0.24232
coef_sexmale <- -0.40564
coef_traveltime <- -0.20677
thresh_1_2 <- -5.232
thresh_2_3 <- -1.927

#mean values
mean_studytime <- 0
mean_traveltime <- 0
  
# Age and sex for prediction
age <- 20
sexmale <- 1  

# Calculate linear predictor
linear_predictor <- coef_studytime * mean_studytime + coef_age * age + 
  coef_sexmale * sexmale + coef_traveltime * mean_traveltime

# Calculate cumulative probabilities
prob_1 <- plogis(thresh_1_2 - linear_predictor)
prob_2 <- plogis(thresh_2_3 - linear_predictor) - prob_1
prob_3 <- 1 - plogis(thresh_2_3 - linear_predictor)

# Display results
cat("Predicted Probabilities for Grades:\n")
cat("Grade U:", prob_1, "\n") #U: 0.5050098  
cat("Grade G:", prob_2, "\n") #G: 0.4602681  
cat("Grade VG:", prob_3, "\n")#VG: 0.03472209



# Task 9: Present Poisson regression model to predict the number of homeworks that someone submitted 

#Poisson regression model studytime
poisson_model1 <- glm(homeworks ~ studytime, data = student_data.1, family = poisson) 
summary(poisson_model1) #studytime is significant

#Poisson regression model traveltime
poisson_model2 <- glm(homeworks ~ traveltime, data = student_data.1, family = poisson) 
summary(poisson_model2) #travel time is significant

#Poisson regression model paid
poisson_model3 <- glm(homeworks ~ paid, data = student_data.1, family = poisson) 
summary(poisson_model3) #paid is not signifcant

#Posion model inclduing controls and random intercept for school
poisson_model4 <- glmer(homeworks ~ studytimeSt + traveltimeSt + age + sex + (1 | school), data = student_data.1, family = poisson)
summary(poisson_model4) 


#Making predictions using poisson_model4

# Create a new data frame with the specific values you want to predict for
TypicalMale20 <- data.frame(
  studytimeSt = mean(student_data.1$studytimeSt),  
  traveltimeSt = mean(student_data.1$traveltimeSt),  
  age = 20,
  sex = "male",
  school = "GP")

predicted_homeworks <- predict(poisson_model4, newdata = TypicalMale20, type = "response") #making prediction on new data

cat("Predicted number of turned-in homeworks for a male student aged 20:", round(predicted_homeworks)) # print predicted number of homworks


#Table for poisson_model4
sjPlot::tab_model(poisson_model4)

#overdispersion
mean(student_data.1$homeworks) #mean = 0.6143079
var(student_data.1$homeworks)  #variance = 1.112692
#variance is higher then mean, indicating over-dispersion in model
#in exploration of data set we also found a lot of zeros in homework, hence zero-inflation 

#building models which considers zero-inflation and over dispersion

#negative binomial model: dealing with over dispersion
NegBinomialM <- glm.nb(homeworks ~ studytimeSt + traveltimeSt + paid + age + sex,
                   data = student_data.1)
summary(NegBinomialM)

#zero-inflation model dealing with excess zeros
ZeroInfMp <- zeroinfl(homeworks ~ studytimeSt + traveltimeSt + paid + age + sex | studytime + traveltime + paid + age + sex,
                      data = student_data.1, link = "logit", dist = "poisson", trace = TRUE)
summary(ZeroInfMp)

#zero-inflated negative binomial model: dealing with both over dispersion and zero-inflation
ZeroInfMpn <- zeroinfl(homeworks ~ studytimeSt + traveltimeSt + paid + age + sex | studytime + traveltime + paid + age + sex,
data = student_data.1, link = "logit", dist = "negbin", trace = TRUE, EM = FALSE)
summary(ZeroInfMpn)


# Comparing models using stargazer
stargazer(NegBinomialM, ZeroInfMp, ZeroInfMpn, type = "text")

