#installing needed libraries
install.packages(c("pscl", "ordinal", "sjPlot", "lmtest","lmerTest", "tidyverse", "gtsummary",
                   "readr", "viridis", "pROC", "corrplot", "RColorBrewer", "patchwork", "lme4", "vtable", "car"))

#loading needed libraries
library(pscl)      
library(ordinal) 
library(sjPlot) 
library(lmtest)
library(lmerTest)
library(tidyverse)   
library(gtsummary) 
library(readr) 
library(viridis) 
library(pROC)
library(corrplot)
library(RColorBrewer)
library(patchwork)
library(lme4)
library(vtable)
library(wesanderson)
library(car)

theme_set(theme_minimal()) #Setting ggplot aesthetic 


setwd("~/Lab2Code") #Setting working directory

#Loading data 
dt <- read.csv("dataLab2-1.Rdata.csv")
#

#loking at dataset
view(dtA)
view(dtB)
str(dtA)
str(dtB)

#we combining datasets due to original dataset being divided into two
dt <- rbind(dtA, dtB)

#looking at our combined dataset
str(dt)
head(dt)
tail(dt)

summary(dt)

#looking at individual variables

#ID          
dt$ID
unique(dt$ID) 
sort(dt$ID) #all of our ID numbers are supposed to be uniqe but this seems not to be the case
            
#pain
dt$pain
unique(dt$pain)
sort(dt$pain)
class(dt$pain)

#sex
dt$sex
unique(dt$sex)  #we have two categories for female: female and women, this needs to be fixed
class(dt$sex)

#age
dt$age
unique(dt$age)
sort(dt$age)
class(dt$age)

#stai_trait
dt$STAI_trait 
unique(dt$STAI_trait)
sort(dt$STAI_trait)
class(dt$STAI_trait)

#pain_cat
dt$pain_cat 
unique(dt$pain_cat)
sort(dt$pain_cat)
class(dt$pain_cat)

#cortisol_serum
dt$cortisol_serum 
unique(dt$cortisol_serum)
sort(dt$cortisol_serum)
class(dt$cortisol_serum)

#cortisol_saliva
dt$cortisol_saliva 
unique(dt$cortisol_saliva)
sort(dt$cortisol_saliva)
class(dt$cortisol_saliva)

#mindfulness
dt$mindfulness 
unique(dt$mindfulness)
sort(dt$mindfulness)
class(dt$mindfulness)

#weight
dt$weight           
unique(dt$weight)
sort(dt$weight)
class(dt$weight)

#IQ
dt$IQ           
unique(dt$IQ)  
sort(dt$IQ)
class(dt$IQ)

#household_income
dt$household_income           
unique(dt$household_income)
sort(dt$household_income)  #negative income detected
class(dt$household_income)

#hospital
dt$hospital           
unique(dt$hospital) 
sort(dt$hospital)  
class(dt$hospital)   
is.ordered(dt$hospital)

#Detecting NA:s
sum(is.na(dt))


#dealing with problems detected in exploration of dataset
dt1 <- dt %>%
  mutate(household_income = if_else(household_income < 0, NA_integer_, household_income)) %>%   #dealing with negative income
  mutate(sex = fct_collapse(sex, "female" = c("female", "woman"), "male" = c("male"))) %>%      #dealing with extra value for female
  mutate(ID = as.factor(1:400)) %>%                                                             #dealing with IDs not being unique
  mutate(hospital = as.factor(hospital)) %>%                                                    #converting hospital to factor
  na.omit()


#looking at tidy dataset
str(dt1)
summary(dt1)


#Task 1. descriptives of "other controls": (age, weight, IQ, income, sex)

summary(dt1$age) 
sd(dt1$age) 
#sd: 4.718711
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#26.0    37.0    40.0    40.2    43.0    53.0  
age_plot <- ggplot(dt1, aes(x = age)) +
  geom_histogram(fill = "skyblue4", color = "black", bins = 30) +
  geom_vline(aes(xintercept = mean(age)), color = "black", linetype = "dotted", size = 1) +  
  geom_text(aes(x = mean(age), label = sprintf("Mean: %.2f", mean(age))),
            y = 0, vjust = -21.6, hjust = -0.1, color = "black", size = 3) +  
  ggtitle("Age Distribution") +
  xlab("Age") +
  ylab("Frequency")

age_plot #print plot

summary(dt1$weight) 
sd(dt1$weight) 
#sd: 10.08415
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#43.20   63.25   70.50   69.90   77.20   97.50
weight_plot <- ggplot(dt1, aes(x = weight)) +
  geom_histogram(fill = "darkolivegreen", color = "black", bins = 30) +
  geom_vline(aes(xintercept = mean(weight)), color = "black", linetype = "dotted", size = 1) +  
  geom_text(aes(x = mean(weight), label = sprintf("Mean: %.2f", mean(weight))),
            y = 0, vjust = -21.6, hjust = -0.1, color = "black", size = 3) +  
  ggtitle("Weight Distribution") +
  xlab("Weight") +
  ylab("Frequency")

weight_plot #print plot

summary(dt1$household_income)
sd(dt1$household_income) 
#sd: 25249.23
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#10745   52253   71987   71565   89681  143809
income_plot <- ggplot(dt1, aes(x = household_income)) +
  geom_histogram(fill = "coral3", color = "black", bins = 30) +
  geom_vline(aes(xintercept = mean(household_income)), color = "black", linetype = "dotted", size = 1) +
  geom_text(aes(x = mean(household_income), label = sprintf("Mean: %.2f", mean(household_income))),
            y = 0, vjust = -21.6, hjust = -0.1, color = "black", size = 3) +  
  ggtitle("household_income Distribution") +
  xlab("Household Income") +
  ylab("Frequency")

income_plot #print plot


summary(dt1$IQ)
sd(dt1$IQ)
#sd:14.44134 
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#60.0    89.0   100.0    99.6   110.0   141.0
IQ_plot <- ggplot(dt1, aes(x = IQ)) +
  geom_histogram(fill = "darkgoldenrod3", color = "black", bins = 30) +
  geom_vline(aes(xintercept = mean(IQ)), color = "black", linetype = "dotted", size = 1) +
  geom_text(aes(x = mean(IQ), label = sprintf("Mean: %.2f", mean(IQ))),
            y = 0, vjust = -21.6, hjust = -0.1, color = "black", size = 3) +
  ggtitle("IQ Distribution") +
  xlab("IQ") +
  ylab("Frequency")

print(IQ_plot) # Print the plot

# Combining plots
combined_plots <- age_plot + weight_plot + income_plot +
  IQ_plot + plot_layout(ncol = 2)

# Print the combined plot
print(combined_plots)

str(dt1)

table(dt1$sex) 
#female   male 
#98    201     
sex_plot <- ggplot(dt1, aes(x = sex, fill = sex)) +
  geom_bar() +
  ggtitle("Sex Distribution") +
  xlab("Sex") +
  ylab("Count") +
  scale_fill_manual(values = c("male" = "darkslategrey", "female" = "burlywood"))  

print(sex_plot)



#task 2: present graphs or statistics that show whether there is clustering in the data

#boxplot shwoing that there exists clustering in data
dt1 %>%
  ggplot(aes(x = pain, y = hospital, fill = hospital)) +                      
  geom_boxplot() +
  labs(title = "Boxplots of Median Pain by Hospital",
       x = "Median Pain", 
       y = "") +
  theme_minimal() +
  scale_fill_manual(values = wes_palette("Cavalcanti1", n = 20, type = "continuous")) +
  theme(axis.text.y = element_blank())  




#task 3: Analyze the correlations between all numerical variables
#correlations

# Selecting numerical varibles and renaming to fit in correlation plot
dt_num <- dt1  %>%
  select(STAI.T = STAI_trait,
         Pain.c = pain_cat,
         Serum = cortisol_serum,
         Saliva = cortisol_saliva,
         Pain = pain,
         Inco = household_income,
         minf = mindfulness,
         IQ = IQ,
         Weight = weight,
         Age = age)

cormet <- cor(dt_num)  # Calculating correlation matrix

cormet  #looking at correlations

#plotting correlations
corrplot(cormet, method = 'square', order = 'AOE', addCoef.col = 'black', tl.pos = 'd',
         cl.pos = 'n', tl.col = 'black', tl.cex = 1, col = wes_palette("Cavalcanti1", n = 20, type = "continuous"),
         title = "Correlation Plot", title.col = "black", mar = c(0, 0, 3, 0))

#high correlation between cortisol saliva and cortsiol serum




#task 4: A regression table with all established findings regarding pain perception is tested

#linnear model inclduing estabilshed findings
mod1 <- lm(pain ~ STAI_trait + mindfulness + pain_cat + age + sex + cortisol_serum + cortisol_saliva,
           data = dt1)

summary(mod1)  


vif(mod1) #multicollineraty: values over 5 between cortisol serum and cortisol saliva as expected since they where highly correlated


#having both serum and saliva leads to problems with multicollineraty as seen by vif values
#altough cortisol serum is often regarded in medical research as more reliably related to stress we exclude it since 
#in our anlysis cortisol saliva was more signficant, indcating that it is more assocatied with our otcome varible

#model 2: random intercept model
#hospital set as random effect due to identified clustering in the data
#stai_trait, mindfulness, pain_cat and cortisol_saliva are the predictors, age and sex are the control varibles.
mod2 <- lmer(pain ~ STAI_trait + mindfulness + pain_cat + cortisol_saliva + (1|hospital) + age + sex,
             data = dt1)
             
summary(mod2)


vif(mod2) # good, vif values are now all under 5

#model3 (random slope model)
mod3 <- lmer(pain ~ STAI_trait + mindfulness + pain_cat + cortisol_saliva + (STAI_trait|hospital) + age + sex,
             data = dt1,
             control = lmerControl(optimizer = "Nelder_Mead"))
summary(mod3)


#comparing models 
anova(mod2, mod3) #p-value (0.846), hence there is no significant difference
#model 2 is therefore chosen as our final model to present


#making a summary table for chosen model
sjPlot::tab_model(mod2)



#Diagnostics

#Q-Q plot
qqnorm(residuals(mod2)) 

#checking for hetroscedasiticity
plot(residuals(mod2) ~ fitted(mod2), main = "Residuals vs Fitted", ylab = "Residuals")

#checking cooks distance
infl <- influence(mod2, obs = TRUE) # Computing influence statistics
cooks_distances <- cooks.distance(infl) # Extract Cook's distances
plot(cooks_distances, main = "Cook's Distance Plot") # Creating plot
#!this may take some time to compute


#Task 6: To what extent are the residuals and random effects normally distributed?

#Residuals
qplot(fitted.values(mod2), residuals(mod2)) + geom_smooth(method = "lm", se = F) #no paterns, randomly scatted and no systamtic trends detected
qqnorm(residuals(mod2)) #Q-Q plot for residuals
#almost follows a straight line

# Extract residuals from the model
residuals_mod2 <- residuals(mod2)
# Shapiro-Wilk test on residuals
shapiro.test(residuals_mod2) #p = 0.31, thus fails to reject the null hypothesis that the data follows a normal distribution

#diagnostics seems to indicate that residuals are normally distributed

#Random effect
random_effects <- ranef(mod2) 

hist(random_effects$hospital[, "(Intercept)"], main = "Histogram of Random Effects")
#not normaly distributed and not symmetric, which suggests non-normality or influential outliers 

qqnorm(random_effects$hospital[, "(Intercept)"])
qqline(random_effects$hospital[, "(Intercept)"])  #Q-Q plot for random effects 
#follows the line, but deviates at the tails which indicates departure from normality

#cooks distance for random effects
cooks_distances_random <- cooks.distance(mod2, group = "hospital") #calculating Cook's distance for random effects
print(cooks_distances_random) #print 
plot(cooks_distances_random, main = "Cook's Distance for Random Effects") #plot
#some value at almost 0.06, indicating that there exist influential outliers in our model
#which therefore may have a significant influence on the estimated random effects


#task 9: Predict with the regression model what the perceived pain of a
#typical male age 30 would be

# Creating a data frame for a typical male age 30 using mean values
typicalMale30 <- data.frame(
  STAI_trait = mean(dt1$STAI_trait),
  mindfulness = mean(dt1$mindfulness),
  pain_cat = mean(dt1$pain_cat),
  cortisol_saliva = mean(dt1$cortisol_saliva),
  age = 30,
  sex = "male")

summary(typicalMale30)
#STAI_trait     mindfulness       pain_cat     cortisol_saliva      
#Mean   :39.75   Mean   :2.959   Mean   :30.11   Mean   :4.989  

# Prediction of perceived pain for a typical male age 30 
predict(mod2, newdata = typicalMale30, re.form = NA)  #re.form = NA is used to make prediction without specifying a specific hospital
#our model predicts that typical male age 30 have a self-perceived pain of 5.4

