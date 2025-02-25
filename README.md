# Sleep-and-Health-data-analysis-and-modelling
R, multiple linear regression, hypothesis testing

# Projet Raport

Marcel Kapica 50061 Statistics for ML Final Project Report

1. Objective Description
The objective of this analysis is to pHYPERLINK "https://www.linkedin.com/in/marcel-kapica-30b016334/", sleep duration, sleep disorders, and dietary habits. The goal is to gain insights into which factors contribute most significantly to sleep quality and develop a predictive model using multiple linear regression.

2. Dataset Selection
The dataset used in this analysis is Health and Sleep Statistics, obtained from Kaggle (https://www.kaggle.com/datasets/hanaksoy/health-and-sleep-statistics). This artificially made dataset contains a combination of categorical and continuous variables related to sleep and health behaviors.

3. Literature Review (Non-Obligatory)
Research shows that physical activity, diet, and sleep disorders significantly impact sleep quality. Regular exercise (≥150 minutes per week) improves sleep efficiency and reduces sleep onset time, though intense workouts before bed may disrupt sleep (ScienceDaily).
Diet also plays a role—high fiber intake promotes deep sleep, while excessive saturated fats and sugar correlate with poorer sleep (PubMed). Sleep disorders, such as insomnia and sleep apnea, severely impair sleep quality but can be mitigated through lifestyle changes and medical interventions (Sleep Foundation).

4. Dependent Variable
- Sleep Quality: The target variable represents the perceived sleep quality on a discrete scale (4-9). Although it is a discrete variable, the assignment didn’t specify multinomial model so it will be treated as continuous when predicting.

5. Independent Variables
- Daily Steps (continuous - Ratio) – A key indicator of physical activity.
- Sleep Duration (continuous - Ratio) – Derived from bedtime and wake-up time.
- Sleep Disorders (categorical – binary Ordinal) – Affects sleep quality.
- Dietary Habits (categorical – Ordinal) – Potential influence on sleep patterns.

Excluded variables:
- Age – Highly correlated with Daily Steps but less informative for predicting Sleep Quality.
- Bedtime Minutes – Replaced with Sleep Duration, which provides more comprehensive information.
- Wakeup Minutes – Low correlation with Sleep Quality
- Gender – Highly associated with Daily Steps but does not add predictive power beyond chosen variables.
- Calories Burned – Correlated with Daily Steps, adding redundancy.
- Medication Usage – Highly associated with Sleep Disorders, making it unnecessary as an additional predictor.

6. Exploratory Data Analysis (EDA)
- Described in R file comments

7. Model Selection
- Multiple Linear Regression is chosen because multinomial model which would match this data better wasn’t included in the assignment description. Continuous predictions are then converted (rounded) to discrete numbers in order to get accuracy.
- Assumptions checked:
  - Multicollinearity (VIF test) – High multicollinearity is observed, because of the characteristics of dataset, but removing variables decreases model performance.
  - Homoscedasticity and linearity validated visually.
  - Normality of residuals assessed using Q-Q plots.

8. Predictor Selection
Selected predictors based on EDA and statistical tests:
1. Daily Steps – Strongest predictor of sleep quality.
2. Sleep Duration – Strong predictor, not highly correlated with Daily Steps.
3. Sleep Disorders – Statistically significant impact on Sleep Quality.
4. Dietary Habits – Statistically significant and contributes additional predictive power.

9. Model Fitting and Interpretation
Final Regression Model:

 Sleep Quality = 1.93 + (0.00039 * Daily Steps) + (0.31 * Sleep Duration) - (0.41 * Sleep Disorder) + (0.43 * Dietary Habit_{medium}) + (0.60 * Dietary Habit_{high})

- R² = 97%, indicating that the model explains 97% of the variance in Sleep Quality.
- All predictors are statistically significant.
- Multicollinearity (VIF test): Some correlation exists, but excluding predictors reduces model accuracy.

10. Result Visualization
- Line plots to compare continous predictors
- Bar plots to compare means of categorical predictors.
- Regression diagnostics to assess residuals and model assumptions.

Model Performance Metrics (these are not super accurate because of discrete target values) :
- Mean Absolute Error (MAE): 0.21 (low average prediction error) 
- Mean Squared Error (MSE): 0.21 (model does not make huge errors)
- Accuracy after rounding predictions to discrete values: 94%

 Conclusion
- Daily Steps and Sleep Duration are the most influential predictors of Sleep Quality.
- Sleep Disorders and Dietary Habits also contribute significantly.
- The model provides strong predictive accuracy, suggesting that promoting physical activity and maintaining good sleep hygiene can improve sleep quality.
- My analysis, although on fabricated data supports previous research.
- Further improvements could involve Principal Component Analysis (PCA) to address multicollinearity and trying multinomial model.


Marcel Kapica 50061 Statistics for ML Final Project Report

1. Objective Description
The objective of this analysis is to pHYPERLINK "https://www.linkedin.com/in/marcel-kapica-30b016334/", sleep duration, sleep disorders, and dietary habits. The goal is to gain insights into which factors contribute most significantly to sleep quality and develop a predictive model using multiple linear regression.

2. Dataset Selection
The dataset used in this analysis is Health and Sleep Statistics, obtained from Kaggle (https://www.kaggle.com/datasets/hanaksoy/health-and-sleep-statistics). This artificially made dataset contains a combination of categorical and continuous variables related to sleep and health behaviors.

3. Literature Review (Non-Obligatory)
Research shows that physical activity, diet, and sleep disorders significantly impact sleep quality. Regular exercise (≥150 minutes per week) improves sleep efficiency and reduces sleep onset time, though intense workouts before bed may disrupt sleep (ScienceDaily).
Diet also plays a role—high fiber intake promotes deep sleep, while excessive saturated fats and sugar correlate with poorer sleep (PubMed). Sleep disorders, such as insomnia and sleep apnea, severely impair sleep quality but can be mitigated through lifestyle changes and medical interventions (Sleep Foundation).

4. Dependent Variable
- Sleep Quality: The target variable represents the perceived sleep quality on a discrete scale (4-9). Although it is a discrete variable, the assignment didn’t specify multinomial model so it will be treated as continuous when predicting.

5. Independent Variables
- Daily Steps (continuous - Ratio) – A key indicator of physical activity.
- Sleep Duration (continuous - Ratio) – Derived from bedtime and wake-up time.
- Sleep Disorders (categorical – binary Ordinal) – Affects sleep quality.
- Dietary Habits (categorical – Ordinal) – Potential influence on sleep patterns.

Excluded variables:
- Age – Highly correlated with Daily Steps but less informative for predicting Sleep Quality.
- Bedtime Minutes – Replaced with Sleep Duration, which provides more comprehensive information.
- Wakeup Minutes – Low correlation with Sleep Quality
- Gender – Highly associated with Daily Steps but does not add predictive power beyond chosen variables.
- Calories Burned – Correlated with Daily Steps, adding redundancy.
- Medication Usage – Highly associated with Sleep Disorders, making it unnecessary as an additional predictor.

6. Exploratory Data Analysis (EDA)
- Described in R file comments

7. Model Selection
- Multiple Linear Regression is chosen because multinomial model which would match this data better wasn’t included in the assignment description. Continuous predictions are then converted (rounded) to discrete numbers in order to get accuracy.
- Assumptions checked:
  - Multicollinearity (VIF test) – High multicollinearity is observed, because of the characteristics of dataset, but removing variables decreases model performance.
  - Homoscedasticity and linearity validated visually.
  - Normality of residuals assessed using Q-Q plots.

8. Predictor Selection
Selected predictors based on EDA and statistical tests:
1. Daily Steps – Strongest predictor of sleep quality.
2. Sleep Duration – Strong predictor, not highly correlated with Daily Steps.
3. Sleep Disorders – Statistically significant impact on Sleep Quality.
4. Dietary Habits – Statistically significant and contributes additional predictive power.

9. Model Fitting and Interpretation
Final Regression Model:

 Sleep Quality = 1.93 + (0.00039 * Daily Steps) + (0.31 * Sleep Duration) - (0.41 * Sleep Disorder) + (0.43 * Dietary Habit_{medium}) + (0.60 * Dietary Habit_{high})

- R² = 97%, indicating that the model explains 97% of the variance in Sleep Quality.
- All predictors are statistically significant.
- Multicollinearity (VIF test): Some correlation exists, but excluding predictors reduces model accuracy.

10. Result Visualization
- Line plots to compare continous predictors
- Bar plots to compare means of categorical predictors.
- Regression diagnostics to assess residuals and model assumptions.

Model Performance Metrics (these are not super accurate because of discrete target values) :
- Mean Absolute Error (MAE): 0.21 (low average prediction error) 
- Mean Squared Error (MSE): 0.21 (model does not make huge errors)
- Accuracy after rounding predictions to discrete values: 94%

 Conclusion
- Daily Steps and Sleep Duration are the most influential predictors of Sleep Quality.
- Sleep Disorders and Dietary Habits also contribute significantly.
- The model provides strong predictive accuracy, suggesting that promoting physical activity and maintaining good sleep hygiene can improve sleep quality.
- My analysis, although on fabricated data supports previous research.
- Further improvements could involve Principal Component Analysis (PCA) to address multicollinearity and trying multinomial model.


# Code

install.packages("dplyr")
library(dplyr)
library(gplots)
library(lubridate)
library(ggplot2)
library(car)



df <- read.csv("/Users/marcelkapica/Downloads/Health_Sleep_Statistics.csv", stringsAsFactors = TRUE)# load data
#i chose this dataset because it has a nice combination of categorical and continuous variables that are related.
#so i figured it will be good for learning the most from this assignment
#this is artificially created data, but i will treat it as real in order to provide analysis this assignment requires
#source :https://www.kaggle.com/datasets/hanaksoy/health-and-sleep-statistics

#I've always had troubles falling asleep so i thought this is my area of interest

#I will be predicting sleep quality, as the goal is to have a better sleep, not for example longer sleep
#Sleep quality column has discrete values but we will predict continuous as the assignment dont include multinomial model

#create a column with Sleep_Duration from bedtime and wake up time as i think it might be useful for prediction of Sleep.Quality
df <- df %>%
  mutate(
    Bedtime = as.POSIXct(Bedtime, format = "%H:%M"),  # Convert Bedtime to datetime
    Wake.up.Time = as.POSIXct(Wake.up.Time, format = "%H:%M"),  # Convert Wake Time to datetime
    Sleep_Duration = as.numeric(difftime(
      if_else(Wake.up.Time < Bedtime, Wake.up.Time + ddays(1), Wake.up.Time), 
      Bedtime, 
      units = "hours"
    ))
  )

#create columns with bedtime and wake up time in minutes so the model can process it
df <- df %>%
  mutate(
    Bedtime = as.POSIXct(Bedtime, format = "%H:%M"), 
    Wake.up.Time = as.POSIXct(Wake.up.Time, format = "%H:%M"),
    
    Bedtime_Minutes = hour(Bedtime) * 60 + minute(Bedtime),
    Bedtime_Minutes = ifelse(Bedtime_Minutes < 720, Bedtime_Minutes + 1440, Bedtime_Minutes), 
    Wakeup_Minutes = hour(Wake.up.Time) * 60 + minute(Wake.up.Time),
  )



#change categorical columns to factors for model processing and ordered categories on graphs (just to make sure, i know there was stringsasfactors before, sometimes it still dont work)
df$Gender <- factor(df$Gender, levels = c("m", "f"))
df$Physical.Activity.Level <- factor(df$Physical.Activity.Level, levels = c("low", "medium", "high"))
df$Dietary.Habits <- factor(df$Dietary.Habits, levels = c("unhealthy", "medium", "healthy"))
df$Sleep.Disorders <- factor(df$Sleep.Disorders, levels = c("no", "yes"))
df$Medication.Usage <- factor(df$Medication.Usage, levels = c("no", "yes"))




#Dependent variable - Sleep Quality
#Frequency plot as values in Sleep.Quality are discrete
ggplot(df, aes(x=Sleep.Quality)) +
  geom_histogram( fill="lightblue", color="black") +
  theme_minimal()

df %>%
  count(Sleep.Quality)
#we can see that people don't ever rate their sleep below 4 or above 9
#we can se that half the people have quality of sleep - 8 or 9




#independent variables - Potential Predictiors for Sleep Quality
# i will start with picking the continuous variables as they contain most information
cor.test(df$Age, df$Sleep.Quality, method = "pearson")
# high negative correlation
cor.test(df$Daily.Steps, df$Sleep.Quality, method = "pearson")
# very high positive correlation
cor.test(df$Age, df$Daily.Steps, method = "pearson")
plotmeans(Sleep.Quality ~ Age, data = df,
          xlab = "Age", 
          ylab = "Sleep Quality",
          main = "Sleep Quality by Age ",
          ylim = c(4, 9))
plotmeans(Sleep.Quality ~ Daily.Steps, data = df,
          xlab = "Daily Steps", 
          ylab = "Sleep Quality",
          main = "Sleep Quality by Daily Steps ")
# as those 2 are also highly correlated with each other we will only pick Daily.Steps as the one for predicting
# because its correlation with target is higher it has less dispersion and its plot looks more linear

cor.test(df$Calories.Burned, df$Sleep.Quality, method = "pearson")
cor.test(df$Calories.Burned, df$Daily.Steps, method = "pearson")
#we will exclude Calories.burned because it is correlated with our best predictor and is not better than the predictor

cor.test(df$Bedtime_Minutes, df$Sleep.Quality, method = "pearson")
#very high negative correlation
cor.test(df$Sleep_Duration, df$Sleep.Quality, method = "pearson")
#high positive correlation
cor.test(df$Wakeup_Minutes, df$Sleep.Quality, method = "pearson")
#very low negative correlation
cor.test(df$Wakeup_Minutes, df$Bedtime_Minutes, method = "pearson")
#very low positive correlation means that people going to bed later dont necesarilly wake up later so in effect they sleep less
cor.test(df$Bedtime_Minutes, df$Sleep_Duration, method = "pearson")
#we can see that bedtime minutes and sleep duration have high negative correlation so people going to bed later in fact sleep less
cor.test(df$Sleep_Duration, df$Daily.Steps, method = "pearson")
cor.test(df$Bedtime_Minutes, df$Daily.Steps, method = "pearson")
#bedtime minutes is more correlated than sleep duration with daily.steps which we picked as the best predictor
#so i think it will be better to exclude bedtime and use sleep duration as it also contain more information (taking in wakup minutes)

#null hypothesis - sleep duration follows normal distribution
shapiro.test(df$Sleep_Duration)
#p-value < 0.05 → Reject H₀ → Data is not normally distributed

ggplot(df, aes(x=Sleep_Duration)) +
  geom_histogram( fill="lightblue", color="black") +
  theme_minimal()
unique(df$Sleep_Duration)
#as people dont remember exact times they wake up and go to bed the values concentrate around rounded values 
# if we tried to account for that we can see kind of normal distribution
ggplot(df, aes(x=Sleep_Duration)) +
  geom_histogram(aes(y=..density..), fill="lightblue", color="black") +
  stat_function(fun=dnorm, args=list(mean=mean(df$Sleep_Duration, na.rm=TRUE), 
                                     sd=sd(df$Sleep_Duration, na.rm=TRUE)), 
                color="red", size=1) +
  labs(title="Histogram with Normal Curve", x="Sleep Duration (hours)", y="Density") +
  theme_minimal()

ggplot(df, aes(x=Sleep_Duration)) +
  geom_histogram(aes(y=..density..), bins=5, fill="lightblue", color="black") +
  stat_function(fun=dnorm, args=list(mean=mean(df$Sleep_Duration, na.rm=TRUE), 
                                     sd=sd(df$Sleep_Duration, na.rm=TRUE)), 
                color="red", size=1) +
  labs(title="Histogram with Normal Curve", x="Sleep Duration (hours)", y="Density") +
  theme_minimal()

ggplot(df, aes(x=Sleep_Duration)) +
  geom_density(fill="lightblue", alpha=1) + 
  labs(title="Density Plot of Sleep Duration", x="Sleep Duration (hours)", y="Density") +
  theme_minimal()

ggplot(df, aes(sample = Sleep_Duration)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot of Sleep Duration") +
  theme_minimal()
#on the qqplot we can see that the the point are around the normality line but we can see the jumps because of rounded values



#now the categorical variables
#Gender
plotmeans(Sleep.Quality ~ Gender, data = df,
          xlab = "Gender", 
          ylab = "Sleep Quality",
          main = "Sleep Quality by Gender ")
df %>% 
  group_by(Gender) %>% 
  summarise(mean(Sleep.Quality))
#we can see that on average females rate their sleep around 3 points better

#assumptions
shapiro.test(df$Sleep_Duration[df$Gender == "m"])
shapiro.test(df$Sleep_Duration[df$Gender == "f"])
#shapiro test does not indicate normal distribution because of discrete values
#but we can kind of see normal distribution on the qqplot
ggplot(df, aes(sample = Sleep_Duration)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  facet_wrap(~ Gender) +  # Separate plots for each gender
  labs(title = "Q-Q Plot of Sleep Duration by Gender") +
  theme_minimal()

leveneTest(Sleep_Duration ~ Gender, data = df)
#Levene’s test fails to reject H₀, meaning variances are equal across genders.

#null hypothesis - true difference in sleep quality means of males and females is equal to 0
t.test(Sleep_Duration ~ Gender, data = df)
#we can see that true difference in sleep duration means of males and females is not equal to 0 as p value is lower than alpha of 0.05 so we reject the null hypothesis
#this means that with such dataset we can infer that there is a diffrence in the whole population in means of sleep duration between genders
table(df$Gender, df$Sleep.Quality)
#we can see that actually almost only females rate their sleep 8-9 and only males rate 4-7
plotmeans(Daily.Steps ~ Gender, data = df,
          xlab = "Gender", ylab = "Daily.Steps",
          main = "Daily Steps by Gender")
#but sex is highly associated with our best predictor (Daily.Steps) so we will not use gender in our model

#Physical Activity Level
plotmeans(Sleep.Quality ~ Physical.Activity.Level, data = df,
          xlab = "Physical Activity Level", ylab = "Sleep Quality",
          main = "Sleep Quality by Activity Level")
#we can see that physical activity level could be a good predictor 
plotmeans(Daily.Steps ~ Physical.Activity.Level, data = df,
          xlab = "Physical Activity Level", ylab = "Daily Steps",
          main = "Daily Steps by Activity Level")
#but it is also highly associated with daily steps which is the best predictor so far
# the graphs look almost the same so adding physical activity level as another predictor would not add much

plotmeans(Calories.Burned ~ Physical.Activity.Level, data = df,
          xlab = "Physical Activity Level", ylab = "Calories Burned",
          main = "Calories Burned by Activity Level")
#calories burned differ over physical activity levels as we would suspect
ggplot(df, aes(sample = Calories.Burned)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  facet_wrap(~ Physical.Activity.Level) +  # Separate plots for each gender
  labs(title = "Q-Q Plot of Sleep Duration by Gender") +
  theme_minimal()
leveneTest(Calories.Burned ~ Physical.Activity.Level, data = df)
#assumptions for anova not met

#Dietary Habits
plotmeans(Sleep.Quality ~ Dietary.Habits, data = df,
          xlab = "Dietary Habits", ylab = "Sleep Quality",
          main = "Sleep Quality by Dietary Habits")
plotmeans(Daily.Steps ~ Dietary.Habits, data = df,
          xlab = "Dietary Habits", ylab = "Daily Steps",
          main = "Daily Steps by Dietary Habits")
#we can see that Dietary habits could be a good predictor 
#but it is also highly associated with daily steps which is the best predictor so far
ggplot(df, aes(sample = Sleep.Quality)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  facet_wrap(~ Dietary.Habits) +  # Separate plots for each gender
  labs(title = "Q-Q Plot of Sleep quality by dietary habits") +
  theme_minimal()
leveneTest(Sleep.Quality ~ Dietary.Habits, data = df)
#the assumptions for anova not met

#Sleep Disorders
plotmeans(Sleep.Quality ~ Sleep.Disorders, data = df,
          xlab = "Sleep Disorders", ylab = "Sleep Quality",
          main = "Sleep Quality by Sleep Disorders")
plotmeans(Daily.Steps ~ Sleep.Disorders, data = df,
          xlab = "Sleep Disorders", ylab = "Daily Steps",
          main = "Daily Steps by Sleep Disorders")
ggplot(df, aes(sample = Sleep.Quality)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  facet_wrap(~ Sleep.Disorders) +  # Separate plots for each gender
  labs(title = "Q-Q Plot of Sleep Quality by Sleep Disorders") +
  theme_minimal()
leveneTest(Sleep.Quality ~ Sleep.Disorders, data = df) # p value just over 5%
t.test(Sleep.Quality ~ Sleep.Disorders, data = df)# sleep quality differs between people with or without sleep disorders
#we can see that Sleep disorders could be a good predictor 
#but it is also highly associated with daily steps which is the best predictor so far

#Medication Usage
plotmeans(Sleep.Quality ~ Medication.Usage, data = df,
          xlab = "Medication Usage", ylab = "Sleep Quality",
          main = "Sleep Quality by Medication Usage")
plotmeans(Daily.Steps ~ Medication.Usage, data = df,
          xlab = "Medication Usage", ylab = "Daily Steps",
          main = "Daily Steps by Medication Usage")
ggplot(df, aes(sample = Sleep.Quality)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  facet_wrap(~ Medication.Usage) +  # Separate plots for each gender
  labs(title = "Q-Q Plot of Sleep Quality by Medication Usage") +
  theme_minimal()
leveneTest(Sleep.Quality ~ Medication.Usage, data = df)
t.test(Sleep.Quality ~ Medication.Usage, data = df) # sleep quality differs between people with or without medication usage
#we can see that medication could be a good predictor 
#but it is also highly associated with daily steps which is the best predictor so far
chisq.test(table(df$Medication.Usage, df$Sleep.Disorders))$expected
#assumptions for chisq met
chisq.test(table(df$Medication.Usage, df$Sleep.Disorders))
table(df$Medication.Usage, df$Sleep.Disorders)
#we can see that it is also highly associated with sleep disorders so there is no point in putting both in the same model




#i didnt choose gender as predictor because information about these 2 classes can be deduced from the rest of the predictors
# and adding it to the model does not increase accuracy

# for the model as predictors i chose :
# daily steps as it is most correlated with sleep quality,
# sleep duration as it is highly correlated with the target and less with daily steps, sleep duration rationally influences sleep quality
# sleep disorder as it rationally influences sleep quality, and we managed to prove it with t test
# dietary habits because it only has little association with other predictors, it increases accuracy

#the target variable has discrete number values but multinomial model is not specified in the assignment so we are going to treat it like continuous and predict continuous with multiple linear regression
model <- lm(Sleep.Quality ~ Daily.Steps + Sleep_Duration + Sleep.Disorders + Dietary.Habits, data = df)
#we can se that every predictor feature is statistically significant
summary(model) # very good R2 of 97% meaning model explains 97% of variance
coef(model) # model works this way:  1.929760053 + 0.000391091 * Daily Steps + 0.314517978 * Sleep Duration - 0.413042966 * 1( if Sleep Disorder = yes) + 0.434438148 * 1 (if dietary habits = medium) + 0.609934101 * 1(if dietary habits = high)

#predicted as continuous here
df$Predicted <- predict(model, newdata = df)

vif(model) # we can see high multicolinearity as all of the data in this dataset are somehow related and corelated
# but deleting any of the predictors decreases models R2
# one thing we could do is Principal component analysis but we did not learn that

df$Sleep.Quality_Num <- as.numeric(df$Sleep.Quality)
df$Predicted_Num <- as.numeric(df$Predicted)

mae <- mean(abs(df$Sleep.Quality_Num - df$Predicted_Num))
print(paste("Mean Absolute Error (MAE):", round(mae, 2))) #"average" error 

rmse <- mean(sqrt((df$Sleep.Quality_Num - df$Predicted_Num)^2))
print(paste("Root Mean Squared Error (RMSE):", round(rmse, 2))) #average error penalizing bigger errors is the same meaning our model does not make big errors

#as we had a multiple linear regression model predicting continuous values we can round them up to discrete numbers and check accuracy
df$Predicted_Rounded <- round(df$Predicted)
accuracy <- mean(df$Sleep.Quality == df$Predicted_Rounded)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))

#predictors vs target plots for clarity and insight
plotmeans(Sleep.Quality ~ Daily.Steps, data = df,
          xlab = "Daily Steps", 
          ylab = "Sleep Quality",
          main = "Sleep Quality by Daily Steps ")
plotmeans(Sleep.Quality ~ Sleep_Duration, data = df,
          xlab = "Sleep Duration", 
          ylab = "Sleep Quality",
          main = "Sleep Quality by Sleep Duration ")
plotmeans(Sleep.Quality ~ Sleep.Disorders, data = df,
          xlab = "Sleep Disorders", 
          ylab = "Sleep Quality",
          main = "Sleep Quality by Sleep Disorders ")
plotmeans(Sleep.Quality ~ Dietary.Habits, data = df,
          xlab = "Dietary Habits", 
          ylab = "Sleep Quality",
          main = "Sleep Quality by Dietary Habits ")


# Q-Q plot for normality of residuals
qqnorm(resid(model), main = "Q-Q Plot of Residuals", pch = 20, col = "blue")
qqline(resid(model), col = "red", lwd = 2)

# Scale-Location Plot (for Homoscedasticity and linearity)
plot(model, which = 3)
#model is slightly less accurate for lower values
