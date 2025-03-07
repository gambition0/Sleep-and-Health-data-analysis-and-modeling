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
