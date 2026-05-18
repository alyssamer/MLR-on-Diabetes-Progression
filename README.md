# Multiple Linear Regression on Diabetes Progression

This project uses multiple linear regression to identify what factors most significantly predict the progression of diabetes one year after baseline. The dataset contains 442 patients with 10 predictors, age, sex, body mass index, average blood pressure and six blood serum measurements (labeled S1-S6). The target variable is the disease progression indicator (Target).



## Steps

- exploratory data analysis    // summary statistics, graphs
- initial full model           // uses all 10 predictors, checks VIF for multicollinearity
	- remove s1 due to severe multicollinearity
	- Box-Cox transformation to correct non-constant variance
- L.I.N.E. assumptions tests   // residual plots, shapiro-wilks, ncv testing
- backward selection           // removes predictors with high p-values (>0.05)
- final model                  // recheck L.I.N.E tests on model after removals/transformations, 



## Results

The final model (Target ~ Sex + BMI + BP + S3 + S5) explains 50.18% of the variance in diabetes progression. The model only explaining about half is understandable, as diabetes is a complex condition influenced by many variables. 



## Files

MLR-on-Diabetes-Progression/
- dataset 
	- Diabetes.txt
- diabetes_progression.R    // r-code with comments
- report.pdf                // original report for class 
- README.md



## Original Analysis: April 2025
