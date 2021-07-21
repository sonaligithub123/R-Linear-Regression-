# R-Linear-Regression-
Linear Regression R Programming
This is data is from credit card company with 5000 customers database. 
The objective of the data analysis is to predict credit card future spend for its clients. The Data has 5000 observations and 132 features. 
Data analysis steps:
1)	Data preparation (Pre modelling)- Performed data preparation by cleaning data, removing unwanted variables with high or constant variance. Performed missing and outlier treatment 
2)	Feature reduction was performed using RFE (Recursive Feature Engineering) , Stepwise Regression and Lasso regression 
3)	Normality of Y variable was checked with histogram, checking its skewness and kurtosis
4)	Correlation matrix was performed for Y with X variables to eliminate variables with lower correlation
5)	VIF analysis (variance inflation factor) was performed to eliminate X variables with high cardinality 
6)	Data splitting into development and validation data after which Data modelling with iterative traditional Linear Regression model was performed. The final model was achieved after 9 iterations. 
7)	The model gave good accuracy with multiple metrices as below 
	MAPE is 0.02 for dev data and 0.02 for validation data 
	RMSLE is 0.05 for dev data and 0.04 for validation data 
	R2 is 0.987 for dev data and 0.989 for validation data 

