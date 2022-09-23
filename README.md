# Modelling Football Player Potential using R.

*Project Duration: December 2021 - January 2022*

#### A project conducted with R to model the potential of footballers from a dataset with 12849 rows and 17 columns 

Use the ``` install.packages("package_name") ``` to install any package used within the implementation which you may not already have installed

---

This project was conducted using R and R Studio. 

The main implementation can be found within the .Rmd file within the repository directory.

A .R option of the code is available for anyone who prefers that option, though the markdown looks much better and is easier to read.

---
•	The dataset was cleaned by forcing variables into the right type, removal of duplicate values, and removal and imputation of missing values.

•	For data exploration, the shape of the data was observed by plotting of histograms and density plots. To observe the skewness and kurtosis in more detail, certain formulas were used to achieve this goal. Covariation between the variables within the dataset was investigated also. 

•	Outliers within the dataset were investigated (using various visualisations such as boxplots and quantile-quantile plots) and then removed from the dataset.

•	Interactions between variables were observed to find out whether the relationships between them were linear. It was concluded that they were, so a linear model was built, first using all the explanatory variables against the response variable. Following that, a minimal adequate model was determined.

•	Diagnostic plots were made to observe any potential weaknesses of the model, and they revealed that the model could be easily affected by outliers and could miss non-linear relationships that existed between variables.

---
Details of the steps taken for the implementation can be found within the R Markdown Notebook File.

