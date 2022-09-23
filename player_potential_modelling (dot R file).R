---
title: "Player Potential Modelling with R"
author: "Edudzi Mamattah"
date: "`r format(Sys.time(), '%d %B, %Y')`"
output: html_notebook
version: 1.0
---

# 0. Loading Libraries

 
# Loading required libraries   

library(validate)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(dlookr)
library(skimr)
library(funModeling)
library(flextable)
library(ggstatsplot)
library(ggcorrplot)
library(moments)
library(tree)
 


# 1. Organise and clean the data

## 1.1 Subset the data into the specific dataset allocated
 

 
# Load the .Rda file
load("football_analysis_data.Rda")


# Put into your data frame named mydf (you can rename it)
mydf <- football.analysis
 


## 1.2 Data quality analysis
  # 
  # Plan to assess quality of data:  
  # 
  # I would check first of all that all the data attributes are of the right data type that they are supposed to be. Meaning, to check for whether a column which is supposed to be of a specific data type according to the data description, is showing values of another data type.
  # 
  # I would check that data falls in the range that it is supposed to, for all attributes; according to the defined rules stated in the metadata.
  # I would check for outliers in the data columns as well. If there are any outliers, I would investigate them on a case by case basis, and give suggestions and/or take actions where needed.
  # 
  # I would check to see if there is any missing data from any columns in the data set, and to suggest possible actions that can be taken to resolve those rows of data for which those values exist.

 
# This step is undertaken to see all the column names in the data frame. It shows that we have 17 columns like we are supposed to 
names(mydf)
 

 
# over here I use the str function to check the structure of the data - to check the individual columns' data type. This is done to get a brief description and a not-too-detailed overview of the data frame
str(mydf)

 
  # From this snippet above, it can be seen that "sofifa_id" is an integer variable. This should be a categorical variable, since it is just an identifier for the players. It will have to be converted to character.
  # 
  # "potential" is an integer value and is of the right data type so no conversion is needed. 
  # 
  # "wage_eur" is a numerical type. This is also consistent with the descriptions in the metadata and will not be changed.
  # 
  # "age" is an integer column, meaning it is of the right type. No changes to the type are required.
  # 
  # "height_cm" is also of integer type and wouldn't require any changes to its type.
  # 
  # "weight_kg" is of the integer type like it's supposed to be.
  # 
  # "club_name" is of the character data type like its description suggests.  
  # 
  # "preferred_foot" is also character type and requires no change.
  # 
  # "pace" is of the integer type. This is consistent with the metadata.             
  # 
  # "shooting" is an integer column, meaning it is of the right type. No changes to the type are required 
  # 
  # "passing" is also of the integer type, hence is of the correct type.         
  # 
  # "dribbling" is also of integer type and wouldn't require any changes to its type.    
  # 
  # "defending" is an attribute that is of integer type. This type is consistent with the data descriptions in the metadata. 
  # 
  # "physic" is of the integer type. This is consistent with the metadata.           
  # 
  # "power_strength" is an integer attribute as well. No changes to the type are required.
  # 
  # "power_long_shots" is also of integer type and wouldn't require any changes to its type.
  # 
  # "high.wage.ind"  is supposed to be a binary variable based on whether a player's weekly wage is above 8,000 Euro, but it shows as an integer data type. This will have to be rectified, and the type of this attribute would have to be changed to a factor.



 

# Over here I run the summary function to be able to get some basic data on each attribute such as the minimum value, maximum value, the mean and median, and the 3 quartiles. Summary will help to tell if data is within the specified range for numerical variables.

summary(mydf)
 
# Besides the values that have to be converted to categorical variables (sofifa_id), and those that  need to be converted to binary factors (high.wage.ind), the rest of the categorical variables seem to follow the rules stated in the metadata.
# 
# There seems to be no apparent problems with the summary output values of the numerical variables except for that of  "age", "pace" and "dribbling".
# 
# With "age" the maximum value is 79 years. This value does satisfy the condition that the values for "age" should range between 0-100; but the value is such an extreme outlier from the other values that it may need to be explored further, and an action would need to be taken.
# 
# 
# To get an idea of the next largest value after the max value, the code chunk below is implemented.

 
head(sort(mydf$age, decreasing = T))
 

# As at 21st December, 2021, the longest playing professional footballer is 54 year old Kazuyoshi Miura (11 Oldest Soccer Players in the World (Updated 2021), 2021). Due to how far away the value in our data frame is from the others in the data frame, and from the real life value, we will need to take some action on it as we progress.
# 
# 
#   For the "pace" attribute, we have a negative value (-88) as the minimum value, which violates the conditions of the metadata description. Pace is supposed to be in the range 0 -100. We will need to perform some action on this piece of data.
# 
# 
# 
# To run some more views on the data, I will use the "skim()" function from the "skimr" package. It provides a type of high level summary but with additional information that isn't provided by the regular Base R "summary()" function (Zablotski, 2021).
# 
#  

skim(mydf)

 

# From the output above, it shows that we have two categorical variables and fifteen numerical variables. There is no missing data in any of the columns.
# 
# 
# 
# To get a clearer picture of the data we use diagnose
 
diagnose(mydf) %>% flextable()
 
# From the above table, we are able to see that there are no missing values in the data frame for any variable.
# 
#   "preferred_foot" shows 4 unique variables, which we know should only be two. That would need to be investigated later.
# 
#   "sofifa_id" shows a unique count of 12819, which means that 30 of them are the exact same value. For a variable which is supposed to be a unique identifier for a player, it cannot have any duplicates.
# 
# Now, to find more information on the categorical and numerical variables, we run the next two chunks of code.
# 
# 
# 
# For the chunk below, we diagnose the categorical variables and take a deeper dive into it that before.
 
diagnose_category(mydf) %>% flextable()
 
# From the above result, we see that for the "preferred_foot" variable, we have 3 different values, even though it's supposed to just be two.
# We run the next chunk just to see get a clearer picture about the situation.
 
table(mydf$preferred_foot)
 
# The  "right" and "left" values are causing that column to have 4 factors instead of two and so, that will need to be investigated and corrected.
# Besides that categorical variable having an issue, the rest seem to be fine.
# 




# Now, in the next code chunk, we diagnose the numerical variables from the data.
 
diagnose_numeric(mydf) %>% flextable()
 

#   From the above table, we can observe that two variables have negative values(shown in the "min" column).
# 
#   From previous analysis of the result of the "summary()" function we detected that "pace" had a negative value.
# 
#   We can also observe now that "dribbling" has its minimum value being negative as well (-74). This value is a complete violation of the rules specified in the metadata, as the "dribbling" column in the data frame is supposed to have a range between 0 and 100. With this information, we would have to work on that attribute and find a solution to the negative value it possesses during the data cleaning phase.  
# 
#   The minimum value of "weight_kg" is 14, which is not the weight that a football player would have. An action would need to be taken with regard to that value.
# 
# The minimum value of "wage_eur" is 1, which is so much of an outlier that it cannot be possible. That value is too small, and is less than the minimum wage for most professional footballers. An action would need to be taken with regard to that value.
# 
#   We can also see that the nature of "sofifa_id" requires it to be changed into a categorical attribute, and the attribute "high.wage.ind" needs to be changed to a factor attribute with levels "0" and "1".

 
## 1.3 Data cleaning  
 

  # Some data quality issues found in 1.2 include:  
  # 
  # "sofifa_id" being of a wrong type. It was stored in the data as a numeric variable instead of a categorical variable.
  # 
  # "sofifa_id" also violates the uniqueness constraint for its column, and that will need to be looked into.
  # 
  # "high.wage.ind" is also of the wrong data type, as it shows that it is numerical, while it is supposed to be a binary factor.
  # 
  # The "dribbling" variable contains invalid data (a negative value) and is within the wrong range.
  # 
  # The "pace" variable also contains an invalid instance with a negative value, and also violates the range for the variable.
  # 
  # "preferred_foot" violates a set membership constraint. This column of data should have only two possible values, yet it has three.
  # 
  # The minimum value for "weight_kg" is too small to be a realistic value for player weight. 
  # 
  # The minimum value for "wage_eur" is too small to be a realistic value for player weekly wage. 
  # 
  # 
  #   Additional comment:   For the age column, there is one instance of data which shows the age of a player as 79 years. The age is completely within the constraints of the acceptable range but the value is so much of an outlier because of the improbability of a footballer being that age.

--------------------------------------------------------------------------
#   Now, data cleaning can begin.  
# 
# I will make a copy of the original data frame to work on from this point henceforth, as it is not good practice to work on the original data set directly. This data frame would be named "temp_df".

 
temp_df <- mydf
 


 
names(temp_df)
 


  # Will use the validate package to write the rules of the metadata into the programme, to have a visual way of spotting certain errors
 
# Using the validate package to check whether the rules of the metadata are obeyed

mydf.rules <- validator(okSofifaID = is_unique(sofifa_id),
                        okPotential = potential >= 0 & potential <= 100,
                        okWageEur = wage_eur > 0,
                        okAge = age > 0 & age <= 54,
                        okHeight = height_cm > 0,
                        okWeight = weight_kg > 0,
                        okPreferredFoot = is.element(preferred_foot, c("Right", "Left")),
                        okPace = pace >=0 & pace <= 100,
                        okShooting = shooting >=0 & shooting <= 100,
                        okPassing = passing >=0 & passing <= 100,
                        okDribbling = dribbling >=0 & dribbling <= 100,
                        okDefending = defending >=0 & defending <= 100,
                        okPhysic = physic >=0 & physic <= 100,
                        okPowerStrength = power_strength >=0 & power_strength <= 100,
                        okPowerLongShots = power_long_shots >=0 & power_long_shots <= 100
                                                                 
                           )




quality.check <- confront(temp_df, mydf.rules)


summary(quality.check)

plot(quality.check)
 

# The columns with problems are put at the top and have the red markings within their bar graphs.
# 
# 
# Will first work on "preferred_foot"
 
table(temp_df$preferred_foot)
 


 
temp_df$preferred_foot[temp_df$preferred_foot == "right"] <- "Right"
temp_df$preferred_foot[temp_df$preferred_foot == "left"] <- "Left"

table(temp_df$preferred_foot)
 

  # We now have only two values for "preferred_foot"






  # Now the duplicates in "sofifa_id" will have to be removed.




# To convert all duplicates to NA, we run the next chunk
 
temp_df$sofifa_id[duplicated(temp_df$sofifa_id)] <- NA
 



# Now let's check if the changes took effect

 


diagnose(temp_df) %>% flextable()
 
# We can now see that "sofifa_id" has  unique values and a few missing vlaues which will be removed.


 
  # Now I will change "sofifa_id" into a categorical variable.
 
 

temp_df$sofifa_id <- as.character(temp_df$sofifa_id)

 

# To check whether the change has taken effect, the next chunk is executed.
 
str(temp_df$sofifa_id)
 





  # In the next chunk, "high.wage.ind" is converted into a factor.
 
temp_df$high.wage.ind <- as.factor(temp_df$high.wage.ind)

 


# To check whether the change has taken effect, the next chunk is executed.
 
str(temp_df$high.wage.ind)

 

#   For the "age" column, we have outliers, values > 54. This value does not validate the rules of the column, but realistically, is a highly improbable value as no footballer has ever played to 60 years as of 2021 (11 Oldest Soccer Players in the World (Updated 2021), 2021).
# 
# 
# Unrealistic ages will be converted to NA then removed
 
temp_df$age <- ifelse(temp_df$age > 53, NA, temp_df$age)

 


# Negative pace will be removed
 
temp_df$pace <- ifelse(temp_df$pace < 0, NA, temp_df$pace)

 


 

head(sort(temp_df$pace, decreasing = F))
summary(temp_df$pace)
 
# We observe that the minimum value for "pace" is no longer negative.
# 
# 
# 
#   For the "dribbling" variable, the dataframe contains some negative values To resolve this issue, I will remove them.

 
temp_df$dribbling <- ifelse(temp_df$dribbling < 0, NA, temp_df$dribbling)
 

# The chunk below is to check the smallest value in the "dribbling" column
 
head(sort(temp_df$dribbling, decreasing = F))   
summary(temp_df$dribbling)
 
# We observe that the smallest value is no longer negative.


# Now we perform complete case removal for the NA's remaining 
 
temp_df <- temp_df[complete.cases(temp_df), ]

 


  # Now we check the columns to be sure. First with the numeric.

 
diagnose_numeric(temp_df) %>% flextable()
 

 
diagnose_category(temp_df) %>% flextable()
 


# We find that all changes have been executed and the data is clean.


## This next part has been commented out. I would run it if I wanted to preserve the realness in the data. Removing the older age values (age >54) can be considered part of this section but I found that too unreasonble to ignore, hence why I did that above. To uncomment, highlight area and press Ctrl+Shift+C in Rstudio



# <!--    For the minimum value of "weight_kg", I cannot ascertain the veracity of the entry, and have no one to ask about it. I also want to avoid having NA's in the data frame. Due to these reasons, I will not simply replace the value with NA. I would instead replace the value with the median of that column. -->

# 
# <!--   -->
# <!-- temp_df$weight_kg <- ifelse(temp_df$weight_kg < 50, median(temp_df$weight_kg), temp_df$weight_kg ) -->
# <!--   -->
# 
# 
# <!-- To check, if the change has taken effect, I would run some summaries -->
# <!--   -->
# <!-- head(sort(temp_df$weight_kg, decreasing = F))  -->
# <!-- summary(temp_df$weight_kg) -->
# <!--   -->
# <!-- We see that 22 is no longer the minimum, and that the median is still 74. -->
# 
# 
# <!--   I will replace the minimum value of wage_eur with the median value for that column. This will preserve the estimate of the central tendency but may deflate the estimate in the variance (Shepperd, 2021). Since I am not doing this(replacing values with the median) too many times in the data set, I am willing to take this risk. -->
# 
# <!--   -->
# <!-- temp_df$wage_eur <- ifelse(temp_df$wage_eur < 60, median(temp_df$wage_eur), temp_df$wage_eur ) -->
# <!--   -->
# 
# <!-- We can observe that the change has taken effect in the next chunk -->
# <!--   -->
# <!-- head(sort(temp_df$wage_eur, decreasing = F))  -->
# <!-- summary(temp_df$wage_eur) -->
# <!--   -->


## End of

# 2. Exploratory Data Analysis (EDA)

## 2.1 EDA plan
# 
#   Plan to explore, describe and visualise data:    
# 
#   I would eyeball the data to find patterns by way of trying to gain some descriptive statistics and visualisations. I would also check for missing data in the dataset.
# 
#   I would classify features into categorical or numerical (based on their properties and the metadata), and this would inform my choice of visualisation for each feature.
# 
#   Next, I would try to identify the shape of my data (by way of histograms, density plots, probability density functions or probability mass functions), and to see if there is any skewness in the data. Skewness measures how asymmetrical the data is.
# 
#   I would identify the significant correlations, by using a correlation matrix for all features. For categorical features, the Chi-Square test can be used to check the correlation between those features. 
# 
#   I would formulate some questions based on the look of the data, which I would look into. Then I would also find out the types I am dealing with, the type of variations that occur within the variables, and the type of covariation that occurs between them.
# 
# 
#   I would then attempt to find out whether outliers exist in the data and find out how they affect the data.
# 
#   Then I would search for answers to the questions through visualisations (boxplots, histograms, plots of normality/q-q plots, density plots, tables, bar charts, frequencies, among others), transforming, and modelling the data.



## 2.2 EDA and summary of results  



# First off, I will check the structure of the data. I will be working on "temp_df" for this.

 
status(temp_df) %>% flextable()
 


 
str(temp_df)
cat("\n\n")
skim(temp_df)
#  
# We can see that we have 13 continuous variables and 4 categorical.
# 
# 
# 
# 
# I would now check to see if there are any missing values in the data.

 

diagnose(temp_df) %>% flextable()

 
# We can see that no column has any empty cells, so we can carry on with the analysis.
# 
# 
# ---------------
# So now, I carry out uni-variate visualisations. I would first carry them out for the categorical features.
# We have only 2 categorical variables for which these operations would be carried on("preferred_foot" and "high.wage.ind"), because "sofifa_id" and "club_name" are just unique identifiers for each individual even though multiple individuals can share the same "club_name".
# 
# 
# I would start with "preferred_foot".
 
table(temp_df$preferred_foot)
 
# We can observe from the above that more than half of the players are right footed. 
# We can observe a bar chart of this to get some better visualisation below.
# 

## Bar charts of categorical data

 
barplot(table(temp_df$preferred_foot), xlab = "Preferred Foot", ylab = "Frequency", main="Bar chart of Preferred Foot")
 

# Now, we can check the plots for "high.wage.ind"
 
table(temp_df$high.wage.ind)

barplot(table(temp_df$high.wage.ind), xlab = "High Wage Indicator", ylab = "Frequency", main="Bar chart of High Wage Indicator")

#  
# From this we see that the number of players with weekly wages less than 8000 Euros is 8981 , to 3763  for those with wages higher than 8000 Euros a week.


## Tests for normality

# Now the numerical variables can be explored.
# Histograms, Density plots and quantile-quantile plots will be done for all numerical variables. Then, tests of skewness and kurtosis will be carried out. 
# 
# Skewness is a value that measures the symmetry in a set of data, whereas kurtosis measures the presence of heavy tails or outliers in a distribution (Zablotski, 2021).


 
# Histogram, Density Plot and Q-Q plot of potential

hist(temp_df$potential, xlab = "Player Potential", main = "Histogram of Player Potential" )

potential_density <- density(temp_df$potential)

plot(potential_density, lwd= 2, col= "blue")

qqnorm(temp_df$potential)
qqline(temp_df$potential, col = "red", lwd = 2)

# Tests of skewness 

skewness(temp_df$potential)
agostino.test(temp_df$potential)


# Test of kurtosis

anscombe.test(temp_df$potential)

 
# Distribution of the histogram looks normal, and so does the density plot. The Q-Q plot also shows that the player potential is normally distributed.
# 
# The test for skewness produces a value of 0.16 and a p-value that is significant enough to make us conclude that there is not enough evidence for us to reject the null hypothesis: that there is no skewness in the data.
# Therefore we conclude from the skewness that the data is normally distributed.
# With the kurtosis, we have a p-value of 0.56, which proves significant enough to not reject the null hypothesis that the kurtosis is equal to 3; which would mean that the data is normally distributed.
# 
# So, we can say with certainty that "potential" is normally distributed.




 
# Histogram, Density Plot and Q-Q plot of Player wage

hist(temp_df$wage_eur, xlab = "Player Weekly Wage", main = "Histogram of Weekly Wage" )

wage_density <- density(temp_df$wage_eur)

plot(wage_density, lwd= 2, col= "blue")

qqnorm(temp_df$wage_eur)
qqline(temp_df$wage_eur, col = "red", lwd = 2)

# Tests of skewness 

skewness(temp_df$wage_eur)
agostino.test(temp_df$wage_eur)


# Test of kurtosis

anscombe.test(temp_df$wage_eur)
#  
# For this variable weekly wage, from the plots of the histogram, density plot and Q-Q plot, we can see quite clearly that the variable is not normally distributed.
# We get further confirmation of this when we run the skewness and kurtosis tests, which both have very small p-values, leading us to conclude that we can reject the null hypotheses that there is no skewness in the data(for the skewness test), and that the kurtosis value is equal to 3(for the kurtosis test).
# Therefore, we can say with certainty that player wage is not normally distributed.




 

# Histogram, Density Plot and Q-Q plot of Player Age

hist(temp_df$age, xlab = "Player Ages", main = "Histogram of Player Age" )

age_density <- density(temp_df$age)

plot(age_density, lwd= 2, col= "blue")

qqnorm(temp_df$age)
qqline(temp_df$age, col = "red", lwd = 2)

# Tests of skewness 

skewness(temp_df$age)
agostino.test(temp_df$age)


# Test of kurtosis

anscombe.test(temp_df$age)

 
# The histogram, density plot and Q-Q plots of player age do not show that the data may be normally distributed. To properly find out if that is the case, the results from the statistical tests of skewness and kurtosis must be looked at. 
# 
# The p-value for the test of skewness is so small that we can reject the null hypothesis that the data for "age" is not skewed. The p-value for kurtosis is also significantly small that we can conclude that the data is not normally distributed.




 

# Histogram, Density Plot and Q-Q plot of Player Height(cm)

hist(temp_df$height_cm, xlab = "Player Height(cm)", main = "Histogram of Player Height in cm" )

height_density <- density(temp_df$height_cm)

plot(height_density, lwd= 2, col= "blue")

qqnorm(temp_df$height_cm)
qqline(temp_df$height_cm, col = "red", lwd = 2)

# Tests of skewness 

skewness(temp_df$height_cm)
agostino.test(temp_df$height_cm)


# Test of kurtosis

anscombe.test(temp_df$height_cm)

 
# The plots of player height show that height is not normally distributed and is skewed to the left. 
# We get further confirmation of this when we check the results of the statistical tests for skewness and kurtosis.
# 
# Both of them have significantly low p-values which lead us to the conclusions that the data is skewed and is not normally distributed.





 
# Histogram, Density Plot and Q-Q plot of Player Weight(kg)

hist(temp_df$weight_kg, xlab = "Player Weight(kg)", main = "Histogram of Player Weight in kg" )

weight_density <- density(temp_df$weight_kg)

plot(weight_density, lwd= 2, col= "blue")

qqnorm(temp_df$weight_kg)
qqline(temp_df$weight_kg, col = "red", lwd = 2)

# Tests of skewness 

skewness(temp_df$weight_kg)
agostino.test(temp_df$weight_kg)


# Test of kurtosis

anscombe.test(temp_df$weight_kg)
 
# Weight also shows that the data is not normally distributed and is skewed.
# Both the tests for kurtosis and skewness have significantly small p-values which lead us to this conclusion.




 
# Histogram, Density Plot and Q-Q plot of Player Pace

hist(temp_df$pace, xlab = "Player Pace", main = "Histogram of Player Pace" )

pace_density <- density(temp_df$pace)

plot(pace_density, lwd= 2, col= "blue")

qqnorm(temp_df$pace)
qqline(temp_df$pace, col = "red", lwd = 2)

# Tests of skewness 

skewness(temp_df$pace)
agostino.test(temp_df$pace)


# Test of kurtosis

anscombe.test(temp_df$pace)
 
# For the "pace" variable, the test of skewness produces a significantly small p-value which leads us to reject the null hypothesis; hence we can conclude that the variable pace is skewed.
# 
# However, for the kurtosis, the p-value of 0.1 does not provide enough evidence for us to reject the null hypothesis, that the data is normally distributed. Due to this we can conclude that the data is relatively normally distributed.





 
# Histogram, Density Plot and Q-Q plot of Player Shooting 

hist(temp_df$shooting, xlab = "Player Shooting", main = "Histogram of Player Shooting" )

shooting_density <- density(temp_df$shooting)

plot(shooting_density, lwd= 2, col= "blue")

qqnorm(temp_df$shooting)
qqline(temp_df$shooting, col = "red", lwd = 2)

# Tests of skewness 

skewness(temp_df$shooting)
agostino.test(temp_df$shooting)


# Test of kurtosis

anscombe.test(temp_df$shooting)

 
# The diagnostics for the "shooting" variable are very conclusive, given they have significant p-values. We can see that shooting is skewed and is not normally distributed, as both the skewness test and kurtosis test return p-values that are significantly smaller than 0.05.





 
# Histogram, Density Plot and Q-Q plot of Player Passing 

hist(temp_df$passing, xlab = "Player Passing", main = "Histogram of Player Passing" )

passing_density <- density(temp_df$passing)

plot(passing_density, lwd= 2, col= "blue")

qqnorm(temp_df$passing)
qqline(temp_df$passing, col = "red", lwd = 2)

# Tests of skewness 

skewness(temp_df$passing)
agostino.test(temp_df$passing)


# Test of kurtosis

anscombe.test(temp_df$passing)
 
# The histogram and density plot for passing look relatively normal; and so does the quantile-quantile plot.
# The test for skewness, with a p-value of 0.12 means we do not have enough evidence to reject the null hypothesis that there is no skewness in the data; and the p-value of 0.76 for kurtosis, shows us that the data is indeed normally distributed.




 
# Histogram, Density Plot and Q-Q plot of Player Dribbling

hist(temp_df$dribbling, xlab = "Player Dribbling", main = "Histogram of Player Dribbling" )

dribbling_density <- density(temp_df$dribbling)

plot(dribbling_density, lwd= 2, col= "blue")

qqnorm(temp_df$dribbling)
qqline(temp_df$dribbling, col = "red", lwd = 2)

# Tests of skewness 

skewness(temp_df$dribbling)
agostino.test(temp_df$dribbling)


# Test of kurtosis

anscombe.test(temp_df$dribbling)
 
# The statistical test for skewness produces a significantly low p-value which we can use to confirm that the data for the "dribbling" attribute is skewed.
# For kurtosis too, the p-value leads us to reject the null hypothesis. Using this, we can be sure that the "dribbling" attribute is not normally distributed.




 

# Histogram, Density Plot and Q-Q plot of Player Defending

hist(temp_df$defending, xlab = "Player Defending", main = "Histogram of Player Defending" )

defending_density <- density(temp_df$defending)

plot(defending_density, lwd= 2, col= "blue")

qqnorm(temp_df$defending)
qqline(temp_df$defending, col = "red", lwd = 2)

# Tests of skewness 

skewness(temp_df$defending)
agostino.test(temp_df$defending)


# Test of kurtosis

anscombe.test(temp_df$defending)

 
# For the test of skewness,we reject the null hypothesis and conclude that the data is skewed.
# However, for this same "defending" variable, the p-value of the test for kurtosis is significant enough for us to conclude that the data is not normally distributed.




 

# Histogram, Density Plot and Q-Q plot of Player Physic Attribute

hist(temp_df$physic, xlab = "Player Physic Attribute", main = "Histogram of Player Physic Attribute" )

physic_density <- density(temp_df$physic)

plot(physic_density, lwd= 2, col= "blue")

qqnorm(temp_df$physic)
qqline(temp_df$physic, col = "red", lwd = 2)

# Tests of skewness 

skewness(temp_df$physic)
agostino.test(temp_df$physic)


# Test of kurtosis

anscombe.test(temp_df$physic)
 
# For the "physic" attribute, we can see from the p-value of the test for skewness, that the data is skewed.
# 
# The kurtosis p-value also leads us to conclude that the data isn't normally distributed
# 



 

# Histogram, Density Plot and Q-Q plot of Player Strength

hist(temp_df$power_strength, xlab = "Player Strength", main = "Histogram of Player Strength" )

strength_density <- density(temp_df$power_strength)

plot(strength_density, lwd= 2, col= "blue")

qqnorm(temp_df$power_strength)
qqline(temp_df$power_strength, col = "red", lwd = 2)

# Tests of skewness 

skewness(temp_df$power_strength)
agostino.test(temp_df$power_strength)


# Test of kurtosis

anscombe.test(temp_df$power_strength)
 
# The "power_strength" attribute has a p-value for skewness which leads us to the conclusion that the data is skewed. 
# Its kurtosis p-value on the other hand is so significant that we can conclude that the data is normally distributed.
# 



 
# Histogram, Density Plot and Q-Q plot of Player Long shots

hist(temp_df$power_long_shots, xlab = "Player Long shots", main = "Histogram of Player Long shots" )

long_shots_density <- density(temp_df$power_long_shots)

plot(long_shots_density, lwd= 2, col= "blue")

qqnorm(temp_df$power_long_shots)
qqline(temp_df$power_long_shots, col = "red", lwd = 2)

# Tests of skewness 

skewness(temp_df$power_long_shots)
agostino.test(temp_df$power_long_shots)


# Test of kurtosis

anscombe.test(temp_df$power_long_shots)
 
# The diagnostic tests of skewness and kurtosis are significantly small enough for "power_long_shots" that we can conclude that the data is both skewed and not normally distributed.
# The Q-Q plot also shows that it is not normally distributed.




---
## Boxplots


# In the next section, I would check to see if there is any relationship between the numerical variables and the categorical variables.


# By Preferred Foot
 
ggplot(temp_df, aes(x=preferred_foot, y=potential)) +
  geom_boxplot(fill="steelblue2") +
  theme_classic() +
  labs(title="Box Plot of Potential by Preferred Foot")
 
# We can see from the above diagram that the player's preferred foot doesn't seem to have any significant effect on their potential.



 
ggplot(temp_df, aes(x=preferred_foot, y=wage_eur)) +
  geom_boxplot(fill="steelblue2") +
  theme_classic() +
  labs(title="Box Plot of Weekly Wage by Preferred Foot")
 
# From the above plot, it can be observed that preferred foot does not have any significant effect on player wages.



 
ggplot(temp_df, aes(x=preferred_foot, y=height_cm)) +
  geom_boxplot(fill="steelblue2") +
  theme_classic() +
  labs(title="Box Plot of Height by Preferred Foot")
 
# Preferred foot does not have a significant effect on the height of a player, from the above plot.



 
ggplot(temp_df, aes(x=preferred_foot, y=weight_kg)) +
  geom_boxplot(fill="steelblue2") +
  theme_classic() +
  labs(title="Box Plot of Weight by Preferred Foot")
 
# Neither does preferred foot have any significant effect on player weight.



 
ggplot(temp_df, aes(x=preferred_foot, y=pace)) +
  geom_boxplot(fill="steelblue2") +
  theme_classic() +
  labs(title="Box Plot of Pace by Preferred Foot")
 
# From the above plot, the median of the pace of players who prefer to use their left foot is higher than that of players who prefer to use their right. This relationship could be explored further.




 
ggplot(temp_df, aes(x=preferred_foot, y=shooting)) +
  geom_boxplot(fill="steelblue2") +
  theme_classic() +
  labs(title="Box Plot of Shooting by Preferred Foot")
 
# Shooting ability does not seem to be determined by player preferred foot, from the above diagram, as the medians are not significantly apart.




 
ggplot(temp_df, aes(x=preferred_foot, y=passing)) +
  geom_boxplot(fill="steelblue2") +
  theme_classic() +
  labs(title="Box Plot of Passing by Preferred Foot")
 
# Left footed players seem to have slightly better median passing ability than right footed players, although the difference is not so significant.




 
ggplot(temp_df, aes(x=preferred_foot, y=dribbling)) +
  geom_boxplot(fill="steelblue2") +
  theme_classic() +
  labs(title="Box Plot of Dribbling by Preferred Foot")
 
# The median dribbling ability of left footed players shows to be slightly higher than that of the right footed players, from the above plot.



 
ggplot(temp_df, aes(x=preferred_foot, y=defending)) +
  geom_boxplot(fill="steelblue2") +
  theme_classic() +
  labs(title="Box Plot of Defending by Preferred Foot")
 
# The median defending ability for a player whose preferred foot is the left, is higher than that of a player whose preferred foot is the right.



 
ggplot(temp_df, aes(x=preferred_foot, y=physic)) +
  geom_boxplot(fill="steelblue2") +
  theme_classic() +
  labs(title="Box Plot of Player Physic Attribute by Preferred Foot")
 
# There doesn't seem to be a significant difference in median physical ability based on player preferred foot.



 
ggplot(temp_df, aes(x=preferred_foot, y=power_strength)) +
  geom_boxplot(fill="steelblue2") +
  theme_classic() +
  labs(title="Box Plot of Strength by Preferred Foot")
 
# There doesn't seem to be a significant difference in the median strength of a player based on their preferred foot, though the it is slightly higher for right footed players.


 
ggplot(temp_df, aes(x=preferred_foot, y=power_long_shots)) +
  geom_boxplot(fill="steelblue2") +
  theme_classic() +
  labs(title="Box Plot of Long Shots by Preferred Foot")
 
# The median long shot ability is similar for players regardless of their preferred foot. The difference is not significant.


---
# By High Wage Indicator

 

ggplot(temp_df, aes(x=high.wage.ind, y=potential)) +
  geom_boxplot(fill="steelblue2") +
  theme_classic() +
  labs(title="Box Plot of Potential by High Wage Indicator")

 
# The median potential for players with higher wages is significantly higher than that for players with lower wages, hinting at a possible relationship between those two variables.



 
ggplot(temp_df, aes(x=high.wage.ind, y=wage_eur)) +
  geom_boxplot(fill="steelblue2") +
  theme_classic() +
  labs(title="Box Plot of Weekly Wage by High Wage Indicator")
 
# The median wage for high wage players is higher than the median wage for low wage players, as we would expect.



 
ggplot(temp_df, aes(x=high.wage.ind, y=age)) +
  geom_boxplot(fill="steelblue2") +
  theme_classic() +
  labs(title="Box Plot of Age by High Wage Indicator")
 
# The median age of players with higher wages seems to be higher than the median age of players with lower wages. This could indicate that there may be a relationship between those two variables, and they may have to be looked at later. Younger (and most likely inexperienced) players are usually not paid as much as their senior and more experienced counterparts in most organisations



 
ggplot(temp_df, aes(x=high.wage.ind, y=height_cm)) +
  geom_boxplot(fill="steelblue2") +
  theme_classic() +
  labs(title="Box Plot of Height by High Wage Indicator")
 
# There doesn't seem to be a significat difference in the median height of a player based on their high wage status.



 
ggplot(temp_df, aes(x=high.wage.ind, y=weight_kg)) +
  geom_boxplot(fill="steelblue2") +
  theme_classic() +
  labs(title="Box Plot of Weight by High Wage Indicator")
 
# The difference in median weight of a player based on high wage status is not very significant, from the above diagram.



 
ggplot(temp_df, aes(x=high.wage.ind, y=pace)) +
  geom_boxplot(fill="steelblue2") +
  theme_classic() +
  labs(title="Box Plot of Pace by High Wage Indicator")
 
# From the above plot, we can see that there is a significant difference in median pace of players with higher wage status and players with low wage status. This relationship can be probed further if required, at a later stage.



 
ggplot(temp_df, aes(x=high.wage.ind, y=shooting)) +
  geom_boxplot(fill="steelblue2") +
  theme_classic() +
  labs(title="Box Plot of Shooting by High Wage Indicator")
 
# We clearly see that the median shooting ability for players with high wage status is significantly higher than the median shooting ability for players with low wage status. This points to a relationship between these two variables.


 
ggplot(temp_df, aes(x=high.wage.ind, y=passing)) +
  geom_boxplot(fill="steelblue2") +
  theme_classic() +
  labs(title="Box Plot of Passing by High Wage Indicator")
 
# The median passing ability for players with high wage status is significantly higher than the median passing ability for players with low wage status. This is a relationship that can be explored later.


 
ggplot(temp_df, aes(x=high.wage.ind, y=dribbling)) +
  geom_boxplot(fill="steelblue2") +
  theme_classic() +
  labs(title="Box Plot of Dribbling by High Wage Indicator")
 
# The median dribbling ability for players with high wage status is significantly higher than that for players with a low wage status, from the plot above. This points to a potential relationship between "dribbling" and "high.wage.ind".


 
ggplot(temp_df, aes(x=high.wage.ind, y=defending)) +
  geom_boxplot(fill="steelblue2") +
  theme_classic() +
  labs(title="Box Plot of Defending by High Wage Indicator")
 
# There is a significant difference in the median defending ability of players who are of a higher wage status, and players who are of a lower wage status.


 
ggplot(temp_df, aes(x=high.wage.ind, y=physic)) +
  geom_boxplot(fill="steelblue2") +
  theme_classic() +
  labs(title="Box Plot of Player Physic Attribute by High Wage Indicator")
 
# There is a significant difference in the median physical ability of players who are of a higher wage status, and players who are of a lower wage status.



 
ggplot(temp_df, aes(x=high.wage.ind, y=power_strength)) +
  geom_boxplot(fill="steelblue2") +
  theme_classic() +
  labs(title="Box Plot of Strength by High Wage Indicator")
 
# There is a significant difference in the median strength of players who are of a higher wage status, and players who are of a lower wage status.


 
ggplot(temp_df, aes(x=high.wage.ind, y=power_long_shots)) +
  geom_boxplot(fill="steelblue2") +
  theme_classic() +
  labs(title="Box Plot of Long Shots by High Wage Indicator")
 

# There is a significant difference in the median long shot ability of players who are of a higher wage status, and players who are of a lower wage status.

# If anything can be taken from the previous section, it's that better players are paid much better.

---------------

## Multivariate analyses



# I create a subset of the data frame but with only the numerical variables. This will enable the plots of correlation matrices to be performed with ease.
 
numeric_temp_df <- subset(temp_df, select = c(potential, wage_eur,age,height_cm,weight_kg,pace,shooting,passing,dribbling,defending,physic,power_strength,power_long_shots))
 


# Now we check the structure of the new subset, to make sure that only numerical variables are present.
 
str(numeric_temp_df)
 
# We can see that only numerical variables are present, so we can perform the tests for correlation between the numerical variables.





# From the "dlookr" package we can use the correlate function and it is worth noting that it computes the Pearson correlation by default (Zablotski, 2021). Pearson measures the degree of the relationship between linearly related variables (Sarmento, n.d.).
 
correlate(temp_df,potential)
 
# We see from the result above that the significantly correlated variables with "potential"(moderate correlation($\pm 0.3$) to strong correlation ($\pm 1$)), are "age", "wage_eur", "pace", "shooting", "passing", "dribbling" and "power_long_shots".





# To plot the correlation matrix, I would use the Kendall correlation to do so. This is because the Kendall is more suited to measuring the strength of the dependence between two variables, without the assumption of them being normally distributed like Pearson assumes (Correlation (Pearson, Kendall, Spearman) - Statistics Solutions, n.d.).
 
plot_correlate(temp_df, method = "kendall")
 
# From the matrix above, we can see that the variables that show the highest levels of correlative dependence with potential are "wage_eur", "age"(albeit in a negative direction), "passing", "shooting", "dribbling" and "power_long_shots". 
# 
# 
# 
# Now I check for multi-colinearity.
 
cor(numeric_temp_df)
 
# From the above there does not seem to be any obvious multi collinearity and a few of the plots above point to potential for linear relationships. As a result of this, I am not going to explore any transformations at this stage.




# The next plot shows the variables which are not significant with regard to how much they correlate with "potential."
 
ggcorrmat(data = numeric_temp_df,)
 


# Here, we can see that "height_cm", "weight_kg" and "power_strength" are not at all significant with regard to how much they correlate with "potential".


## 2.3 Additional insights and issues



  # A potential issue that may come up could do with the variables that are not normally distributed. For such variables, how they will affect the model cannot be predicted at this stage, but some of these variables may require transformation. It is necessary to keep that in mind going forward.
  # The outlier values in "temp_df", as compared to "temp2_df" will have to be noted when the models are built, in order to observe the effects of such an outlier on a model.



# 3. Modelling

## 3.1 Build a model for player potential


  # Analysis Plan  
  # 
  # To start with, I would check for outliers in the data. In (1.3) I did that, and the only outliers that required removal were the negative values for "dribbling" and "pace", and values for "weight_kg" and "wage_eur" that were too small to be legitimate values. Nothing could be done about the other outliers for "wage_eur", as they fell within the possible range of values.
  # 
  # I would also check for colinearity between the explanatory variables, which I did in (2.2). There did not seem to be any multi-colinearity.
  # 
  # I would check for interactions between the variables.

 

model.tree<-tree(temp_df$potential~temp_df$wage_eur+temp_df$age+temp_df$height_cm+temp_df$weight_kg+temp_df$preferred_foot+temp_df$pace+temp_df$shooting+temp_df$passing+temp_df$dribbling+temp_df$defending+temp_df$physic+temp_df$power_strength+temp_df$power_long_shots+temp_df$high.wage.ind)

plot(model.tree)

text(model.tree)
 
# The above plot shows that "wage_eur" is the most important factor affecting "potential." "age" is important at both high and low "wage_eur". 
# We also see that there are a good number of interactions going on in the data set, with some being two-way or three-way interactions.
# Due to this, I will not use any interactions for this model as, with the kind of model I will build will have many explanatory variables and so adding too many interactions will generate an overly complex model that will require a larger sample of data to be able to model it.




# 
#   I would find out if the relationship between my response variable and explanatory variables is linear. In (2.2), I concluded that the data points to having a linear relationship between response and explanatory variables.
# 
#   I would check to see if I have a big enough sample. With over 500 rows of data in the data frames, I can conclude that this is so.
# 
#   Then finally, I would build a maximal model first, and then attempt to simplify it to its most useful elements. 
# 
# 
# 
#   Model Building  
# 
# 
# Now, I will build the model.
# I would start by building a maximal model for potential, using all variables (continuous or otherwise).

 
potential_maximal_model.lm <- lm(temp_df$potential~temp_df$wage_eur+temp_df$age+temp_df$height_cm+temp_df$weight_kg+temp_df$preferred_foot+temp_df$pace+temp_df$shooting+temp_df$passing+temp_df$dribbling+temp_df$defending+temp_df$physic+temp_df$power_strength+temp_df$power_long_shots+temp_df$high.wage.ind)

summary.lm(potential_maximal_model.lm)
 
# From the maximal model, it can be seen that the prediction of "potential" is most dependent on the variables "wage_eur", "age", "dribbling", "shooting", "passing", "power_strength", "high.wage.ind1", "defending", "height_cm", "physic", and,"power_long_shots".
# In all these cases, except for that of "age" and "power_long_shots", an increase in their values accounts for an increase in the estimate for "potential".
# 
# In (2.2) from the correlation matrix, "height_cm" was found out to not be significantly correlated with "potential", but we find in the maximal model that "height_cm" is actually significant to predict "potential".
# 
# 
# 
# 
# I will build the minimal adequate model in the chunk below, to see how the step function would simplify the model.
 

potential_minimal_model.lm <- step(potential_maximal_model.lm)

 

# Let's look at the resulting model in more detail in the next chunk
 
 final_potential_minimal_model.lm <- lm(temp_df$potential ~ temp_df$wage_eur + temp_df$age + temp_df$height_cm + temp_df$shooting + temp_df$dribbling + temp_df$defending + temp_df$physic + temp_df$power_long_shots + temp_df$high.wage.ind)

summary.lm(final_potential_minimal_model.lm)
 
# The minimal model equation now looks like:

# $$potential= 46.81 + 0.00004949\times wage\_eur - 0.7612 \times age + 0.07273\times height\_cm + 0.1112 \times shooting+ 0.2568\times dribbling + 0.09918 \times defending + 0.08644\times physic - 0.06568\times power\_long\_shots + 3.178\times high.wage.ind1$$


## 3.2 Critique model using relevant diagnostics

# 
#   We can see that for each variable in the model, "potential" is inversely related to "age" and "power_long_shots", but for the other constituent variables of the model, any increase in their values results in an increase in the predicted value of "potential".
# 
#   From 3.1, we can see that the model has a more than 60% goodness of fit with a significant F-statistic. All the coefficients of the model are significant, hence there is no need to simplify further.
# 
# 
# Next, I take a look at the diagnostic plots.

 
plot(final_potential_minimal_model.lm)
 
# For this model, the residuals look fine. The variance seems to be steady in the first plot.
# 
# The Q-Q plot also looks to be aligned, save a few outliers which can be looked at, if there is a need to.
# 
# 
#   Potential weaknesses:  
# 
#   The model can easily be affected by outliers. We seem to have a considerable number of outliers showing in the plots and too many of them could have unforeseen effects on the model.
#   There is also a tendency for the model to miss any non-linear relationships between variables.



# 4. Extension 

## 4.1 Model the likelihood of a player having a weekly wage above 8000 Euro (using the high.wage.ind variable provided).

#  Given this second research question (i.e., involving the binary target attribute) provide a plan of analysis based on relevant EDA for this attribute. The model is described, explained and critiqued. 
# 
#   Analysis Plan  
# 
#   I would check for outliers in the data set. The "wage_eur" outliers were checked in (1.3) and it was concluded that they did not violate any rules of data integrity, hence they were left in the data set.
#   Check how "high.wage.ind" interacts with the other variables of the data set. I did this in (2.2) with the boxplots. Those plots only showed the quartiles and so, may not clearly depict the relationships among the variables. I would use all variables to build the model, so it shows which variables are actually significant in predicting "high.wage.ind".
# 
# 
# 
# To find out what the interaction is between the only two binary variables in the data set ("high.wage.ind" and "preferred_foot"), we run the next chunk.
 
table(temp_df$high.wage.ind, temp_df$preferred_foot)
chisq.test(table(temp_df$high.wage.ind, temp_df$preferred_foot))
#  
# This p-value confirms that the variables "high.wage.ind" and "preferred_foot" are independent. 
#  It's good to note that there is no cell in the table that is so small that we need to consider using Fisher's exact test. 
# 
#   
#   After all that is done, I will build the model.



## 4.2 Model Building, Description and Explanation

# I will now build the logistic regression model for high wage indicator below. I would start off with a maximal model and then use stepwise selection to find out the minimal adequate model.
 
wage_model.glm<-glm(temp_df$high.wage.ind~temp_df$potential+temp_df$wage_eur+temp_df$age+temp_df$height_cm+temp_df$weight_kg+temp_df$preferred_foot+temp_df$pace+temp_df$shooting+temp_df$passing+temp_df$dribbling+temp_df$defending+temp_df$physic+temp_df$power_strength+temp_df$power_long_shots, family = "binomial")

summary(wage_model.glm)
 
# At this stage, we get two warnings. The first one tells us that the algorithm did not converge. 
# A warning like this occurs when we try to fit a logistic regression model and we experience perfect separation - "having a predictor variable that is able to perfectly separate the response variable into 0's and 1's"(How to Handle R Warning: glm.fit: algorithm did not converge - Statology, 2021).
# 
# The second warning reads:fitted probabilities numerically 0 or 1 occurred. This warning occurs when a logistic regression model is fitted and "the predicted probabilities of one or more observations in the data frame are indistinguishable from 0 or 1" (How to Handle: glm.fit: fitted probabilities numerically 0 or 1 occurred - Statology, 2021).
# 
# It is worth noting though that these warning messages aren't error messages, and so, will allow the model to be fit.


# Looking at this output above, none of the variables appear to be significant. Let's simplify the model to see whether this will change.




# Using stepwise selection now.
 
step(wage_model.glm)
 


# Now we can scrutinise the minimal model selected more closely in the next chunk.
 
final_wage_model.glm <- glm(formula = temp_df$high.wage.ind ~ temp_df$wage_eur, family = "binomial")

summary(final_wage_model.glm)

#  
# 
# The model has the following formula for a player who earns less than 8000 Euros a week, i.e., for high.wage.ind = 0:
# 
# $$log(\frac{p}{1-p})= -31.63+ 0.003728 \times wage\_eur \times high.wage.ind (0)$$
# 
# Now, for a player who earns more than 8000 Euros a week, i.e. for high.wage.ind = 1:
# 
# 
# $$log(\frac{p}{1-p})= -31.63+ 0.003728 \times wage\_eur \times high.wage.ind (1)$$



# Now, I will calculate the odds ratio 

 
exp(coef(final_wage_model.glm))
 
# From this, we can see that the odds of "high.wage.ind" occurring at "wage_eur" = 0 is $1.838841 \times 10 ^{-14}$. In other words, it's basically impossible.


## 4.3 Model Critique

  # We find that only "wage_eur" is necessary in predicting "high.wage.ind" (and this is consistent with the description in the assessment brief, because the distinction in "high.wage.ind" is based on whether a player's "wage_eur" value is greater than 8000 or not) - and even with that, not to any notable degree of significance.
  # 
  # Due to having only one variable predicting the model, this model is unnecessary. We don't actually need a model to know what the value of high.wage.ind will be.







# References  

# Oldest.Org. 2021. 11 Oldest Soccer Players in the World (Updated 2021). [online] Available at: <https://www.oldest.org/sports/soccer-players/> [Accessed 18 December 2021].
# 
# Sarmento, D., n.d. Chapter 22: Correlation Types and When to Use Them. [online] Ademos.people.uic.edu. Available at: <https://ademos.people.uic.edu/Chapter22.html> [Accessed 28 December 2021].
# 
# 
# Shepperd, M., 2021. CS5702 Modern Data Book. [online] Bookdown.org. Available at: <https://bookdown.org/martin_shepperd/ModernDataBook/> [Accessed 17 December 2021].
# 
# Statistics Solutions. n.d. Correlation (Pearson, Kendall, Spearman) - Statistics Solutions. [online] Available at: <https://www.statisticssolutions.com/free-resources/directory-of-statistical-analyses/correlation-pearson-kendall-spearman/> [Accessed 29 December 2021].
# 
# Statology. 2021. How to Handle R Warning: glm.fit: algorithm did not converge - Statology. [online] Available at: <https://www.statology.org/glm-fit-algorithm-did-not-converge/> [Accessed 4 January 2022].
# 
# Statology. 2021. How to Handle: glm.fit: fitted probabilities numerically 0 or 1 occurred - Statology. [online] Available at: <https://www.statology.org/glm-fit-fitted-probabilities-numerically-0-or-1-occurred/> [Accessed 4 January 2022].
# 
# Zablotski, Y., 2021. yuzaR-Blog: Deep Exploratory Data Analysis (EDA) in R. [online] yuzaR-Blog. Available at: <https://yuzar-blog.netlify.app/posts/2021-01-09-exploratory-data-analysis-and-beyond-in-r-in-progress/#kurtosis> [Accessed 17 December 2021].

