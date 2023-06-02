#loading The dataset
heart_failure <- read.csv("HeartFailure.csv", header = TRUE)
heart_failure
str(heart_failure)

# To check Na values
any(is.na(heart_failure))
cor_hf <- cor(heart_failure)
cor_hf
#*age	anaemia	creatinine_phosphokinase	diabetes
#*	ejection_fraction	high_blood_pressure	platelets
#*		serum_creatinine	serum_sodium
#*			sex	smoking	time	DEATH_EVENT *#


#*  
#* 
#* 
#* 
#* 
#*  Examine initial linearity between variables in the dataset*#
install.packages("psych")
library(psych)
pairs.panels(heart_failure,
             smooth = FALSE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = FALSE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals


#*
#* Examine linearity in more detail 
#* platelets vs DEATH_EVENT *#
scatter.smooth(x = heart_failure$DEATH_EVENT,
               y = heart_failure$platelets,
               xlab = "DEATH_EVENT",
               ylab = "serum_sodium", 
               main = "Correlation of serum_sodium ~ DEATH_EVENT")

# chart appears to have medium correlation
# low correlation (-0.2 < x < 0.2) suggests that 
# much of variation of the response
# variable (Y) is unexplained by the predictor (X)
# in which case, we should probably look for better
# explanatory variables
scatter.smooth(x = heart_failure$DEATH_EVENT,
               y = heart_failure$platelets,
               xlab = "DEATH_EVENT",
               ylab = "platelets", 
               main = "Correlation of platelets ~ DEATH_EVENT")

# chart appears to have medium correlation
# low correlation (-0.2 < x < 0.2) suggests that 
# much of variation of the response
# variable (Y) is unexplained by the predictor (X)
# in which case, we should probably look for better
# explanatory variables
cor(heart_failure$platelets, heart_failure$DEATH_EVENT)
cor(heart_failure)

#* Examine linearity in more detail 
#* anaemia vs platelets *#
scatter.smooth(x = heart_failure$anaemia,
               y = heart_failure$platelets,
               xlab = "anaemia",
               ylab = "platelets", 
               main = "Correlation of anaemia ~ platelets")

# chart appears to have medium correlation
# low correlation (-0.2 < x < 0.2) suggests that 
# much of variation of the response
# variable (Y) is unexplained by the predictor (X)
# in which case, we should probably look for better
# explanatory variables
cor(heart_failure$platelets, heart_failure$anaemia)


#* Examine linearity in more detail 
#* creatinine_phosphokinase vs platelets *#
scatter.smooth(x = heart_failure$creatinine_phosphokinase,
               y = heart_failure$platelets,
               xlab = "creatinine_phosphokinase",
               ylab = "platelets", 
               main = "Correlation of creatinine_phosphokinase ~ platelets")

# chart appears to have medium correlation
# low correlation (-0.2 < x < 0.2) suggests that 
# much of variation of the response
# variable (Y) is unexplained by the predictor (X)
# in which case, we should probably look for better
# explanatory variables
cor(heart_failure$creatinine_phosphokinase, heart_failure$platelets)

#* Examine linearity in more detail 
#* diabetes vs platelets *#
scatter.smooth(x = heart_failure$diabetes,
               y = heart_failure$platelets,
               xlab = "diabetes",
               ylab = "platelets", 
               main = "Correlation of diabetes ~ platelets")

# chart appears to have medium correlation
# low correlation (-0.2 < x < 0.2) suggests that 
# much of variation of the response
# variable (Y) is unexplained by the predictor (X)
# in which case, we should probably look for better
# explanatory variables
cor(heart_failure$diabetes, heart_failure$platelets)

#* Examine linearity in more detail 
#* ejection_fraction vs platelets *#
scatter.smooth(x = heart_failure$ejection_fraction,
               y = heart_failure$platelets,
               xlab = "ejection_fraction",
               ylab = "platelets", 
               main = "Correlation of ejection_fraction ~ platelets")

# chart appears to have medium correlation
# low correlation (-0.2 < x < 0.2) suggests that 
# much of variation of the response
# variable (Y) is unexplained by the predictor (X)
# in which case, we should probably look for better
# explanatory variables
cor(heart_failure$ejection_fraction, heart_failure$platelets)


#* Examine linearity in more detail 
#* high_blood_pressure vs platelets *#
scatter.smooth(x = heart_failure$high_blood_pressure,
               y = heart_failure$platelets,
               xlab = "high_blood_pressure",
               ylab = "platelets", 
               main = "Correlation of high_blood_pressure ~ platelets")

# chart appears to have medium correlation
# low correlation (-0.2 < x < 0.2) suggests that 
# much of variation of the response
# variable (Y) is unexplained by the predictor (X)
# in which case, we should probably look for better
# explanatory variables
cor(heart_failure$high_blood_pressure, heart_failure$platelets)

#* Examine linearity in more detail 
#* platelets vs age *#
scatter.smooth(x = heart_failure$age,
               y = heart_failure$platelets,
               xlab = "age",
               ylab = "platelets", 
               main = "Correlation of age ~ platelets")

# chart appears to have medium correlation
# low correlation (-0.2 < x < 0.2) suggests that 
# much of variation of the response
# variable (Y) is unexplained by the predictor (X)
# in which case, we should probably look for better
# explanatory variables
cor(heart_failure$platelets, heart_failure$age)



#* Examine linearity in more detail 
#* serum_creatinine vs age *#
scatter.smooth(x = heart_failure$serum_creatinine,
               y = heart_failure$platelets,
               xlab = "serum_creatinine",
               ylab = "platelets", 
               main = "Correlation of serum_creatinine ~ platelets")

# chart appears to have medium correlation
# low correlation (-0.2 < x < 0.2) suggests that 
# much of variation of the response
# variable (Y) is unexplained by the predictor (X)
# in which case, we should probably look for better
# explanatory variables
cor(heart_failure$serum_creatinine, heart_failure$platelets)



#* Examine linearity in more detail 
#* serum_sodium vs DEATH_EVENT *#
scatter.smooth(x = heart_failure$serum_sodium,
               y = heart_failure$platelets,
               xlab = "serum_sodium",
               ylab = "platelets", 
               main = "Correlation of serum_sodium ~ platelets")

# chart appears to have medium correlation
# low correlation (-0.2 < x < 0.2) suggests that 
# much of variation of the response
# variable (Y) is unexplained by the predictor (X)
# in which case, we should probably look for better
# explanatory variables
cor(heart_failure$serum_sodium, heart_failure$platelets)


#* Examine linearity in more detail 
#* sex vs platelets *#
scatter.smooth(x = heart_failure$sex,
               y = heart_failure$platelets,
               xlab = "sex",
               ylab = "platelets", 
               main = "Correlation of sex ~ platelets")

# chart appears to have medium correlation
# low correlation (-0.2 < x < 0.2) suggests that 
# much of variation of the response
# variable (Y) is unexplained by the predictor (X)
# in which case, we should probably look for better
# explanatory variables
cor(heart_failure$sex, heart_failure$platelets)

#* Examine linearity in more detail 
#* smoking vs platelets *#
scatter.smooth(x = heart_failure$smoking,
               y = heart_failure$platelets,
               xlab = "smoking",
               ylab = "platelets", 
               main = "Correlation of smoking ~ platelets")

warnings()
# chart appears to have medium correlation
# low correlation (-0.2 < x < 0.2) suggests that 
# much of variation of the response
# variable (Y) is unexplained by the predictor (X)
# in which case, we should probably look for better
# explanatory variables
cor(heart_failure$smoking, heart_failure$platelets)



#* Examine linearity in more detail 
#* time vs platelets *#
scatter.smooth(x = heart_failure$time,
               y = heart_failure$platelets,
               ylab = "time",
               xlab = "platelets", 
               main = "Correlation of time ~ platelets")

# chart appears to have medium correlation
# low correlation (-0.2 < x < 0.2) suggests that 
# much of variation of the response
# variable (Y) is unexplained by the predictor (X)
# in which case, we should probably look for better
# explanatory variables
cor(heart_failure$time, heart_failure$platelets)


# Examining correlation of all the variables
paste("Correlation for platelets and DEATH_EVENT: ", cor(heart_failure$platelets, heart_failure$DEATH_EVENT))
paste("Correlation for platelets and anaemia: ", cor(heart_failure$platelets, heart_failure$anaemia))
paste("Correlation for platelets and creatinine_phosphokinase: ", cor(heart_failure$platelets, heart_failure$creatinine_phosphokinase))
paste("Correlation for platelets and diabetes: ", cor(heart_failure$platelets, heart_failure$diabetes))
paste("Correlation for platelets and ejection_fraction: ", cor(heart_failure$platelets, heart_failure$ejection_fraction))
paste("Correlation for platelets and high_blood_pressure: ", cor(heart_failure$platelets, heart_failure$high_blood_pressure))
paste("Correlation for platelets and platelets: ", cor(heart_failure$age, heart_failure$platelets))
paste("Correlation for platelets and serum_creatinine: ", cor(heart_failure$platelets, heart_failure$serum_creatinine))
paste("Correlation for platelets and serum_sodium: ", cor(heart_failure$platelets, heart_failure$serum_sodium))
paste("Correlation for platelets and sex: ", cor(heart_failure$platelets, heart_failure$sex))
paste("Correlation for platelets and smoking: ", cor(heart_failure$platelets, heart_failure$smoking))
paste("Correlation for platelets and time: ", cor(heart_failure$platelets, heart_failure$time))

corelation <- cor(heart_failure)

corelation

#* It appears that the variable serum_creatinine:    has a vary low correlation with DEATH_EVENT.
#*  Therefore I am going to remove it from the dataset. 
#*  Alternatively we can choose to exclude these independent variables when
#*  we are constructing the MLR model.*#


# Check for outliers
opar <- par(no.readonly = TRUE)
par(mfrow = c(3, 2)) # divide graph area in 6 rows by 2 columns
attach(heart_failure)
boxplot(DEATH_EVENT,
        main = "DEATH_EVENT",
        sub = paste("Outlier rows: ",
                    boxplot.stats(DEATH_EVENT)$out)) # box plot for 'DEATH_EVENT'
boxplot(age,
        main = "age",
        sub = paste("Outlier rows: ",
                    boxplot.stats(age)$out)) # box plot for 'age'
boxplot(anaemia,
        main = "anaemia",
        sub = paste("Outlier rows: ",
                    boxplot.stats(anaemia)$out)) # box plot for 'anaemia'
boxplot(creatinine_phosphokinase,
        main = "creatinine_phosphokinase",
        sub = paste("Outlier rows: ",
                    boxplot.stats(creatinine_phosphokinase)$out)) # box plot for 'creatinine_phosphokinase'
boxplot(diabetes,
        main = "diabetes",
        sub = paste("Outlier rows: ",
                    boxplot.stats(diabetes)$out)) # box plot for 'diabetes'
boxplot(ejection_fraction,
        main = "ejection_fraction",
        sub = paste("Outlier rows: ",
                    boxplot.stats(ejection_fraction)$out)) # box plot for 'ejection_fraction'
boxplot(high_blood_pressure,
        main = "high_blood_pressure",
        sub = paste("Outlier rows: ",
                    boxplot.stats(high_blood_pressure)$out)) # box plot for 'high_blood_pressure'
boxplot(platelets,
        main = "platelets",
        sub = paste("Outlier rows: ",
                    boxplot.stats(platelets)$out)) # box plot for 'platelets'
boxplot(serum_creatinine,
        main = "serum_creatinine",
        sub = paste("Outlier rows: ",
                    boxplot.stats(serum_creatinine)$out)) # box plot for 'serum_creatinine'
boxplot(serum_sodium,
        main = "serum_sodium",
        sub = paste("Outlier rows: ",
                    boxplot.stats(serum_sodium)$out)) # box plot for 'serum_sodium'
boxplot(sex,
        main = "sex",
        sub = paste("Outlier rows: ",
                    boxplot.stats(sex)$out)) # box plot for 'sex'
boxplot(smoking,
        main = "smoking",
        sub = paste("Outlier rows: ",
                    boxplot.stats(smoking)$out)) # box plot for 'smoking'
boxplot(time,
        main = "time",
        sub = paste("Outlier rows: ",
                    boxplot.stats(time)$out)) # box plot for 'time'

summary(heart_failure)
detach(heart_failure)
par(opar)
#*Variables with outliers
#*
#*creatinine_phosphokinase
#*ejection_fraction
#*platelets	
#*serum_creatinine	
#*serum_sodium*#

# Use boxplot.stats() function to generate relevant outliers
outlier_values <- boxplot.stats(heart_failure$creatinine_phosphokinase)$out # outlier values.
paste("creatinine_phosphokinase outliers: ", paste(outlier_values, collapse=", "))

# Use boxplot.stats() function to generate relevant outliers
outlier_values <- boxplot.stats(heart_failure$ejection_fraction)$out # outlier values.
paste("ejection_fraction outliers: ", paste(outlier_values, collapse=", "))

# Use boxplot.stats() function to generate relevant outliers
outlier_values <- boxplot.stats(heart_failure$platelets)$out # outlier values.
paste("platelets outliers: ", paste(outlier_values, collapse=", "))

# Use boxplot.stats() function to generate relevant outliers
outlier_values <- boxplot.stats(heart_failure$serum_creatinine)$out # outlier values.
paste("serum_creatinine outliers: ", paste(outlier_values, collapse=", "))

# Use boxplot.stats() function to generate relevant outliers
outlier_values <- boxplot.stats(heart_failure$serum_sodium)$out # outlier values.
paste("serum_sodium outliers: ", paste(outlier_values, collapse=", "))

#*creatinine_phosphokinase,
#*ejection_fraction
#*platelets 
#*serum_creatinine 
#*serum_sodium has outliers..
#*since 
#*serum_creatinine ,
#* platelets, 
#* creatinine_phosphokinase 
#* has toomany ouliers we 
#* avoid these 3 parameters*#

# Remove ejection_fraction outliers outliers
heart_failure <- subset(heart_failure,
                        heart_failure$ejection_fraction != 80
                        & heart_failure$ejection_fraction != 70)
# Remove serum_sodium outliers
heart_failure <- subset(heart_failure,
                 heart_failure$serum_sodium != 116
                 & heart_failure$serum_sodium != 121
                 & heart_failure$serum_sodium != 124
                 & heart_failure$serum_sodium != 113)

# Remove platelets  outliers
heart_failure <- subset(heart_failure,
                        heart_failure$platelets  != 454000
                        & heart_failure$platelets  != 47000
                        & heart_failure$platelets  != 451000
                        & heart_failure$platelets  != 461000
                        & heart_failure$platelets  != 497000
                        & heart_failure$platelets  != 621000
                        & heart_failure$platelets  != 850000
                        & heart_failure$platelets  != 507000
                        & heart_failure$platelets  != 448000
                        & heart_failure$platelets  != 75000
                        & heart_failure$platelets  != 70000
                        & heart_failure$platelets  != 73000
                        & heart_failure$platelets  != 481000
                        & heart_failure$platelets  != 504000
                        & heart_failure$platelets  != 62000
                        & heart_failure$platelets  != 533000
                        & heart_failure$platelets  != 25100
                        & heart_failure$platelets  != 451000
                        & heart_failure$platelets  != 51000
                        & heart_failure$platelets  != 543000
                        & heart_failure$platelets  != 742000
                        & heart_failure$platelets  != 427000
                        & heart_failure$platelets  != 422000
                        & heart_failure$platelets  != 418000)

# Re-run the box-plots to verify that outliers have now gone.
# Check for normality
# Skewness function to examine normality
install.packages("e1071")
library(e1071)
opar <- par(no.readonly = TRUE)
par(mfrow = c(4,2)) # divide graph area into 1 row x 2 cols

# skewness of < -1 or > 1 = highly skewed
# -1 to -0.5 and 0.5 to 1 = moderately skewed
# Skewness of -0.5 to 0.5 = approx symetrical

plot(density(heart_failure$DEATH_EVENT),
     main = "Density plot : DEATH_EVENT",
     ylab = "Frequency", xlab = "DEATH_EVENT",
     sub = paste("Skewness : ", round(e1071::skewness(heart_failure$DEATH_EVENT), 2)))
# fill the area under the plot
polygon(density(heart_failure$DEATH_EVENT), col = "red")

plot(density(heart_failure$age),
     main = "Density plot : age",
     ylab = "Frequency", xlab = "age",
     sub = paste("Skewness : ", round(e1071::skewness(heart_failure$age), 2)))
# fill the area under the plot
polygon(density(heart_failure$age), col = "red")

plot(density(heart_failure$anaemia),
     main = "Density plot : anaemia",
     ylab = "Frequency", xlab = "anaemia",
     sub = paste("Skewness : ", round(e1071::skewness(heart_failure$anaemia), 2)))
# fill the area under the plot
polygon(density(heart_failure$anaemia), col = "red")

plot(density(heart_failure$diabetes),
     main = "Density plot : diabetes",
     ylab = "Frequency", xlab = "diabetes",
     sub = paste("Skewness : ", round(e1071::skewness(heart_failure$diabetes), 2)))
# fill the area under the plot
polygon(density(heart_failure$diabetes), col = "red")

plot(density(heart_failure$ejection_fraction),
     main = "Density plot : ejection_fraction",
     ylab = "Frequency", xlab = "ejection_fraction",
     sub = paste("Skewness : ", round(e1071::skewness(heart_failure$ejection_fraction), 2)))
# fill the area under the plot
polygon(density(heart_failure$ejection_fraction), col = "red")

plot(density(heart_failure$high_blood_pressure),
     main = "Density plot : high_blood_pressure",
     ylab = "Frequency", xlab = "high_blood_pressure",
     sub = paste("Skewness : ", round(e1071::skewness(heart_failure$high_blood_pressure), 2)))
# fill the area under the plot
polygon(density(heart_failure$high_blood_pressure), col = "red")

plot(density(heart_failure$serum_sodium),
     main = "Density plot : serum_sodium",
     ylab = "Frequency", xlab = "serum_sodium",
     sub = paste("Skewness : ", round(e1071::skewness(heart_failure$serum_sodium), 2)))
# fill the area under the plot
polygon(density(heart_failure$serum_sodium), col = "red")

plot(density(heart_failure$sex),
     main = "Density plot : sex",
     ylab = "Frequency", xlab = "sex",
     sub = paste("Skewness : ", round(e1071::skewness(heart_failure$sex), 2)))
# fill the area under the plot
polygon(density(heart_failure$sex), col = "red")

plot(density(heart_failure$smoking),
     main = "Density plot : smoking",
     ylab = "Frequency", xlab = "smoking",
     sub = paste("Skewness : ", round(e1071::skewness(heart_failure$smoking), 2)))
# fill the area under the plot
polygon(density(heart_failure$smoking), col = "red")

plot(density(heart_failure$time),
     main = "Density plot : time",
     ylab = "Frequency", xlab = "time",
     sub = paste("Skewness : ", round(e1071::skewness(heart_failure$time), 2)))
# fill the area under the plot
polygon(density(heart_failure$time), col = "red")



par(opar)

# NB a skewness value <-1 or >1 = highly skewed. 
# Skewness -1 to -05 and 0.5 to 1 = moderately skewed. 
# And skewness -0.5 to 0-5 = approx symetric.

#*DEATH_EVENT -  0.8 - mod skew
#*age - 0.4  - approx symetric.
#*anaemia 0.28 approx symetric.
#*diabetes - 0.35
#*ejection_fraction - 0.45
#*high_blood_pressure - 0.62
#*serum_sodium - -0.12
#*sex  - -0.67 moderatly skewed to lrft
#*smoking - 0.75 moderatly skewed to right
#*time - 0.13
#**#

paste("Skewness for DEATH_EVENT : ", round(e1071::skewness(heart_failure$DEATH_EVENT), 2))
paste("Skewness for age : ", round(e1071::skewness(heart_failure$age), 2))
paste("Skewness for anaemia : ", round(e1071::skewness(heart_failure$anaemia), 2))
paste("Skewness for diabetes : ", round(e1071::skewness(heart_failure$diabetes), 2))
paste("Skewness for ejection_fraction : ", round(e1071::skewness(heart_failure$ejection_fraction), 2))
paste("Skewness for high_blood_pressure : ", round(e1071::skewness(heart_failure$high_blood_pressure), 2))
paste("Skewness for serum_sodium : ", round(e1071::skewness(heart_failure$serum_sodium), 2))
paste("Skewness for sex : ", round(e1071::skewness(heart_failure$sex), 2))
paste("Skewness for smoking : ", round(e1071::skewness(heart_failure$smoking), 2))
paste("Skewness for time : ", round(e1071::skewness(heart_failure$time), 2))

hist(heart_failure$DEATH_EVENT)
hist(heart_failure$age)
hist(heart_failure$anaemia)
hist(heart_failure$diabetes)
hist(heart_failure$ejection_fraction)
hist(heart_failure$high_blood_pressure)
hist(heart_failure$serum_sodium)
hist(heart_failure$sex)
hist(heart_failure$smoking)
hist(heart_failure$time)


# p-value indices that the data is not normally distributed
shapiro.test(heart_failure$DEATH_EVENT)
shapiro.test(heart_failure$age)
shapiro.test(heart_failure$anaemia)
shapiro.test(heart_failure$diabetes)
shapiro.test(heart_failure$ejection_fraction)
shapiro.test(heart_failure$high_blood_pressure)
shapiro.test(heart_failure$serum_sodium)
shapiro.test(heart_failure$sex)
shapiro.test(heart_failure$smoking)
shapiro.test(heart_failure$time)

# If p-value < 0.05 then variable
# is not normally distributed


# Need to transform the data
library(MASS)

# Convert Age
# find optimal lambda for box-cox transform
# Plotting lambda from -6 to 6 in steps of 0.1
Box = boxcox(heart_failure$age ~ 1, lambda = seq(-6,6,0.1))
# Create a data frame with the results
Cox = data.frame(Box$x, Box$y)   
# Order the new data frame by decreasing y
cox_smallest_y = Cox[with(Cox, order(-Cox$Box.y)),] 
# Display the lambda with the greatest
# log likelihood
cox_smallest_y[1,]                                  
# Extract that lambda
lambda = cox_smallest_y[1, "Box.x"]                 
lambda
transformed_age = (heart_failure$age ^ lambda - 1)/lambda
hist(transformed_age)

# p-value indictes that the data is now normally distributed
shapiro.test(transformed_age)
# age is still not normally distributed

# Convert ejection_fraction
# find optimal lambda for box-cox transform
# Plotting lambda from -6 to 6 in steps of 0.1
Box = boxcox(heart_failure$ejection_fraction ~ 1, lambda = seq(-6,6,0.1))
# Create a data frame with the results
Cox = data.frame(Box$x, Box$y)   
# Order the new data frame by decreasing y
cox_smallest_y = Cox[with(Cox, order(-Cox$Box.y)),] 
# Display the lambda with the greatest
# log likelihood
cox_smallest_y[1,]                                  
# Extract that lambda
lambda = cox_smallest_y[1, "Box.x"]                 
lambda
transformed_ejection_fraction = (heart_failure$age ^ lambda - 1)/lambda
hist(transformed_ejection_fraction)

# p-value indictes that the data is now normally distributed
shapiro.test(transformed_ejection_fraction)
# ejection_fraction is still not normally distributed

# Convert serum_sodium
# find optimal lambda for box-cox transform
# Plotting lambda from -6 to 6 in steps of 0.1
Box = boxcox(heart_failure$serum_sodium ~ 1, lambda = seq(-6,6,0.1))
# Create a data frame with the results
Cox = data.frame(Box$x, Box$y)   
# Order the new data frame by decreasing y
cox_smallest_y = Cox[with(Cox, order(-Cox$Box.y)),] 
# Display the lambda with the greatest
# log likelihood
cox_smallest_y[1,]                                  
# Extract that lambda
lambda = cox_smallest_y[1, "Box.x"]                 
lambda
transformed_serum_sodium = (heart_failure$serum_sodium ^ lambda - 1)/lambda
hist(transformed_serum_sodium)

# p-value indictes that the data is now normally distributed
shapiro.test(transformed_serum_sodium)
# serum_sodium is still not normally distributed


# Convert time
# find optimal lambda for box-cox transform
# Plotting lambda from -6 to 6 in steps of 0.1
Box = boxcox(heart_failure$time ~ 1, lambda = seq(-6,6,0.1))
# Create a data frame with the results
Cox = data.frame(Box$x, Box$y)   
# Order the new data frame by decreasing y
cox_smallest_y = Cox[with(Cox, order(-Cox$Box.y)),] 
# Display the lambda with the greatest
# log likelihood
cox_smallest_y[1,]                                  
# Extract that lambda
lambda = cox_smallest_y[1, "Box.x"]                 
lambda
transformed_time = (heart_failure$time ^ lambda - 1)/lambda
hist(transformed_time)

# p-value indictes that the data is now normally distributed
shapiro.test(transformed_time)
# time is still not normally distributed

cor_hf

# Tukey’s Ladder of Powers transformation
# also indicates that the best value for 
# 
library(rcompanion)

transformed_turkey_age = transformTukey(heart_failure$age, plotit=FALSE)
transformed_turkey_age <- -1 * heart_failure$age ^ -0.625
shapiro.test(transformed_turkey_age)

transformed_turkey_ejection_fraction = transformTukey(heart_failure$ejection_fraction, plotit=FALSE)
transformed_turkey_ejection_fraction <- -1 * heart_failure$ejection_fraction ^ -0.625
shapiro.test(transformed_turkey_ejection_fraction)

transformed_turkey_serum_sodium = transformTukey(heart_failure$serum_sodium, plotit=FALSE)
transformed_turkey_serum_sodium <- -1 * heart_failure$serum_sodium ^ -0.625
shapiro.test(transformed_turkey_serum_sodium)

transformed_turkey_time = transformTukey(heart_failure$time, plotit=FALSE)
transformed_turkey_time <- -1 * heart_failure$time ^ -0.625
shapiro.test(transformed_turkey_time)


# Tukey’s Ladder of Powers transformation is also failed

# Converting data in data frame
heart_failure$transformed_age <- transformed_age
heart_failure$transformed_ejection_fraction <- transformed_ejection_fraction
heart_failure$transformed_serum_sodium <- transformed_serum_sodium
heart_failure$transformed_time <- transformed_time
# model

attach(heart_failure)

# Split the data into training and testing
set.seed(1)
no_rows_data <- nrow(heart_failure)
sample <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace = FALSE)

training_data <- heart_failure[sample, ]
testing_data <- heart_failure[-sample, ]

unmodified_model <- lm(serum_sodium ~ DEATH_EVENT + age + anaemia + diabetes + ejection_fraction + high_blood_pressure + serum_sodium + sex + smoking + time , data=training_data)
summary(unmodified_model)

# Examine which combination of independent variables best fits the model
install.packages("leaps")
library(leaps)
# See https://cran.r-project.org/web/packages/leaps/leaps.pdf
# Regression Subset Selection
MLR_subset_selection <-regsubsets(platelets ~ DEATH_EVENT + age + anaemia + diabetes + ejection_fraction + high_blood_pressure + serum_sodium + sex + smoking + time , data=training_data, nbest=6)
plot(MLR_subset_selection, scale="adjr2")



stepAIC(unmodified_model, direction="backward")

#Unmodified model consisting of platelets ~ DEATH_EVENT + ejection_fraction  + serum_sodium + time, removing rest

unmodified_model <- lm(serum_sodium ~  transformed_serum_sodium  ,data=training_data)
summary(unmodified_model)


coef(unmodified_model)

library(pscl)
pR2(unmodified_model)


confint(unmodified_model)


#* Outlier if the entire model

install.packages("car")
library(car)
qqPlot(unmodified_model,
       labels = row.names(train_set$name),
       id.method = "Identify" , simulate = TRUE,
       main = "Q- Q plot for unmodified model")


#View the errors on a histogram
#Studentized residuals larger than 2 or < -2 then they require attention
studentized_fit <- rstudent(unmodified_model)

hist(studentized_fit, 
     breaks = 10, 
     freq = FALSE,
     xlab = "Studentized residuals",
     main = "Distribution of errors")


rug(jitter(studentized_fit), col = "red")
curve(dnorm(x, 
            mean = mean(studentized_fit),
            sd = sd(studentized_fit)),
      add = TRUE,
      col = "blue", 
      lwed = 2)


# model fit 
lines(density(studentized_fit)$x,
      density(studentized_fit)$y,
      lwd = 2,
      col = "red",
      lty = 2
)



outlierTest(unmodified_model)


# Influential observations
# Cook's D value > 4/(n-k-1)
# where n = sample size , 
# k = number of predicator values

cutoff <- 4 / (nrow(training_data) - length(unmodified_model$coefficients) - 1)
plot(unmodified_model, which = 4 , cook.levels = cutoff)
abline(h = cutoff , lty = 2, col = "red")



influencePlot(unmodified_model, 
              main = "Influence plot for unmodified_model",
              sub = "Circle size is proportional to Cook's distance")

#* homoscedacity =  the data needs to have a constant
#* varience in the residuals
#* if the p value <0.05 ten the rror varience
#* changes with the level of fitted value


ncvTest(unmodified_model)


#*transforming  a model

spreadLevelPlot(unmodified_model)


# global validation of the model

install.packages("gvlma")
library(gvlma)
gvmodel <- gvlma(unmodified_model)
summary(gvmodel)

# predition with the test data
names(heart_failure)
unmodified_model <- lm(serum_sodium  ~ age + anaemia + creatinine_phosphokinase +
                         diabetes + ejection_fraction + high_blood_pressure +
                         platelets + serum_creatinine  + sex +
                         smoking  ,data=training_data)
summary(unmodified_model)

predicted_model <- predict(unmodified_model, testing_data)

actual_prediction <- data.frame(cbind(actuals = testing_data$serum_sodium , predicted = predicted_model))

head(actual_prediction)

# accuracy percentage caluculation
cor_accuracy <- cor(actual_prediction)
cor_accuracy

