## Load Data

library(readr)
library(knitr)
library(dplyr)
filepath <- "C:/Users/wille/Documents/AppliedStats/Project/CommViolPredUnnormalizedData.txt"
crimeData <- read.delim(filepath, header= FALSE, sep = ",")

#Clean the data
dim_cd <- dim(crimeData)

indicative_info <- crimeData[, 1:5]
X <- crimeData[,6:129]
Y <- crimeData[,130:147]

dim_info <- dim(indicative_info)
dim_X <- dim(X)
dim_Y <- dim(Y)

# Drop unnecessary indicative info
drop <- c("V3","V4")
indicative_info <- indicative_info[,!(names(indicative_info) %in% drop)]

# Count missing data by column in X
crimeData[crimeData == "?"] <- NA
X[X == "?"] <- NA
Y[Y == "?"] <- NA
empValues_X <- colSums(is.na(X))
X_cols_to_drop <- which(empValues_X > 50)

X <- X[,-X_cols_to_drop]

# Now do the same for Y - but also delete total crimes, as I want to focus on rates
empValues_Y <- colSums(is.na(Y))
Y_empty <- which(empValues_Y > 50)
Y_to_drop <- c(1,3,4,5,7,9,11,13,15,16,17)
Y <- Y[,-Y_to_drop]

# Manually remove some highly correlated, or irrelevant data
X_to_remove_corr <- c(8,9,11,14,15,18,19,20,21,22,23,24,25,26,27,32,30,34,35,36,39,41,43,46,47,49,50,52,53,56,55,60,57,58,59,64,67,65,66,76,72,73,74,75,78,80,82,84,86,88, 90,92,93,94,95,96,97,98,102,99)             
X <- X[,-X_to_remove_corr]
X <- X[,-13]


# Check the location of the towns, for sampling bias
state_counts <- indicative_info %>% 
  group_by(indicative_info$V2) %>% 
  count() %>%
  arrange(desc(n))

# Check the distribution of populations
hist(log(crimeData$V6), 
     breaks = 100,  
     xlab = "Population",  # Label for x-axis
     ylab = "Log of Frequency",  # Label for y-axis
     main = "Histogram of Community Population")
    
summary(crimeData$V6)
Y <- data.frame(Y)
Y <- as.data.frame(sapply(Y, as.numeric))


for (col in names(Y)) {
  na_indices <- is.na(Y[[col]])
  Y[[col]][na_indices] <- mean(Y[[col]], na.rm = TRUE)
}

Y <- as.data.frame(sapply(Y, as.numeric))
tot<- rowSums(Y)-Y$V147
Y <- cbind(Y, tot)
hist(Y$tot, 
     breaks = 100,  
     xlab = "Crime Rate per 100k",  # Label for x-axis
     ylab = "Frequency",  # Label for y-axis
     main = "Histogram of Community Crime Rates")

summary(Y$tot)
categories = c("Murders", "Robberies","Assaults","Burglaries","Larcenies","Auto_Thefts", "Non-Violent", "Total_Violent")
colnames(Y) <- categories
boxplot(Y[1:7], 
        main = "Crime Rate Boxplot",  # Title of the boxplot
        xlab = "Crime Rates"  # Label for x-axis
)


## Look at what crimes are most common

barplot(colMeans(Y[1:7]), names.arg = categories[1:7], main = "Average Crime Rate by Type", xlab = "Crime Types", ylab = "Average Crime Rate")

# Get correlation matrix

cor_mat = cor(Y[1:7])
library(corrplot)
corrplot(cor_mat, method = "color")


# Plot crimes vs median income
library(ggplot2)

# Create scatter plot with trend-line (linear regression line)
plot(X$V18, Y$Total_Violent, 
     xlab = "Median House-Hold Income",  # Label for x-axis
     ylab = "Crime Rate",  # Label for y-axis
     col = "blue",  # Color of the points
     pch = 16  # Type of point (16 is a solid circle)
)
mod1 = lm(Y$Total_Violent ~ X$V18)
abline(mod1, col = "blue")

points(X$V18, Y$'Non-Violent', 
     col = "red",  # Color of the points
     pch = 16  # Type of point (16 is a solid circle)
)
mod2 = lm(Y$'Non-Violent' ~ X$V18)
abline(mod2, col = "red")
legend("topright", legend = c("Violent Crime", "Non-Violent Crime"),
       col = c("blue", "red"), pch = c(1, 1, 19,19),  cex = 0.8)


## Pair plot
ppdata <- cbind(X$V18, X$V34, X$V38,X$V36)
colnames(ppdata) <- c("Median HH Income", "Poverty Rate", "Unemployment Rate", "Non HS Graduate Rate")

pairs(ppdata, 
      main = "Pair Plot",  # Title for the plot
      pch = 16,  # Type of points (16 is a solid circle)
      col = "black"  # Color of the points
)

## Now, look at family structure

# Kids in housing with 2 parents
plot(X$V53, Y$Total_Violent, 
     xlab = "Percentage of Kids Under 6 with working moms",  # Label for x-axis
     ylab = "Crime Rate",  # Label for y-axis
     col = "blue",  # Color of the points
     pch = 16  # Type of point (16 is a solid circle)
)
mod1 = lm(Y$Total_Violent ~ X$V53)
abline(mod1, col = "blue")

points(X$V53, Y$'Non-Violent', 
       col = "red",  # Color of the points
       pch = 16  # Type of point (16 is a solid circle)
)
mod2 = lm(Y$'Non-Violent' ~ X$V53)
abline(mod2, col = "red")
legend("topright", legend = c("Violent Crime", "Non-Violent Crime"),
       col = c("blue", "red"), pch = c(1, 1, 19,19),  cex = 0.8)

#Kids in housing with 2 parents
plot(X$V50, Y$Total_Violent, 
     xlab = "Percentage of Kids in Housing with 2 parents",  # Label for x-axis
     ylab = "Crime Rate",  # Label for y-axis
     col = "blue",  # Color of the points
     pch = 16  # Type of point (16 is a solid circle)
)
mod1 = lm(Y$Total_Violent ~ X$V50)
abline(mod1, col = "blue")

points(X$V50, Y$'Non-Violent', 
       col = "red",  # Color of the points
       pch = 16  # Type of point (16 is a solid circle)
)
mod2 = lm(Y$'Non-Violent' ~ X$V50)
abline(mod2, col = "red")
legend("topright", legend = c("Violent Crime", "Non-Violent Crime"),
       col = c("blue", "red"), pch = c(1, 1, 19,19),  cex = 0.8)


# Pair plot 2 

ppdata2 <- cbind(X$V47, X$V50, X$V56)
colnames(ppdata2) <- c("Divorce Rate", "Percentage of Kids Living with Both Parents", "Percentage of Kids to Never Married Parents")

pairs(ppdata2, 
      main = "Pair Plot",  # Title for the plot
      pch = 16,  # Type of points (16 is a solid circle)
      col = "black"  # Color of the points
)

## Save clean data

file_path_X <- 'C:/Users/wille/Documents/AppliedStats/Project/CleanData/X.csv'
file_path_Y <- 'C:/Users/wille/Documents/AppliedStats/Project/CleanData/Y.csv'

write.csv(Y, file = file_path_Y, row.names = TRUE)
write.csv(X, file = file_path_X, row.names = TRUE)


