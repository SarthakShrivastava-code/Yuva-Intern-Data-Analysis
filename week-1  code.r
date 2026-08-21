
# WEEK 1: Data Cleaning and Preliminary Analysis


# Load libraries
library(dplyr)
library(ggplot2)
library(corrplot)
library(tidyr)

# 1. Load the Dataset
url <- "https://raw.githubusercontent.com/datasciencedojo/datasets/master/titanic.csv"
titanic_data <- read.csv(url, na.strings = c("", "NA"))

# 2. Exploratory Analysis: Initial Inspection
print("Structure of the dataset:")
str(titanic_data)

print("Summary statistics:")
summary(titanic_data)

# 3. Data Cleaning: Handling Missing Values
print("Missing values per column:")
colSums(is.na(titanic_data))

# Filling missing 'Age' with the median age
titanic_data$Age[is.na(titanic_data$Age)] <- median(titanic_data$Age, na.rm = TRUE)

# Filling missing 'Embarked' with the mode (most common port: 'S')
titanic_data$Embarked[is.na(titanic_data$Embarked)] <- "S"

# Drop 'Cabin' column as it has too many missing values to be useful
titanic_clean <- titanic_data %>% select(-Cabin)

# 4. Data Cleaning: Encoding Categorical Variables
# Convert 'Sex' to numeric (Male = 0, Female = 1)
titanic_clean$Sex_Encoded <- ifelse(titanic_clean$Sex == "female", 1, 0)

# 5. Data Cleaning: Outlier Detection and Handling
# Visualize outliers in Fare
boxplot(titanic_clean$Fare, main="Boxplot of Passenger Fares", col="lightblue")

# Cap outliers using the Interquartile Range (IQR) method
Q1 <- quantile(titanic_clean$Fare, 0.25)
Q3 <- quantile(titanic_clean$Fare, 0.75)
IQR <- Q3 - Q1
upper_bound <- Q3 + 1.5 * IQR
titanic_clean$Fare[titanic_clean$Fare > upper_bound] <- upper_bound

# 6. Data Cleaning: Normalization
# Normalize the 'Age' and 'Fare' columns using Z-score standardization
titanic_clean$Age_Normalized <- scale(titanic_clean$Age)
titanic_clean$Fare_Normalized <- scale(titanic_clean$Fare)

# 7. Exploratory Analysis: Visualizations
# Survival rate by Gender
ggplot(titanic_clean, aes(x = Sex, fill = factor(Survived))) +
  geom_bar(position = "dodge") +
  labs(title = "Survival Count by Gender", fill = "Survived (0=No, 1=Yes)") +
  theme_minimal()

# Age distribution
ggplot(titanic_clean, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  labs(title = "Age Distribution of Passengers") +
  theme_minimal()

# Correlation Matrix (Numerical variables only)
numeric_vars <- titanic_clean %>% select(Survived, Pclass, Age, SibSp, Parch, Fare, Sex_Encoded)
cor_matrix <- cor(numeric_vars)
corrplot(cor_matrix, method = "color", type = "upper", addCoef.col = "black", tl.col = "black")