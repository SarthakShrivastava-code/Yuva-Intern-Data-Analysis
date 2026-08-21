
# WEEK 4: Comprehensive Master Script


# Load required libraries
library(dplyr)
library(ggplot2)
library(caret)


# PART 1: Data Cleaning (Titanic)
cat("\n--- PART 1: Titanic Data Cleaning ---\n")
titanic_url <- "https://raw.githubusercontent.com/datasciencedojo/datasets/master/titanic.csv"
titanic_data <- read.csv(titanic_url, na.strings = c("", "NA"))

# Impute missing Age with median and drop Cabin
titanic_data$Age[is.na(titanic_data$Age)] <- median(titanic_data$Age, na.rm = TRUE)
titanic_clean <- titanic_data %>% select(-Cabin)
cat("Titanic Data cleaned successfully. Remaining NA count:\n")
print(colSums(is.na(titanic_clean)))


# PART 2: Visualization (Diamonds)
cat("\n--- PART 2: Diamonds Visualization ---\n")
data(diamonds)

# Generate and save a visualization locally
p <- ggplot(diamonds, aes(x = cut, fill = cut)) +
  geom_bar() +
  labs(title = "Diamonds: Inventory by Cut", x = "Cut Quality", y = "Count") +
  theme_minimal()
print(p)
cat("Plot generated successfully.\n")

# PART 3: Predictive Modeling (Breast Cancer)
cat("\n--- PART 3: Breast Cancer Modeling ---\n")
cancer_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
cols <- c("id", "diagnosis", "radius_mean", "texture_mean", "perimeter_mean", "area_mean", "smoothness_mean")
cancer_data <- read.csv(cancer_url, header = FALSE)[, 1:7]
colnames(cancer_data) <- cols
cancer_data$diagnosis <- as.factor(cancer_data$diagnosis)

# Train-Test Split and Model Training
set.seed(123)
train_idx <- createDataPartition(cancer_data$diagnosis, p = 0.8, list = FALSE)
model <- glm(diagnosis ~ radius_mean + texture_mean, data = cancer_data[train_idx, ], family = "binomial")

# Predict and Evaluate
preds <- ifelse(predict(model, cancer_data[-train_idx, ], type = "response") > 0.5, "M", "B")
conf_matrix <- confusionMatrix(as.factor(preds), cancer_data[-train_idx, ]$diagnosis)
print(conf_matrix$table)
cat(sprintf("Model Accuracy: %.2f%%\n", conf_matrix$overall['Accuracy'] * 100))