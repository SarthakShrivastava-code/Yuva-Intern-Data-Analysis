
# WEEK 3: Statistical Analysis & Predictive Modeling

library(caret)
library(corrplot)
library(pROC)

# 1. Load the Dataset (Breast Cancer Wisconsin Diagnostic)
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
column_names <- c("id", "diagnosis", "radius_mean", "texture_mean", "perimeter_mean", "area_mean", "smoothness_mean",
                  "compactness_mean", "concavity_mean", "concave_points_mean", "symmetry_mean", "fractal_dimension_mean",
                  "radius_se", "texture_se", "perimeter_se", "area_se", "smoothness_se", "compactness_se", "concavity_se",
                  "concave_points_se", "symmetry_se", "fractal_dimension_se", "radius_worst", "texture_worst",
                  "perimeter_worst", "area_worst", "smoothness_worst", "compactness_worst", "concavity_worst",
                  "concave_points_worst", "symmetry_worst", "fractal_dimension_worst")

cancer_data <- read.csv(url, header = FALSE, col.names = column_names)

# Convert diagnosis to a factor (M = Malignant, B = Benign)
cancer_data$diagnosis <- as.factor(cancer_data$diagnosis)

# 2. Hypothesis Testing (t-test)
# H0: No difference in mean radius between Malignant and Benign tumors
t_test_result <- t.test(radius_mean ~ diagnosis, data = cancer_data)
print(t_test_result)

# Boxplot for visualization
boxplot(radius_mean ~ diagnosis, data = cancer_data,
        main = "Mean Radius by Diagnosis",
        xlab = "Diagnosis", ylab = "Mean Radius",
        col = c("lightblue", "lightcoral"))

# 3. Correlation Matrix (Check for Multicollinearity)
numeric_features <- cancer_data[, 3:7] 
cor_matrix <- cor(numeric_features)
corrplot(cor_matrix, method = "number", type = "upper", tl.col = "black")

# 4. Model Building: Data Splitting
set.seed(123)
trainIndex <- createDataPartition(cancer_data$diagnosis, p = 0.8, list = FALSE)
train_data <- cancer_data[trainIndex, ]
test_data  <- cancer_data[-trainIndex, ]

# 5. Logistic Regression Model
model <- glm(diagnosis ~ radius_mean + texture_mean + perimeter_mean + area_mean + smoothness_mean,
             data = train_data, family = "binomial")
summary(model)

# 6. Diagnostic Analysis: Predictions and Confusion Matrix
# Predict probabilities
probabilities <- predict(model, test_data, type = "response")

# Convert probabilities to class labels (M or B)
predicted_classes <- ifelse(probabilities > 0.5, "M", "B")
predicted_classes <- as.factor(predicted_classes)

# Generate Confusion Matrix
conf_matrix <- confusionMatrix(predicted_classes, test_data$diagnosis)
print(conf_matrix)

# 7. ROC Curve and AUC
roc_obj <- roc(test_data$diagnosis, probabilities, levels = c("B", "M"))
plot(roc_obj, col = "darkorange", main = "ROC Curve - Logistic Regression", lwd = 2)
auc_value <- auc(roc_obj)
print(paste("AUC:", auc_value))