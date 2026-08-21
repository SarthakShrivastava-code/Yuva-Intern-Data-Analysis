
# WEEK 2: Data Visualization using ggplot  

# Load the library
library(ggplot2)

# The 'diamonds' dataset is built into ggplot2, so we can load it directly
data(diamonds)

# 1. Histogram: Distribution of Diamond Prices
ggplot(diamonds, aes(x = price)) +
  geom_histogram(binwidth = 500, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Diamond Prices", x = "Price (USD)", y = "Count") +
  theme_minimal()

# 2. Scatter Plot: Carat vs. Price by Cut
ggplot(diamonds, aes(x = carat, y = price, color = cut)) +
  geom_point(alpha = 0.5) +
  labs(title = "Carat vs. Price by Cut", x = "Carat", y = "Price (USD)") +
  scale_color_viridis_d() +
  theme_minimal()

# 3. Bar Chart: Count of Diamonds by Cut Quality
ggplot(diamonds, aes(x = cut, fill = cut)) +
  geom_bar() +
  labs(title = "Count of Diamonds by Cut Quality", x = "Cut Quality", y = "Count") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()

# 4. Box Plot: Price Distribution by Diamond Color
ggplot(diamonds, aes(x = color, y = price, fill = color)) +
  geom_boxplot() +
  labs(title = "Price Distribution by Diamond Color", x = "Color (D=Best, J=Worst)", y = "Price (USD)") +
  theme_minimal()