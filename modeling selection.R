library(dplyr)        # For data manipulation
library(ggplot2)      # For plotting
library(GGally)       # For pairwise plots
library(car)          # For VIF (multicollinearity check)
library(patchwork)    # For combining plot
library(esquisse)     # For Visualization

# Load the Ames Housing dataset
data <- read.csv('ames.csv')

# View the structure and a summary of the dataset
str(data)

summary(data)

#================================MODELING AND FEATURE SELECTION=========================
# Fit a linear model using the top predictors (or you could use all numeric predictors)
pseudo_model <- lm(Log_Sale_Price ~ ., data = data)

# Calculate VIF values
vif_values <- vif(pseudo_model);sort(vif_values,decreasing =TRUE)

# Calculate the coefficient of variation for each variable, excluding Sale_Price
variable_cv <- sapply(data %>% select(-Log_Sale_Price),
                      function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE))
variable_cv_sorted <- sort(variable_cv)
print(variable_cv_sorted)

summary(pseudo_model)

# 1. Remove Highest P-Value
coef_summary <- summary(pseudo_model)$coefficients;coef_summary

## Exclude the intercept and extract p-values
pvals <- coef_summary[-1, "Pr(>|t|)"];pvals

## Set number of variables to remove, e.g., top 2 or top 3
n_to_remove <- 2  # Change to 3 for top 3, etc.

## Identify the variables with the highest p-values
vars_to_remove <- names(sort(pvals, decreasing = TRUE))[1:n_to_remove]
cat("Removing variables:", paste(vars_to_remove, collapse = ", "), "\n")

## Get remaining predictor names (assuming the first column is Sale_Price)
remaining_vars <- setdiff(names(data), c("Log_Sale_Price",vars_to_remove));remaining_vars

## Create new formula and refit the model
new_formula <- as.formula(paste("Log_Sale_Price ~", paste(remaining_vars, collapse = " + ")))
model_new <- lm(new_formula, data = data)
summary(model_new)

# 2. Remove VIF > 5
# Identify variables with VIF greater than 5
vars_high_vif <- names(vif_values)[vif_values > 5]
cat("Variables with VIF > 5:", paste(vars_high_vif, collapse = ", "), "\n")

# Remove these variables, explicitly excluding the response variable
remaining_vars <- setdiff(names(data), c("Log_Sale_Price", vars_high_vif))
new_formula <- as.formula(paste("Log_Sale_Price ~", paste(remaining_vars, collapse = " + ")))

# Refit the model with remaining predictors
refined_model <- lm(new_formula, data = data)
summary(refined_model)

# 3. Remove Less Variability Variable
# Set the number of variables to remove (e.g., 2 for the two smallest, 3 for three smallest, etc.)
n_to_remove <- 2

# Identify the variables with the smallest CV values
vars_low_cv <- names(variable_cv_sorted)[1:n_to_remove]

cat("Removing variables with lowest variability:", paste(vars_low_cv, collapse = ", "), "\n")

# Remove these variables, explicitly excluding the response variable
remaining_vars <- setdiff(names(data), c("Log_Sale_Price", vars_low_cv))
new_formula <- as.formula(paste("Log_Sale_Price ~", paste(remaining_vars, collapse = " + ")))

# Refit the model with remaining predictors
refined_model <- lm(new_formula, data = data)
summary(refined_model)

