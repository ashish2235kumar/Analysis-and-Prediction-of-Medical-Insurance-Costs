# =====================================================================
# Healthcare Mini Project - LIVE DEMO VERSION
# Shows every plot in the RStudio Plots pane and prints results in the
# Console. After each step it PAUSES: click in the Console and press
# ENTER to continue (so you can explain each output while presenting).
#
# HOW TO RUN: select all (Ctrl+A) and press Ctrl+Enter
#             (or click Source). Then pick insurance.csv when asked.
# =====================================================================

library(ggplot2)

pause <- function(msg) {
  cat("\n>>> ", msg, "\n")
  invisible(readline("Press ENTER in the Console to continue... "))
}

# ---- 1. Load data ----
data <- read.csv(file.choose(), stringsAsFactors = TRUE)

cat("===== 1. DATASET OVERVIEW =====\n")
print(dim(data))
str(data)
print(summary(data))
cat("\nMissing values per column:\n")
print(colSums(is.na(data)))
pause("Next: histograms")

# ---- 2. Histograms ----
hist(data$charges, main = "Distribution of Medical Charges",
     col = "skyblue", border = "black", xlab = "Charges (USD)", breaks = 20)
pause("Histogram of charges. Next: BMI")

hist(data$bmi, main = "Distribution of BMI", col = "orange",
     border = "black", xlab = "BMI", breaks = 15)
pause("Histogram of BMI. Next: age")

hist(data$age, main = "Distribution of Age", col = "lightgreen",
     border = "black", xlab = "Age", breaks = 10)
pause("Histogram of age. Next: boxplots")

# ---- 3. Boxplots ----
boxplot(charges ~ smoker, data = data, main = "Charges vs Smoker",
        xlab = "Smoker", ylab = "Charges (USD)", col = c("lightblue", "salmon"))
pause("Boxplot: smoker. Next: region")

boxplot(charges ~ region, data = data, main = "Charges by Region",
        xlab = "Region", ylab = "Charges (USD)", col = "lightyellow")
pause("Boxplot: region. Next: scatter plots")

# ---- 4. Scatter plots (print() is needed for ggplot) ----
print(
  ggplot(data, aes(age, charges, color = smoker)) +
    geom_point(alpha = 0.6) +
    labs(title = "Age vs Charges", x = "Age", y = "Charges (USD)") +
    theme_minimal()
)
pause("Scatter: age vs charges. Next: BMI")

print(
  ggplot(data, aes(bmi, charges, color = smoker)) +
    geom_point(alpha = 0.6) +
    labs(title = "BMI vs Charges", x = "BMI", y = "Charges (USD)") +
    theme_minimal()
)
pause("Scatter: BMI vs charges. Next: correlation and t-test")

# ---- 5. Correlation and hypothesis test ----
cat("\n===== 2. CORRELATION MATRIX =====\n")
print(round(cor(data[, c("age", "bmi", "children", "charges")]), 2))

cat("\n===== 3. T-TEST: CHARGES BY SMOKER =====\n")
print(t.test(charges ~ smoker, data = data))
pause("Next: regression models")

# ---- 6. Regression ----
set.seed(123)
idx   <- sample(nrow(data), 0.8 * nrow(data))
train <- data[idx, ]
test  <- data[-idx, ]

rmse <- function(a, p) sqrt(mean((a - p)^2))
r2   <- function(a, p) 1 - sum((a - p)^2) / sum((a - mean(a))^2)

m1 <- lm(charges ~ age + sex + bmi + children + smoker + region, data = train)
cat("\n===== 4. MODEL 1: LINEAR REGRESSION =====\n")
print(summary(m1))
pr1 <- predict(m1, test)
pause("Next: Model 2")

m2 <- lm(charges ~ age + I(age^2) + sex + bmi + children + smoker + region +
           smoker:bmi, data = train)
cat("\n===== 5. MODEL 2: POLYNOMIAL + INTERACTION =====\n")
print(summary(m2))
pr2 <- predict(m2, test)
pause("Next: model comparison")

results <- data.frame(
  Model = c("Linear", "Polynomial + Interaction"),
  RMSE  = c(rmse(test$charges, pr1), rmse(test$charges, pr2)),
  R2    = c(r2(test$charges, pr1),   r2(test$charges, pr2))
)
results[, 2:3] <- round(results[, 2:3], 3)
cat("\n===== 6. MODEL COMPARISON (test data) =====\n")
print(results)
pause("Next: residual diagnostics")

# ---- 7. Diagnostics ----
par(mfrow = c(2, 2)); plot(m2); par(mfrow = c(1, 1))
pause("Residual diagnostics. Next: actual vs predicted")

plot(test$charges, pr2, main = "Actual vs Predicted Charges",
     xlab = "Actual", ylab = "Predicted", col = "steelblue", pch = 16)
abline(0, 1, col = "red", lwd = 2)

cat("\nDONE. Project complete.\n")