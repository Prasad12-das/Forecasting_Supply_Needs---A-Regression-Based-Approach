# Reading the dataset
df <- read.csv("/Users/sailajprasad/Downloads/SPRING 2025/Courses/LINEAR-REGRESS-N-TIME-SERIES-2025-Jan-13_19-52-18-268/Final Project/FMCG_data.csv", header = TRUE)
attach(df) 
head(df)
str(df)


# No of Null values
colSums(is.na(df))

# Removing rows with null values
df <- na.omit(df)
colSums(is.na(df))
nrow(df)

#Empty strings in Govt_certificate column
sum(trimws(df$approved_wh_govt_certificate) == "")
library(dplyr)
df <- df %>%
  filter(trimws(approved_wh_govt_certificate) != "")
sum(trimws(df$approved_wh_govt_certificate) == "")
nrow(df)

# Dropping invalid columns
df <- df[, !colnames(df) %in% c("Ware_house_ID", "WH_Manager_ID")]

# Renaming columns
colnames(df) <- c("location", "capacity", "zone", "reg_zone", 
                  "refill", "transport_issue", "competitor", 
                  "retail_shop", "warehouse_Owner", "distributors", 
                  "flood_impacted", "flood_proof", "electric_supply", "distance_hub", 
                  "workers_num", "w_est_year", "storage_issues", 
                  "temperature_regulation", "govt_cert", 
                  "warehouse_breakdown", "govt_check", "product_weight")
# Pair plots
library(ggplot2)
library(GGally)
selected_columns <- df[, c("retail_shop", "distributors", "distance_hub", "workers_num", "storage_issues", "govt_check", "product_weight")]
ggpairs(selected_columns)

str(df)
# Count the number of numerical and categorical variables
num_numerical <- sum(sapply(df, is.numeric))
num_categorical <- sum(sapply(df, is.character))

# Display the results
cat("Number of numerical variables:", num_numerical, "\n")
cat("Number of categorical variables:", num_categorical, "\n")
#Distribution of Product weight
ggplot(df, aes(x = product_weight)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7, color = "black") +
  labs(title = "Distribution of Product Weight",
       x = "Product Weight", y = "Frequency") +
  theme_minimal()
#Product weight by location
library(dplyr)
library(ggplot2)
agg_data <- df %>%
  group_by(location) %>%
  summarise(mean_weight = mean(product_weight))
ggplot(agg_data, aes(x = location, y = mean_weight, fill = location)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  geom_text(aes(label = round(mean_weight, 2)), 
            vjust = -0.3, color = "black", size = 4) +  # Adjust vjust to control text placement
  labs(title = "Average Product Weight by Warehouse Location",
       x = "Warehouse Location", y = "Average Product Weight") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Product weight vs zone with median annotations
ggplot(df, aes(x = zone, y = product_weight, fill = zone)) +
  geom_boxplot(outlier.color = "red", alpha = 0.7) +
  stat_summary(fun = "median", geom = "text", aes(label = round(..y.., 2)), 
               color = "black", vjust = -0.5) +  # Adding median labels
  labs(title = "Product Weight by Zone",
       x = "Zone", y = "Product Weight") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Correlation matrix for numerical variables
library(ggplot2)
library(tidyr)
numerical_columns <- c("retail_shop", "product_weight", "distributors", "distance_hub", "workers_num", "storage_issues", "govt_check")
corr_matrix <- cor(df[, numerical_columns])
corr_data <- as.data.frame(as.table(corr_matrix))
ggplot(corr_data, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  labs(title = "Correlation Heatmap of Selected Numerical Variables", x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  geom_text(aes(label = round(Freq, 2)), color = "black", size = 4)  # Adding correlation values to the heatmap
corr_matrix
# Data Preprocessing
#quadtratic terms
numerical_columns <- c("retail_shop", "distributors", "distance_hub", "workers_num", "storage_issues", "govt_check")
for (i in 1:(length(numerical_columns))) {  
    interaction_name <- paste0(numerical_columns[i], "_",numerical_columns[i] )
    df[[interaction_name]] <- df[[numerical_columns[i]]] * df[[numerical_columns[i]]]
  }


str(df[, numerical_columns])

#Encoding
categorical_col <- c("location", "capacity", "zone", "reg_zone", "refill", "transport_issue", "competitor", "warehouse_Owner", "flood_impacted", "flood_proof", "electric_supply", "w_est_year", "temperature_regulation" , "govt_cert", "warehouse_breakdown")
#install.packages("fastDummies")
#library(fastDummies)
#df <- dummy_cols(df, categorical_col, remove_selected_columns = TRUE, remove_first_dummy = TRUE)
df[categorical_col] <- lapply(df[categorical_col], as.factor)
colnames(df)


# Log Transformation
numerical_columns <- c("retail_shop", "distributors", "distance_hub", "workers_num" ,"storage_issues", "govt_check", "product_weight", "retail_shop_retail_shop", "distributors_distributors", "distance_hub_distance_hub", "workers_num_workers_num", "storage_issues_storage_issues", "govt_check_govt_check" )
df[numerical_columns] <- lapply(df[numerical_columns], function(x) log(x + 1))
str(df)
str(df[,numerical_columns])


#Pair plots after transformation
ggpairs(selected_columns)

#Standardization
df[, c("retail_shop", "distributors", "distance_hub", "workers_num" ,"storage_issues", "govt_check", "retail_shop_retail_shop", "distributors_distributors", "distance_hub_distance_hub", "workers_num_workers_num", "storage_issues_storage_issues", "govt_check_govt_check" )] <- scale(df[, c("retail_shop", "distributors", "distance_hub", "workers_num" ,"storage_issues", "govt_check", "retail_shop_retail_shop", "distributors_distributors", "distance_hub_distance_hub", "workers_num_workers_num", "storage_issues_storage_issues", "govt_check_govt_check" )])
str(df)

# Model Validation
set.seed(123)
#train_indices <- sample(1:nrow(df), size = 0.08247* nrow(df), replace = FALSE )
train_indices <- sample(1:nrow(df), size = 0.8 * nrow(df), replace = FALSE )
train_df <- df[train_indices,]
nrow(train_df)
test_df <- df[-train_indices,]
nrow(test_df)


# Model Selection
#SLR Model
slr_model <- lm(product_weight ~  storage_issues , data =train_df)
summary(slr_model)
anova(slr_model)


# Hypothesis test for correlation coefficient rho
n<- nrow(train_df)
#since  the relationship between Product weight and storage issues are positive
r_xy <- sqrt(summary(slr_model)$r.squared)
alpha <- 0.05
t_statistic <- sqrt(n - 2) * (r_xy / sqrt(1 - r_xy^2))
t_statistic
critical_t <- qt(1 - alpha / 2, df = n - 2)
p_value <- 2 * (1 - pt(abs(t_statistic), df = n - 2))

cat("r_xy:", r_xy )
cat("t_statistic",t_statistic )
cat("Significance Level",alpha )
cat("t_critical", critical_t)
cat("p_value", p_value)

#interpretation
if (abs(t_statistic) > critical_t) {
  cat("Reject H0: The correlation is statistically significant.\n")
} else {
  cat("Fail to reject H0: The correlation is not statistically significant.\n")
}

r_xy


# Confidence Interval for Pearson Product Moment Coefficient
alpha <- 0.05
z_p <- 0.5 * log((1 + r_xy) / (1 - r_xy)) #Fisher's Transformation
z_alpha <- qnorm(1 - alpha / 2)
z_prime_l <- z_p - z_alpha * sqrt(1 / (n - 3))
z_prime_u <- z_p + z_alpha * sqrt(1 / (n - 3))
rho_lower <- (exp(z_prime_l) - 1) / (exp(z_prime_l) + 1)
rho_upper <- (exp(z_prime_u) - 1) / (exp(z_prime_u) + 1)

cat("Fisher Z transformation (Z')", z_p)
cat("The 95% confidence interval for the true correlation coefficient is:\n", round(rho_lower, 4), "<= rho <=", round(rho_upper, 4), "\n")


#First Order MLR Model
fo_model <- lm(product_weight ~ location + capacity + zone + reg_zone + refill + transport_issue + competitor +
                                   retail_shop + warehouse_Owner + distributors +
                                   flood_impacted + flood_proof + electric_supply + distance_hub +
                                   workers_num + w_est_year + storage_issues +
                                   temperature_regulation + govt_cert + 
                                   warehouse_breakdown + govt_check , data = train_df)
formula(fo_model)
summary(fo_model)
anova(fo_model)

reg.null <- lm(product_weight ~ 1,data=train_df)
best_fo_model <- step(reg.null,direction="both",scope=list(upper= fo_model,lower=reg.null))

summary(best_fo_model)
anova(best_fo_model)


# Test for Regression some coefficients
formula(best_fo_model)

best_fo_model_1 <- lm(product_weight ~ storage_issues + w_est_year + govt_cert + transport_issue + 
  temperature_regulation , data = train_df)

anova(best_fo_model, best_fo_model_1)
anova(best_fo_model_1)
summary(best_fo_model_1)
library(car)
vif_values <- vif(best_fo_model_1)
vif_values

#Full_model 
full_model <- lm(product_weight ~ (location + capacity + zone + reg_zone + refill + transport_issue + competitor +
                                    retail_shop + warehouse_Owner + distributors +
                                    flood_impacted + flood_proof + electric_supply + distance_hub +
                                    workers_num + w_est_year + storage_issues +
                                    temperature_regulation + govt_cert + 
                                    warehouse_breakdown + govt_check + retail_shop_retail_shop + distributors_distributors +
                                    distance_hub_distance_hub + workers_num_workers_num + storage_issues_storage_issues + govt_check_govt_check )^2 , data = train_df)
summary(full_model)
anova(full_model)

reg.null <- lm(product_weight ~ 1,data=train_df)
both_model <- step(reg.null,direction="both",scope=list(upper= full_model,lower=reg.null))
summary(both_model)

anova(both_model)
anova(full_model, both_model)
anova_both_table <-anova(both_model) 
sse_both_model <- anova_both_table["Residuals", "Sum Sq"]
ssr_both_model <- sum(anova_both_table$`Sum Sq`) - sse_both_model

#F-test for interaction terms

#Interaction Model
interaction_model <- lm(product_weight ~ storage_issues_storage_issues + w_est_year + 
                          govt_cert + storage_issues + transport_issue + temperature_regulation + 
                          govt_check_govt_check + capacity + location + electric_supply + 
                          storage_issues_storage_issues:w_est_year + w_est_year:storage_issues + 
                          govt_cert:storage_issues + storage_issues_storage_issues:storage_issues + 
                          temperature_regulation:govt_check_govt_check + storage_issues:temperature_regulation + 
                          storage_issues_storage_issues:temperature_regulation + storage_issues_storage_issues:capacity + 
                          govt_check_govt_check:electric_supply + capacity:electric_supply, train_df)

#Additive Model (Derived from Both_model)
additive_model <- lm(product_weight ~ storage_issues_storage_issues + w_est_year + 
  govt_cert + storage_issues + transport_issue + temperature_regulation + 
  govt_check_govt_check + capacity + location + electric_supply, data =train_df)

# F-test for interaction terms
anova(additive_model, both_model)


# Test for Regression some coefficients
both_model_1 <- lm(product_weight ~ storage_issues_storage_issues + w_est_year + 
                     govt_cert + storage_issues + transport_issue + temperature_regulation + 
                     storage_issues_storage_issues:w_est_year + w_est_year:storage_issues + 
                     govt_cert:storage_issues + storage_issues_storage_issues:storage_issues, data = train_df)
anova(both_model_1)
anova(both_model, both_model_1)
anova_table <- anova(both_model_1)
sse_reduced <- anova_table["Residuals", "Sum Sq"]
ssr_reduced <- sum(anova_table$`Sum Sq`) - sse_reduced

# Coefficient of Partial Determination between both_model and reduced_both_model i.e. removing insignificant predictors
sse_both_model
ssr_both_model
sse_reduced
ssr_reduced

c_df <- ((ssr_both_model) - (ssr_reduced)) / (sse_reduced)
c_df



# Test for Regression individual coefficients

#Removing electric_supply column
both_model_rm_electric_supply <- lm(product_weight ~ storage_issues_storage_issues + w_est_year + 
  govt_cert + storage_issues + transport_issue + temperature_regulation + 
  govt_check_govt_check + capacity + location + 
  storage_issues_storage_issues:w_est_year + w_est_year:storage_issues + 
  govt_cert:storage_issues + storage_issues_storage_issues:storage_issues + 
  temperature_regulation:govt_check_govt_check + storage_issues:temperature_regulation + 
  storage_issues_storage_issues:temperature_regulation + storage_issues_storage_issues:capacity + 
  govt_check_govt_check:electric_supply + capacity:electric_supply, data =train_df)
anova(both_model, both_model_rm_electric_supply)

#Removing all of electric_supply interaction terms
both_model_rm_electric_supply_all <- lm(product_weight ~ storage_issues_storage_issues + w_est_year + 
                                      govt_cert + storage_issues + transport_issue + temperature_regulation + 
                                      govt_check_govt_check + capacity + location + 
                                      storage_issues_storage_issues:w_est_year + w_est_year:storage_issues + 
                                      govt_cert:storage_issues + storage_issues_storage_issues:storage_issues + 
                                      temperature_regulation:govt_check_govt_check + storage_issues:temperature_regulation + 
                                      storage_issues_storage_issues:temperature_regulation + storage_issues_storage_issues:capacity , data =train_df)
anova(both_model, both_model_rm_electric_supply_all)


updated_both_model <- lm(product_weight ~ storage_issues_storage_issues + w_est_year + 
                                      govt_cert + storage_issues + transport_issue + temperature_regulation + 
                                      govt_check_govt_check + capacity + location + 
                                      storage_issues_storage_issues:w_est_year + w_est_year:storage_issues + 
                                      govt_cert:storage_issues + storage_issues_storage_issues:storage_issues + 
                                      temperature_regulation:govt_check_govt_check + storage_issues:temperature_regulation + 
                                      storage_issues_storage_issues:temperature_regulation + storage_issues_storage_issues:capacity + capacity:electric_supply, data =train_df)
anova(both_model, updated_both_model)
updated_both_model <- lm(product_weight ~ storage_issues_storage_issues + w_est_year + 
                           govt_cert + storage_issues + transport_issue + temperature_regulation + 
                           govt_check_govt_check + capacity + location + 
                           storage_issues_storage_issues:w_est_year + w_est_year:storage_issues + 
                           govt_cert:storage_issues + storage_issues_storage_issues:storage_issues + 
                           temperature_regulation:govt_check_govt_check + storage_issues:temperature_regulation + 
                           storage_issues_storage_issues:temperature_regulation + storage_issues_storage_issues:capacity, data =train_df)
anova(both_model, updated_both_model)
anova(updated_both_model)
updated_both_model <- lm(product_weight ~ storage_issues_storage_issues + w_est_year + 
                           govt_cert + storage_issues + transport_issue + temperature_regulation + 
                           storage_issues_storage_issues:w_est_year + w_est_year:storage_issues + 
                           govt_cert:storage_issues + storage_issues_storage_issues:storage_issues , data =train_df)
anova(updated_both_model)
summary(updated_both_model)
summary(both_model)
anova(both_model, updated_both_model)
library(car)
vif(updated_both_model)
#removing high Vif valued column
formula(updated_both_model)
updated_both_model_mc <- lm(product_weight ~ w_est_year + 
  govt_cert + storage_issues + transport_issue + temperature_regulation + 
  storage_issues_storage_issues:w_est_year + w_est_year:storage_issues + 
  govt_cert:storage_issues + storage_issues_storage_issues:storage_issues, data =train_df)
summary(updated_both_model)
summary(updated_both_model_mc)
anova(updated_both_model, updated_both_model_mc)


vif(updated_both_model_mc)
#Removing all storage_issues_storage_issues interactions
updated_both_model_mc1 <- lm(product_weight ~ w_est_year + govt_cert + storage_issues + transport_issue 
                             + temperature_regulation + w_est_year:storage_issues + govt_cert:storage_issues, data =train_df)



summary(updated_both_model_mc)
summary(updated_both_model_mc1)
vif(updated_both_model_mc1)

# Removing interaction terms
updated_both_model_mc2 <- lm(product_weight ~ w_est_year + govt_cert + storage_issues + transport_issue + 
                               temperature_regulation, data =train_df)
summary(updated_both_model_mc2)
summary(updated_both_model_mc1)
vif(updated_both_model_mc2)

summary(updated_both_model_mc2)
anova(updated_both_model_mc2)
summary(best_fo_model_1)
summary(slr_model)



BIC(updated_both_model_mc2)
BIC(best_fo_model_1)
BIC(slr_model)


#Residual and Outlier

predicted <- fitted(updated_both_model_mc2)
observed <- train_df$product_weight


plot(predicted, observed, pch = 19, col = "blue",
     xlab = "Predicted Values", ylab = "Observed Values",
     main = "Observed vs Predicted Values")
abline(0, 1, col = "red", lwd = 2)

#Residual vs fitted
residuals <- residuals(updated_both_model_mc2)

# Scatterplot
plot(predicted, residuals, pch = 19, col = "blue",
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red", lwd = 2)

#Outlier Analysis
#X-direction
leverage_values <- hatvalues(updated_both_model_mc2)
threshold <- 2 * mean(leverage_values)
print(threshold)
# Test for outlying X observation
high_leverage <- which(leverage_values > threshold)
print("Leverage Values:")
print(leverage_values[1:100])
print("High-Leverage Points (Indices):")
print(high_leverage[1:100])

#Y-direction
studentized_residuals <- rstudent(updated_both_model_mc2)
print("Studentized Residuals:")
print(studentized_residuals[1:100])

n <- nrow(train_df)
p <- length(coef(updated_both_model_mc2))

y_outliers <- which(abs(studentized_residuals) > 3)
cat("Outliers on Y-direction", y_outliers, "\n")


#Influential Cases
#DFFITS
dffits_values <- dffits(updated_both_model_mc2)
print(dffits_values)
#Identifying influential observation
n <- nrow(train_df)
k <- length(coefficients(updated_both_model_mc2))
cutoff <- 2 * sqrt(k / n)
print(cutoff)
# Identify observations exceeding the cutoff
influential <- which(abs(dffits_values) > cutoff)
print(influential[1:100])
#Visualizing
plot(dffits_values, type = "h", main = "DFFITS Plot", xlab = "Observation Index", ylab = "DFFITS")
abline(h = c(cutoff, -cutoff), col = "red", lty = 1)  # Add threshold lines



#Cook's Distance
cooks_d <- cooks.distance(updated_both_model_mc2)
print(cooks_d[1:100])

#Identify Influential Observation
n <- nrow(train_df)
k <- length(coefficients(updated_both_model_mc2))
threshold <- 4 / (n - k)
print(threshold)
# Find observations exceeding the threshold
influential <- which(cooks_d > threshold)
print(influential[1:100])
#Visualizing
plot(cooks_d, type = "h", main = "Cook's Distance Plot", xlab = "Observation Index", ylab = "Cook's Distance")
abline(h = threshold, col = "red", lty = 2)  # Add threshold line

#DFBETAS's
dfbetas_values <- dfbetas(updated_both_model_mc2)
print(dfbetas_values[1:100])
#threshold
n <- nrow(train_df)  # Number of observations
threshold <- 2 / sqrt(n)
print(threshold)

influential_obs <- apply(dfbetas_values, 2, function(x) which(abs(x) > threshold))
print(influential_obs)

#Visualizing 
dfbetas_wt <- dfbetas_values[, "storage_issues"]

plot(dfbetas_wt, type = "h", main = "DFBETAS for 'Storage Issues'", xlab = "Observation Index", ylab = "DFBETAS")
abline(h = c(threshold, -threshold), col = "red", lty = 2)  # Add threshold lines


# Testing the model with train datasets
residuals <- residuals(updated_both_model_mc2)
MSE <- mean(residuals^2)
MSE

predicted_Y <- predict(updated_both_model_mc2, newdata = test_df)
actual_Y <- test_df$product_weight
MSPR <- mean((actual_Y - predicted_Y)^2)
MSPR

test_residuals <- actual_Y - predicted_Y
SS_Residual <- sum(test_residuals^2)
SS_Total <- sum((actual_Y - mean(actual_Y))^2)
R_squared <- 1 - (SS_Residual / SS_Total)
R_squared

cat("The MSE value and R-square value of train data is", MSE ,"and", summary(updated_both_model_mc2)$r.squared)
cat("The MSPR value and R-square value of test data is", MSPR ,"and", R_squared)
cat("The ratio of MSPR and MSE is:", (MSPR/MSE))


#K-cross validation
install.packages("caret")
library(caret)
formula_1 <- formula(updated_both_model_mc2)
cv <- trainControl(method = "cv", number = 5)
cv_model <- train(formula_1,data = df,method = "lm",trControl = cv)
print(cv_model)

coef(updated_both_model_mc2)

#Inference in Regression Analysis
summary(updated_both_model_mc2)
# Assuming your model is stored as 'model'
# Extract coefficients and standard errors
coefficients <- coef(summary(updated_both_model_mc2))
estimates <- coefficients[, "Estimate"]
std_errors <- coefficients[, "Std. Error"]

# Degrees of freedom and t-value for 95% confidence
df <- updated_both_model_mc2$df.residual  # Residual degrees of freedom
alpha <- 0.05
t_value <- qt(1 - alpha / 2, df)

# Calculate confidence intervals
lower_bound <- estimates - t_value * std_errors
upper_bound <- estimates + t_value * std_errors

# Combine into a data frame for better readability
conf_intervals <- data.frame(
  Coefficient = rownames(coefficients),
  Estimate = estimates,
  Lower_95_CI = lower_bound,
  Upper_95_CI = upper_bound
)
# View the confidence intervals
print(conf_intervals)



## Confidence interval for E(Xh)
Xh_values <- c(2021, "A", 4, 0, 0) #for a hypothetical value Xh
coefficients <- coef(updated_both_model_mc2) 
X <- model.matrix(updated_both_model_mc2)
mse <- sum(residuals(updated_both_model_mc2)^2) / df.residual(updated_both_model_mc2)
XtX_inv <- vcov(updated_both_model_mc2)
x_h <- X[1, , drop = FALSE]
sd_Y_cap_h <- sqrt(mse * as.numeric(x_h %*% XtX_inv %*% t(x_h)))
Yh_hat <- sum(coefficients * x_h)
alpha <- 0.05
df <- df.residual(updated_both_model_mc2)
t_critical <- qt(1 - alpha / 2, df)

lower_bound <- Yh_hat - t_critical * sd_Y_cap_h
upper_bound <- Yh_hat + t_critical * sd_Y_cap_h

cat("The 95% confidence interval for E(X_h) is:\n", 
    round(lower_bound, 4), "<= E(X_h) <=", round(upper_bound, 4), "\n")

# Prediction Interval for New Observation 𝒀𝒉(𝒏𝒆𝒘)

Xh_values <- c(2021, "A", 4, 0, 0)

X_inv <- solve(t(X) %*% X)
x_h <- X[1, , drop = FALSE]
var_pred <- mse * (1 + x_h %*% X_inv %*% t(x_h))
s_pred <- sqrt(var_pred)

#Prediction Intervel at 95% confidence
lower_bound <- Yh_hat - t_critical * s_pred
upper_bound <- Yh_hat + t_critical * s_pred
cat("The 95% prediction interval for New Observation is:\n", 
    round(lower_bound, 4), "<= Y_h_new <=", round(upper_bound, 4), "\n")

# Mean Prediction Interval
m <- nrow(train_df)
var_meanpred <- mse * ( (1/m) + x_h %*% X_inv %*% t(x_h))
sd_meanpred <- sqrt(var_meanpred)
lower_bound <- Yh_hat - t_critical * sd_meanpred
upper_bound <- Yh_hat + t_critical * sd_meanpred
cat("The 95% prediction interval for New Observation is:\n", 
    round(lower_bound, 4), "<= mean of m obsr <=", round(upper_bound, 4), "\n")

#Confidence of Region (CR) for Regression Surface
sd_Y_cap_h

alpha <- 0.05
p <- length(coef(updated_both_model_mc2)) - 1
n <- nrow(train_df)  
F <- qf(1 - alpha, p, n - p)
W2 <- p * F
lower_bound <- Yh_hat - W2 * sd_Y_cap_h
upper_bound <- Yh_hat + W2 * sd_Y_cap_h
cat("The 95% prediction interval for New Observation is:\n", 
    round(lower_bound, 4), "<= Confidence Region at Xh <=", round(upper_bound, 4), "\n")


library(dplyr)
grouped_data <- train_df %>% 
  group_by(w_est_year, govt_cert, storage_issues, transport_issue, temperature_regulation) %>%
  summarise(mean_product_weight = mean(product_weight), .groups = "drop")

# Compute lack-of-fit and pure error
fitted_values <- predict(updated_both_model_mc2, newdata = train_df)

# Lack-of-fit SS
SS_LOF <- sum((fitted_values - grouped_data$mean_product_weight)^2)

# Pure error SS
SS_PE <- sum((train_df$product_weight - fitted_values)^2)

# Degrees of freedom
df_LOF <- nrow(grouped_data) - length(coefficients(updated_both_model_mc2))
df_PE <- nrow(train_df) - nrow(grouped_data)

# Mean squares
MS_LOF <- SS_LOF / df_LOF
MS_PE <- SS_PE / df_PE

# F-statistic
F_stat <- MS_LOF / MS_PE
p_value <- pf(F_stat, df_LOF, df_PE, lower.tail = FALSE)

# Output results
cat("F-statistic for Lack-of-Fit:", F_stat, "\n")
####place pvalue here
# Calculate the Lack-of-Fit and Pure Error
# SS_LOF: Lack of fit sum of squares - comparing model fitted values to the group means
SS_LOF <- sum((fitted_values - train_df$mean_product_weight)^2)

# SS_PE: Pure error sum of squares - comparing actual values to fitted values
SS_PE <- sum((train_df$product_weight - fitted_values)^2)

# Degrees of freedom
df_LOF <- nrow(grouped_data) - length(coefficients(updated_both_model_mc2))
df_PE <- nrow(train_df) - nrow(grouped_data)

# Mean squares
MS_LOF <- SS_LOF / df_LOF
MS_PE <- SS_PE / df_PE

# p-value
p_value <- pf(F_stat, df_LOF, df_PE, lower.tail = FALSE)


# Output p-value
print(p_value)

cat("Degrees of Freedom (LOF):", df_LOF, "\n")
cat("Degrees of Freedom (Pure Error):", df_PE, "\n")
#fitted
train_df$residuals <- residuals(updated_both_model_mc2)
train_df$fitted <- fitted(updated_both_model_mc2)

ggplot(train_df, aes(x = fitted, y = residuals)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()
#qq
ggplot(train_df, aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "QQ Plot of Residuals",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

# Scale-Location Plot
train_df$sqrt_abs_residuals <- sqrt(abs(train_df$residuals))

ggplot(train_df, aes(x = fitted, y = sqrt_abs_residuals)) +
  geom_point(color = "darkgreen", alpha = 0.5) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(title = "Scale-Location Plot",
       x = "Fitted Values",
       y = "√|Residuals|") +
  theme_minimal()
#r VS leverage
train_df$leverage <- hatvalues(updated_both_model_mc2)

ggplot(train_df, aes(x = leverage, y = residuals)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(title = "Residuals vs Leverage",
       x = "Leverage",
       y = "Residuals") +
  theme_minimal()
train_df$group <- cut(train_df$fitted, breaks = quantile(train_df$fitted, probs = seq(0, 1, by = 0.25), na.rm = TRUE), include.lowest = TRUE)

# Brown-Forsythe Test
bf_test <- oneway.test(residuals ~ group, data = train_df, var.equal = FALSE)

# Output results
cat("Brown-Forsythe Test Results:\n")
cat("F-statistic:", bf_test$statistic, "\n")
cat("Degrees of Freedom:", bf_test$parameter, "\n")
cat("p-value:", bf_test$p.value, "\n")

# Install and load necessary package
install.packages("lmtest")  # Install if necessary
library(lmtest)

# Durbin-Watson Test on the final model
dw_test <- dwtest(updated_both_model_mc2)

# Output the results of the Durbin-Watson Test
cat("Durbin-Watson Test Results:\n")
cat("Durbin-Watson Statistic:", dw_test$statistic, "\n")
cat("p-value:", dw_test$p.value, "\n")

# Perform Shapiro-Wilk Test for normality on residuals
# Create a subset of 5000 rows
set.seed(123)  # Setting a seed for reproducibility
subset_df <- train_df[sample(nrow(train_df), 5000), ]

# Perform Shapiro-Wilk Test for normality on residuals from the subset
shapiro_test <- shapiro.test(subset_df$residuals)

# Output the results of the Shapiro-Wilk test
cat("Shapiro-Wilk Test Results:\n")
cat("W-statistic:", shapiro_test$statistic, "\n")
cat("p-value:", shapiro_test$p.value, "\n")


# Grouping data by unique predictor combinations
grouped_data <- train_df %>% 
  group_by(w_est_year, govt_cert, storage_issues, transport_issue, temperature_regulation) %>%
  summarise(mean_product_weight = mean(product_weight), .groups = "drop")

# Compute the fitted values for the grouped data
fitted_grouped <- predict(updated_both_model_mc2, newdata = grouped_data)

# Lack-of-fit Sum of Squares (SS_LOF)
SS_LOF <- sum((fitted_grouped - grouped_data$mean_product_weight)^2)

# Pure Error Sum of Squares (SS_PE)
# We use the original 'train_df' and calculate residuals from the model
residuals <- train_df$product_weight - fitted_values
SS_PE <- sum(residuals^2)

# Degrees of Freedom
df_LOF <- nrow(grouped_data) - length(coefficients(updated_both_model_mc2))
df_PE <- nrow(train_df) - nrow(grouped_data)

# Mean Squares
MS_LOF <- SS_LOF / df_LOF
MS_PE <- SS_PE / df_PE

# F-statistic
F_stat <- MS_LOF / MS_PE

# p-value for the F-statistic
p_value <- pf(F_stat, df_LOF, df_PE, lower.tail = FALSE)

# Output the results
cat("F-statistic for Lack-of-Fit:", F_stat, "\n")
cat("p-value for Lack-of-Fit:", p_value, "\n")
cat("Degrees of Freedom (LOF):", df_LOF, "\n")
cat("Degrees of Freedom (Pure Error):", df_PE, "\n")


