# Load necessary libraries
library(data.table)
library(dplyr)
library(ggplot2)
library(caret)
library(corrplot)
library(xgboost)
library(cowplot)
library(glmnet)
library(ranger)

# Load the datasets
test <- read.csv("/Users/saicharanreddy/Desktop/BDA_Project/test_AbJTz2l.csv")
train <- read.csv("/Users/saicharanreddy/Desktop/BDA_Project/train_v9rqX0R.csv")

# Convert data frames to data.table if needed
setDT(test)
setDT(train)

# Assign NA to Item_Outlet_Sales in the test dataset
test[, Item_Outlet_Sales := NA]

# Combine train and test datasets
combi <- rbind(train, test)

# Check the dimensions of the combined dataset
dim(combi)

# Print the values of the 'Item_Outlet_Sales' column
print(combi$Item_Outlet_Sales)

# Save the combined dataset to a CSV file in the same directory
write.csv(combi, "/Users/saicharanreddy/Desktop/BDA_Project/combi.csv", row.names = FALSE)

# Plot histogram for Item_Outlet_Sales
ggplot(train) +
  geom_histogram(aes(x = Item_Outlet_Sales), binwidth = 100, fill = "darkgreen") +
  xlab("Item_Outlet_Sales")

# Create individual plots
p1 <- ggplot(combi) +
  geom_histogram(aes(x = Item_Weight), binwidth = 0.5, fill = "blue")

p2 <- ggplot(combi) +
  geom_histogram(aes(x = Item_Visibility), binwidth = 0.005, fill = "blue")

p3 <- ggplot(combi) +
  geom_histogram(aes(x = Item_MRP), binwidth = 1, fill = "blue")

# Combine plots using cowplot
plot_grid(p1, p2, p3, nrow = 1)

# Plot bar chart for Item_Fat_Content
ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) +
  geom_bar(aes(x = Item_Fat_Content, y = Count), stat = "identity", fill = "coral1")

# Update Item_Fat_Content values
combi[Item_Fat_Content == "LF", Item_Fat_Content := "Low Fat"]
combi[Item_Fat_Content == "low fat", Item_Fat_Content := "Low Fat"]
combi[Item_Fat_Content == "reg", Item_Fat_Content := "Regular"]

# Plot bar chart for updated Item_Fat_Content
ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) +
  geom_bar(aes(x = Item_Fat_Content, y = Count), stat = "identity", fill = "coral1")

# Plot for Item_Type
p4 <- ggplot(combi %>% group_by(Item_Type) %>% summarise(Count = n())) +
  geom_bar(aes(x = Item_Type, y = Count), stat = "identity", fill = "coral1") +
  xlab("") +
  geom_label(aes(x = Item_Type, y = Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Item_Type")

# Plot for Outlet_Identifier
p5 <- ggplot(combi %>% group_by(Outlet_Identifier) %>% summarise(Count = n())) +
  geom_bar(aes(x = Outlet_Identifier, y = Count), stat = "identity", fill = "coral1") +
  geom_label(aes(x = Outlet_Identifier, y = Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot for Outlet_Size
p6 <- ggplot(combi %>% group_by(Outlet_Size) %>% summarise(Count = n())) +
  geom_bar(aes(x = Outlet_Size, y = Count), stat = "identity", fill = "coral1") +
  geom_label(aes(x = Outlet_Size, y = Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine plots using cowplot
second_row <- plot_grid(p5, p6, nrow = 1)
plot_grid(p4, second_row, ncol = 1)

# Split the combined data back into train and test sets
train <- combi[1:nrow(train)]
test <- combi[(nrow(train) + 1):nrow(combi)]

# Plot for Item_Weight vs Item_Outlet_Sales
p9 <- ggplot(train) +
  geom_point(aes(x = Item_Weight, y = Item_Outlet_Sales), colour = "violet", alpha = 0.3) +
  theme(axis.title = element_text(size = 8.5))

# Plot for Item_Visibility vs Item_Outlet_Sales
p10 <- ggplot(train) +
  geom_point(aes(x = Item_Visibility, y = Item_Outlet_Sales), colour = "violet", alpha = 0.3) +
  theme(axis.title = element_text(size = 8.5))

# Plot for Item_MRP vs Item_Outlet_Sales
p11 <- ggplot(train) +
  geom_point(aes(x = Item_MRP, y = Item_Outlet_Sales), colour = "violet", alpha = 0.3) +
  theme(axis.title = element_text(size = 8.5))

# Combine plots using cowplot
second_row_2 <- plot_grid(p10, p11, ncol = 2)
plot_grid(p9, second_row_2, nrow = 2)

# Plot for Item_Type vs Item_Outlet_Sales
p12 <- ggplot(train) +
  geom_violin(aes(x = Item_Type, y = Item_Outlet_Sales), fill = "magenta") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 8.5))

# Plot for Item_Fat_Content vs Item_Outlet_Sales
p13 <- ggplot(train) +
  geom_violin(aes(x = Item_Fat_Content, y = Item_Outlet_Sales), fill = "magenta") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8.5))

# Plot for Outlet_Identifier vs Item_Outlet_Sales
p14 <- ggplot(train) +
  geom_violin(aes(x = Outlet_Identifier, y = Item_Outlet_Sales), fill = "magenta") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8.5))

# Combine plots using cowplot
second_row_3 <- plot_grid(p13, p14, ncol = 2)
plot_grid(p12, second_row_3, ncol = 1)

# Plot for Outlet_Size vs Item_Outlet_Sales
ggplot(train) +
  geom_violin(aes(x = Outlet_Size, y = Item_Outlet_Sales), fill = "magenta")

# Plot for Outlet_Location_Type vs Item_Outlet_Sales
p15 <- ggplot(train) +
  geom_violin(aes(x = Outlet_Location_Type, y = Item_Outlet_Sales), fill = "magenta")

# Plot for Outlet_Type vs Item_Outlet_Sales
p16 <- ggplot(train) +
  geom_violin(aes(x = Outlet_Type, y = Item_Outlet_Sales), fill = "magenta")

# Combine plots using cowplot
plot_grid(p15, p16, ncol = 1)

# Identify the indices of missing values in Item_Weight
missing_index <- which(is.na(combi$Item_Weight))

# Loop through each missing index
for (i in missing_index) {
  item <- combi$Item_Identifier[i]
  
  # Calculate the mean Item_Weight for the same Item_Identifier, excluding NA values
  mean_weight <- mean(combi$Item_Weight[combi$Item_Identifier == item], na.rm = TRUE)
  
  # Assign the mean weight to the missing value
  combi$Item_Weight[i] <- mean_weight
}

# Plot histogram for Item_Visibility
ggplot(combi) +
  geom_histogram(aes(x = Item_Visibility), bins = 100)

# Identify the indices where Item_Visibility is zero
zero_index <- which(combi$Item_Visibility == 0)

# Loop through each zero index
for (i in zero_index) {
  item <- combi$Item_Identifier[i]
  
  # Calculate the mean Item_Visibility for the same Item_Identifier, excluding zero values
  mean_visibility <- mean(combi$Item_Visibility[combi$Item_Identifier == item & combi$Item_Visibility != 0], na.rm = TRUE)
  
  # Assign the mean visibility to the zero value
  combi$Item_Visibility[i] <- mean_visibility
}

# Plot histogram for Item_Visibility
ggplot(combi) +
  geom_histogram(aes(x = Item_Visibility), bins = 100)

# Define the categories
perishable <- c("Breads", "Breakfast", "Dairy", "Fruits and Vegetables", "Meat", "Seafood")
non_perishable <- c("Baking Goods", "Canned", "Frozen Foods", "Hard Drinks", "Health and Hygiene", "Household", "Soft Drinks")

# Create a new feature 'Item_Type_new'
combi[, Item_Type_new := ifelse(Item_Type %in% perishable, "perishable",
                                ifelse(Item_Type %in% non_perishable, "non_perishable", "not_sure"))]

# Create a new feature 'Item_category'
combi[, Item_category := substr(Item_Identifier, 1, 2)]

# Modify Item_Fat_Content for "NC" category
combi[Item_category == "NC", Item_Fat_Content := "Non-Edible"]

# Calculate years of operation for outlets
combi[, Outlet_Years := 2013 - Outlet_Establishment_Year]

# Convert Outlet_Establishment_Year to a factor
combi[, Outlet_Establishment_Year := as.factor(Outlet_Establishment_Year)]

# Calculate price per unit weight
combi[, price_per_unit_wt := Item_MRP / Item_Weight]

# Create a new independent variable - Item_MRP_clusters
combi[, Item_MRP_clusters := ifelse(Item_MRP < 69, "1st",
                                    ifelse(Item_MRP >= 69 & Item_MRP < 136, "2nd",
                                           ifelse(Item_MRP >= 136 & Item_MRP < 203, "3rd", "4th")))]

# Label encoding for Outlet_Size
combi[, Outlet_Size_num := ifelse(Outlet_Size == "Small", 0,
                                  ifelse(Outlet_Size == "Medium", 1, 2))]

# Label encoding for Outlet_Location_Type
combi[, Outlet_Location_Type_num := ifelse(Outlet_Location_Type == "Tier 3", 0,
                                           ifelse(Outlet_Location_Type == "Tier 2", 1, 2))]

# Remove categorical variables after label encoding
combi[, c("Outlet_Size", "Outlet_Location_Type") := NULL]

# One-hot encoding for the remaining categorical variables
ohe <- dummyVars("~ .", data = combi[, -c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")], fullRank = TRUE)
ohe_df <- data.table(predict(ohe, combi[, -c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")]))

# Combine the one-hot encoded data with Item_Identifier
combi <- cbind(combi[, "Item_Identifier", with = FALSE], ohe_df)

# Apply log transformation to Item_Visibility
combi[, Item_Visibility := log(Item_Visibility + 1)]

# Apply log transformation to price_per_unit_wt
combi[, price_per_unit_wt := log(price_per_unit_wt + 1)]

# Identify numeric features
num_vars <- which(sapply(combi, is.numeric))
num_vars_names <- names(num_vars)

# Extract numeric features except Item_Outlet_Sales
combi_numeric <- combi[, setdiff(num_vars_names, "Item_Outlet_Sales"), with = FALSE]

# Preprocess numeric features: center and scale
prep_num <- preProcess(combi_numeric, method = c("center", "scale"))
combi_numeric_norm <- predict(prep_num, combi_numeric)

# Remove original numeric independent variables from combi
combi[, setdiff(num_vars_names, "Item_Outlet_Sales") := NULL]

# Combine normalized numeric features with the rest of the data
combi <- cbind(combi, combi_numeric_norm)

# Split the combined data back into train and test sets
train <- combi[1:nrow(train)]
test <- combi[(nrow(train) + 1):nrow(combi)]

# Remove Item_Outlet_Sales from the test set as it contains only NA
test[, Item_Outlet_Sales := NULL]

# Plot correlation matrix
cor_train <- cor(train[, -c("Item_Identifier")])
corrplot(cor_train, method = "pie", type = "lower", tl.cex = 0.9)

# Train the linear regression model
linear_reg_mod <- lm(Item_Outlet_Sales ~ ., data = train[, -c("Item_Identifier")])

# Make predictions on the test data
test_predictions <- predict(linear_reg_mod, test[, -c("Item_Identifier")])

# Prepare the submission DataFrame
submission <- data.frame(Item_Identifier = test$Item_Identifier, Item_Outlet_Sales = test_predictions)

# Write the submission DataFrame to a CSV file
write.csv(submission, "Linear_Reg_submit.csv", row.names = FALSE)

# Calculate RMSE
# Assuming 'actual_values' is the actual Item_Outlet_Sales from the test set
# Since the test set doesn't have actual values, you would typically use a validation set or cross-validation for this purpose
# For demonstration, let's assume you have actual values in a variable called 'actual_values'
actual_values <- train$Item_Outlet_Sales  # Replace this with your actual test values if available

# Calculate RMSE
rmse <- sqrt(mean((actual_values - test_predictions)^2))

# Print the RMSE score
print(paste("RMSE:", rmse))

# Lasso Regression
set.seed(1235)
my_control <- trainControl(method = "cv", number = 5)
lasso_grid <- expand.grid(alpha = 1, lambda = seq(0.001, 0.1, by = 0.0002))
lasso_linear_reg_mod <- train(
  x = train[, -c("Item_Identifier", "Item_Outlet_Sales")],
  y = train$Item_Outlet_Sales,
  method = 'glmnet',
  trControl = my_control,
  tuneGrid = lasso_grid,
  metric = "RMSE"
)
print(lasso_linear_reg_mod$bestTune)
print(lasso_linear_reg_mod)

# Ridge Regression
set.seed(1236)
ridge_grid <- expand.grid(alpha = 0, lambda = seq(0.001, 0.1, by = 0.0002))
ridge_linear_reg_mod <- train(
  x = train[, -c("Item_Identifier", "Item_Outlet_Sales")],
  y = train$Item_Outlet_Sales,
  method = 'glmnet',
  trControl = my_control,
  tuneGrid = ridge_grid,
  metric = "RMSE"
)
print(ridge_linear_reg_mod$bestTune)
print(ridge_linear_reg_mod)

# Random Forest Regression
set.seed(1237)
tgrid <- expand.grid(
  mtry = c(3:10),
  splitrule = "variance",
  min.node.size = c(10, 15, 20)
)
rf_mod <- train(
  x = train[, -c("Item_Identifier", "Item_Outlet_Sales")],
  y = train$Item_Outlet_Sales,
  method = 'ranger',
  trControl = my_control,
  tuneGrid = tgrid,
  num.trees = 400,
  importance = "permutation"
)
print(rf_mod$bestTune)
print(rf_mod)
plot(rf_mod)
plot(varImp(rf_mod))



library(xgboost)

# Define the parameter list for XGBoost
param_list <- list(
  objective = "reg:linear",
  eta = 0.01,
  gamma = 1,
  max_depth = 6,
  subsample = 0.8,
  colsample_bytree = 0.5
)

# Create DMatrix for training data
dtrain <- xgb.DMatrix(data = as.matrix(train[, -c("Item_Identifier", "Item_Outlet_Sales")]),
                      label = train$Item_Outlet_Sales)

# Create DMatrix for test data
dtest <- xgb.DMatrix(data = as.matrix(test[, -c("Item_Identifier")]))

# Set seed for reproducibility
set.seed(112)

# Perform cross-validation
xgbcv <- xgb.cv(params = param_list,
                data = dtrain,
                nrounds = 1000,
                nfold = 5,
                print_every_n = 10,
                early_stopping_rounds = 30,
                maximize = FALSE,
                prediction = TRUE)

# Print the cross-validation results
print(xgbcv)

xgb_model = xgb.train(data = dtrain, params = param_list, nrounds = 430)


xgb_model <- xgb.train(
  params = param_list,
  data = dtrain,
  nrounds = 100,
  watchlist = list(train = dtrain),
  print_every_n = 10,
  early_stopping_rounds = 30,
  maximize = FALSE,
  verbose = 1
)

# Extract feature importance
var_imp <- xgb.importance(feature_names = setdiff(names(train), c("Item_Identifier", "Item_Outlet_Sales")),
                          model = xgb_model)

# Plot feature importance
xgb.plot.importance(var_imp)



# Create DMatrix for test data
dtest <- xgb.DMatrix(data = as.matrix(test[, -c("Item_Identifier")]))

# Make predictions using the trained XGBoost model
test_predictions <- predict(xgb_model, dtest)

# Prepare the submission DataFrame
submission <- data.table(
  Item_Identifier = test$Item_Identifier,
  Outlet_Identifier = test$Outlet_Identifier,
  Item_Outlet_Sales = test_predictions
)

# Write the submission DataFrame to a CSV file
write.csv(submission, "XGBoost_submit.csv", row.names = FALSE)