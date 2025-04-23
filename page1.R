# Load necessary libraries
# install.packages("glmnet")
# install.packages("ranger")
library(data.table) # used for reading and manipulation of data
library(dplyr)      # used for data manipulation and joining
library(ggplot2)    # used for plotting
library(caret)      # used for modeling
library(corrplot)   # used for making correlation plot
library(xgboost)    # used for building XGBoost model
library(cowplot)    # used for combining multiple plots

# Load the datasets
test <- read.csv("/Users/saicharanreddy/desktop/bda-project/test_AbJTz2l.csv")
train <- read.csv("/Users/saicharanreddy/desktop/bda-project/train_v9rqX0R.csv")

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
write.csv(combi, "/Users/saicharanreddy/desktop/bda-project/combi.csv", row.names = FALSE)

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
combi$Item_Fat_Content[combi$Item_Fat_Content == "LF"] <- "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content == "low fat"] <- "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content == "reg"] <- "Regular"

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

train = combi[1:nrow(train)] # extracting train data from the combined data

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

ggplot(train) + geom_violin(aes(Outlet_Size, Item_Outlet_Sales), fill = "magenta")


# Plot for Outlet_Location_Type vs Item_Outlet_Sales
p15 <- ggplot(train) +
  geom_violin(aes(x = Outlet_Location_Type, y = Item_Outlet_Sales), fill = "magenta")

# Plot for Outlet_Type vs Item_Outlet_Sales
p16 <- ggplot(train) +
  geom_violin(aes(x = Outlet_Type, y = Item_Outlet_Sales), fill = "magenta")

# Combine plots using cowplot
plot_grid(p15, p16, ncol = 1)


sum(is.na(combi$Item_Weight))


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


ggplot(combi) + geom_histogram(aes(Item_Visibility), bins = 100)


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

ggplot(combi) + geom_histogram(aes(Item_Visibility), bins = 100)

# Define the categories
perishable <- c("Breads", "Breakfast", "Dairy", "Fruits and Vegetables", "Meat", "Seafood")
non_perishable <- c("Baking Goods", "Canned", "Frozen Foods", "Hard Drinks", "Health and Hygiene", "Household", "Soft Drinks")

# Create a new feature 'Item_Type_new'
combi[, Item_Type_new := ifelse(Item_Type %in% perishable, "perishable",
                                ifelse(Item_Type %in% non_perishable, "non_perishable", "not_sure"))]

table(combi$Item_Type, substr(combi$Item_Identifier, 1, 2))

combi[,Item_category := substr(combi$Item_Identifier, 1, 2)]

# Modify Item_Fat_Content for "NC" category
combi$Item_Fat_Content[combi$Item_category == "NC"] <- "Non-Edible"

# Calculate years of operation for outlets
combi[, Outlet_Years := 2013 - Outlet_Establishment_Year]

# Convert Outlet_Establishment_Year to a factor
combi$Outlet_Establishment_Year <- as.factor(combi$Outlet_Establishment_Year)

# Calculate price per unit weight
combi[, price_per_unit_wt := Item_MRP / Item_Weight]

# Create a new independent variable - Item_MRP_clusters
combi[, Item_MRP_clusters := ifelse(Item_MRP < 69, "1st",
                                    ifelse(Item_MRP >= 69 & Item_MRP < 136, "2nd",
                                           ifelse(Item_MRP >= 136 & Item_MRP < 203, "3rd", "4th")))]


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


cor_train = cor(train[,-c("Item_Identifier")]) 
corrplot(cor_train, method = "pie", type = "lower", tl.cex = 0.9)

# Train the linear regression model
linear_reg_mod <- lm(Item_Outlet_Sales ~ ., data = train[, -c("Item_Identifier")])

# Make predictions on the test data
test_predictions <- predict(linear_reg_mod, test[, -c("Item_Identifier")])

# Prepare the submission DataFrame
submission <- data.frame(Item_Identifier = test$Item_Identifier, Item_Outlet_Sales = test_predictions)

# Write the submission DataFrame to a CSV file
write.csv(submission, "Linear_Reg_submit.csv", row.names = FALSE)



test_predictions <- predict(linear_reg_mod, newdata = test[, -c("Item_Identifier")])

# Assuming 'actual_values' is the actual Item_Outlet_Sales from the test set
# Since the test set doesn't have actual values, you would typically use a validation set or cross-validation for this purpose
# For demonstration, let's assume you have actual values in a variable called 'actual_values'
actual_values <- train$Item_Outlet_Sales  # Replace this with your actual test values if available

# Calculate RMSE
rmse <- sqrt(mean((actual_values - test_predictions)^2))

# Print the RMSE score
print(paste("RMSE:", rmse))



library(glmnet)

# Set seed for reproducibility
set.seed(1235)

# Define control for cross-validation
my_control <- trainControl(method = "cv", number = 5)

# Lasso Regression
# Define grid for Lasso (alpha = 1)
lasso_grid <- expand.grid(alpha = 1, lambda = seq(0.001, 0.1, by = 0.0002))

# Train Lasso regression model
lasso_linear_reg_mod <- train(
  x = train[, -c("Item_Identifier", "Item_Outlet_Sales")],
  y = train$Item_Outlet_Sales,
  method = 'glmnet',
  trControl = my_control,
  tuneGrid = lasso_grid,
  metric = "RMSE"
)

# Print the best tuning parameters and performance
print(lasso_linear_reg_mod$bestTune)
print(lasso_linear_reg_mod)

# Ridge Regression
# Set seed for reproducibility
set.seed(1236)

# Define grid for Ridge (alpha = 0)
ridge_grid <- expand.grid(alpha = 0, lambda = seq(0.001, 0.1, by = 0.0002))

# Train Ridge regression model
ridge_linear_reg_mod <- train(
  x = train[, -c("Item_Identifier", "Item_Outlet_Sales")],
  y = train$Item_Outlet_Sales,
  method = 'glmnet',
  trControl = my_control,
  tuneGrid = ridge_grid,
  metric = "RMSE"
)

# Print the best tuning parameters and performance
print(ridge_linear_reg_mod$bestTune)
print(ridge_linear_reg_mod)



library(ranger)

# Set seed for reproducibility
set.seed(1237)

# Define control for cross-validation
my_control <- trainControl(method = "cv", number = 5)

# Define grid for hyperparameter tuning
tgrid <- expand.grid(
  mtry = c(3:10),
  splitrule = "variance",
  min.node.size = c(10, 15, 20)
)

# Train Random Forest model using ranger
rf_mod <- train(
  x = train[, -c("Item_Identifier", "Item_Outlet_Sales")],
  y = train$Item_Outlet_Sales,
  method = 'ranger',
  trControl = my_control,
  tuneGrid = tgrid,
  num.trees = 400,
  importance = "permutation"
)

# Print the best tuning parameters and model performance
print(rf_mod$bestTune)
print(rf_mod)
plot(rf_mod)
