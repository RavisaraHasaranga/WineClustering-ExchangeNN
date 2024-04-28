# Read the data from the file
library(readxl)

set.seed(42)

data <- read_excel("datasets/ExchangeUSD (2).xlsx")
data <- data.frame(data)
summary(data)
head(data)

# Encode the dates
data$YYYY.MM.DD <- as.Date(data$YYYY.MM.DD, format = "%Y-%m-%d")
reference_date <- as.Date("13/10/2011", format = "%d/%m/%Y")
data$YYYY.MM.DD <- as.numeric(difftime(data$YYYY.MM.DD, reference_date, units = "days"))

# Hot encode the weekDays
days_factor <- factor(data$Wdy, levels = c("Mon", "Tue", "Wed", "Thu", "Fri"))
data$Wdy <- as.integer(days_factor) - 1

data$USD.EUR <- as.vector(data$USD.EUR)
data$USD.EUR 

# Normalize the data
data <- as.data.frame(scale(data))
names(data) <- c("date", "day","t")

# Create lagged datasets
lagged_datasets <- list()
library(dplyr)

lagged_datasets[[1]] <- data %>%
  mutate(t.1 = lag(t, 1)) %>%
  na.omit() 

lagged_datasets[[2]] <- data %>%
  mutate(t.1 = lag(t, 1),
         t.2 = lag(t, 2)) %>%
  na.omit() 

lagged_datasets[[3]] <- data %>%
  mutate(t.1 = lag(t, 1),
         t.2 = lag(t, 2),
         t.3 = lag(t, 3)) %>%
  na.omit() 

lagged_datasets[[4]] <- data %>%
  mutate(t.1 = lag(t, 1),
         t.2 = lag(t, 2),
         t.3 = lag(t, 3),
         t.4 = lag(t, 4)) %>%
  na.omit()

lagged_datasets
# Split the data into training and testing


lagged_train_data <- list()
lagged_test_data <- list()

lagged_train_data[[1]] <- lagged_datasets[[1]][1:400, ]
lagged_test_data[[1]] <- lagged_datasets[[1]][401:nrow(lagged_datasets[[1]]), ]

lagged_train_data[[2]] <- lagged_datasets[[2]][1:400, ]
lagged_test_data[[2]] <- lagged_datasets[[2]][401:nrow(lagged_datasets[[2]]), ]

lagged_train_data[[3]] <- lagged_datasets[[3]][1:400, ]
lagged_test_data[[3]] <- lagged_datasets[[3]][401:nrow(lagged_datasets[[3]]), ]

lagged_train_data[[4]] <- lagged_datasets[[4]][1:400, ]
lagged_test_data[[4]] <- lagged_datasets[[4]][401:nrow(lagged_datasets[[4]]), ]

# Train the model

# Define the formulas for different lagged inputs
lag_formulas <- list(
  lag1_formula = as.formula("t  ~t.1"),
  lag2_formula = as.formula("t  ~t.1 + t.2"),
  lag3_formula = as.formula("t  ~t.1 + t.2 + t.3"),
  lag4_formula = as.formula("t  ~t.1 + t.2 + t.3 + t.4")
)

# Define different hidden layer structures
hidden_layer_structures <- list(c(5), c(10), c(10, 5))

# Create a list to store neural networks
nn_list <- list()

library(neuralnet)

nn_list[[7]]

# Train neural networks for each lag and hidden layer structure
# Define different activation functions

# Train neural networks for each lag, hidden layer structure, activation function, and output layer configuration
for (i in 1:length(lagged_train_data)) {
  for (j in 1:length(hidden_layer_structures)) {
    nn_name <- paste("lag", i, "hidden", paste(hidden_layer_structures[[j]], collapse = ""), "_","_output", output_layer_config, sep = "")
    
    # Train the neural network
    nn_list[[nn_name]] <- neuralnet(
      formula = lag_formulas[[i]],
      data = lagged_train_data[[i]],
      hidden = hidden_layer_structures[[j]],
    )
  }
}

# Evaluate
evaluation_results <- list()
counter = 1
for (i in 1:length(lagged_test_data)) {
  for (j in 1:length(hidden_layer_structures)) {
    nn_name <- paste("lag", i, "hidden", paste(hidden_layer_structures[[j]], collapse = ""), "_", "_output", output_layer_config, sep = "")
    
    nn <- nn_list[[nn_name]]
    # Make predictions on the evaluation dataset
    print(counter)
    counter = counter + 1
    predictions <- as.vector(neuralnet::compute(nn, lagged_test_data[[i]][, -3]))
    
    # Get the corresponding test data for evaluation
    test_data <- lagged_test_data[[i]]$t  # Assuming the target variable is in the first column
    
    
    # Calculate evaluation metrics
    rmse <- sqrt(mean((predictions - test_data)^2))
    mae <- mean(abs(predictions - test_data))
    mape <- mean(abs((test_data - predictions) / test_data)) * 100
    smape <- mean(200 * abs(test_data - predictions) / (abs(test_data) + abs(predictions)))
    
    # Store evaluation results
    evaluation_results[[nn_name]] <- list(
      RMSE = rmse,
      MAE = mae,
      MAPE = mape,
      sMAPE = smape
    )
  }
}

print(evaluation_results)



for (nn_name in names(nn_list)) {
  nn <- nn_list[[nn_name]]
  
  # Make predictions on the evaluation dataset
  predictions <- predict(nn, test_data)
  
  # Ensure predictions are a vector
  predictions <- as.vector(predictions)
  
  # Calculate evaluation metrics
  rmse <- sqrt(mean((predictions - test_data$USD.EUR)^2))
  mae <- mean(abs(predictions - test_data$USD.EUR))
  mape <- mean(abs((test_data$USD.EUR - predictions) / test_data$USD.EUR)) * 100
  smape <- mean(200 * abs(test_data$USD.EUR - predictions) / (abs(test_data$USD.EUR) + abs(predictions))) 
  
  # Store evaluation results
  evaluation_results[[nn_name]] <- list(
    RMSE = rmse,
    MAE = mae,
    MAPE = mape,
    sMAPE = smape
  )
}

# Print or analyze the evaluation results as needed
print(evaluation_results)


# Print or analyze the evaluation results as needed
print(evaluation_results)


# Convert the list of evaluation results to a data frame
results_df <- do.call(rbind, lapply(names(evaluation_results), function(model_name) {
  model_results <- evaluation_results[[model_name]]
  data.frame(
    Model = model_name,
    RMSE = model_results$RMSE,
    MAE = model_results$MAE,
    MAPE = model_results$MAPE,
    sMAPE = model_results$sMAPE
  )
}))

# Print the data frame
print(results_df)


# Print the model(s) with the minimum RMSE
print(results_df[which.min(results_df$RMSE), ])

# Print the model(s) with the minimum MAE

# Print the model(s) with the minimum MAPE
print(results_df[which.min(results_df$MAPE), ])

# Print the model(s) with the minimum sMAPE
print(results_df[which.min(results_df$sMAPE), ])

library(ggplot2)

# Function to create a plot for a specific evaluation metric
plot_metric <- function(metric_name) {
  ggplot(results_df, aes(x = Model, y = eval(parse(text = metric_name)))) +
    geom_bar(stat = "identity", fill = "skyblue", color = "black") +
    labs(title = paste(metric_name, "for Each Model"),
         x = "Model",
         y = metric_name) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Create a list of plots for each evaluation metric
plots <- lapply(c("RMSE", "MAE", "MAPE", "sMAPE"), plot_metric)

# Print each plot
for (plot in plots) {
  print(plot)
}



# Create a small dummy dataset
dummy_data <- data.frame(
  date = 1:10,
  day = rep(1:5, 2),
  t.1 = rnorm(10),
  t.2 = rnorm(10),
  t.3 = rnorm(10),
  t.4 = rnorm(10),
  t = rnorm(10)
)

# Train a new neural network model
nn2 <- neuralnet(
  formula = t ~ date + day + t.1 + t.2 + t.3 + t.4,
  data = dummy_data,
  hidden = c(5),
  linear.output = TRUE
)

# Try making predictions with the dummy dataset
dummy_predictions <- predict(nn2, dummy_data[, -7])
dummy_predictions