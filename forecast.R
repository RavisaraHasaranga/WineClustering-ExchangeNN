library(readxl)
library(dplyr)

data<-read_excel(path = "datasets/ExchangeUSD (2).xlsx")
data<-data.frame(data[,3])
names(data)<-c("rate")
summary(data)


#creating lagged datasets
lag_data<-list()

lag_data[[1]]<- data %>%
  mutate(t.1 = lag(rate,1))%>%
  na.omit()

lag_data[[2]] <- data %>%
  mutate(t.1 = lag(rate, 1),
         t.2 = lag(rate, 2)) %>%
  na.omit() 

lag_data[[3]] <- data %>%
  mutate(t.1 = lag(rate, 1),
         t.2 = lag(rate, 2),
         t.3 = lag(rate, 3)) %>%
  na.omit() 

lag_data[[4]] <- data %>%
  mutate(t.1 = lag(rate, 1),
         t.2 = lag(rate, 2),
         t.3 = lag(rate, 3),
         t.4 = lag(rate, 4)) %>%
  na.omit()

head(lag_data[[3]])

#normalize each lagged dataset
lag_data_norm<-list()

for (i in 1:length(lag_data)){
  lag_data_norm[[i]]<-scale(lag_data[[i]])
}

head(lag_data_norm[[3]])

# Split data into training and testing sets
lag_train_data <- list()
lag_test_data <- list()

for (i in 1: length(lag_data_norm)){
  lag_train_data[[i]]<-lag_data_norm[[i]][1:400,]
  lag_test_data[[i]]<-lag_data_norm[[i]][401:nrow(lag_data_norm[[i]]),]
  
}


library(neuralnet)

#defining hidden layers
architectures<-list(c(4),c(5),c(5,3))

#activation functions
activate<-c("logistic","tanh")

#output layers
output_type<-c(TRUE,FALSE) #true for linear, false for non linear

#function to train data
model<-function(architecture,train_data,test_data,activation,output){
  
  #creating a formula for each lag dataset
  formula <- as.formula(paste("rate ~", paste(colnames(train_data)[-1],
                                              collapse = " + ")))
  
  #training the model
  tryCatch({
    model <- neuralnet::neuralnet(formula, data = train_data, hidden = architecture, 
                                  act.fct = activation, linear.output = output)
  }, warning = function(e) {
    print(paste("Warning: ", e))
    return(NULL)
  })
  return(model)
  
}




#list to store neural networks
NN<-list()

# Loop through lags, architectures, activation functions, and output types
###Training for Lag 1
for (j in 1:length(architectures)) { 
  for (k in 1:length(activate)) {
    for (l in 1:length(output_type)) {
      result <- model(architectures[[j]], lag_train_data[[1]], lag_test_data[[1]], 
                      activate[k], output_type[l])
      if (!is.null(result)) {
        NN[[paste("lag1", "arch", j, "act", k, "output", l)]] <- result
      }
    }
  }
}


###Training for Lag 2
for (j in 1:length(architectures)) { 
  for (k in 1:length(activate)) { 
    for (l in 1:length(output_type)) {
      result <- model(architectures[[j]], lag_train_data[[2]], lag_test_data[[2]], 
                      activate[k], output_type[l])
      NN[[paste("lag2", "arch", j, "act", k, "output", l)]] <- result
    }
  }
}

###Training for Lag 3
for (j in 1:length(architectures)) {
  for (k in 1:length(activate)) { 
    for (l in 1:length(output_type)) { 
      result <- model(architectures[[j]], lag_train_data[[3]], lag_test_data[[3]], 
                      activate[k], output_type[l])
      NN[[paste("lag3", "arch", j, "act", k, "output", l)]] <- result
    }
  }
}

###Training for Lag 4
for (j in 1:length(architectures)) { 
  for (k in 1:length(activate)) {
    for (l in 1:length(output_type)) {
      result <- model(architectures[[j]], lag_train_data[[4]], lag_test_data[[4]], 
                      activate[k], output_type[l])
      NN[[paste("lag4", "arch", j, "act", k, "output", l)]] <- result
    }
  }
}

names(NN)

# Function to Calculate Performance Metrics 
evaluate <- function(predictions, actual) {
  rmse <- sqrt(mean((predictions - actual)^2))
  mae <- mean(abs(predictions - actual))
  mape <- mean(abs((predictions - actual) / actual)) * 100
  smape <- mean(2 * abs(predictions - actual) / (abs(predictions) 
                                                 + abs(actual))) * 100
  
  return(list(rmse = rmse, mae = mae, mape = mape, smape = smape))
}

#list to store evaluations
evaluations<-list()

#remove null
NN <- NN[sapply(NN, is.list)]


# Loop through trained models and evaluate
for (model_name in names(NN)) {
  # Extract model and test data
  model <- NN[[model_name]]
  lag_num <- as.numeric(substr(model_name, 4, 4))
  test_data <- (lag_test_data[[lag_num]])
  end_col <- ncol(test_data)
  test_data <- as.matrix(test_data[,-1])
  colnames(test_data) <- NULL
  print(head(test_data))
  
  # Get predictions with type check and conversion
  predictions <- neuralnet::compute(model, test_data)$net.result
  if (!is.numeric(predictions)) {
    predictions <- as.numeric(predictions)
  }
  
  # Evaluate and store results
  evaluations[[model_name]] <- evaluate(predictions, test_data[,1])
}



# Print or analyze the evaluations
print(evaluations)

library(ggplot2)

# Extract RMSE values and output types
rmse_data <- data.frame(
  model = names(evaluations),
  rmse = sapply(evaluations, function(x) x$rmse),
  output_type = ifelse(grepl("output 1", names(evaluations)), "Linear", "Non-Linear") 
)

#plotting the RMSE values
ggplot(rmse_data, aes(x = model, y = rmse, fill = output_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "RMSE Performance of MLP Models", x = "Model", y = "RMSE") +
  scale_fill_manual(values = c("Linear" = "cyan3", "Non-Linear" = "coral1")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


# Extract MAE values and output types
mae_data <- data.frame(
  model = names(evaluations),
  mae = sapply(evaluations, function(x) x$mae),
  output_type = ifelse(grepl("output 1", names(evaluations)), "Linear", "Non-Linear")
)

#plotting the MAE values
ggplot(mae_data, aes(x = model, y = mae, fill = output_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "MAE Performance of MLP Models", x = "Model", y = "MAE") +
  scale_fill_manual(values = c("Linear" = "cyan3", "Non-Linear" = "coral1")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Extract MAPE values and output types
mape_data <- data.frame(
  model = names(evaluations),
  mape = sapply(evaluations, function(x) x$mape),
  output_type = ifelse(grepl("output 1", names(evaluations)), "Linear", "Non-Linear")
)

#plotting the MAPE values
ggplot(mape_data, aes(x = model, y = mape, fill = output_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "MAPE Performance of MLP Models", x = "Model", y = "MAPE") +
  scale_fill_manual(values = c("Linear" = "cyan3", "Non-Linear" = "coral1")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Extract SMAPE values and output types
smape_data <- data.frame(
  model = names(evaluations),
  smape = sapply(evaluations, function(x) x$smape),
  output_type = ifelse(grepl("output 1", names(evaluations)), "Linear", "Non-Linear")
)

#Plotting the SMAPE values
ggplot(smape_data, aes(x = model, y = smape, fill = output_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "SMAPE Performance of MLP Models", x = "Model", y = "SMAPE") +
  scale_fill_manual(values = c("Linear" = "cyan3", "Non-Linear" = "coral1")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Filter linear models
linear_models <- NN[grepl("output 1", names(NN))]

# Create a new evaluations list for linear models
linear_evaluations <- list()
for (model_name in names(linear_models)) {
  # Extract model and test data
  model <- linear_models[[model_name]]
  lag_num <- as.numeric(substr(model_name, 4, 4))
  test_data <- as.matrix(lag_test_data[[lag_num]][, 1:(ncol(lag_test_data[[lag_num]])-1)])
  
  # Get predictions
  predictions <- compute(model, test_data)$net.result
  predictions <- as.vector(predictions)
  
  # Evaluate and store results
  linear_evaluations[[model_name]] <- evaluate(predictions, test_data)
}

print(linear_evaluations)


# Function to create performance metric plots
plot_performance <- function(data, metric_name, plot_title) {
  ggplot(data, aes(x = model, y = !!sym(metric_name), fill = output_type)) +
    geom_bar(stat = "identity", position = "dodge", fill = "cyan3") +
    labs(title = plot_title, x = "Model", y = metric_name) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Create data frames and plots for each metric
rmse_data <- data.frame(
  model = names(linear_evaluations),
  rmse = sapply(linear_evaluations, function(x) x$rmse),
  output_type = "Linear"
)
rmse_plot <- plot_performance(rmse_data, "rmse", "RMSE Performance of Linear MLP Models")

mae_data <- data.frame(
  model = names(linear_evaluations),
  mae = sapply(linear_evaluations, function(x) x$mae),
  output_type = "Linear"
)
mae_plot <- plot_performance(mae_data, "mae", "MAE Performance of Linear MLP Models")

mape_data <- data.frame(
  model = names(linear_evaluations),
  mape = sapply(linear_evaluations, function(x) x$mape),
  output_type = "Linear"
)
mape_plot <- plot_performance(mape_data, "mape", "MAPE Performance of Linear MLP Models")

smape_data <- data.frame(
  model = names(linear_evaluations),
  smape = sapply(linear_evaluations, function(x) x$smape),
  output_type = "Linear"
)
smape_plot <- plot_performance(smape_data, "smape", "SMAPE Performance of Linear MLP Models")

# Display the plots
rmse_plot
mae_plot
mape_plot
smape_plot


# Extract top 5 models for each metric
top_rmse <- head(rmse_data[order(rmse_data$rmse), ], 5)
top_mae <- head(mae_data[order(mae_data$mae), ], 5)
top_mape <- head(mape_data[order(mape_data$mape), ], 5)
top_smape <- head(smape_data[order(smape_data$smape), ], 5)

# Combine top models into a single data frame
top_NN <- bind_rows(top_rmse, top_mae, top_mape, top_smape)

# Remove duplicates (keeping the first occurrence)
top_NN <- top_NN[!duplicated(top_NN$model), ]

# Print the resulting data frame
print(top_NN)


# Extract the best model
best_model_name <- "lag1_arch_1_act_1_output_1"
best_model <- NN[[best_model_name]]

# Extract the test data for lag 1
lag_num <- 1
test_data <- lag_test_data[[lag_num]]
actual_values <- test_data[, 1]

# Get predictions
test_data_matrix <- as.matrix(test_data[, -1])
colnames(test_data_matrix) <- NULL
predictions <- compute(best_model, test_data_matrix)$net.result

# Plot the neural network
plot(best_model)

# Plot predictions vs actual values
library(ggplot2)

data_to_plot <- data.frame(
  Actual = actual_values,
  Predictions = predictions
)

ggplot(data_to_plot, aes(x = seq_along(Actual), y = Actual)) +
  geom_line(color = "blue", linetype = "dashed") +
  geom_line(aes(y = Predictions), color = "red") +
  labs(title = "Predictions vs Actual Values",
       x = "Observation",
       y = "Exchange Rate")