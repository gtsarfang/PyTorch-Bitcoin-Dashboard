library(rTorch)
library(tidyverse)
library(dplyr)
library(caTools)

dataframe <- read.csv('./Pytorch-Bitcoin-Dashboard-master/coinbase_data_clean.csv')
output <- select(dataframe,WP_Increase)
data <- dataframe %>%
  subset(select=-c(X,Timestamp,WP_Increase)) %>%
  add_column(output)

sample = sample.split(data,SplitRatio = 0.75)
train = subset(data,sample==TRUE)
test = subset(data,sample==FALSE)


X_train = train[1:length(data)-1]
X_test = test[1:length(data)-1]
y_train = train[length(data)]
y_test = test[length(data)]

X_train_scaled = scale(X_train)
X_test_scaled = scale(X_test,
                      center = attr(X_train_scaled,"scaled:center"),
                      scale = attr(X_train_scaled, "scaled:scale"))

X_train <- torch$FloatTensor(X_train_scaled)
X_test <- torch$FloatTensor(X_test_scaled)
y_train <- torch$LongTensor(np$array(y_train))
y_test <- torch$LongTensor(np$array(y_test))

########### Model ############
nn          <- torch$nn
transforms  <- torchvision$transforms
dsets       <- torchvision$datasets
builtins    <- import_builtins()

device = torch$device('cpu')

D_in <- 15L; H <- 100L; D_out <- 2L

model <- torch$nn$Sequential(
  torch$nn$Linear(D_in, H),              # first layer
  torch$nn$ReLU(),
  torch$nn$Linear(H, D_out))$to(device)  # output layer

loss_fn = torch$nn$CrossEntropyLoss()
optimizer = torch$optim$Adam(model$parameters(),lr=0.01)

learning_rate = 1e-4

for (t in 1:500) {
  # Forward pass: compute predicted y by passing x to the model. Module objects
  # override the __call__ operator so you can call them like functions. When
  # doing so you pass a Tensor of input data to the Module and it produces
  # a Tensor of output data.
  y_pred = model(X_train)
  
  # Compute and print loss. We pass Tensors containing the predicted and true
  # values of y, and the loss function returns a Tensor containing the loss.
  loss = loss_fn(y_pred, torch$squeeze(y_train))
  
  cat(t, "\t")
  cat(loss$item(), "\n")
  
  # Zero the gradients before running the backward pass.
  optimizer$zero_grad()
  
  # Backward pass: compute gradient of the loss with respect to all the learnable
  # parameters of the model. Internally, the parameters of each Module are stored
  # in Tensors with requires_grad=True, so this call will compute gradients for
  # all learnable parameters in the model.
  loss$backward()
  optimizer$step()
  
  # Update the weights using gradient descent. Each parameter is a Tensor, so
  # we can access its data and gradients like we did before.
  with(torch$no_grad(), {
    for (param in iterate(model$parameters())) {
      # in Python this code is much simpler. In R we have to do some conversions
      
      # param$data <- torch$sub(param$data,
      #                         torch$mul(param$grad$float(),
      #                           torch$scalar_tensor(learning_rate)))
      
      param$data <- param$data - param$grad * learning_rate
    }
  })
}



torch$save(model,'bitcoin_modelr.pt')