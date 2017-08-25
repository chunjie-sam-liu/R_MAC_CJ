# Multi-Layer Perceptrons
# Converluted Neural Network
# Recurrent Neural Network
# Skip-Gram
# VGG16, RESNET


# loading keras library
library(keras)
data <- dataset_mnist()

train_x <- data$train$x
train_y <- data$train$y

test_x <- data$test$x
test_y <- data$test$y
rm(data)


train_x <- array(train_x, dim = c(dim(train_x)[1], prod(dim(train_x)[-1]))) / 255
test_x <- array(test_x, dim = c(dim(test_x)[1], prod(dim(test_x)[-1]))) / 255

train_y<-to_categorical(train_y,10)
test_y<-to_categorical(test_y,10)

model <- keras_model_sequential()
#defining the model with 1 input layer[784 neurons], 1 hidden layer[784 neurons] with dropout rate 0.4 and 1 output layer[10 neurons]

model %>%
  layer_dense(units = 784, input_shape = 784) %>%
  layer_dropout(rate=0.4)%>%
  layer_activation(activation = 'relu') %>%
  layer_dense(units = 10) %>%
  layer_activation(activation = 'softmax')

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = 'adam',
  metrics = c('accuracy')
)

model %>% fit(train_x, train_y, epochs = 100, batch_size = 128)

loss_and_metrics <- model %>% evaluate(test_x, test_y, batch_size = 128)
