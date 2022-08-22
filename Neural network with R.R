install.packages('MASS')
library(MASS)
head(Boston)
str(Boston)
any(is.na(Boston))

# normalize the data first
maxs <- apply(Boston, 2, max) # 2 indicates all columns
mins <- apply(Boston, 2, min)

scale.data <- scale(Boston, center=mins, scale=maxs-mins)
data <- as.data.frame(scale.data)
head(data)

# split the data into train and test sets
library(caTools)
split <- sample.split(data, SplitRatio = 0.7)
train <- subset(data, split=T)
test <- subset(data, split=F)

# install the neural network package
install.packages('neuralnet')
library(neuralnet)

# build the neural network model
n <- names(data)
n
f <- as.formula(paste('medv~', paste(n[! n %in% 'medv' ], collapse = '+')))
f

neural <- neuralnet(f, train, hidden = c(5,3), linear.output = T)
plot(neural) # plot the neural network

# make predictions 
prediction <- compute(neural, test[1:13])
str(prediction)


unscaled.prediction <- prediction$net.result * (max(Boston$medv) - min(Boston$medv)) + min(Boston$medv)
unscaled.test <- (test$medv) * (max(Boston$medv) - min(Boston$medv)) + min(Boston$medv)

mse.neural <- sum((unscaled.test-unscaled.prediction)^2)/nrow(test)
mse.neural


unscaled.prediction
error.df <- data.frame(unscaled.test, unscaled.prediction)
error.df

library(ggplot2)
ggplot(error.df, aes(unscaled.test, unscaled.prediction)) +geom_point()+stat_smooth()
# this looks good





  