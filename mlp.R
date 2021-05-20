#Import the packages
library(fpp)
library(MASS)
library(readxl)
library(neuralnet)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(fpp2)
library(e1071)
library(openxlsx)
library(MLmetrics)

exUSD <- read_excel("ExchangeUSD.xlsx")
value<-exUSD[,3]
print(value)

scalling <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Create the reverse of normalised function - renormalized
unscalling <- function(x, min, max) { 
  return( (max - min)*x + min )
}


column_year<-factor(exUSD$`YYYY/MM/DD`)
column_year<-as.numeric(column_year)
column_year

day_column<-factor(exUSD$`Wdy`)
day_column<-as.numeric(day_column)
day_column

exchange<-data.frame(column_year,day_column,exUSD$`USD/EUR`)
exchange
names(exchange)[1] <- "valueDate"
names(exchange)[2] <- "valueDay"
names(exchange)[3] <- "USD"
exchange_scaled <- as.data.frame(lapply(exchange, scalling))


# Training a nn model
set.seed(1234)
exchange_scaled_train <- exchange_scaled[1:400, ]
exchange_scaled_test <- exchange_scaled[401:497, ]
nn<- neuralnet(USD ~ valueDay + valueDate ,hidden=c(3,3) , data = exchange_scaled_train)

plot(nn)

# Evaluation of model performance
nn_model_results <- predict(nn, exchange_scaled_test[2:3])
nn_model_results

# extract the original (not normalized) training and testing desired Output
exchange_train_original <- value[1:400,"USD/EUR"]  # the first 400 rows
exchange_test_original <- value[401:497,"USD/EUR"] # the remainining rows


# find its maximum & minimum value
exUSD_min1 <- min(exchange_train_original)
exUSD_max1 <- max(exchange_train_original)

# display contents of it
head(exchange_train_original)


# renormalize the normalised NN's output
exUSD_renorm_pred1.1 <- unscalling(nn_model_results, exUSD_min1, exUSD_max1)
exUSD_renorm_pred1.1   # this is NN's output renormalized to original ranges


# Define RMSE function
RMSE(exp(exUSD_renorm_pred1.1),exchange_test_original$`USD/EUR`)

# MSE
MSE(exp(exUSD_renorm_pred1.1),exchange_test_original$`USD/EUR`)

# Define MAPE
MAPE(exp(exUSD_renorm_pred1.1),exchange_test_original$`USD/EUR`)

# examine the correlation between predicted and actual values
cor(exUSD_renorm_pred1.1,exchange_test_original$`USD/EUR`)

#Plot for exchange_model
par(mfrow=c(1,1))
plot(exchange_test_original$`USD/EUR`, exUSD_renorm_pred1.1 ,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN', pch=18,col='red', bty='n')

final_output <- cbind(exchange_test_original, exUSD_renorm_pred1.1)
final_output

#Plot for final output
plot(exchange_test_original$`USD/EUR` , ylab = "Predicted vs Expected", type="l", col="red" )
par(new=TRUE)
plot(exUSD_renorm_pred1.1, ylab = " ", yaxt="n", type="l", col="green" )
legend("topright",
       c("Predicted","Expected"),
       fill=c("red","green")
)
