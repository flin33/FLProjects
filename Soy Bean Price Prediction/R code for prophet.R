options(stringsAsFactors = F)
library(readxl)
library(tidyverse)
library(lubridate)
library(prophet)


### read market data 
Jul20 <- read_xlsx("datasets/ActiveSoybeanContractsforJuly2020.CSV.xlsx",skip = 2)
Mar20 <- read_xlsx("datasets/ActiveSoybeanContractsforMarch2020.CSV.xlsx",skip = 2)
May20 <- read_xlsx("datasets/ActiveSoybeanContractsforMay2020.CSV.xlsx",skip = 2)
Jul20$Date = as.Date(Jul20$Date)
Mar20$Date = as.Date(Mar20$Date)
May20$Date = as.Date(Mar20$Date)



### some early stage data may meaningless due to low volume
### run function to compare MSE with data start from different day
MSE <- function(n,df){
  df %>% select(Date, `Close`) %>% 
    rename(ds = Date, y = `Close`)-> df1
  df1 <- df1[-1:-n,]
  train <- round(nrow(df1)*0.8)
  trainset= df1[1:train,]
  testset= df1[-1:-train,]
  test <- nrow(testset)
  m = prophet(trainset)
  prediction = make_future_dataframe(m, test)
  forecast = predict(m, prediction)
  MSE <- mean((forecast$yhat[(train+1):nrow(df1)] - df1$y[(train+1):nrow(df1)])^2)
  return(MSE)
}

result <- vector("numeric") 
result_mar <- vector("numeric")
result_may <- vector("numeric")

for(i in 1:150){
  result[i] <- MSE(i,Jul20)
  print(i)
}

for(i in 1:150){
  result_may[i] <- MSE(i,May20)
  print(i)
}

for(i in 1:150){
  result_mar[i] <- MSE(i,Mar20)
  print(i)
}

which.min(result) ### this equal to 5, let delete 5 rows in our dataset. 
which.min(result_may) ### this equlas to 113
which.min(result_mar) ### this equals to 19 

### first perdiction with July 
Jul20 %>% select(Date, `Close`) %>% 
  rename(ds = Date, y = `Close`)-> Jul20_1
Jul20_1 <- Jul20_1[-1:-5,]
m = prophet(Jul20_1)
future_j20 = make_future_dataframe(m, 60)
# fit the model to future observations
forecast = predict(m, future_j20)
## delete predictions for weekends 
n <- c(seq(-744,-nrow(forecast), by = -7),seq(-745,-nrow(forecast), by = -7))
forecast <- forecast[n,]
dyplot.prophet(m, forecast)
j20_yhat = forecast %>% select(ds, yhat) %>% tail(60)
View(j20_yhat)


#### Prediction with May20 
May20 %>% select(Date, `Close`)%>% 
  rename(ds = Date, y = `Close`) -> May_20
May_20 <- May_20[-1:-113,]
s = prophet(May_20)
future_may20 = make_future_dataframe(s, 60)
f1 = predict(s, future_may20)
n_may <- c(seq(-384,-nrow(f1), by = -7),seq(-385,-nrow(f1), by = -7))
f1<- f1[n_may,]
dyplot.prophet(s, f1)
May20_yhat = f1 %>% select(ds, yhat) %>% tail(60)
View(May20_yhat)


##### Prediction with Mar20
Mar20 %>% select(Date, `Close`) %>% 
  rename(ds = Date, y = `Close`) -> Mar_20
Mar_20 <- Mar_20[-1:-19,]
d = prophet(Mar_20)
future_Mar20 = make_future_dataframe(d, 60)
f2 = predict(d, future_Mar20)
n_mar <- c(seq(-478,-nrow(f2), by = -7),seq(-479,-nrow(f2), by = -7))
f2<- f2[n_mar,]
dyplot.prophet(d, f2)
Mar20_yhat = f2 %>% select(ds, yhat) %>% tail(60)
View(Mar20_yhat)
