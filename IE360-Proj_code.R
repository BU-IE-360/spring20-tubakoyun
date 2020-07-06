require(jsonlite)
require(httr)
require(data.table)
library(stats)
library(forecast)
library(ggplot2)

get_token <- function(username, password, url_site){
  
  post_body = list(username=username,password=password)
  post_url_string = paste0(url_site,'/token/')
  result = POST(post_url_string, body = post_body)
  
  # error handling (wrong credentials)
  if(result$status_code==400){
    print('Check your credentials')
    return(0)
  }
  else if (result$status_code==201){
    output = content(result)
    token = output$key
  }
  
  return(token)
}

get_data <- function(start_date='2020-03-20', token, url_site){
  
  post_body = list(start_date=start_date,username=username,password=password)
  post_url_string = paste0(url_site,'/dataset/')
  
  header = add_headers(c(Authorization=paste('Token',token,sep=' ')))
  result = GET(post_url_string, header, body = post_body)
  output = content(result)
  data = data.table::rbindlist(output)
  data[,event_date:=as.Date(event_date)]
  data = data[order(product_content_id,event_date)]
  return(data)
}


send_submission <- function(predictions, token, url_site, submit_now=F){
  
  format_check=check_format(predictions)
  if(!format_check){
    return(FALSE)
  }
  
  post_string="list("
  for(i in 1:nrow(predictions)){
    post_string=sprintf("%s'%s'=%s",post_string,predictions$product_content_id[i],predictions$forecast[i])
    if(i<nrow(predictions)){
      post_string=sprintf("%s,",post_string)
    } else {
      post_string=sprintf("%s)",post_string)
    }
  }
  
  submission = eval(parse(text=post_string))
  json_body = jsonlite::toJSON(submission, auto_unbox = TRUE)
  submission=list(submission=json_body)
  
  print(submission)
  # {"31515569":2.4,"32939029":2.4,"4066298":2.4,"6676673":2.4,"7061886":2.4,"85004":2.4} 
  
  if(!submit_now){
    print("You did not submit.")
    return(FALSE)      
  }
  
  
  header = add_headers(c(Authorization=paste('Token',token,sep=' ')))
  post_url_string = paste0(url_site,'/submission/')
  result = POST(post_url_string, header, body=submission)
  
  if (result$status_code==201){
    print("Successfully submitted. Below you can see the details of your submission")
  } else {
    print("Could not submit. Please check the error message below, contact the assistant if needed.")
  }
  
  print(content(result))
  
}

check_format <- function(predictions){
  
  if(is.data.frame(predictions) | is.data.frame(predictions)){
    if(all(c('product_content_id','forecast') %in% names(predictions))){
      if(is.numeric(predictions$forecast)){
        print("Format OK")
        return(TRUE)
      } else {
        print("forecast information is not numeric")
        return(FALSE)                
      }
    } else {
      print("Wrong column names. Please provide 'product_content_id' and 'forecast' columns")
      return(FALSE)
    }
    
  } else {
    print("Wrong format. Please provide data.frame or data.table object")
    return(FALSE)
  }
  
}

# this part is main code
subm_url = 'http://167.172.183.67'

u_name = "Group18"
p_word = "ZbaJBxomdx2OKmj9"
submit_now = FALSE

username = u_name
password = p_word

token = get_token(username=u_name, password=p_word, url=subm_url)
data = get_data(token=token,url=subm_url)


## Product1

data1 <- data[product_content_id == "31515569"]
data1 <- data1[146:length(data1$price)]

ggplot(data1,aes(event_date,sold_count)) + geom_line() + xlab("Date") + ylab("Number of Sales") + geom_line(aes(event_date,mean(data1$sold_count)),color = "blue")

acf(data1$sold_count,100)

data1_alt <- data1
data1_alt$sold_count[40:80] <- mean(data1$sold_count[30:90])

model1.1 <- lm(sold_count~visit_count+favored_count+category_sold+basket_count+price+category_brand_sold+category_visits,data1)
summary(model1.1)
AIC(model1.1)

model1.2 <- lm(sold_count~visit_count+favored_count+category_sold+basket_count+category_brand_sold+category_visits,data1)
summary(model1.2)
AIC(model1.2)

model1.3 <- lm(sold_count~visit_count+favored_count+basket_count+category_brand_sold+category_visits,data1)
summary(model1.3)
AIC(model1.3)

model1.4 <- lm(sold_count~visit_count+favored_count+basket_count+category_visits,data1)
summary(model1.4)
AIC(model1.4)


model_alt1.1 <-  lm(sold_count~visit_count+favored_count+category_sold+basket_count+price+category_brand_sold+category_visits,data1_alt)
summary(model_alt1.1)
AIC(model_alt1.1)

model_alt1.2 <-  lm(sold_count~visit_count+favored_count+category_sold+basket_count+category_brand_sold+category_visits,data1_alt)
summary(model_alt1.2)
AIC(model_alt1.2)



```{r}
MAE_lm1 <- c()
MAE_lm2 <- c()
MAE_arima1 <- c()
MAE_arima2 <- c()
MAE_arima3 <- c()
MAE_arima4 <- c()


for(i in 1:50){
  data1_train <- data1[1:(.N-i)]
  data1_test <-  data1[(.N-i+1):.N]
  
  data1_alt_train <-  data1_alt[1:(.N-i)]
  data1_alt_test <- data1_alt[(.N-i+1):.N]
  
  model_lm1 <- lm(sold_count~visit_count+favored_count+basket_count+category_visits,data1_train)
  model_lm2 <- lm(sold_count~visit_count+favored_count+category_sold+basket_count+category_brand_sold+category_visits,data1_alt_train)
  
  model_arima1 <-  auto.arima(data1_train$sold_count)
  model_arima2 <- arima(data1_train$sold_count, c(3,1,2))
  
  model_arima3 <- auto.arima(data1_alt_train$sold_count)
  model_arima4 <- arima(data1_alt_train$sold_count, c(3,1,2))
  
  
  pred_lm1 <- predict(model_lm1,newdata=data1_test)
  pred_lm2 <- predict(model_lm2,newdata=data1_alt_test)
  
  for_arima1 <- forecast(model_arima1,i)
  for_arima2 <- forecast(model_arima2,i)
  for_arima3 <- forecast(model_arima3,i)
  for_arima4 <- forecast(model_arima4,i)
  
  
  
  MAE_lm1 <- c(MAE_lm1,mean(abs(pred_lm1 - data1_test$sold_count)))
  MAE_lm2 <- c(MAE_lm2,mean(abs(pred_lm2 - data1_alt_test$sold_count)))
  MAE_arima1 <- c(MAE_arima1,mean(abs(for_arima1$mean - data1_test$sold_count)))
  MAE_arima2 <- c(MAE_arima2,mean(abs(for_arima2$mean - data1_test$sold_count)))
  MAE_arima3 <- c(MAE_arima3,mean(abs(for_arima3$mean - data1_alt_test$sold_count)))
  MAE_arima4 <- c(MAE_arima4,mean(abs(for_arima4$mean - data1_alt_test$sold_count)))
  
}

mean(MAE_lm1)
mean(MAE_lm2)
mean(MAE_arima1)
mean(MAE_arima2)
mean(MAE_arima3)
mean(MAE_arima4)

# Product 2

data2 <- data[product_content_id == "32939029"]
data2 <- data2[206:length(data2$price)]
for(i in 1:length(data2$price)){
  if(data2$price[i]==-1){
    data2$price[i]=mean(data2$price[1:length(data2$price)])
  }
}

ggplot(data2,aes(event_date,sold_count)) + geom_line() + xlab("Date") + ylab("Number of Sales") + geom_line(aes(event_date,data2$price),color = "blue") + geom_line(aes(event_date,data2$visit_count/100),color = "red")



ar.model2.1 <- auto.arima(data2$sold_count)
summary(ar.model2.1)


ar.model2.2 <- arima(data2$sold_count, c(3,1,2))
summary(ar.model2.2)

#Lineer regression model
#starting with using every possible parameter in the lineer model.

model2.1 <- lm(sold_count~visit_count+favored_count+category_sold+basket_count+price+category_brand_sold+category_visits,data2)
summary(model2.1)
AIC(model2.1)

#after the first model, insignificant parameters will be eliminated one by one.
# in the first step, catogory_brand cound will be delated in the model

model2.2 <-lm(sold_count~visit_count+favored_count+category_sold+basket_count+price+category_visits,data2)
summary(model2.2)
AIC(model2.2)

# we will add square of some so significant parameters.
model2.3 <-lm(sold_count~visit_count+I(visit_count^2)+favored_count+category_sold+basket_count+price+category_visits,data2)
summary(model2.3)
AIC(model2.3)

model2.4 <- lm(sold_count~visit_count+I(visit_count^2)+favored_count+category_sold+basket_count+I(basket_count^2)+price+category_visits,data2)
summary(model2.4)
AIC(model2.4)
# square of the basket count is not so significant so deleted and add 
model2.5 <- lm(sold_count~visit_count+I(visit_count^2)+favored_count+category_sold+basket_count+price+I(price^2)+category_visits,data2)
summary(model2.5)
AIC(model2.5)
#the akaike value decreased more 

MAE_model2.5 <- c()
MAE_ar.model1 <- c()
MAE_ar.model2 <- c()

for(i in 1:50){
  data2_train <- data2[2:(.N-i)]
  data2_test <-  data2[(.N-i+1):.N]
  
  
  model2.5 <- lm(sold_count~visit_count+I(visit_count^2)+favored_count+category_sold+basket_count+price+I(price^2)+category_visits,data2)
  
  ar.model2.1 <- auto.arima(data2$sold_count)
  ar.model2.2 <- arima(data2$sold_count, c(3,1,2))
  
  prediction_model2.5 <- predict(model2.5,newdata=data2_test)
  
  for_arima1 <- forecast(ar.model2.1,i)
  for_arima2 <- forecast(ar.model2.2,i)
  
  MAE_model2.5 <- c(MAE_model2.5,mean(abs(prediction_model2.5 - data2_test$sold_count)))
  
  MAE_ar.model1 <- c(MAE_ar.model1,mean(abs(for_arima1$mean - data2_test$sold_count)))
  MAE_ar.model2 <- c(MAE_ar.model2,mean(abs(for_arima2$mean - data2_test$sold_count)))
}



mean(MAE_model2.5)     

mean(MAE_ar.model1)

mean(MAE_ar.model2)



#Product3


data3 <- data[product_content_id == "3904356"]

ggplot(data3,aes(event_date,sold_count)) + geom_line() + xlab("Date") + ylab("Number of Sales") + geom_line(aes(event_date,mean(data3$sold_count)),color = "blue")

data3.2 <- data3
data3.2[,lag1:=shift(sold_count,1)]
data3.2[,lag2:=shift(sold_count,2)]
data3.2[,'is_winter':= { 
  if (month(as.Date(event_date)) == 11) 1
  else if (month(as.Date(event_date)) == 12 ) 1
  else if ( month(as.Date(event_date)) == 1 ) 1
  else if ( month(as.Date(event_date)) == 2 ) 1
  else    0                  }
  ,by = event_date]

data3.2[,'is_summer':= { 
  if (month(as.Date(event_date)) == 5) 1
  else if (month(as.Date(event_date)) == 6 ) 1
  else if ( month(as.Date(event_date)) == 7 ) 1
  else if ( month(as.Date(event_date)) == 8 ) 1
  else    0                  }
  ,by = event_date]

model_lm3.1 <- lm(sold_count~price+visit_count+favored_count+basket_count+category_sold+category_brand_sold+category_visits+ty_visits+lag2+lag7+is_winter+is_summer ,data3.2)
summary(model_lm3.1)
AIC(model_lm3.1)

#removing ty_visits with low significance
model_lm3.2 <- lm(sold_count~price+visit_count+favored_count+basket_count+category_sold+category_brand_sold+category_visits+lag2+lag7+is_winter+is_summer ,data3.2)
summary(model_lm3.2)
AIC(model_lm3.2)

#removing category_sold with low significance
model_lm3.3 <- lm(sold_count~price+visit_count+favored_count+basket_count+category_brand_sold+category_visits+lag2+lag7+is_winter+is_summer ,data3.2)
summary(model_lm3.3)
AIC(model_lm3.3)


#removing basket_count with low significance
model_lm3.4 <- lm(sold_count~price+visit_count+favored_count+category_brand_sold+category_visits+lag2+lag7+is_winter+is_summer ,data3.2)
summary(model_lm3.4)
AIC(model_lm3.4)



#adding square of visit count
data3.2[,visit_sq :=visit_count^2]
model_lm3.5 <- lm(sold_count~price+visit_count+favored_count+category_brand_sold+visit_sq+category_visits+lag2+lag7+is_winter+is_summer ,data3.2)
summary(model_lm3.5)
AIC(model_lm3.5)




#Time series cross-validation

MAE_vec3.1 <- c()
MAE_vec3.2 <- c()
MAE_vec3.3 <- c()

for(i in 1:50){
  data3_train <- data3.2[1:(.N-i)] 
  data3_test <-  data3.2[(.N-i+1):.N]
  
  model3.1 <- auto.arima((data3_train$sold_count))
  model3.2 <- arima(data3_train$sold_count,c(0,1,1))
  model3.3 <- lm(sold_count~price+visit_count+favored_count+category_brand_sold+visit_sq+category_visits+lag2+lag7+is_winter+is_summer ,data3_train)
  
  forecast1 <- forecast(model3.1,i)
  forecast2 <- forecast(model3.2,i)
  forecast3 <- predict(model3.3,data3_test)
  
  MAE3.1 <- mean(abs(forecast1$mean-data3_test$sold_count))
  MAE3.2 <- mean(abs(forecast2$mean-data3_test$sold_count))
  MAE3.3 <- mean(abs(forecast3-data3_test$sold_count))
  
  MAE_vec3.1 <- c(MAE_vec3.1,MAE3.1)
  MAE_vec3.2 <- c(MAE_vec3.2,MAE3.2)
  MAE_vec3.3 <- c(MAE_vec3.3,MAE3.3)
}

mean(MAE_vec3.1)
mean(MAE_vec3.2)
mean(MAE_vec3.3)



# Product 4


# Product 5

data5 <- data[product_content_id == "5926527"]
for(i in 1:length(data5$price)) {
  if(data5$price[i]==-1) data5$price[i]=mean(data5$price[i-8:i+8],na.rm = T)
}


ggplot(data5,aes(event_date,sold_count)) + geom_line() + xlab("Date") + ylab("Number of Sales") + geom_line(aes(event_date,mean(data5$sold_count)),color = "blue") 
data5_2020 <-  data5[event_date> "2019-12-31"]
ggplot(data5_2020,aes(event_date,sold_count)) + geom_line() + xlab("Date") + ylab("Number of Sales") + geom_line(aes(event_date,mean(data5_2020$sold_count)),color = "blue") 

#linear regression model
model5.1 <-  lm(sold_count~ is_summer  + price + visit_count + favored_count + basket_count + category_sold + category_brand_sold + category_visits, data = data5 )
summary(model5.1)
AIC(model5.1)

model5.2 <-  lm(sold_count~ is_summer  + price + visit_count + favored_count  + category_sold + category_brand_sold + category_visits, data = data5 )
summary(model5.2)
AIC(model5.2)

plot(model5.2)
data5.2 <- data5
data5.2[,"visitsq" := visit_count^2]
model5.3 <-  lm(sold_count~ is_summer+ visitsq  + price + visit_count + favored_count  + category_sold + category_brand_sold + category_visits, data = data5.2 )
summary(model5.3)
AIC(model5.3)


model5.4 <-  lm(sold_count~ is_summer+ visitsq  + price + visit_count + favored_count  + category_sold  + category_visits, data = data5.2 )
summary(model5.4)
AIC(model5.4)

model5.5 <-  lm(sold_count~ is_summer+ visitsq  + price + visit_count + favored_count  + category_visits, data = data5.2 )
summary(model5.5)
AIC(model5.5)

model5.6 <-  lm(sold_count~ is_summer+ visitsq  + price + visit_count + favored_count  + category_visits + visit_count:category_visits, data = data5.2 )
summary(model5.6)
AIC(model5.6)

model5.7 <-  lm(sold_count~ is_summer+ visitsq  + price + visit_count + favored_count   + visit_count:category_visits, data = data5.2 )
summary(model5.7)
AIC(model5.7)

#arima model
model5_Arima1 <- auto.arima(data5$sold_count)
model5_Arima2 <- arima(data5$sold_count,c(3,0,2))


MAE5.1 <- c()
MAE5.2 <- c()
MAE5.3 <- c()

for (i in 1:50){
  data5_train <-  data5[1:(.N-i)]
  data5_test <-  data5[(.N-i+1):.N]
  
  data5_2020_train <-  data5_2020[1:(.N-i)]
  data5_2020_test <-  data5_2020[(.N-i+1):.N]
  
  
  model5_lm <-  lm(sold_count~ is_summer+ I(visit_count^2)   + price + visit_count + favored_count   + visit_count:category_visits, data = data5_2020_train )
  
  model5_arima <- arima(data5_train$sold_count,c(3,0,2))
  model5_auto <- auto.arima(data5_train$sold_count)
  
  pred_lm = predict(model5_lm,data5_2020_test)
  
  
  fore_arima <- forecast(model5_arima,i)
  fore_auto  <- forecast(model5_auto,i)
  
  mm <-  abs(Y_lm - data8_test$sold_count )
  mmm <- abs(Y_arima$mean - data8_test$sold_count )
  mmmm <- abs(Y_auto$mean - data8_test$sold_count )
  
  
  MAE5.1 <-  c(MAE1,mm)
  MAE5.2 <-  c(MAE2,mmm)
  MAE5.3 <-  c(MAE3,mmmm)
  
}

mean ( MAE5.1)
mean ( MAE5.2)
mean ( MAE5.3)

# Product 6


data6 <- data[product_content_id == "6676673"]
data6 <- data6[52:length(data6$price)]
for(i in 1:length(data6$price)) {
  if(data6$price[i]==-1) data6$price[i]=data6$price[i+8]
}

ggplot(data6,aes(event_date,sold_count)) + geom_line() + xlab("Date") + ylab("Number of Sales") + geom_line(aes(event_date,mean(data6$sold_count)),color = "blue") +geom_line(aes(event_date,data6$price*10),color = "red")
acf(data6$sold_count,lag.max = 80)
data6.2 <- data6
Mean_correction <-  mean(data6.2$sold_count[100:200])
data6.2$sold_count[140:170] <- Mean_correction

data6[,previous_price := shift(price,1)]
data6.2[,previous_price := shift(price,1)]

data6[,lag2_sold := shift(sold_count,2)]
data6[,lag7_sold := shift(sold_count,7)]



data6[,'is_school_starts':= { 
  if (month(as.Date(event_date)) == 9) 1
  else if (month(as.Date(event_date)) == 10 ) 1
  else    0                  }
  ,by = event_date]

data6[,previous_price := { 
  ifelse ( is.na(previous_price) , price , previous_price )
}
]


data6[,'is_sales' := { 
  ifelse ((price-previous_price)/ price  >= 0.029 , 1 , 0 )
}
,by = (price)
]



data6[,'is_Covid':= { 
  if (as.Date(event_date)>as.Date("2020-03-10") ) 1
  else    0                  }
  ,by = event_date]


data6.2[,previous_price := { 
  ifelse ( is.na(previous_price) , price , previous_price )
}
]

data6.2[,'is_sales' := { 
  ifelse ((price-previous_price)/ price  >= 0.029 , 1 , 0 )
}
,by = (price)
]




data6.2[,'is_Covid':= { 
  if (as.Date(event_date)>as.Date("2020-03-10") ) 1
  else    0                  }
  ,by = event_date]
data6.2[,'is_school_starts':= { 
  if (month(as.Date(event_date)) == 9) 1
  else if (month(as.Date(event_date)) == 10 ) 1
  else    0                  }
  ,by = event_date]


data6.2[,lag2_sold := shift(sold_count,2)]
data6.2[,lag7_sold := shift(sold_count,7)]

model6.1.1 <- lm(sold_count~price+sold_count+visit_count+favored_count+basket_count+category_sold+category_brand_sold+category_visits+
                   ty_visits+previous_price+is_school_starts+is_sales+is_Covid+lag2_sold+lag7_sold,data6)
summary(model6.1.1)
AIC(model6.1.1)

#remove basket count
model6.1.2 <- lm(sold_count~price+sold_count+visit_count+favored_count+category_sold+category_brand_sold+category_visits+
                   ty_visits+previous_price+is_school_starts+is_sales+is_Covid+lag2_sold+lag7_sold,data6)
summary(model6.1.2)
AIC(model6.1.2)

#remove ty_visits
model6.1.3 <- lm(sold_count~price+sold_count+visit_count+favored_count+category_sold+category_brand_sold+category_visits+
                   previous_price+is_school_starts+is_sales+is_Covid+lag2_sold+lag7_sold,data6)
summary(model6.1.3)
AIC(model6.1.3)

#remove category_sold
model6.1.4 <- lm(sold_count~price+sold_count+visit_count+favored_count+category_brand_sold+category_visits+
                   previous_price+is_school_starts+is_sales+is_Covid+lag2_sold+lag7_sold,data6)
summary(model6.1.4)
AIC(model6.1.4)


#remove previous_price
model6.1.5 <- lm(sold_count~price+sold_count+visit_count+favored_count+category_brand_sold+category_visits+
                   is_school_starts+is_sales+is_Covid+lag2_sold+lag7_sold,data6)
summary(model6.1.5)
AIC(model6.1.5)

data6[,sq_visit := visit_count^2]

model6.1.6 <- lm(sold_count~price+sold_count+visit_count+favored_count+category_brand_sold+category_visits+
                   is_school_starts+is_sales+is_Covid+lag2_sold+lag7_sold+sq_visit,data6)
summary(model6.1.6)
AIC(model6.1.6)

data6[,sq_favor := favored_count^2]

model6.1.7 <- lm(sold_count~price+sold_count+visit_count+favored_count+category_brand_sold+category_visits+
                   is_school_starts+is_sales+is_Covid+lag2_sold+lag7_sold+sq_visit+sq_favor,data6)
summary(model6.1.7)
AIC(model6.1.7)

#removing is_covid
model6.1.8 <- lm(sold_count~price+sold_count+visit_count+favored_count+category_brand_sold+category_visits+
                   is_school_starts+is_sales+lag2_sold+lag7_sold+sq_visit+sq_favor,data6)
summary(model6.1.8)
AIC(model6.1.8)

#removing lag2_sold
model6.1.9 <- lm(sold_count~price+sold_count+visit_count+favored_count+category_brand_sold+category_visits+
                   is_school_starts+is_sales+lag7_sold+sq_visit+sq_favor,data6)
summary(model6.1.9)
AIC(model6.1.9)

#removing visit_count
model6.1.10 <- lm(sold_count~price+sold_count+favored_count+category_brand_sold+category_visits+
                    is_school_starts+is_sales+lag7_sold+sq_visit+sq_favor,data6)
summary(model6.1.10)
AIC(model6.1.10)

data6[,sq_cat_br_sold := category_brand_sold^2]

model6.1.11 <- lm(sold_count~price+sold_count+category_brand_sold+favored_count+category_visits+
                    is_school_starts+is_sales+sq_cat_br_sold+lag7_sold+sq_visit+sq_favor,data6)
summary(model6.1.11)
AIC(model6.1.11)

#removing category_brand_sold

model6.1.12 <- lm(sold_count~price+sold_count+favored_count+category_visits+
                    is_school_starts+is_sales+sq_cat_br_sold+lag7_sold+sq_visit+sq_favor,data6)
summary(model6.1.12)
AIC(model6.1.12)


model6.2.1 <- lm(sold_count~price+sold_count+visit_count+favored_count+basket_count+category_sold+category_brand_sold+category_visits+
                   ty_visits+previous_price+is_school_starts+is_sales+is_Covid+lag2_sold+lag7_sold,data6.2)
summary(model6.2.1)
AIC(model6.2.1)

#removing price
model6.2.2 <- lm(sold_count~sold_count+visit_count+favored_count+basket_count+category_sold+category_brand_sold+category_visits+
                   ty_visits+previous_price+is_school_starts+is_sales+is_Covid+lag2_sold+lag7_sold,data6.2)
summary(model6.2.2)
AIC(model6.2.2)

#removing price
model6.2.3 <- lm(sold_count~sold_count+visit_count+favored_count+basket_count+category_brand_sold+category_visits+
                   ty_visits+previous_price+is_school_starts+is_sales+is_Covid+lag2_sold+lag7_sold,data6.2)
summary(model6.2.3)
AIC(model6.2.3)

#removing visit_count
model6.2.4 <- lm(sold_count~sold_count+favored_count+basket_count+category_brand_sold+category_visits+
                   ty_visits+previous_price+is_school_starts+is_sales+is_Covid+lag2_sold+lag7_sold,data6.2)
summary(model6.2.4)
AIC(model6.2.4)

#removing is_school_starts
model6.2.4 <- lm(sold_count~sold_count+favored_count+basket_count+category_brand_sold+category_visits+
                   ty_visits+previous_price+is_sales+is_Covid+lag2_sold+lag7_sold,data6.2)
summary(model6.2.4)
AIC(model6.2.4)

data6.2[,lag1_sold:=shift(sold_count,1)]
data6.2[,lag3_sold:=shift(sold_count,3)]

model6.2.5 <- lm(sold_count~sold_count+favored_count+basket_count+category_brand_sold+category_visits+
                   ty_visits+previous_price+is_sales+is_Covid+lag1_sold+lag2_sold+lag3_sold+lag7_sold,data6.2)
summary(model6.2.5)
AIC(model6.2.5)

#remove lag2_sold
model6.2.6 <- lm(sold_count~sold_count+favored_count+basket_count+category_brand_sold+category_visits+
                   ty_visits+previous_price+is_sales+is_Covid+lag1_sold+lag3_sold+lag7_sold,data6.2)
summary(model6.2.6)
AIC(model6.2.6)

data6.2[,sq_favor := favored_count^2]

model6.2.7 <- lm(sold_count~sold_count+favored_count+sq_favor+basket_count+category_brand_sold+category_visits+
                   ty_visits+previous_price+is_sales+is_Covid+lag1_sold+lag3_sold+lag7_sold,data6.2)
summary(model6.2.7)
AIC(model6.2.7)

data6.2[,sq_basket_count := basket_count^2]

model6.2.8 <- lm(sold_count~sold_count+favored_count+sq_favor+basket_count+sq_basket_count+category_brand_sold+category_visits+
                   ty_visits+previous_price+is_sales+is_Covid+lag1_sold+lag3_sold+lag7_sold,data6.2)
summary(model6.2.8)
AIC(model6.2.8)

#removing basket_count
model6.2.9 <- lm(sold_count~sold_count+favored_count+sq_favor+sq_basket_count+category_brand_sold+category_visits+
                   ty_visits+previous_price+is_sales+is_Covid+lag1_sold+lag3_sold+lag7_sold,data6.2)
summary(model6.2.9)
AIC(model6.2.9)

#removing lag3_sold
model6.2.10 <- lm(sold_count~sold_count+favored_count+sq_favor+sq_basket_count+category_brand_sold+category_visits+
                    ty_visits+previous_price+is_sales+is_Covid+lag1_sold+lag7_sold,data6.2)
summary(model6.2.10)
AIC(model6.2.10)




model6.3.1 <- auto.arima(data6$sold_count)
checkresiduals(model6.3.1)

model6.3.2 <- auto.arima(data6.2$sold_count)
checkresiduals(model6.3.2)



model6.4.1 <- arima(data6$sold_count,c(3,1,3))
checkresiduals(model6.4.1)

model6.4.2 <- arima(data6.2$sold_count,c(3,1,3))
checkresiduals(model6.4.2)


#Time series cross-validation

MAE_vec6.1 <- c()
MAE_vec6.2 <- c()
MAE_vec6.3 <- c()

MAE_vec6.4 <- c()
MAE_vec6.5 <- c()
MAE_vec6.6 <- c()



for(i in 1:50){
  
  data6.1_train <- data6[1:(.N-i)] 
  data6.1_test <-  data6[(.N-i+1):.N]
  
  data6.2_train <- data6.2[1:(.N-i)] 
  data6.2_test <-  data6.2[(.N-i+1):.N]
  
  
  model6.1.1 <- lm(sold_count~price+sold_count+favored_count+category_visits+
                     is_school_starts+is_sales+sq_cat_br_sold+lag7_sold+sq_visit+sq_favor,data6.1_train)
  model6.1.2 <- auto.arima(data6.1_train$sold_count)
  model6.1.3 <- arima(data6.1_train$sold_count,c(3,1,3))
  
  forecast1 <- predict(model6.1.1,data6.1_test)
  forecast2 <- forecast(model6.1.2,i)
  forecast3 <- forecast(model6.1.3,i)
  
  
  MAE6.1 <- mean(abs(forecast1-data6.1_test$sold_count))
  MAE6.2 <- mean(abs(forecast2$mean-data6.1_test$sold_count))
  MAE6.3 <- mean(abs(forecast3$mean-data6.1_test$sold_count))
  
  MAE_vec6.1 <- c(MAE_vec6.1,MAE6.1)
  MAE_vec6.2 <- c(MAE_vec6.2,MAE6.2)
  MAE_vec6.3 <- c(MAE_vec6.3,MAE6.3)
  
  model6.2.1 <- lm(sold_count~sold_count+favored_count+sq_favor+sq_basket_count+category_brand_sold+category_visits+
                     ty_visits+previous_price+is_sales+is_Covid+lag1_sold+lag7_sold,data6.2_train)
  model6.2.2 <- auto.arima(data6.2_train$sold_count)
  model6.2.3 <- arima(data6.2_train$sold_count,c(3,1,3))
  
  forecast4 <- predict(model6.2.1,data6.2_test)
  forecast5 <- forecast(model6.2.2,i)
  forecast6 <- forecast(model6.2.3,i)
  
  
  MAE6.4 <- mean(abs(forecast4-data6.2_test$sold_count))
  MAE6.5 <- mean(abs(forecast5$mean-data6.2_test$sold_count))
  MAE6.6 <- mean(abs(forecast6$mean-data6.2_test$sold_count))
  
  MAE_vec6.4 <- c(MAE_vec6.4,MAE6.4)
  MAE_vec6.5 <- c(MAE_vec6.5,MAE6.5)
  MAE_vec6.6 <- c(MAE_vec6.6,MAE6.6)
  
  
}

mean(MAE_vec6.1)
mean(MAE_vec6.2)
mean(MAE_vec6.3)
mean(MAE_vec6.4)
mean(MAE_vec6.5)
mean(MAE_vec6.6)

# Product 7


data7 <- data[product_content_id == "7061886"]
data7 <- data7[87:length(data7$price)]

for(i in 1:length(data7$price)){
  
  if(data7$price[i]==-1){
    data7$price[i]=data7$price[i+8]
    
  }}

ggplot(data7,aes(event_date,sold_count)) + geom_line() + xlab("Date") + ylab("Number of Sales") + geom_line(aes(event_date,mean(data7$sold_count)),color = "blue") +geom_line(aes(event_date,data7$price/3),color = "red")

model7.1<-lm(sold_count~price+visit_count+favored_count+basket_count+category_sold+category_brand_sold+category_visits+ty_visits,data7)
summary(model7.1)

##eliminate category sold 


model2<-lm(sold_count~price+visit_count+favored_count+basket_count+category_brand_sold+category_visits+ty_visits,data7)
summary(model2)
AIC(model2)

##eliminate category brand sold 

model2<-lm(sold_count~price+visit_count+favored_count+basket_count+category_visits+ty_visits,data7)
summary(model2)
AIC(model2)
## add ty_visits square

model2<-lm(sold_count~price+visit_count+favored_count+I(ty_visits^2)+basket_count+category_visits+ty_visits,data7)
summary(model2)
AIC(model2)
## add basket_count sq

model2<-lm(sold_count~price+visit_count+favored_count+I(ty_visits^2)+basket_count+I(basket_count^2)+category_visits+ty_visits,data7)
summary(model2)
AIC(model2)
##eliminate ty_visits 

model2<-lm(sold_count~price+visit_count+favored_count+basket_count+I(basket_count^2)+category_visits+ty_visits,data7)
summary(model2)
AIC(model2)
##eliminate favored_count 

model2<-lm(sold_count~price+visit_count+basket_count+I(basket_count^2)+category_visits+ty_visits,data7)
summary(model2)
AIC(model2)
##eliminate ty visits insignificant

model2<-lm(sold_count~price+visit_count+basket_count+I(basket_count^2)+category_visits,data7)
summary(model2)
AIC(model2)
##add price square 

model2<-lm(sold_count~price+visit_count+basket_count+I(price^2)+I(basket_count^2)+category_visits,data7)
summary(model2)
AIC(model2)
##eliminate price square insignificant, add category visit square 

model2<-lm(sold_count~price+visit_count+basket_count+I(category_visits^2)+I(basket_count^2)+category_visits,data7)
summary(model2)
AIC(model2)
## eliminate cat vis sq, add visit count sq

model2<-lm(sold_count~price+visit_count+basket_count+I(visit_count^2)+I(basket_count^2)+category_visits,data7)
summary(model2)
AIC(model2)
## add cat visit ile visit count intercept 

model2<-lm(sold_count~price+visit_count+basket_count+I(visit_count^2)+I(basket_count^2)+category_visits+visit_count*category_visits,data7)
summary(model2)
AIC(model2)
##eliminate cat vis ile vis count intercept, add visit count basket count intercept 

model2<-lm(sold_count~price+visit_count+basket_count+I(visit_count^2)+I(basket_count^2)+category_visits+visit_count*basket_count,data7)
summary(model2)
AIC(model2)
## eliminate basket count sq 

model2<-lm(sold_count~price+visit_count+basket_count+I(visit_count^2)+category_visits+visit_count*basket_count,data7)
summary(model2)
AIC(model2)
##add basket count cat vis intercept 

model2<-lm(sold_count~price+visit_count+basket_count+I(visit_count^2)+category_visits+visit_count*basket_count+visit_count*category_visits,data7)
summary(model2)
AIC(model2)
## eliminate basket count cat vis intercept insignificant

model2<-lm(sold_count~price+visit_count+basket_count+I(visit_count^2)+category_visits+visit_count*basket_count,data7)
summary(model2)
AIC(model2)






# Product 8


data8 <- data[product_content_id == "85004"]
for(i in 1:length(data8$price)){
  if(data8$price[i]==-1){
    data8$price[i]=mean(data8$price[1:length(data8$price)])
  }
}

ggplot(data8,aes(event_date,sold_count)) + geom_line() + xlab("Date") + ylab("Number of Sales") + geom_line(aes(event_date,mean(data8$sold_count)),color = "blue") + geom_line(aes(event_date,(data8$price)),color = "red") 

acf(data8$sold_count,lag.max = 100)

model8.1 <- lm(sold_count~visit_count+favored_count+category_sold+basket_count+price+category_brand_sold+category_visits,data8)
summary(model8.1)

model8.2 <- lm(sold_count~visit_count+favored_count+category_sold+basket_count+price+category_brand_sold,data8)
summary(model8.2)

model8.3 <- lm(sold_count~visit_count+category_sold+basket_count+price+category_brand_sold,data8)
summary(model8.3)



model8.4 <- lm(sold_count~visit_count+I(visit_count^2)+basket_count+price,data8)
summary(model8.4)

data8[,'is_Covid':= { 
  if (as.Date(event_date)>as.Date("2020-03-10") ) 1
  else    0                  }
  ,by = event_date]

model8.5 <- lm(sold_count~visit_count+I(visit_count^2)+basket_count+price,data8)
summary(model8.5)
auto.arima(data8$sold_count)
model8 <- arima(data8$sold_count,c(2,1,3))

MAE1 <- c()
MAE2 <- c()
MAE3 <- c()

for (i in 1:50){
  data8_train <-  data8[1:(.N-i)]
  data8_test <-  data8[(.N-i+1):.N]
  
  model8_lm <-  lm(sold_count~visit_count+is_Covid+I(visit_count^2)+basket_count+price,data8_train)
  
  model8_arima <- arima(data8_train$sold_count,c(2,1,3))
  model8_auto <- auto.arima(data8_train$sold_count)
  
  Y_lm = predict(model8_lm,data8_test)
  
  Y_arima <-forecast(model8_arima,i)
  Y_auto <-  forecast(model8_auto,i)
  
  mm <-  abs(Y_lm - data8_test$sold_count )
  mmm <- abs(Y_arima$mean - data8_test$sold_count )
  mmmm <- abs(Y_auto$mean - data8_test$sold_count )
  
  
  MAE1 <-  c(MAE1,mm)
  MAE2 <-  c(MAE2,mmm)
  MAE3 <-  c(MAE3,mmmm)
  
}


mean ( MAE1)
mean ( MAE2)
mean ( MAE3)
