### function ###
# Prophet: 通常通りにprophetのモデルを利用
# Log_prophet: dataにlog1pを適用し, 予測結果にexpm1を適用し予測(観測値に0を含むため)
# Plot_prophet: 実測値(点), 予測区間, 予測線 
################

Prophet <- function(data,name,event){
  # model
  model <- data %>% 
    filter(park == name) %>% 
    select(datetime,visitors) %>% 
    rename(ds = datetime, y = visitors) %>% 
    prophet(changepoint.prior.scale = 0.5,
            weekly.seasonality = TRUE,
            yearly.seasonality = TRUE,
            daily.seasonality = FALSE,
            holidays = event)
  # predict
  future <- make_future_dataframe(model,365)
  forecast <- predict(model,future) %>% 
    mutate(park = name) %>% 
    select(ds,park,trend,weekly,weekly_lower,weekly_upper,yearly,yearly_lower,
           yearly_upper,yhat,yhat_lower,yhat_upper,holidays) 
  return(forecast)
}
########## 要修正 #############
Log_prophet <- function(data,name,event){
  # model
  model <- data %>% 
    filter(park == name) %>% 
    select(datetime,visitors) %>% 
    rename(ds = datetime, y = visitors) %>% 
    mutate(y = log1p(y)) %>% 
    prophet(changepoint.prior.scale = 0.5,
            weekly.seasonality = TRUE,
            yearly.seasonality = TRUE,
            daily.seasonality = FALSE,
            holidays = event)
  # predict
  future <- make_future_dataframe(model,365)
  forecast <- predict(model,future) %>% 
    mutate(park = name) %>% 
    select(ds,park,trend,weekly,weekly_lower,weekly_upper,yearly,yearly_lower,
           yearly_upper,yhat,yhat_lower,yhat_upper,holidays) %>% 
    mutate_if(is.numeric,expm1)
  # prophet_plot_components(model,forecast) + theme_classic(base_family = "HiraKakuPro-W3")
  return(forecast)
}

Plot_prophet_components <- function(forecast){
  library(lubridate)
  library(gridExtra)
  # trend
  p1 <- ggplot(forecast,aes(x=ds)) +
    geom_line(aes(y=trend)) +
    theme_classic(base_family = "HiraKakuPro-W3") +
    ggtitle(forecast$park[1])
  # weekly 
  p2 <- forecast %>% 
    mutate(week = wday(ds,label = TRUE)) %>% 
    head(7) %>% 
    ggplot(aes(x=week,y=weekly,group=1)) +
    geom_line() +
    theme_classic(base_family = "HiraKakuPro-W3")
  # yearly
  p3 <- forecast %>% 
    filter(year(ds) == 2015) %>% 
    ggplot(aes(x=ds,y=yearly,group=1)) +
    geom_line() +
    theme_classic(base_family = "HiraKakuPro-W3")
  # holidays
  p4 <- forecast %>% 
    ggplot(aes(x=ds,y=holidays)) +
    geom_line() +
    theme_classic(base_family = "HiraKakuPro-W3")
  grid.arrange(p1,p2,p3,p4,nrow=4)
}

Plot_prophet_predict <- function(forecast,data){
  forecast %>% 
    mutate(ds = as.Date(ds)) %>% 
    left_join(data,by=c("ds"="datetime","park" = "park")) %>% 
    ggplot(aes(x=ds)) + 
    geom_ribbon(aes(ymin=yhat_lower,ymax=yhat_upper),alpha=0.5) +
    geom_line(aes(y=yhat),color="blue") +
    geom_point(aes(y=visitors)) + 
    facet_zoom(x = ds >= "2015-08-01" & ds <= "2015-09-30") +
    ggtitle(forecast$park[1]) + 
    theme_classic(base_family = "HiraKakuPro-W3")
}

ResidualCheck <- function(forecast,data){
  library(gridExtra)
  p1 <- forecast %>% 
    mutate(ds = as.Date(ds)) %>% 
    inner_join(data,by=c("ds"="datetime","park" = "park")) %>% 
    mutate(dif = yhat-visitors) %>% 
    ggplot(aes(x=dif)) +
    geom_histogram(bins = 50) +
    theme_classic()
  p2 <- forecast %>% 
    mutate(ds = as.Date(ds)) %>% 
    inner_join(data,by=c("ds"="datetime","park" = "park")) %>% 
    mutate(dif = yhat-visitors) %>% 
    ggplot(aes(x=ds,y=dif)) +
    geom_line() +
    theme_classic()
  grid.arrange(p1,p2,ncol=2)
}
MeanAbsoluteError <- function(forecast,data){
  tmp <- forecast %>% 
    mutate(ds = as.Date(ds)) %>% 
    inner_join(data,by=c("ds"="datetime","park" = "park")) %>% 
    mutate(dif = abs(yhat-visitors)) %>% 
    summarise(mean(dif)) %>% 
    as.numeric() 
  print(paste("公園名:",forecast$park[1],"MAE:",tmp))
}
