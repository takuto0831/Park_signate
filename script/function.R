### function ###
# Log_prophet: log1p -> expm1により予測(観測値に0を含むため)
# Plot_prophet: 実測値(点), 予測区間, 予測線 
################

Log_prophet <- function(data,name){
  # model
  model <- data %>% 
    filter(park == name) %>% 
    select(datetime,visitors) %>% 
    rename(ds = datetime, y = visitors) %>% 
    mutate(y = log1p(y)) %>% 
    prophet()
  # predict
  future <- make_future_dataframe(model,365)
  forecast <- predict(model,future) %>% 
    mutate(park = name, 
           yhat_lower = expm1(yhat_lower),
           yhat_upper = expm1(yhat_upper),
           yhat = expm1(yhat)) %>% 
    select(ds,park,yhat_lower,yhat_upper,yhat)
  return(forecast)
}

Plot_prophet <- function(pred,data){
  pred %>% 
    mutate(ds = as.Date(ds)) %>% 
    left_join(data,by=c("ds"="datetime","park" = "park")) %>% 
    ggplot(aes(x=ds)) + 
    geom_ribbon(aes(ymin=yhat_lower,ymax=yhat_upper),alpha=0.5) +
    geom_line(aes(y=yhat),color="blue") +
    geom_point(aes(y=visitors)) + 
    theme_classic()
}
