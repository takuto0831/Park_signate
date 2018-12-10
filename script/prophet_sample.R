# package
library(tidyverse)
library(prophet)
# input
train <- read_tsv("~/Desktop/Park_signate/input/train.tsv")
test <- read_tsv("~/Desktop/Park_signate/input/test.tsv")
# func 
source('~/Desktop/Park_signate/script/function.R')

# predict 
## 場所ごと(Log)
ans1 <- Log_prophet(train,"阿寒摩周国立公園")
ans2 <- Log_prophet(train,"十和田八幡平国立公園")
ans3 <- Log_prophet(train,"日光国立公園")
ans4 <- Log_prophet(train,"伊勢志摩国立公園")
ans5 <- Log_prophet(train,"大山隠岐国立公園")
ans6 <- Log_prophet(train,"阿蘇くじゅう国立公園")
ans7 <- Log_prophet(train,"霧島錦江湾国立公園")
ans8 <- Log_prophet(train,"慶良間諸島国立公園")

## 要素ごとの可視化
Plot_prophet_components(ans1)
## 可視化例
Plot_prophet(ans1,train)
Plot_prophet(ans6,train)

# combine data
ans <- 
  bind_rows(ans1,ans2,ans3,ans4,ans5,ans6,ans7,ans8)

# make submit file
submit <- test %>% 
  left_join(mutate(ans,ds = as.Date(ds)),
            by=c("datetime" = "ds","park" = "park")) %>% 
  select(index,yhat)

# export
write_tsv(submit,"~/Desktop/Park_signate/output/sample1.tsv",col_names = FALSE)
