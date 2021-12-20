## B2조 사례연구3 ####

# study3.2.0
# 변경내용
  # 결측치 오류처리 해결
  # 변수명 일부 변경


rm(list=ls())
getwd()
setwd('C:/rwork/')
install.packages("car")
install.packages('scatterplot3d')
install.packages("tidyverse")
install.packages("dplyr")
install.packages("zoo")
install.packages("forecast")
library(scatterplot3d)
library(dplyr)
library(car)
library(tidyverse)
library(zoo)
library(forecast)
library(quantmod)



## 문제1 #######################################################################

# 다음 사항을 적용하여 다중회귀분석을 실시하시오

# 1-1) state 데이터 셋을 load하고, state.77 data.set을 데이터프레임으로 변환하고,
# life EXP 변수를 Life.EXP로 HS Grad변수를 HS.Grad로 변경하시오.
state <- data.frame(state.x77)
str(state)


# 1-2) Life Expectancy 변수를 종속변수로 설정하고 나머지 변수를 독립변수로 설정하여
# 회귀분석을 실시하시오. 실시 후 결과에 대해 해석하시오
names(state)
model_1 = Life.Exp ~ Population+Income+Illiteracy+Murder+HS.Grad+Frost+Area  
state_1.lm <- lm(formula = model_1, data=state)  # (1)번모델 생성
summary(state_1.lm)  # 회귀모델의 요약정보
vif(state_1.lm) # 공선성 확인
cor(state) # 상관분석결과
par(mfrow=c(2,2))
plot(state_1.lm)  # 회귀분석 그래프 시각화


# 1-3) 2)번 회귀모형에서 Income, Illiteracy, Area 변수를 제외하고 회귀분석을 실시하고
# 결과에 대해 해석하시오
for2 = Life.Exp ~ Population+Murder+HS.Grad+Frost
state_2.lm <- lm(formula = for2, data=state)  # (2)번모델 생성
summary(state_2.lm)
  # 각 변수간의 산점도 출력
newstate2 <- state %>% select(1,4,5,6,7) %>% pairs(panel=panel.smooth, main='state')


# 1-4) Life Expectancy 변수를 종속변수로 설정하고 HS.Grad와 Murder 변수를 예측변수
# (predictor variable)로 설정하여 회귀분석을 실시하시오
for3 = Life.Exp ~ Murder+HS.Grad
state_3.lm <- lm(formula = for3, data=state)  # (3)번모델 생성
plot(state_3.lm)
summary(state.lm)


# 1-5) 전 인구의 55%가 고졸이고 살인비율이 10만명당 8명일 때 Life Expectancy 결과값을
# 예측하시오
summary(state_3.lm)
# 기대수명=70.29708+0.04389(HS.grad)-0.23709(murder)이므로
# 각각 고졸비율에 55, 살인비율에 8을 넣으면
# life=70.29708+2.41395-1.89672
#     =70.81431살 이라는 예측값을 얻을 수 있다.
newpred <- predict(state_3.lm, newdata=data.frame(HS.Grad=55,Murder=8))
newpred  # 기대수명=70.81416살

# 1-6) 4)번에서처럼 2개의 독립변수, 1개의 종속변수의 데이터와 fit된 회귀평면을
# 3D 그래프로 시각화 하시오.
scatterplot3d(state$Life.Exp~state$HS.Grad+state$Murder, highlight.3d = FALSE, 
              type = "p",xlab='고졸비율',ylab = '살인비율',zlab = '기대수명')



## 문제2 #######################################################################
# 2-1) 한국거래소에서 받아온 코스피 데이터를 변수에 저장한다.
kospi <- read.csv('kospi_data_10year.csv', header=T)


# 2-2) 코스피 데이터 셋에서 일자와 종가 데이터를 ko.date변수에 저장한다.
ko.date <-kospi %>% select(1,2) %>% as.data.frame
ko.date$일자 <- as.Date(ko.date$일자)


# 2-3) 빠져있는 주말과 공휴일을 생성하여 병합한다.
calender <- seq.Date(min(ko.date$일자),max(ko.date$일자),"day") %>%
  data.frame() %>% `colnames<-`('일자')

day365 <- full_join(calender, ko.date, by='일자')


# 2-4) 결측치를 유사값으로 대체한다.
zooval <- zoo(day365$종가,day365$일자)
noNA <- zooval %>% na.approx %>% data.frame


# 2-5) 그래프를 생성하여 추세선을 확인한다.
chartSeries(noNA, theme=chartTheme('white'),
            type = c('auto', 'mathsticks'),
            subset = '2011-11::',
            show.grid=TRUE,
            major.ticks = 'auto',minor.ticks = TRUE,
            multi.col = F,
            TA="addEMA(365.25,col='red');addEMA(1461,col='blue')")


# 2-6) 시계열 데이터를 생성한다.
result.ts<- ts(data = noNA,
               start = c(2011,11,08),end = c(2021,11,08), frequency =365)


# 2-7) 시계열 요소 분해 및 해석
# 기본/ 계절변동/ 추세변동/ 잔차
plot(stl(result.ts,'periodic'))




## test zone ##################################################################
# 1-1
# state.x77 데이터 셋 확인 과정
state <- data.frame(state.x77)
write.csv(state,'state.csv',quote = F)

# 1-2
plot(state_1.lm,which = c(1:6))  # Cock's distance 관련 정보까지 출력

# 1-4
newstate <- state %>% select(4,5,6) %>% pairs(panel=panel.smooth, main='state')
# 산점도를 그려 확인해보면 살인률과 고졸비율은 기대수명과는 밀접한 관련이 있지만
# 서로에게는 관련성이 적은 것을 확인할 수 있다.

pred<- predict(state_3.lm, state)
pred  # (3)번모델의 각 주별 기대수명 예측치

# 2-7
plot(decompose(result.ts))  # stl()과 유사한 방법





