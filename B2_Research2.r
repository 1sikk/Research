# 사례연구2 B2조 문현진(조장) 남원식 오준서
# 환경설정
install.packages("dplyr")
library(dplyr)

rm(list = ls())
getwd()
setwd('C:/james/code/Project/사례연구2_세계인구조사/data/data')
data <- read.csv("subPopCountry.csv")
data2 <- read.csv('subPopCity.csv')
country <- data[,c(3,5)] # 전세계 국가와 인구수 데이터
city <- data2[,c(3,5)] # 나라별 가장 큰 도시의 인구수 데이터



# 1)각 나라에서 가장 큰 도시에 사는 인구의 비율
country_name <-list()
pop_ratio <-list()
for (i in 1:length(country$Country.Name)) {
  if(country[i,2] > city[i,2]){
    
    # 각 나라별 인구비율 계산
    calc <- as.numeric(city[i,2]) / as.numeric(country[i,2]) * 100
    
    # 각 변수에 나라이름과 계산된 인구비율을 저장
    country_name <- append(country[i,1], country_name)
    pop_ratio <- append(calc, pop_ratio)
    
  }
}
# 인구비율 계산 결과 도출
new_country <- unlist(country_name)
new_pop_ratio <- unlist(pop_ratio)
result <- data.frame(new_country, new_pop_ratio)
result



# 2)인구 비율이 가장 높은 나라 20개국
question2 <- result[order(-result$new_pop_ratio),]
answer2 <- head(question2,20)
answer2



# 3)인구 비율이 가장 낮은 나라 20개국
question3 <- result[order(result$new_pop_ratio),]
answer3 <- head(question3,20)
answer3



# 4)결측치 확인
answer4 <- result %>% filter(is.na(result$new_pop_ratio))
answer4

