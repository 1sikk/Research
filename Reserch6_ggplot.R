install.packages("ggplot2")
library(ggplot2)

### 막대 차트(가로, 세로)

rm(list =ls())

# 데이터 준비
chart_data <- c(305, 450, 320, 460, 330, 480, 380, 520)
names <- c("2018 1분기", "2019 1분기",
           "2018 2분기", "2019 2분기", "2018 3분기", "2019 3분기",
           "2018 4분기", "2019 4분기")
data <- data.frame(names,chart_data)

# x축과 y축을 지정한다. 
ggplot(data, aes(x = names, y = chart_data))

# geom_col() : 막대 차트 추가
ggplot(data, aes(x = names, y = chart_data)) + geom_col()
# geom_col() : aes() 안에 들어가는 x변수에 대한 y변수의 "값"을 나타내는 함수

# 1) 세로 막대
ggplot(data, aes(x = names, y = chart_data)) + 
  geom_col(aes(width=0.8, fill=names)) + 
  ggtitle("DATA : 세로 막대 차트", subtitle = '2021.11.30') + 
  labs(x="2018~2019", y="data", fill = 'Types')

# 2) 가로 막대
ggplot(data, aes(x = names, y = chart_data)) + 
  coord_flip() +
  geom_col(aes(width=0.8, fill=names)) + 
  ggtitle("DATA : 가로 막대 차트", subtitle = '2021.11.30') + 
  labs(x="2018~2019", y="data", fill = 'Types')
# coord_flip() : 가로 차트(코드 제거 시 세로 차트)
# geom_col(aes(fill= )) : 색상추가 , width=0.5 : 바 두께 조절
# ggtitle("DATA : diamonds", subtitle = '2021.11.30') : 그래프 제목, 부제목
# labs(x="people", y="number", fill = 'types') : X,Y 축 내용


###########################################################################


### 누적막대 차트
library(dplyr)
View(diamonds)
rm(list =ls())

# 데이터 준비
A <- c("1분기","2분기","3분기","4분기")
S <- c(305,320,330,380)
B <- c(450,460,480,520)
data <- data.frame(A,S,B)

pivoted_data <- pivot_longer(data, cols = S:B, names_to = 'SB', values_to = 'Value')

ggplot(pivoted_data, aes(x = A, y = Value, fill = SB))+
  geom_bar(stat = 'identity', position = 'stack', colour = "yellow", alpha=3/5)


###########################################################################
# 사례연구6 문태웅

### 점 차트

# 데이터 준비
chart_data <- c(305, 200, 320, 460, 330, 480, 380, 520)
names <- c("2018 1분기", "2019 1분기",
           "2018 2분기", "2019 2분기", "2018 3분기", "2019 3분기",
           "2018 4분기", "2019 4분기")

df <- data.frame(names, chart_data)

ggplot(data=df, aes(x = names, y = chart_data))+ 
  ggtitle("점 차트", subtitle = '2021.11.30') +
  geom_point(colour="1")


###########################################################################


### 원형 차트

chart_data <- c(305, 450, 320, 460, 330, 480, 380, 520)
names(chart_data) <- c("2018 1분기", "2019 1분기",
                       "2018 2분기", "2019 2분기", "2018 3분기", "2019 3분기",
                       "2018 4분기", "2019 4분기")
data <- data.frame(chart_data)

chart_data <- c(305, 450, 320, 460, 330, 480, 380, 520)
names <- c("2018 1분기", "2019 1분기",
                       "2018 2분기", "2019 2분기", "2018 3분기", "2019 3분기",
                       "2018 4분기", "2019 4분기")
data <- data.frame(chart_data,names)


# 1)
ggplot(data, aes(x="", y=names, fill=names))+
  geom_bar(stat='identity')+
  theme_void()+
  coord_polar()+
  geom_text(aes(label=paste0(round(chart_data,1))),
            position=position_stack(vjust=0.5))

# 2)
ggplot(data, aes(x="", y=chart_data, fill=names))+
  geom_bar(stat='identity')+
  theme_void()+
  coord_polar('y', start=0)+
  geom_text(aes(label=paste0(round(chart_data,1))),
            position=position_stack(vjust=0.5))

# 3)
library(tidyverse)
library(viridis)

ggplot(data, aes(x='', y=chart_data, fill=names))+
  geom_bar(stat='identity', color=NA)+
  theme_void()+
  coord_polar('y', start=0)+
  geom_text(aes(label=paste0(round(chart_data,1))),
            position=position_stack(vjust=0.5),
            color='white', family='serif', size=5)+
  scale_fill_viridis(option='inferno', discrete=TRUE)

  
###########################################################################


### 상자 그래프
data("VADeaths")
str(VADeaths)
class(VADeaths)

VADeaths3<- as.data.frame.table(VADeaths)

ggplot(data = VADeaths3, aes(x=Var2,y=Freq))+
  geom_boxplot(fill=c('orange','yellow','blue','purple'),color='black',width=0.3)+
  geom_abline(intercept = 37, slope = 0 , color = 'red',linetype = 'dashed')


###########################################################################


### 히스토그램

data("iris")
str(iris)


ggplot(iris, aes(x = Sepal.Length)) +
  ggtitle("히스토그램", subtitle = '2021.11.30') +
  geom_histogram()


###########################################################################


### 산점도

data("iris")

ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color=Species)) +
  ggtitle("산점도", subtitle = '2021.11.30') +
  geom_point()

# 동일한 결과가 나옴
ggplot(iris) +
  ggtitle("산점도", subtitle = '2021.11.30') +
  geom_point(aes(x = Sepal.Length, y = Petal.Length, color=Species))


###########################################################################


### 중첩자료 시각화

library(UsingR)
library(tidyverse)
data("Galton")
class(Galton)

galtonData <- data.frame(table(galton$child, galton$parent))
names(galtonData) <- c("child", "parent", "freq")


ggplot(data=galtonData, aes(x = parent, y = child))+ 
  scale_size(range = c(1,20), guide = 'none')  +
  geom_point(colour="grey10", aes(size=freq)) +
  geom_point(aes(colour=freq,size=freq)) +
  scale_colour_gradient(low="3",high="black") +
  geom_smooth(method = lm, se = FALSE)

###########################################################################


### 변수간의 비교 시각화
library(tidyverse)

AA <- iris %>% 
  ggplot(mapping = aes(x = Sepal.Length, y = Sepal.Width))+
  geom_point(aes(color = Species))+
  geom_smooth(method = 'lm')+
  scale_color_brewer(palette = 'Set1')

BB <- iris %>% 
  ggplot(mapping = aes(x = Petal.Length, y = Sepal.Width))+
  geom_point(aes(color = Species))+
  geom_smooth(method = 'lm')+
  scale_color_brewer(palette = 'Set1')

CC <- iris %>% 
  ggplot(mapping = aes(x = Petal.Width, y = Sepal.Width))+
  geom_point(aes(color = Species))+
  geom_smooth(method = 'lm')+
  scale_color_brewer(palette = 'Set1')

DD <- iris %>% 
  ggplot(mapping = aes(x = Species, y = Sepal.Width))+
  geom_boxplot(aes(color = Species))+
  scale_color_brewer(palette = 'Set1')

gridExtra::grid.arrange(AA, BB, CC, DD,  ncol = 2)

###########################################################################


### 밀도그래프
# 1)
ggplot(data=iris) +
  geom_density(mapping=aes(x=Sepal.Width, colour = Species))

# 2)
ggplot(data=iris) +
  stat_density(mapping=aes(x=Sepal.Width, fill = Species), position = "identity")

###########################################################################


### 3차원 산점도 그래프
install.packages("scatterplot3d")
library(scatterplot3d)

scatterplot3d(iris$Sepal.Length, iris$Sepal.Width, iris$Species)


###########################################################################


### 지도시각화
install.packages("ggmap")
library(ggmap)
library(stringr)

### 지도 이미지에 레이어 적용
# (2019년도 1월 대한민국 인구수를 기준으로 지역별 인구수 표시)
# 1단계: 데이터 셋 가져오기
pop <- read.csv("population201901.csv", header = T)

region <- pop$'지역명'
lon <- pop$LON
lat <- pop$LAT

tot_pop <- as.numeric(str_replace_all(pop$'총인구수', ',', ''))
df <- data.frame(region, lon, lat, tot_pop)
df
df <- df[1:17, ]
df

# 2단계: 정적 지도 이미지 가져오기
daegu <- c(left = 123.4423013, bottom = 32.8528306,
           right = 131.601445, top = 38.8714354)
map <- get_stamenmap(daegu, zoom = 7, maptype = 'watercolor')

# 3단계: 지도 시각화하기
layer1 <- ggmap(map)
layer1

# 4단계: 포인트 추가
layer2 <- layer1 + geom_point(data = df, 
                              aes(x = lon, y = lat,
                                  color = factor(tot_pop), 
                                  size = factor(tot_pop)))
layer2

# 5단계: 텍스트 추가
layer3 <- layer2 + geom_text(data = df, 
                             aes(x = lon + 0.01, y = lat + 0.08,
                                 label = region), size = 3)
layer2

# 6단계: 크기를 지정하여 파일로 저장
ggsave("pop201901.png", scale = 1, width = 10.24, height = 7.68)


