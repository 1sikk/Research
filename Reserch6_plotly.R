# 사례연구6 남원식
install.packages("plotly")
library(plotly)
library(dplyr)
rm(list =ls())
chart_data <- c(305, 450, 320, 460, 330, 480, 380, 520) 
names <- c("2018 1분기", "2019 1분기",
                       "2018 2분기", "2019 2분기", "2018 3분기", "2019 3분기",
                       "2018 4분기", "2019 4분기")
data <- data.frame(names,chart_data)
#### 막대 차트 ####
# 세로차트
fig <- plot_ly(y = chart_data,name = "2018년도 vs 2019년도 매출현황 비교",type = "bar")
# 가로차트
fig <- plot_ly(x = chart_data,name = "2018년도 vs 2019년도 매출현황 비교",type = "bar")
# 누적차트
#  데이터 설정
A <- c("1분기","2분기","3분기","4분기")
S <- c(305,320,330,380)
B <- c(450,460,480,520)
data <- data.frame(A,S,B)
# plot_ly
fig <- plot_ly(data, x = ~A, y = ~S, type = 'bar', name = '2018')
fig <- fig %>% add_trace(y = ~B, name = '2019')
fig <- fig %>% layout(yaxis = list(title = '누적'), barmode = 'stack')

#### 점차트 ####
chart_data <- c(305, 450, 320, 460, 330, 480, 380, 520) 
names <- c("2018 1분기", "2019 1분기",
           "2018 2분기", "2019 2분기", "2018 3분기", "2019 3분기",
           "2018 4분기", "2019 4분기")
df <- data.frame(chart_data)
# x,y축이 데이터로 비교가 되지않아서 점차트 불가능
fig <- plot_ly(data = iris, x = ~Sepal.Length, y = ~Petal.Length)

#### 원차트 ####
chart_data <- c(305, 450, 320, 460, 330, 480, 380, 520) 
names(chart_data) <- c("2018 1분기", "2019 1분기",
                       "2018 2분기", "2019 2분기", "2018 3분기", "2019 3분기",
                       "2018 4분기", "2019 4분기")
data1 <- data.frame("Categorie"=names, chart_data)
data <- data1[,c('Categorie', "chart_data")]

fig <- plot_ly(data, labels = ~Categorie, values = chart_data, type = 'pie')
fig <- fig %>% layout(title = '2018,2019년 분기별 실적', # 제목
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig


#### 상자그래프 ####
data("VADeaths")

fig <- plot_ly(y = VADeaths, type = "box", quartilemethod="linear", name="VADeath")
fig <- fig %>% layout(title = "VADeath") #제목
fig
#### 히스토그램 ####
data("iris")
fig <- plot_ly(x = iris$Sepal.Length, type = "histogram")
# 오버레이 히스토
fig <- plot_ly(alpha = 0.6)
fig <- fig %>% add_histogram(x = iris$Sepal.Length , name = "Sepal.Length")
fig <- fig %>% add_histogram(x = iris$Sepal.Width, name = "Sepal.Width")
fig <- fig %>% add_histogram(x = iris$Petal.Length, name = "Petal.Length")
fig <- fig %>% add_histogram(x = iris$Petal.Width, name = "Petal.Width")
fig <- fig %>% layout(barmode = "overlay")
#### 산점도 ####
#데이터x , y  축 있는거로 해야함.
fig <- plot_ly(data = iris, x = ~Sepal.Length, y = ~Petal.Length)
####
#### 중복된자료 ####
library(UsingR)
data("diamonds")
diamonds
data("Galton")
galtonData <- as.data.frame(table(Galton$child,Galton$parent))
names(galtonData) = c("child","parent","freq")
head(galtonData)
parent <- as.numeric(galtonData$parent) 
child <- as.numeric(galtonData$child)
fig <- plot_ly(
  galtonData, x = ~parent, y = ~child, size = ~freq)

fig
#### 변수간의 비교 시각화 ####
# 산점도 그래프는 있으나 pairs 그래프의경우
# 여러 그래프를 이어붙여 쓰는것보다 pairs로 한번에 비교하는게 편함
#### 밀도 그래프 ####
density <- density(iris$Sepal.Width)
fig <- plot_ly(x = iris$Sepal.Width, type = "histogram" , name = "Sepal.Width.freq")
fig <- fig %>% add_lines(x = density$x, y = density$y, name = "Sepal.Width.density")
#### 3차원 산점도 그래프 ####
data("iris")
iris_setosa=iris[iris$Species == 'setosa', ]
iris_versicolor = iris[iris$Species == 'versicolor', ] 
iris_virginica = iris[iris$Species == 'virginica', ]
iris
fig <- plot_ly(iris, x = iris$Sepal.Length, y = iris$Sepal.Width, z = iris$Species, color = iris$Species)
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Sepal.Length'),
                                   yaxis = list(title = 'Sepal.Width'),
                                   zaxis = list(title = 'Species')))
#### 지도시각화 ####
pop = read.csv("c:/Rwork/Data/population201901.csv")
names(pop) <- c("LOC","LAT","LON","ALL","MAN","WOMAN","HOME")
fig <- pop 
fig <- fig %>%
  plot_ly(
    type = 'densitymapbox',
    lat = ~LAT,
    lon = ~LON,
    coloraxis = 'coloraxis',
    radius = 10) 
fig <- fig %>%
  layout(
    mapbox = list(
      style="stamen-terrain",
      center= list(lon=180)), coloraxis = list(colorscale = "Viridis"))

# geo styling 
df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2011_february_us_airport_traffic.csv')

# geo styling
g <- list(
  scope = 'Kor',
  projection = list(type = 'Asia'),
  showland = TRUE,
  landcolor = toRGB("gray95"),
  subunitcolor = toRGB("gray85"),
  countrycolor = toRGB("gray85"),
  countrywidth = 0.5,
  subunitwidth = 0.5
)

fig <- plot_geo(pop, lat = ~LAT, lon = ~LON)
fig <- fig %>% add_markers(
  text = ~paste(LOC, paste("총인구수", ALL),
                paste("남자인구수",MAN),
                paste("여자인구수",WOMAN),
                paste("세대수",HOME),
                sep = "<br />"),
  color = ~ALL, symbol = I("square"), size = I(8), hoverinfo = "text"
)
fig <- fig %>% colorbar(title = "인구수")
fig <- fig %>% layout(
  title = 'Most trafficked US airports<br />(Hover for airport)', geo = g
)

fig

