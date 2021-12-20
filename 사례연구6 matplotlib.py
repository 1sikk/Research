# 파이썬 plotly() 사용 이순규

# 세로 막대 차트
chart_data = [305, 450, 320, 460, 330, 480, 380, 520]
chart_data_p = ['2018 1분기', '2019 1분기','2018 2분기', '2019 2분기','2018 3분기', '2019 3분기','2018 4분기', '2019 4분기']

import plotly.express as px
fig = px.bar(x = chart_data_p, y = chart_data, height=480, width=720)
fig.show()

# 가로 막대 차트
import plotly.graph_objects as go

fig = go.Figure(go.Bar(
            x = [305, 450, 320, 460, 330, 480, 380, 520],
            y = ['2018 1분기', '2019 1분기','2018 2분기', '2019 2분기','2018 3분기', '2019 3분기','2018 4분기', '2019 4분기'],
            orientation = 'h'))

fig.show()

# 누적 차트 그래프
import pandas as pd
VADeaths = pd.read_csv('C:/Rwork/VADeaths.csv')
VADeaths

import plotly.graph_objects as go
age_data = ['Rural Male', 'Rural Female', 'Urban Male', 'Urban Female']

fig = go.Figure(data = [
    go.Bar(name = '50~54', x = age_data, y = [11.7, 8.7, 15.4, 8.4]),
    go.Bar(name = '55~59', x = age_data, y = [18.1, 11.7, 24.3, 13.6]),
    go.Bar(name = '60~64', x = age_data, y = [26.9, 20.3, 37.0, 19.3]),
    go.Bar(name = '65~69', x = age_data, y = [41.0, 30.9, 54.6, 35.1]),
    go.Bar(name = '70~74', x = age_data, y = [66.0, 54.3, 71.1, 50.0])
])
# Change the bar mode
fig.update_layout(barmode = 'stack') # 누적형 막대로 변환
fig.show()

# 점 차트 그래프
chart_data = [305, 450, 320, 460, 330, 480, 380, 520]
chart_data_p = ['2018 1분기', '2019 1분기','2018 2분기', '2019 2분기','2018 3분기', '2019 3분기','2018 4분기', '2019 4분기']
import plotly.express as px

fig = px.scatter(y = chart_data_p , x= chart_data)
fig.update_traces(marker_size=10)
fig.show()

# 원형 차트 데이터
chart_data = [305, 450, 320, 460, 330, 480, 380, 520]
chart_data_p = ['2018 1분기', '2019 1분기','2018 2분기', '2019 2분기','2018 3분기', '2019 3분기','2018 4분기', '2019 4분기']

import plotly.express as px

fig = px.pie(values = chart_data, names = chart_data_p, color=chart_data_p,
             color_discrete_map={'2018 1분기':'lightcyan',
                                 '2019 1분기':'cyan',
                                 '2018 2분기':'royalblue',
                                 '2019 2분기':'darkblue',
                                 '2018 3분기':'red',
                                 '2019 3분기':'green',
                                 '2018 4분기':'purple',
                                 '2019 4분기':'yellow'
                                 })
fig.show()

# 상자 그래프 시각화 데이터
import pandas as pd
VADeaths = pd.read_csv('C:/Rwork/VADeaths.csv')
VADeaths

age_data = ['50~54', '55~59', '60~64', '65~69', '70~74']
sex_data = ['Rural Male', 'Rural Female', 'Urban Male', 'Urban Female']
import plotly.express as px
fig = px.box(VADeaths, x = age_data, y = sex_data, points = 'all')
fig.show()

# 히스토그램 그래프 시각화 데이터
iris = px.data.iris() # 아이리스 데이터
iris.head()

# 수평 히스토그램
import plotly.express as px
import plotly.graph_objects as go

df = px.data.iris()

fig = go.Figure(data=[go.Histogram(x=df['sepal_width'])])
fig.show()

# 누적 히스토그램
import plotly.express as px
import plotly.graph_objects as go

df = px.data.iris()

fig = go.Figure()
fig.add_trace(go.Histogram(name = 'sepal_width', x=df['sepal_width']))
fig.add_trace(go.Histogram(name = 'sepal_length' , x=df['sepal_length']))

fig.update_layout(barmode='stack')
fig.show()

# 산점도 데이터
iris = px.data.iris() # 아이리스 데이터

fig = px.scatter(iris,
                 x='sepal_length',
                 y='sepal_width',
                 color='species')
fig.show()

# 산점도 매트릭스
fig = px.scatter_matrix(iris, dimensions=['petal_width', 'petal_length', 'sepal_length', "sepal_width"], color="species")
fig.show()

# 중복된 자료 시각화
data("Galton")

# 변수간 비교 시각화
data("iris")

# 3차원 산점도 시각화 데이터
fig = px.scatter_3d(iris,
                    x='sepal_length',
                    y='sepal_width',
                    z='petal_width',
                    color='petal_length',
                    symbol='species',
                    opacity=0.7)
fig.show()

# 파이썬 mapplotlib() 사용 이순규
# 세로 막대 그래프

import matplotlib.pyplot as plt
from matplotlib import font_manager, rc
font_path = 'C:/Windows/Fonts/malgun.TTF'
font = font_manager.FontProperties(fname=font_path).get_name()
rc('font', family = font)

price = [305, 450, 320, 460, 330, 480, 380, 520]
year = ['2018 1분기', '2019 1분기','2018 2분기', '2019 2분기','2018 3분기', '2019 3분기','2018 4분기', '2019 4분기']
plt.title('2018년도 vs 2019년도 매출현황 비교')
plt.bar(year, price, width=0.7,  color = 'skyblue') # .bar 세로형 막대

# 가로 막대 그래프

import matplotlib.pyplot as plt
price = [305, 450, 320, 460, 330, 480, 380, 520]
year = ['2018 1분기', '2019 1분기','2018 2분기', '2019 2분기','2018 3분기', '2019 3분기','2018 4분기', '2019 4분기']

plt.title('2018년도 vs 2019년도 매출현황 비교')
plt.barh(year, price, color = 'skyblue') # .barh 가로형 막대

# 누적 차트 그래프
import pandas as pd
VADeaths = pd.read_csv('C:/Rwork/VADeaths.csv')
VADeaths

VADeaths.index = ['age', 'age1', 'age2', 'age3', 'age4']
VADeaths.columns.names = ['성별']
VADeaths.index.names = ['나이']
VADeaths

import matplotlib.pyplot as plt
fig, ax = plt.subplots(1,2) # R에서 par(mfrow = c(1,2))와 같은 기능
VADeaths.plot(kind = 'bar', ax = ax[0], stacked = True) # 세로 누적 막대 그래프
VADeaths.plot(kind ='barh', ax = ax[1], stacked = True) # 가로 누적 막대 그래프

import matplotlib.pyplot as plt
price = [305, 450, 320, 460, 330, 480, 380, 520]
year = ['2018 1분기', '2019 1분기','2018 2분기', '2019 2분기','2018 3분기', '2019 3분기','2018 4분기', '2019 4분기']
totaldata = pd.DataFrame([[305, '2018 1분기', 450, '2019 1분기'],
                          [320, '2018 2분기', 460, '2019 2분기'],
                          [330, '2018 3분기', 480, '2019 3분기'],
                          [380, '2018 4분기', 520, '2019 4분기']], columns = ['2018price', 'year', '2019price', 'year1'])
totaldata
totaldata.index = ['1분기 매출', '2분기 매출', '3분기 매출', '4분기 매출']
totaldata.plot.bar(stacked = True)
totaldata.plot.barh(stacked = True)

# 점 차트 그래프
price = [305, 450, 320, 460, 330, 480, 380, 520]
year = ['2018 1분기', '2019 1분기','2018 2분기', '2019 2분기','2018 3분기', '2019 3분기','2018 4분기', '2019 4분기']
import matplotlib.pyplot as px

plt = px.scatter(y = year , x= price)

# 원형 차트 데이터

import matplotlib.pyplot as plt
price = [305, 450, 320, 460, 330, 480, 380, 520]
year = ['2018 1분기', '2019 1분기','2018 2분기', '2019 2분기','2018 3분기', '2019 3분기','2018 4분기', '2019 4분기']

plt.pie(price, labels = year, autopct='%.1f%%', startangle=260, counterclock=False)

# 상자 그래프 시각화 데이터
import pandas as pd
VADeaths = pd.read_csv('C:/Rwork/VADeaths.csv')
VADeaths

import matplotlib.pyplot as plt
fig, ax = plt.subplots(1,4)
plt.title('미국 버지니아주 하위계층 사망비율')
VADeaths.boxplot(column = 'Rural Male', ax = ax[0])
VADeaths.boxplot(column = 'Rural Female', ax = ax[1])
VADeaths.boxplot(column = 'Urban Male', ax = ax[2])
VADeaths.boxplot(column = 'Urban Female', ax = ax[3])

# 히스토그램 그래프 시각화 데이터
import matplotlib.pyplot as plt
import seaborn as sns
iris = sns.load_dataset('iris')
iris

fig, ax = plt.subplots(1,4)
sns.histplot(data = iris, x = 'sepal_length', ax = ax[0])
sns.histplot(data = iris, x = 'sepal_width', ax = ax[1])
sns.histplot(data = iris, x = 'petal_length', ax = ax[2])
sns.histplot(data = iris, x = 'petal_width', ax = ax[3])

# 산점도 데이터
groups = iris.groupby('species')
fig, ax = plt.subplots()

for name, group in groups:
    ax.plot(group.petal_length,
            group.petal_width,
            marker='o',
            linestyle='',
            label=name)

ax.legend(fontsize=12, loc='upper left')  # legend position
plt.title('Scatter Plot of iris by matplotlib', fontsize=20)
plt.xlabel('Petal Length', fontsize=14)
plt.ylabel('Petal Width', fontsize=14)

# 산점도 매트릭스
pd.plotting.scatter_matrix(iris.iloc[:, :4])

# 중복된 자료 시각화 데이터
import pandas as pd

galton = pd.read_csv('C:/Users/tj-bu/Documents/카카오톡 받은 파일/galton.csv')
galton[['child', 'parent']]

# 교차 테이블 작성
galton.table = pd.crosstab(galton.child, galton.parent).stack().reset_index(name='C')
galton.table

# 그래프
galton.table.plot.scatter('child', 'parent', s=galton.table.C * 25, alpha=0.5)

plt.scatter(x=galton.table['child'], y=galton.table['parent'],
            s=galton.table['C'] * 15, c=galton.table['C'],
            cmap='rainbow', alpha=0.6, edgecolors='white')

# 변수간 비교 시각화 데이터
from sklearn import datasets
import matplotlib.pyplot as plt
iris = datasets.load_iris()
iris
plt.scatter(iris.data[:,0],iris.data[:,1]) # sepal.length 와 sepal.width 비교
plt.scatter(iris.data[:,0],iris.data[:,2]) # sepal.length 와 petal.width 비교
plt.scatter(iris.data[:,0],iris.data[:,3]) # sepal.length 와 petal.length 비교
# plt.scatter(iris.data[:,2],iris.data[:,3]) # petal.length 와 petal.width 비교

# 3차원 산점도 시각화 데이터
from mpl_toolkits.mplot3d import Axes3D # 3차원
import matplotlib.pyplot as plt
import seaborn as sns
iris = sns.load_dataset('iris')
iris

x = iris['sepal_length']
y = iris['sepal_width']
z = iris['petal_length']
cdata = [ ]
for s in iris['species'] :
    if s == 'setosa' :
        cdata.append(1)
    elif s == 'versicolor' :
        cdata.append(2)
    else :
        cdata.append(3)

fig = plt.figure(figsize = (10, 5))
chart = fig.add_subplot(1,1,1, projection='3d')
chart.scatter(x, y, z, c = cdata)
chart.set_xlabel('sepal_length')
chart.set_ylabel('sepal_width')
chart.set_zlabel('petal_length')

# 밀도 그래프
import matplotlib.pyplot as plt
import seaborn as sns
iris = sns.load_dataset('iris')
iris

fig, ax = plt.subplots(1,4)
sns.kdeplot(iris['sepal_width'], shade = True, color = 'black')
sns.kdeplot(iris['sepal_length'], shade = True, color = 'red')
sns.kdeplot(iris['petal_width'], shade = True, color = 'yellow')
sns.kdeplot(iris['petal_length'], shade = True, color = 'blue')

import pandas as pd
Chem97 = pd.read_csv('C:/Rwork/Chem97.csv')
Chem97

fig, ax = plt.subplots(1,2)
sns.kdeplot(Chem97['gcsescore'], shade = True, color = 'black', ax = ax[0])
sns.kdeplot(Chem97['gcsecnt'], shade = True, color = 'red', ax = ax[1])

import gmplot

# Create the map plotter:
apikey = 'AIzaSyDeRNMnZ__VnQDiATiuz4kPjF_c9r1kWe8' # (your API key here)
gmap = gmplot.GoogleMapPlotter(37.5651, 126.98955, 14, apikey=apikey) # 서울중심

# Outline the Golden Gate Park:
golden_gate_park_lats,golden_gate_park_lngs = zip(*[
    (35.162766, 129.044678), # 부산
    (35.87975, 128.5667),
    (37.456223, 126.705173),
    (35.160077, 126.851467),
    (36.3731, 127.3888),
    (35.538735, 129.31138),
    (36.480862, 127.290757),
    (37.421845, 127.529308),
    (37.8665202, 128.2039048),
    (36.6374037, 127.49137)
])

gmap.scatter(golden_gate_park_lats,golden_gate_park_lngs, color = 'purple', size = 40, marker = True)
gmap.marker(37.5651, 126.98955, color='cornflowerblue')

# Highlight some attractions:
attractions_lats, attractions_lngs = zip(*[
    (36.5212735, 126.7988864),
    (35.821001, 127.11138),
    (34.816206, 126.462924),
    (36.3565811, 128.6892794),
    (35.2210614, 128.3910462),
    (33.489013, 126.49834)
])
gmap.scatter(attractions_lats, attractions_lngs, color = 'red', size = 40, marker = True)

# Draw the map:
gmap.draw('map.html')