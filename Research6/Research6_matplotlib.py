# 사례연구6 남원식
import matplotlib
import matplotlib.pyplot as plt
import matplotlib.font_manager as fm
import numpy as np
import pandas
# matplotlib는 폰트가 한글을 지원하지 않아서 폰트를 강제설정해준다.
font_name = fm.FontProperties(fname = 'C:/Windows/Fonts/malgun.ttf').get_name()
matplotlib.rc('font', family = font_name)
matplotlib.rcParams['axes.unicode_minus'] = False
#막대 차트
x =np.arange(8)
y = np.arange(8)
chart_data = [305, 450, 320, 460, 330, 480, 380, 520]
data_names = ["2018-1분기", "2019-1분기","2018-2분기", "2019-2분기", "2018-3분기", "2019-3분기","2018-4분기", "2019-4분기"]
# 가로 그래프
plt.bar(x,chart_data)
plt.xticks(x,data_names)
plt.show
#세로 그래프
plt.barh(y,chart_data)
plt.yticks(y,data_names)
plt.show()
#누적그래프
# 점차트 그리기
# 점차트 데이터
chart_data = [305, 450, 320, 460, 330, 480, 380, 520]
data_names = ["2018-1분기", "2019-1분기","2018-2분기", "2019-2분기", "2018-3분기", "2019-3분기","2018-4분기", "2019-4분기"]
# 코드
plt.scatter(chart_data,chart_data)
plt.show()
# 파이차트
# 데이터
chart_data = [305, 450, 320, 460, 330, 480, 380, 520]
data_names = ["2018-1분기", "2019-1분기","2018-2분기", "2019-2분기", "2018-3분기", "2019-3분기","2018-4분기", "2019-4분기"]
# 코드
plt.pie(chart_data, labels = data_names, autopct= '%.1f%%')
plt.show()
# 상자 그래프
import pandas as pd
import  matplotlib.pyplot as plt
# 데이터
vadata = pd.read_csv('c:/Python/Data/VADeaths.csv')
vadata.index = ["50-54","55-59","60-64","65-69","70-74"]
values = ["50-54","55-59","60-64","65-69","70-74"]
vadata = vadata.drop([vadata.columns[0]],axis=1) #axis 0=열 1 = 행 drop의 method
vadata

vadata_a = vadata[["Rural Male"]]
vadata_b = vadata[["Rural Female"]]
vadata_c = vadata[["Urban Male"]]
vadata_d = vadata[["Urban Female"]]

fig, ax = plt.subplots(1,4)
ax.boxplot(vadata_a, ax = ax[1])
ax.boxplot(vadata_b, ax= ax[2])
ax.boxplot(vadata_c,ax = ax[3])
ax.boxplot(vadata_d,ax = ax[4])
ax.show()
plt.boxplot[vadata_a, vadata_b, vadata_c, vadata_d]

# 히스토그램
import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd
import numpy as np

iris = sns.load_dataset("iris")
sns.histplot(iris[iris.species == "setosa"]["petal_length"],
             color="blue", label="setosa")
sns.histplot(iris[iris.species == "versicolor"]["petal_length"],
             color="red", label="versicolor")
sns.histplot(iris[iris.species == "virginica"]["petal_length"],
             color="green", label="virginica")
plt.legend(title="Species")
plt.show()
# 산점도 그래프
import matplotlib.pyplot as plt
iris = sns.load_dataset("iris")
plt.scatter(iris[iris.species == "setosa"]["petal_length"],iris[iris.species == "setosa"]["petal_width"])

# 중복화된 자료 ?? 빈도수 * 0.1 ? 크기?
import pandas as pd
galton = pd.read_csv("c:/Python/Data/galton.csv")
galton = galton.drop([galton.columns[0]],axis=1)
galton

#변수간 비교
iris = sns.load_dataset("iris")

# 3차원 그래프
from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as plt
x = iris['sepal_length']
y = iris['sepal_width']
iris["species"]=iris["species"].replace('setosa',1)
iris["species"]=iris["species"].replace('versicolor',2)
iris["species"]=iris["species"].replace('virginica',3)
iris
fig = plt.figure(figsize = (10, 5))
chart = fig.add_subplot(1,1,1, projection='3d')

chart.scatter(x, y,  c= iris["species"])
chart.set_xlabel('sepal_length')
chart.set_ylabel('sepal_width')
