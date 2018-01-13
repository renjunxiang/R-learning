#画大地图
library(maps)
map("world", fill = TRUE, col = rainbow(200),
    ylim = c(-60, 90), mar = c(0, 0, 0, 0))
title("世界地图")

map("state", fill = TRUE, col = rainbow(209),
    mar = c(0, 0, 2, 0))#mar就是(不详，大小，标题高低，不详)
title("美国地图")

library(mapdata)
map("china", fill = TRUE, col = rainbow(209))
title("中国地图")

#画省会位置
par(mar=rep(0,4))
dat = read.csv('C:/Users/hasee/Desktop/rstudy/city_34.csv')
library(maps)
library(mapdata)
map("china", col = "darkgray", ylim = c(18, 54), panel.first = grid())
points(dat$jd, dat$wd, pch = 19, col = rgb(0, 0, 0, 0.5))
text(dat$jd, dat$wd, dat[, 1], cex = 0.9, col = rgb(0,0, 0, 0.7), 
     pos = c(2, 4, 4, 4, 3, 4, 2, 3, 4, 2, 4, 2, 2, 4, 3, 2, 1, 3, 1, 1, 2, 3, 2, 2, 1, 2, 4, 3, 1, 2, 2, 4, 4, 2))
axis(1, lwd = 0); axis(2, lwd = 0); axis(3, lwd = 0); axis(4, lwd = 0)#pos是文字上下左右位置


#数据着色(结合前面的所有功能)
library(maptools)
par(mar=c(0,0,0,0))#各边界距离：底，左，上，右
x=readShapePoly('C:/Users/hasee/Desktop/rstudy/map/bou2_4p.shp')#下文中会继续用到x这个变量，如果你用的是其它的名称，请在下文的程序中也进行相应的改动。
provname=c("北京市","天津市","河北省","山西省","内蒙古自治区",
           "辽宁省","吉林省","黑龙江省","上海市","江苏省",
           "浙江省","安徽省","福建省","江西省","山东省",
           "河南省","湖北省","湖南省","广东省",
           "广西壮族自治区","海南省","重庆市","四川省","贵州省",
           "云南省","西藏自治区","陕西省","甘肃省","青海省",
           "宁夏回族自治区","新疆维吾尔自治区","台湾省",
           "香港特别行政区");
pop=c(7355291,3963604,20813492,10654162,8470472,15334912,9162183,13192935,8893483,
      25635291,20060115,19322432,11971873,11847841,30794664,26404973,17253385,19029894,
      32222752,13467663,2451819,10272559,26383458,10745630,12695396,689521,11084516,
      7113833,1586635,1945064,6902850,23193638,7026400);
provcol=rgb(red=1-pop/max(pop)/2,green=1-pop/max(pop)/2,blue=0)
getColor=function(mapdata,provname,provcol,othercol)
{
  f=function(x,y) ifelse(x %in% y,which(y==x),0);
  colIndex=sapply(mapdata@data$NAME,f,provname);
  fg=c(othercol,provcol)[colIndex+1];
  return(fg);
}
plot(x,col=getColor(x,provname,provcol,"white"),xlab="",ylab="")
#text(dat$jd, dat$wd,dat$省份, cex = 0.9, col = rgb(0,0, 0, 0.7),
#     pos = c(2, 4, 4, 4, 3, 4, 2, 3, 4, 2, 4, 2, 2, 4, 3, 2, 1, 3, 1, 1, 2, 3, 2, 2, 1, 2, 4, 3, 1, 2, 2, 4, 4, 2))
#points(dat$jd, dat$wd, pch = 19, col = rgb(0, 0, 0, 0.5))#省会
text(dat$jd, dat$wd,dat$省份, cex = 0.9, col = rgb(0,0, 0, 0.7),
     pos = c(3,4,4,2,2,4,4,4,3,3,4,2,4,4,4,4,2,2,3,2,4,2,4,4,4,4,4,4,2,2,4,4,2,4))
points(dat$jd, dat$wd, pch = 19, col = rgb(0, 0, 0, 0.5))#也不准

#要翻墙
provname=c("CN-11","CN-12","CN-13","CN-14","CN-15",
           "CN-21","CN-22","CN-23","CN-31","CN-32",
           "CN-33","CN-34","CN-35","CN-36","CN-37",
           "CN-41","CN-42","CN-43","CN-44","CN-45",
           "CN-46","CN-50","CN-51","CN-52","CN-53",
           "CN-54","CN-61","CN-62","CN-63","CN-64","CN-65","TW");


pop=c(110.56,112.51,113.43,112.52,108.45,112.83,111.23,109.71,110.64,116.51,
      113.86,127.85,117.93,114.74,112.17,118.46,128.18,126.16,130.30,
      125.55,135.64,115.13,116.01,107.03,108.71,102.73,122.10,114.82,
      110.35,108.79,106.12,108.00)

library(googleVis)
a<-data.frame(provname,pop)
G2 <- gvisGeoChart(a, locationvar='provname', 
                   colorvar='pop',options=list
                   (region='CN',displayMode="regions",resolution="provinces",colorAxis="{col
                   ors: ['yellow','red']}" ))
plot(G2)

#另一种谷歌地图画法
library(ggmap)
map=get_map(location = 'China', zoom = 4)
library(ggplot2)
ggmap(map)

#===========================================================================================#
library(maptools)
library(ggplot2)
library(plyr)
china_map=readShapePoly('C:/Users/hasee/Desktop/rstudy/map/bou2_4p.shp')

#提取用于绘图的地图数据
x=china_map@data
#提取33个省市的数据
xs=data.frame(x,id=seq(0:924)-1)

#将地图数据转换为数据框
china_map1=fortify(china_map)

#糅合地图数据
china_mapdata=join(china_map1, xs, type = "full") #合并两个数据框

#绘制地图
ggplot(china_mapdata, aes(x = long, y = lat, group = group,fill=NAME))+
  geom_polygon( )+
  geom_path(colour = "grey40")+
  scale_fill_manual(values=colours(),guide=FALSE)

#把人口信息和行政区域地图结合起来有几种办法，下面的是一种偷懒的方法，把人口数据和行政地图合并。
NAME=c("北京市","天津市","河北省","山西省","内蒙古自治区","辽宁省","吉林省",
       "黑龙江省","上海市","江苏省","浙江省","安徽省","福建省", "江西省","山东省","河南省",
       "湖北省", "湖南省","广东省", "广西壮族自治区","海南省", "重庆市","四川省", "贵州省",
       "云南省","西藏自治区","陕西省","甘肃省","青海省","宁夏回族自治区","新疆维吾尔自治区", 
       "台湾省","香港特别行政区")

pop=c(7355291,3963604,20813492,10654162,8470472,15334912,9162183,13192935,8893483,
      25635291,20060115,19322432,11971873,11847841,30794664,26404973,17253385,19029894,
      32222752,13467663,2451819,10272559,26383458,10745630,12695396,689521,11084516,
      7113833,1586635,1945064,6902850,23193638,7026400)

pop=data.frame(NAME,pop)
china_pop=join(china_mapdata, pop, type="full")
ggplot(china_pop, aes(x=long, y=lat, group=group,fill=pop))+
  geom_polygon( )+
  geom_path(colour="grey40")



也可以利用它来区域地图

zhejiang=subset(china_mapdata,NAME=="浙江省")

ggplot(zhejiang, aes(x = long, y = lat, group = group,fill=NAME))+
  geom_polygon(fill="beige" )+ #填充色
  geom_path(colour="grey40")+
  ggtitle("中华人民共和国浙江省")+
  geom_point(x=120.12,y=30.16)+
  annotate("text",x=120.3,y=30,label="杭州市")+
  theme( #清除不需要的元素
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = c(0.1,0.1),
    legend.background = element_blank())#没有背景框，否则是一个白色的


#自己试验人均GDP,标签存在bug,必须和数据同样长度,在gdp加一个0可以骗过去
dat = read.csv('C:/Users/hasee/Desktop/rstudy/city.csv')
gdp=bylw2013[,7]
gdp[c(32,33,34)]=c(0,0,0)
gdp1=data.frame(NAME,gdp)
china_gdp=join(china_mapdata, gdp1, type="full")
ggplot(china_gdp, aes(x=long, y=lat, group=group,fill=gdp))+
     geom_polygon( )+scale_fill_gradient(low = 'white', high = 'red')+
     geom_path(colour="grey40")+ggtitle('人均GDP')+xlab('经度')+ylab('纬度')+
     geom_text(aes(x = jd,y = wd,label = city,group=city), data =dat)+ #加入标签名
     geom_point(aes(x = jd,y = wd,group=city), data =dat)+ #加入点
     theme(legend.position = c(0.92,0.15),
           legend.background = element_blank())
