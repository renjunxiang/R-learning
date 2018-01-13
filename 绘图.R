bylw2013=read.csv('C:/Users/hasee/Desktop/rstudy/bylw2013.csv')
#基础绘图
opar=par(no.readonly = TRUE) #设置初始化
par(mfrow=c(3,1))
plot(x,y,type = "b", #"p" for points,"l" for lines,"b" for both,
     col='red',pch=1,cex=1,lty=1,lwd=1, #点型,点大小,线型,线宽
     main='主标题',col.main='red',sub='副标题',col.sub='blue',
     xlab='x轴名称',ylab='y轴名称',
     xlim=c(0,60),ylim=c(10,20),data=data1)#轴区间,如果变量来自数据集
par(pch=1,cex=1,lty=1,lwd=1)#也可以单独设置
abline(h=2,v=3) #参考线
abline(v=c(1,2,3),lty=1,col='blue')#多条参考线
lines(lowess(x,y))#添加拟合线，基于局部加权多项式
legend('topleft',inset = 0.1,title = '图例名称',legend=c('a','b'),
       lty=c(1,2),pch=c(3,4),col=c('red','blue'))
text(3,4,'文字')#位置坐标，内容
mtext('文字',side = 3,line=2)#内容，放在哪条边，距离
title(main='主标题',sub='副标题',xlab='x轴名称',ylab='y轴名称')#也可以单独设置
par(opar)#还原初始
#----------------------------------------------------------------------------------------
#多图不同比例1
par(fig=c(0,0.85,0,0.8))#x轴占据(0,0.85),y轴占据(0,0.8)
plot(x,y)
par(fig=c(0,0.8,0.35,1),new=TRUE)
boxplot(x,horizontal = TRUE,axes=FALSE)
par(fig=c(0.65,1,0,0.8),new=TRUE)
boxplot(y,axes=FALSE)
par(opar)#还原初始
#----------------------------------------------------------------------------------------
#多图不同比例2: 2x2矩阵，3个图,widths=c(3,1),heights=c(1,2)会报错？
layout(matrix(c(1,1,2,3),2,2,byrow=TRUE),widths=c(3,1),heights=c(1,2))
plot(x,y)
plot(x,y)
plot(x,y)
par(opar)#还原初始
#----------------------------------------------------------------------------------------
#图类型
par(las=2)#默认1,2为横着画
#条形图，beside=TRUE并列、false堆砌
barplot(x,beside = )
barplot(t(as.matrix(data1[,1:3])),beside=FALSE,horiz = TRUE,col=rainbow(3))#堆砌是按照变量名，依据样本要转置t()
#饼状图，n种颜色
pie(x,labels = paste(c('文字1'),c('文字2')),col = rainbow(n))
#直方图，n组
hist(x,breaks = n)
lines(density())#添加密度曲线
xfit=seq(min(x),max(x),length=40)#也可以加正态
yfit=dnorm(xfit,mean=mean(x),sd=sd(x))
lines(xfit,yfit)
#核密度图
d=density(x)
plot(d,lwd=3)
polygon(d,col='yellow',border='green')#区域黄色，线条绿色
#任意函数图
curve(x^2+1,from=-2,to=2)
#可比较的核密度图
library(sm)
sm.density.compare(x,x_label)#数据和数据的分类依据
#箱型图，凹槽没有重叠则中位数存在显著差异
boxplot(x~x_label,notch=TRUE,horizontal = TRUE)#箱型图跨组比较，notch=TRUE为凹槽默认FALSE,horizontal横着画
boxplot.stats(data1$a)#具体统计量
#点图
dotchart(x,labels = )

library(car)
scatterplot(x2~x3|x5,legend.plot = TRUE,boxplots='xy')#按照x5水平，分别绘制x2~x3的散点图，边界画箱型图
qq.plot(x)#qq图
#气泡图
symbols(x2,x3,circles = x4)#x2,x3是圆心坐标，x4为圆圈半径，要数组'数据框$'不能用'数据框[]'
#矩阵散点图
scatterplotMatrix(x,diagonal='density',spread = FALSE)#对角线"density", "boxplot", "histogram", "oned", "qqplot", "none",
#非对角线有散点图，线性回归拟合线，平滑线(曲线拟合),spread = FALSE选项表示不添加展示分散度和对称信息的直线

#########################################################################################################
#########################################################################################################
#########################################################################################################
library(ggplot2)
library(ggthemes)
qplot(a,b,data=data1,geom=c('line','point'),color=e,size=e,shape=as.factor(e))#和基础绘图相似,属性和ggplot一样

#形状颜色
ggplot(data1,aes(x=a,y=b,color=as.factor(e),
                 shape=as.factor(e)))+#离散最好都设成因子型,不适合颜色过渡
  geom_point()+#geom_point(color=c('red'))会覆盖前面，全部为红
  scale_shape_manual(values=c(1,2,3),guide=FALSE)+#设置形状
  scale_colour_manual(values=c('red','blue','black'))+#设置1,2,3颜色
  geom_text(aes(label=c),size=4,color='black',vjust=-0.2)+
  annotate(geom='text',x=-2,y=3,label='加注解',color='blue')

ggplot(data1,aes(x=a,y=b,fill=c,shape=as.factor(e)))+#连续适合fill，1-20只能color=c改变颜色
  geom_point()+
  scale_shape_manual(values=c(21,22,23),guide=FALSE)+#设置形状，21-25用fill可以只改变填充色
  scale_fill_gradient(low = 'white', high = 'red')#用于fill的过渡

ggplot(data1,aes(x=a,y=b,color=c,size=d,shape=as.factor(e)))+#1-20只能color=c改变颜色
  geom_point(alpha=0.7)+#透明防重叠
  scale_shape_manual(values=c(15,16,17),guide=FALSE)+#设置形状，21-25用fill可以改变填充色
  scale_color_gradient(low = 'blue', high = 'orange')+#用于color的过渡
  scale_size(range=c(1,10))+#用于size的过渡
  facet_grid(~e)#分面:~e分成几行(等价facet_wrap(~e))，e~.分成几列

#加标签
ggplot(bylw2013,aes(x=地区,y=财政性教育经费))+
  geom_bar(stat='identity',fill='pink',size=0.25)+#identity表示用每个样本的数据,默认是轴坐标的计数
  geom_text(aes(label=财政性教育经费),
            vjust=-2,color='black')+#加标签,位置(上负下正)，颜色,分面需要设定数据框每个面一个值
  geom_errorbar(aes(ymin=财政性教育经费-100,ymax=财政性教育经费+50),color='blue')+#误差棒
  annotate(geom='text',x=2,y=1500,label='加注解',color='blue')+#这里x只能数字
  annotate(geom='segment',x=3,xend=5,y=1500,yend=1600,color='red',size=1,
           arrow=arrow(ends='both',angle=45,length=unit(0.5,'cm')))+#加双箭头,箭头45度，0.5cm
  annotate(geom='pointrange',x=6,y=1500,ymin=1400,ymax=1600,color='blue')+#加点线
  annotate(geom='rect',xmin=8,xmax=10,ymin=1400,ymax=1600,fill='green',alpha=0.2)#加阴影

library(plyr)
ce=arrange(bylw2013,划分,财政性教育经费)#按照'划分'和'经费'排序
ce=ddply(ce,'划分',transform,label_y=cumsum(财政性教育经费)-
           财政性教育经费/2)#分组求和,否则加标签会错位,label_y自动生成新变量
ggplot(ce,aes(x=划分,y=财政性教育经费))+
  geom_bar(stat='identity',color='black',size=0.25,fill=rainbow(31))+#可以是green
  geom_text(aes(y=label_y,label=财政性教育经费),color='black')

#图形外观
ggplot(diamonds,aes(x=cut,y=price))+
  geom_boxplot(color='black',size=0.6,
               width=0.8)+#color、size边框线颜色黑、宽度0.6,图形宽度0.8,notch=TRUE加凹槽
  facet_wrap(~clarity,nrow=2)+#依据color分面
  labs(title='价格箱型图',x='切割水平',y='价格')+#标题、轴标题
  stat_summary(fun.y='mean',geom='point',shape=20,size=1,color='red')#加入统计量标签
  theme_base()#背景样式，base为基础绘图样式(无法调整图例)，默认好看

ggplot(data1,aes(x=a,y=b))+#加上shape或color=as.factor(e)会分类绘制等高线,除非后面再用color覆盖
  geom_point(data=data1,aes(x=a,y=b,size=2,color=e))+#只能在这里分类形状和颜色,在一起说明一类
  stat_density2d(aes(color=..level..),linetype=2,size=1)+#等高线图,size线宽,颜色代表密度
  scale_color_gradient(low = 'blue', high = 'red')

ggplot(bylw2013,aes(x=财政性教育经费,y=财政性教育经费占比))+
  geom_point(data=bylw2013,size=2,
             aes(x=财政性教育经费,y=财政性教育经费占比,shape=as.factor(bylw2013$划分)))+
  stat_density2d(aes(fill=..level..),linetype=2,size=1)+#等高线图,size线宽,颜色代表密度
  scale_shape_manual(values=c(21,22,23),guide=FALSE)+
  scale_color_gradient(low = 'blue', high = 'red')

ggplot(data1,aes(x=a,y=b))+
  stat_density2d(aes(fill=..density..),geom='raster',contour=FALSE)+#必须contour=FALSE取消线才能填色
  geom_point(data=data1,size=2,color='gray',
             aes(x=a,y=b,shape=as.factor(e)))+#要设置一定要data=重新写一遍
  scale_shape_manual(values=c(4,5,6),guide=FALSE)+
  scale_fill_gradient(low = 'blue', high = 'red')
#轴选项
ggplot(diamonds,aes(x=price,fill=clarity))+#按照fill分组
  geom_density(color='black',size=0.8,alpha=0.5,adjust=2)+#alpha透明度,adjust默认1越大越平滑
  scale_x_continuous(limits=c(0,20000),
                     breaks=c(0,10000,20000),
                     labels=c('a1','a2','a3'))+#x轴区间和刻度并更改标签,可以用xlim=c(0,20000)
  xlab('x轴')+ylab('y轴')+#轴标题
  coord_flip()+#转置
  theme(legend.position = c(0.92,0.75),
        legend.background = element_blank(),
        axis.text=element_text(size=10,colour="blue",
                               hjust=1,vjust=.5,angle=45),#刻度标签为蓝,axis.text.x只改x
        axis.title.y = element_text(face='italic',
                                    size=rel(1.5), angle=90),#y轴标题属性,element_blank()表示移除
        panel.background = element_rect(fill = "#EBEBEB"))#设置背景色

#加阴影区域  
ggplot(bylw2013,aes(x=GDP,y=财政性教育经费))+
  geom_ribbon(aes(ymin=财政性教育经费*0.8,ymax=财政性教育经费*1.2),
              alpha=0.2,fill='blue')+#阴影区域
  geom_ribbon(aes(ymin=财政性教育经费*0.7,ymax=财政性教育经费*1.1),
              alpha=0.2,color='green')+#边框染色
  geom_line(aes(y=财政性教育经费*0.8),linetype=3)+
  geom_line(aes(y=财政性教育经费*1.2),linetype=4)+#可以用其他方法得到预测值后赋值y=predicted
  geom_line()+#线横坐标不能是字符型
  geom_smooth(method=lm,level=0.95,color='red',linetype=2,size=1)+#回归线
  annotate('text',label='R^2==未知',x=40000,y=500,parse=TRUE)+#parse转为数学表达式
  geom_rug()#边际地毯，就是某个维度的一维散点图





#详细说明
#ggplot绘图，colour参数指定的是曲线的颜色，而fill是往曲线下面填充颜色
ggplot(data=data1, mapping=aes(x=b,y=a,col=e,shape=as.factor(e)))
#上面这行代码把数据映射XY坐标轴上，需要告诉ggplot2，这些数据要映射成什么样的几何对象
#不同的几何对象，要求的属性会有些不同，这些属性也可以在几何对象映射时提供，比如上一图，也可以用以下语法来画
ggplot(data1,aes(x=b,y=a,col=e,shape=as.factor(e)))+geom_point()#shape要用因子需要转换
ggplot(data1,aes(x=as.factor(e),y=a))+geom_boxplot()#用来分类的最好进行转换，否则可能报错
ggplot(data1,aes(x='ppp',y=a))+geom_boxplot()#不想分类可以弄个字符

#直方图最容易，提供一个x变量，画出数据的分布
library(plyr)
ggplot(diamonds,aes(x=price, fill=cut),
       order=desc(cut))+geom_histogram()#默认累积直方图,order改变堆叠顺序(导入plyr包)
ggplot(diamonds,aes(x=price, fill=cut), 
       position="dodge")+geom_histogram()#也可以将其分开，side-by-side地画直方图
ggplot(diamonds,aes(x=price, fill=cut), 
       position="fill")+geom_histogram()#按照cut各类的相对比例来画

#柱状图两个要素，一个是分类变量，一个是数目，也就是柱子的高度。数目在这里不用提供，因为ggplot2会通过x变量计算各个分类的数目
ggplot(diamonds,aes(x=clarity))+geom_bar(color='black')
ggplot()+geom_bar(aes(x=c(LETTERS[1:3]),y=1:3), 
                  stat="identity")#通过stat参数，可以让geom_bar按指定高度画图

#密度函数图
ggplot(small)+geom_density(aes(x=price, colour=cut))
ggplot(small)+geom_density(aes(x=price,fill=clarity))

geom_abline 	geom_area 	
geom_bar 		geom_bin2d
geom_blank 		geom_boxplot 	
geom_contour 	geom_crossbar
geom_density 	geom_density2d 	
geom_dotplot 	geom_errorbar
geom_errorbarh 	geom_freqpoly 	
geom_hex 		geom_histogram
geom_hline 		geom_jitter 	
geom_line 		geom_linerange
geom_map 		geom_path 	
geom_point 		geom_pointrange
geom_polygon 	geom_quantile 	
geom_raster 	geom_rect
geom_ribbon 	geom_rug 	
geom_segment 	geom_smooth
geom_step 		geom_text 	
geom_tile 		geom_violin
geom_vline

#统计变换对原始数据进行某种计算，然后在图上表示出来，例如对散点图上加一条回归线
#，aes所提供的参数，就通过ggplot提供，而不是提供给geom_point，因为ggplot里的参数，相当于全局变量
ggplot(diamonds, aes(x=carat, y=price))+geom_point()+scale_y_log10()+stat_smooth()

stat_abline       stat_contour      stat_identity     stat_summary
stat_bin          stat_density      stat_qq           stat_summary2d
stat_bin2d        stat_density2d    stat_quantile     stat_summary_hex
stat_bindot       stat_ecdf         stat_smooth       stat_unique
stat_binhex       stat_function     stat_spoke        stat_vline
stat_boxplot      stat_hline        stat_sum          stat_ydensity

x_2=function(x){
  x^2+1
}
ggplot(data.frame(x=c(-2,2)),aes(x=x))+stat_function(fun=x_2,geom='line')

ggplot(data1,aes(x=a,y=b))+#加上shape或color=as.factor(e)会分类绘制等高线,除非后面再用color覆盖
  geom_point(size=2,shape=as.factor(data1$e))+
  scale_shape_manual(values=c(3,4,5))+
  stat_density2d(aes(color=..level..),linetype=2,size=1)+#等高线图,size线宽,颜色代表密度
  scale_color_gradient(low = 'blue', high = 'red')



#分面（Facet）可以让我们按照某种给定的条件，对数据进行分组，然后分别画图
ggplot(diamonds, aes(x=carat, y=price))+
  geom_point(aes(colour=cut))+scale_y_log10()+
  facet_wrap(~cut,scales='free')+stat_smooth()#加上free则纵轴坐标按照每个面调整

ggplot(small, aes(x=carat, y=price))+
  geom_point(aes(colour=cut))+scale_y_log10()+facet_grid(size~cut)+stat_smooth()#列size行cut分面

#主题（Theme）原代码+ggtitle('try')
ggtitle(), xlab()和ylab()#等价于labs(title='',x='x标题',y='y标题')
library(ggthemes)#原代码+theme_solarized()黄色,theme_few()无背景
