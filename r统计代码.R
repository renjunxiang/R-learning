
#统计函数，x为向量等
11%%2#求模运算
mean(x) median(x) sd(x) var(x) sum(x) min(x) max(x) range(c(1.2,4.3,5.6)) quantile(x,probs =c(0.1,0.2))#分位数
dist(rbind(c(0, 0, 1, 1, 1, 1), c(1, 0, 1, 1, 0, 1)), method = "binary")#距离计算
weighted.mean(x=data1$a,w=data1$b,na.rm = TRUE)#用来忽略缺失值,上面一行也可以这样
range(c(1,3,5,6)) diff(x,lag=) #值域，滞后差分
scale(x,center = T,scale = T) #默认c=t,s=t,标准化,z分值;c=T,减均值;s=t,除均方根;全f返回原始值
rescale(x)#本质(x-min)/(max-min),缩放至0-1
dnorm(2,0,1)#均值0，标准差1，x=2的密度函数值
pnorm(2,0,1)#均值0，标准差1，x=2的累计概率
qnorm(0.5,0,1)#均值0，标准差1的0.5分位数
rnorm(100)#均值0，标准差1的100个随机数
order(runif(100))#用来生成随机序列，先产生100个随机数，然后替换成排位
help("distribution")#可以查看分布函数名称,在stat包中
#########################################################################################
#数学函数，x为向量等
abs() sqrt() #绝对值，开方
ceiling(x) floor(x) trunc(x) #不小于x的最小整数，不大于x的最大整数，绝对值的floor
round(x,digits = 3) signif(x,digits = 3)#保留3位小数，保留3位有效数字
log(x,base = 3) log(x) log10(x) exp(x)#3为底求x对数，ln，lg，e的x次
t(sp500) #转置
#########################################################################################
#统计方法
str(data1)#数据框结构
summary(x)#描统
nlevels(x)#有几种数值
by(x,x_label,summary)#分组统计
data1=data.frame(a=1:4,b=3:6,c=c(1,2,1,2))
rowSums(data1) colSums(data1) aggregate(a~c,FUN = sum,by=a~c) 
apply(data1,1,mean)#1按行,2按列,相当于axis=1和0,data可以矩阵、数据框
sapply(data1,function(x){sd(x)/mean(x)})#可以自己设置统计量在function,对data1的每一个变量执行函数
lapply(data1,function(x){sd(x)/mean(x)})#主要针对向量，数据框、向量结果和sapply一样
addmargins(table(diamonds[,2:3]))#行列均计算，只能针对table

tapply(data1$a,data1$e,mean)#对a按照e分组求均值,输出为array(但不可以自己写函数)
tapply(diamonds$price,list(diamonds$cut,diamonds$color),mean)#两个因子变量则输出表格是matrix,多个因子则是复合表格
by(data1,data1$e,function(x){lm(x[,1]~x[,2]+x[,3])})#类似tapply，还可以回归,输出为'by'特定类型
split(data1,list(data1$e))#对数据框拆分,直接data1$e也可以

table(x)#一维,table(x,y)二维频数表,二维等价于table(z),加入参数useNA='ifany'输出na
prop.table(table(z1)) #计算频率,prop.table(table(z1,1)) 按行计算，2按列
#其他包
library(plyr)
ddply(data1,.variables = 'e',summarize,#summarize表示输出的未统计量，
      mean=mean(a),sd=sd(b),cov=mean/sd)#分组,数据框有三列'e'=1、2、3,'mean','sd'
ddply(data1,.variables = 'e',nrow)#输出为每类行数
ddply(data1,'e',transform,
      d_a=a-mean(a,na.rm=TRUE),
      n=sum(!is.na(b)))#数据集,分组依据,转换,加入新变量为离差,这里不能引用d_a(d_b=d_a+10)
dcast(ddply(diamonds,.variables=c('cut','color'),summarise,a=mean(price)),
      cut~color,value.var='a')#依据因子求价格均值得到的是长数据，再转为宽数据(把color设为列变量)

library(Hmisc)#不好用
describe(x)

library(psych)
describe(z)#好用
describeBy(x,x_label)

library(pastecs)
stat.desc(x,norm = TRUE)#normtest.p为p值
#################################################################################################
#假设检验
prop.test(x=89,n=100,p=0.9,alternative="two.sided")#单样本二项分布比率检验
prop.test(x=c(36,22), n=c(120,100),alternative="two.sided",conf.level=.95)#两样本二项分布比率是否相等，u检验在总体方差已知的情况使用，要求两组样本量之和超过40
chisq.test(table(z1),correct = FALSE) #不带连续校正的情况,H0:x与y不相关,卡方独立性检验(列联表检验）,原始数据要用table()转成列联表(x,y)的频数
library(gmodels)#更方便
CrossTable(teacher[,2],teacher[,3],chisq=TRUE)#行1频数，行2卡方统计量，行3在该行比例，行4在该列比例，行5频率

shapiro.test()#正态性检验,非正态不能用f和bartlett方差齐性
ansari.test()#非正态方差齐性检验
t.test(x,mu=2,alternative = )#用alternative表示备择假设，two.sided(原假设x均值-y均值=0)，less(原假设x均值-y均值>=0)和greater(原假设不大于0)
t.test(x~x_label),t.test(x,y,paired = TRUE)#一个变量混合，两个变量比较
var.test(x~x_label)#方差齐性检验，也可以bartlett.test(x~x_label，data)
wilcox.test()#符号秩和检验(也叫Mann-Whitney U)，与t用法一样
kruskal.test(x~x_label)
#相关系数
cor(z)#相关系数矩阵，corr(z)为相关系数值，cov()为协方差矩阵
#'na.or.complete'一起比会忽略缺失值行列，'pairwise.complete.obs'两两比更常用,cor要求行列中不能有缺失值
cor(data1,use='pairwise.complete.obs')
cor.test(z$x,z$y)#两变量的相关性检验

library(car)
scatterplot(x2~x3|x5,legend.plot = TRUE,boxplots='xy')#按照x5水平，分别绘制x2~x3的散点图，边界画箱型图
qq.plot(x)#qq图
#气泡图
symbols(x2,x3,circles = x4)#x2,x3是圆心坐标，x4为圆圈半径，要数组'数据框$'不能用'数据框[]'
#矩阵散点图
scatterplotMatrix(x,diagonal='density',spread = FALSE)#对角线"density", "boxplot", "histogram", "oned", "qqplot", "none",
#非对角线有散点图，线性回归拟合线，平滑线(曲线拟合),spread = FALSE选项表示不添加展示分散度和对称信息的直线

library(GGally)
ggpairs(data1)#矩阵散点图,同时显示相关系数

library(psych)
corr.test(z,use='pairwise',method='pearson')#相关系数矩阵含p值,(pearson、spearman、kendall),paireise或complete
#################################################################################################
#回归
y=1:10
x1=1:10+rnorm(10)
x2=1:10+rnorm(10)
x3=1:10+rnorm(10)
x4=1:10+rnorm(10)
data=data.frame(y,x1,x2,x3,x4)
lm(y~x-1)#-1代表不含截距
lm(y~x1+x2,data = )#多元回归
lm(y~x1*x2)#包含交互，等价于lm(y~x1+x2+x1:x2)
lm(y~.)#数据框除因变量外其余
lm(y~x+I(x^2))#多项式回归
model.matrix(y~x1+x2)#哑变量回归
glm(a~b+c,data = data1,family = binomial(link = 'logit'))#默认逻辑回归,family=poisson(link='log')泊松回归
#拟合测度采用deviance和AIC，增加一个变量如果不变小则没用,后面回归优化提到

summary(lm(y~x1+x2))
fitted(lm(y~x1+x2))#拟合值
resid(lm(y~x1+x2))#回归值与观测值之差，即fitted(lm(y~x1+x2))-y
abline(lm(y~x))#画回归线，等价于lines(x,fitted(lm(y~x)))
confint(lm(y~x))#回归诊断，给出置信区间
coef(lm(y~x))#系数

model_lm=lm(y~x+I(x^2))
model_new=update(model_lm,y~x+I(x^2))#当数据集略有改动时,update会节约时间,收敛的更快
#回归优化
par(mfrow=c(2,2))
plot(lm(data1$a~data1$b+data1$c))#回归假设四图，零条件均值（残差与因变量不相关为白噪声），残差正态检验qq，同方差（标准误应随机），残差杠杆看离群点
par(opar)

library(car)#正态性通过残差正态检验即可
vif(lm(data1$a~data1$b+data1$c))#多重共线性不存在假设检验，VIF>10严重共线、>4共线
durbinWatsonTest(lm(data1$a~data1$b+data1$c))#杜宾外特，自相关检验，四张图没有，原假设不相关
ncvTest(lm(data1$a~data1$b+data1$c))#得分检验异方差性，原假设同方差

library(coefplot)
coefplot(lm(data1$a~data1$b+data1$c))#系数的估计值为点,粗线一个标准误,细线两个,如果两个标准误(置信区间0.954)不含0则拒绝‘系数为0’的假设
ggplot(aes(x=.fitted,y=.resid),data=lm(data1$a~data1$b+data1$c))+geom_point()+geom_hline(yintercept = 0)#零条件均值的ggplot画法

#选择最佳模型，多元的x2不显著时
anova(lm(y~x1),lm(y~x1+x2))#模型后嵌套在前中，若p大于0.05认为x2没必要
AIC(lm(d~a+b+c,data=data1),lm(d~a+b,data=data1))#不必要嵌套，AIC越小越好
#逐步回归
library(MASS)
stepAIC(lm(d~a+b+c,data=data1),direction='backward')#"both", "backward", "forward"，只能局部最优
#全子集回归,有两种，可以画一起
par(mfrow=c(1,2))
#第一种方块图
library(leaps)
leaps=regsubsets(d~a+b+c,data=data1,nbest=4)
plot(leaps,scale='adjr2')#横着看,第一行为最优解调整r方最大
#第二种线图
library(car)
subsets(leaps,statistic='cp')#越好的模型离斜率1的线越近
abline(1,1,lty=2,col='red')
par(opar)
#曲线拟合(也称非线性最小二乘,目的在于得到一组向量使得距离和最小，向量可以是系数也可以是点坐标)
nlsout=nls(y~A*exp(-alpha*x1),start=c(A=2,alpha=0.05))#函数形式自己写，参数自己写，初始值别太偏，用plot(x1,fitted(nls(y~A*exp(-alpha*x1),start=c(A=2,alpha=0.05))),type = "b")，不能abline
#样条用于平滑数据
smooth.spline(x=data1$a,y=data1$b,df=3)#df越高越接近内插线，df=1为直线

#LDA线性判别,假设每一类数据都满足正态分布
library(MASS)
lda()
#----------------------------------------------------------------------------------------
#方差分析,label要是字符，正态同方差假设协方差还假定回归斜率相同（存在交互项），交互项不显著说明斜率相等
anova(lm(x~x_label))
summary(aov(lm(x~x_label)))#和上面等价,x~x1+x2_label*x3_label:x2,x3的双因素,x1为协变量(放前面),后者根据前者调整，控制x1可以认为...协变量要连续
#交互项不显著意味不同协变量水平下因子的影响都相同，证明已经剔除协变量的影响；区别多因素方差分析，因子的交互项显著，因为不是控制某个因子


library(gplots)
plotmeans(data1$a~data1$e,xlab = 'x',ylab = 'y',main='main')#组均值图

TukeyHSD(aov(data1$a~as.factor(data1$e)))#多重比较,要因子
par(las=1)#旋转轴刻度
par(mar=c(5,8,4,2))#增大左边界面积
plot(TukeyHSD(aov(data1$a~as.factor(data1$e))))#均值成对比较图，plot(aov(y~y1))会画qq一类的好多，用于评估假设条件是否满足
par(opar)

library(HH)
ancova(y~x1+x2,data=)#绘制协方差分析图
#双因素方差分析图
interaction(x1,x2,y,type='b')#最常用的
library(gplots)
plotmeans(y~interaction(x1,x2,sep = ''),connect = list(c(1,2,3),c(2,4,6)),
          col=c('red','green'),main='main',xlab='xlab')#误差棒图
library(HH)
interaction2wt(y~x1*x2)#任意顺序的因子设计的主效应和交互效应
#a元b因素方差分析，a个因变量是否受b个自变量的影响，假设：多元正态，方差-协方差矩阵同质
library(MASS)
x=cbind(x2,x3,x4)
aggregate(x,by=list(x1),FUN=mean) #单个的数据列出组均值,一定要list
aggregate(a~c,x,mean)#数据框可以aggregate(a~e+b,data1,mean),如果b也是分类，则按照e*b种聚合,对分组的每一个变量调用taply
aggregate(.~c,x,mean)#除c以外的列按照c分组求均值
aggregate(cbind(a,b)~e,data1,mean)#要聚合两列一定要先组合
library(plyr)
aggregate(a~e,data1,each(mean,sd))#可以用多个函数

cov(x)#协方差矩阵，书上为什么不用corr？
fit=manova(x~x1)#x2,x3,x4受x1影响
summary(fit)
summary.aov(fit)#输出单变量结果，既每个因变量是否受x1影响
#若不满足上述条件，或有离群点，可以用稳健或非参数版本的manova
library(rrcov)
Wilks.test(x,x1,method='mcd')
#################################################################################################
#功效分析
#系统自带power.X.test
#样本数450，均值差0.5（真实差异，不是样本差异），标准差2（真实标准差），显著性水平0.01，功效0.9
#我们用4个求另一个，例如求n，要多大的样本才能使我的试验有0.9的概率均值差为0.5不显著（接受0.5的差异不犯错的概率为0.9）
power.t.test(n=450,delta = 0.5,sd=2,sig.level = 0.01,power = 0.9,type = 'paired或one.sample')
power.prop.test(n=450,p1=0.2,p2=0.3,sig.level = 0.01,power = 0.9，alternative='greater')#同上，p1,p2代替差和标准差，求缺一，求n多大两者存在差异
power.prop.anova
#pwr包，画图要做循环求出不同的n对应的power，然后点图
library(pwr)
pwr.t.test(n=450,d = 0.5,sig.level = ，power= ,type='two.sample',alternative='two.sided'或less或greater)#d为效应值，标准化的均值差（delta/sd)
pwr.anova.test(n=450,k= 组数,f=效应值,sig.level =)
pwr.r.test(n=450,r = 效应值,sig.level = ，power= ,type=)
pwr.2p.test((n=450,h = 效应值,sig.level = ，power= ,type=))#比例，h为两个比例算出
#################################################################################################
#主成分和因子分析
#主成分：因子=原始变量的线性组合,和spss结果一样
data1=read.csv('C:/Users/hasee/Desktop/rstudy/try.csv')
library(psych)
#生成碎石图，10次模拟,虚线为特征值均值,,建议多少个因子(特征根大于0),多少成分(特征根大于1,后面成分方差解释率太低)
fa.parallel(data1,n.iter=10,show.legend=TRUE,fa='pc',main='main')
pc=principal(data1,nfactors = 3,rotate = 'none',scores = TRUE)#只选3个因子的未旋转成分矩阵,实际上是对相关系数矩阵进行分析，输出h2为主成分对每个变量的方差解释度，u2为未解释
fa.varimax=principal(data1,nfactors = 2,rotate = 'varimax',scores = TRUE)#去噪更易解释，正交旋转，方差极大，旋转后为回归表达式，pc1=0.99y+0.90x1+0.98x2+...，promax为斜交
pc$scores#得分存储在scores中，就是每个样本的原始数据代入主成分得分方程中
pc$weights#得分矩阵
#因子分析：原始变量=因子的线性组合
fa.parallel(data1,n.iter=10,show.legend=TRUE,fa='fa',main='main')#默认fa='both’，因子分析特征根要求大于0而不是1，看图中红线拐点判定数量
fa(r,nfactors=3,rotate='none',fm='pa')#r可以使相关系数矩阵也可以是原始数据，fm为因子提取方法ml最大似然(统计学家青睐，可能会不收敛)、pa主轴迭代、wls最小二乘
factor.plot(fa.varimax,labels = rownames(data1))#载荷图
fa.diagram(fa.varimax,simple = FALSE)

#################################################################################################
#时间序列
as.Date('1-1-70',format='%m-%d-%y')#format根据字符格式写,大写对应全称
as.Date('January 15, 2015',format='%B %d, %Y')
months(as.Date('1-1-70',format='%m-%d-%y'))#返回中文,weekdays周几
a=sort(bylw2013$财政性教育经费)#生成一个趋势性时间序列
acf(a);pacf(a);diff(a,differences = 1)#系统自带,自相关，偏自相关，差分，靠看的不准
library(forecast)
ndiffs(a)#自动判定差分阶数
a_best=auto.arima(a)#依据AIC全自动定阶(p,q是在几阶开始收敛)
acf(a_best$residuals);pacf(a_best$residuals);coef(a_best)#查看函数拟合后的acf和pacf,ar、ma系数，
predict(a_best,n.ahead = 5,se.fit = TRUE)
plot(forecast(object = a_best,h=5))#绘制预测图，阴影为置信区间

#向量自回归：多个时间序列互相影响
bylw2013_ts=ts(data=bylw2013[,-1],start=min(bylw2013$年份),end=max(bylw2013$年份))#把数据变为时间序列,假设有一列变量是年份
n_diff=ndiffs(bylw2013_ts)#求差分阶数
bylw2013_diff=diff(bylw2013_ts,differences=n_diff)#做差分
library(vars)
bylw2013_var=VAR(bylw2013_diff,lag.max=12)#本质是对每个时间序列关于自身和其他序列的过去值线性拟合
predict(bylw2013_var,n.ahead = 5,se.fit = TRUE)
library(coefplot)
coefplot(bylw2013_var$生均教育经费)#画相关系数图
#################################################################################################
#对应分析,基于列联表频数数
pp=data.frame()
pp[1:100,1]=sample(1:3,100,replace = TRUE)#抽样,生成问卷随机数round(runif(100,0.5,3.5))也可以
pp[1:100,2]=sample(1:3,100,replace = TRUE)
qq=table(pp)
rownames(qq)=paste("R",1:5,sep='')#如果是矩阵可以直接dimnames(pp)=list(paste("R",1:5),paste("C",1:6))
colnames(qq)=paste("C",1:6,sep='')

choose(n,k) #组合数
factorial(k)#阶乘，配合组合就是排列
combn(5,2)#组合矩阵


library(MASS)#最原始的包
pp_dim=corresp(qq[,],nf=2)#不识别table，要转为矩阵或者数据框
plot(pp_dim)

library(ca)#较为先进的包,识别table,还可以不是整数(直接分析二维表的关系)，spss必须是原始数据
pp_dim1=ca(qq)#简单对应分析
plot(pp_dim1)
pp=data.frame()#多重对应分析和联合对应分析
pp[1:100,1]=round(runif(100,1,5))
pp[1:100,2]=round(runif(100,1,6))
pp[1:100,3]=round(runif(100,1,7))
qq=table(pp)
pp_dim2=mjca(qq)
plot(pp_dim2)

#例子
bylw=read.csv('C:/Users/hasee/Desktop/rstudy/bylw2013.csv',stringsAsFactors = FALSE,header = TRUE)
bylw2013=bylw[,-c(1,2)]
rownames(bylw2013)=bylw[,1]
bylw_ca=ca(bylw2013[,-7])
plot(bylw_ca)

bylw2013_1=data.frame(scale(bylw2013,center = T,scale = T))
bylw_ca1=ca(bylw2013_1)
plot(bylw_ca)

ranre=read.csv('ranre.csv')
ranre1=ranre[,-c(1,2,3)]
rownames(ranre1)=ranre[,3]
ranre_ca=ca(ranre1)
shuxing=ranre_ca$rowcoord
shuxing=as.data.frame(shuxing)
pinpai=ranre_ca$colcoord
pinpai=as.data.frame(pinpai)
ggplot()+
  geom_point(data=shuxing,color='blue',alpha=0.7,aes(x=shuxing[,1],y=shuxing[,2]))+
  geom_text(data=shuxing,color='blue',vjust=-1,alpha=0.7,size=3,
            aes(x=shuxing[,1],y=shuxing[,2],label=rownames(shuxing)))+
  geom_point(data=pinpai,color='red',alpha=0.7,aes(x=pinpai[,1],y=pinpai[,2]))+
  geom_text(data=pinpai,color='red',vjust=-1,alpha=0.7,size=3,
            aes(x=pinpai[,1],y=pinpai[,2],label=rownames(pinpai)))+
  geom_hline(aes(yintercept=0),linetype=2)+geom_vline(aes(xintercept=0),linetype=2)+
  xlab('第一维度')+ylab('第二维度')+
  theme(axis.text=element_blank(), #去刻度数字
        axis.ticks=element_blank(), #去刻度点
        panel.background=element_blank()) #去背景
#################################################################################################






