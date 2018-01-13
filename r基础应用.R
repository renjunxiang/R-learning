#系统应用
#################################################################################################
list.files('C:/Users/Xiong_Intern/Desktop/raw data for all cities')#显示文件下内所有文件
Sys.time( )#系统时间，可以用于测试

mm=function(x,y){
  return(x^y)
}
system.time(mm(4,5))#更直接的测试

Rprof(filename = "D:/rstudy/Rprof.out",append = TRUE)#更详细的测试
mm=function(x,y){
  return(x^y)
}
mm(4,5)
a=0
for(i in 1:100){
  a=a+i
}
Rprof()
summaryRprof("D:/rstudy/Rprof.out")
#################################################################################################
#包相关用法
getOption('defaultPackages')#查看默认加载的包
(.packages())#查看已加载的包,(.packages(all.available = TRUE))会显示全部包

install.packages('xlsx')#安装包
remove.packages('xlsx')#卸载包
library(devtools)
install_github('ggplot2')#devtools包含若干其他来源用以安装包

library('包名')#载入
detach('package::包名')#移除
arm::coefplot(object)#当几个包有同名函数，用包名::函数名()调用，同时可以调用未加载包

ls('package:')#查看包内函数
help('lm')#等价于?'lm'
help(package=MASS)#查询包信息
help.search('normal')#查找和normal有关的函数，等价于??'normal'
example(lm)#会把例子的数据和代码全部导入
#################################################################################################
#工作环境
getwd()#工作目录
setwd('C:/Users/hasee/Desktop/rstudy')#设置工作目录

assign('xx',1)#创建变量xx,值为1
get('xx')#获取叫xx的变量值
exists('xx')#判断是否存在叫xx的变量
objects()#返回全部变量名称
rm(list = ls())#清空变量

x=.GlobalEnv#定义一个全局环境
x<<-1#会递归的在父环境找到x赋值为1，若找不到再往上直到全局定义
#################################################################################################
x=try({mean()},silent = TRUE)#silent = TRUE会把返回值/错误写进
x=tryCatch(sd(),error = function(e) e, finally = print("Hello"))
#################################################################################################
sink("study.txt") #保存sink间的输出输出
mean(1:10)
sink()
#################################################################################################
#数据类型
#is判断
is.numeric(teacher)#判断是否为数值
is.integer()#是否为整数
is.na()#是否为缺失值

class(1)#查看数据的类,numeric
typeof(1)#查看数据的类型,double

data=data.frame(a=1:4,b=c(1,2,NaN,4))
any((1:10)>2)#对应all((1:10)>2)全部为true才为true,all通常用来做整体判断
complete.cases(data)#输出元素是否全不缺失的true和false,complete.cases(c(1,2,NaN))

#列表
a=list('a1'=1,'a2'=2)#列表里的元素也可以加索引,第一个元素的名字为a1、值1
a$a1

#赋值
`1+1`=2#反引号用于转义,变量叫做1+1
#################################################################################################
#索引操作
order(x)#从小到大排，返回的是原先的索引，x[order(x)]等价于sort(x)
seq(from=12,to=30,by=3)#生成序列
x=matrix(1:10,2,5,byrow=TRUE)#横着排，默认竖着，做加减等也是竖着开始,x[2]=2,x[2,2]=7
seq(x)#等价于1:length(x)
subset(x,x>0)#可以忽略NA，优于x[x>0];which(x>0,arr.ind=TRUE)返回符合条件的位置,arr适用于矩阵返回行列坐标否则视为向量
r=data1[1,,drop=FALSE]#可以防止降维成向量，as.matrix可以把向量转为矩阵

x=1:10#等价于x=c(1:10)等价于seq(1,10,1)等价于seq(length=5,from=1,by=1),是一个向量,可以做算术,等长向量可以相互运算
c(1:2,3:4,c(5,NaN))#会展平
x='aaa'#字符型
nchar(data1)#字符长度,这里给出的是每个变量拥有的总字符数，对factor类型无效
length(data$a)#数据长度(数据框为变量数,length(data1)=5),用nrow(data1)行数ncol(data1)列数更好,dim()输出行列数
x=factor('aaa')#设为因子型
x=as.Date('2012-06-28')#仅储存日期
x=as.POSIXct('2012-06-28 17:42')#还存储时间
x=(2>3)#逻辑值，t=1，f=0,这里x+1=1

dim(x)=c(2,5)#2x5的矩阵，等价于x=matrix(1:10,2,5,byrow=TRUE)
colnames(x)=c('a','b','c','d','e')
rownames(x)=c('a','b')
a=array(1:24,dim=c(4,3,2))

x=c('a'=1,'b'=2)#1,2的索引分别为'a','b'
gl(2,5,labels=c('m','fm'))#生成f,f,f,f,f,m,m,m,m,m,有些类似as.factor()
y=rnorm(10)
z=data.frame(x,y)

x=cut(x,breaks=c(1,3,5,Inf),
      labels=c('小','中','大'),#连续变量转为离散,刻度间为一个集合,加名称
      right=FALSE)#默认左开右闭设为false左闭右开
x=factor(x,levels=c('a','b','c'))#设置因子顺序,也可用于添加因子，后面不能添加非因子值
x=revalue(x,c('a'='aa','b'='bb','c'='cc'))#因子重命名
data1$e=reorder(data1$e,data1$a,FUN = mean)#改变e的因子顺序，依据分组a列的均值大小

x1=rep(c('a','b'),c(5,5))#生成aaaaabbbbb,rep(c('a','b'),5)则为ababababab
x1[c(2,3)]#第2,3个

y1=rep(c('a','b'),c(5,5))
z1=data.frame(x1,y1,stringsAsFactors = FALSE)#不把字符向量转为因子
save(data1,z1,file = 'mydata.RData')#保存为r格式
load('mydata.RData')#读取r格式

data1[c(1,2),c(2,3)]=data1[1:2,2:3]=data[,c('b','c')]#标逗号比较标准
z=data.frame(x,yy=y)#第二列的列名为yy
z$yy=NULL#删除'yy'列,null可以用于删除列表等元素
data1[a=c(1,2),b=c(2,3)]#设置列名
names(data1)#访问标签此处为列名,names(data1)=c('a1','a2','a3','a4','a5')列重命名
nrow(data1)#行数
ncol(data1)#列数
dim(data1)#行列数
head(data1,n=7)#显示前7行
tail(data1,n-7)#尾7行
rownames(data1)#显示行索引，rownames(data1)=c()可以用向量赋值
z[2,3]#数据框z第二行第三列，z[2]第二列，z[1:2]第1,2列,$输出数组，[]输出数据框切片
z['x']#z的'x'列(数据框展示），等价于z$x（向量展示）
sort(x)#排序
aggregate(x,by=list(x1),FUN=mean) #列出组均值,一定要list
aggregate(a~e,data1,mean)#aggregate(a~e+b,data1,mean),如果b也是分类，则按照e*b种聚合
aggregate(cbind(a,b)~e,data1,mean)#要聚合两列一定要先组合

a=list(1:10);unlist(a)#把列表转为数字或字符串向量,不同于as.array

#################################################################################################
#dataframe操作

a=data.frame(p=1:4,q=5:8)
b=data.frame(p=1,q=2)
a-matrix(b)#a每行减去b,行数不一样不能直接减,不能matrix(a)-matrix(b),不能a-1:2会竖着减
#数据长宽变换
library(reshape2)
data1=data.frame(a=1:10,b=2:11,c=3:12,d=4:13,e=5:14)
dcast(data1,a+b+c~e,value.var='d')#数据集，保留变量~标识变量,表示变量填充值,没有的未na
dcast(diamonds,carat+clarity+color~cut,value.var='price')#长变宽，这个数据

melt(data1,id.vars=c('a','e'),variable.name='b_c_d',
     value.name='b_c_d_value')#宽变长，数据集，保留的变量，新增标识变量名，新增填充数值

#数据清洗，剔除重复值
data_yonghu=data_yonghu[order(data_yonghu$tm_encode),]#按照tm_encode排序
ddply(data_yonghu,.variables='user_id', summarise,sort(tm_encode))#另一种排序方法
data_yonghu_clear=unique(data_yonghu,fromLast=TRUE)#剔除重复值，取最后一个

index=duplicated(data_yonghu$user_id)#第二个重复的标记t
data_yonghu[!index,]#!index则为第一个的f，首个非重复

data1[,1][data1[,5]==3]=888#批量替换，等价于data1$a[data1$e==3]=999
x=data.frame();x[1,1]=2;x[1,2]=3#声明变量类型然后赋值,但会占用内存，可以用x=vector(length=5)事先分配5个长度的内存
z=vector(mode='list');z$abc=2#填充，也可以z[['abc']]=2,要双中括号;z[1]输出第一个全部信息(返回新列表)，z[[1]]只输出值

#删除缺失值
pp=na.omit(data1)#只保留不含任何缺失值的样本
pp=data1[complete.cases(data1),]#只保留每一行都不是空的样本，和上面等价

#合并
x1=c(1,2)
x2=c(3,4)
x3=c(5,6)
x4=data.frame(m=7:8,n=9:10)
x=cbind(x1,x2,x3,x4)#行数相同时纵向合并，每个向量一列,增加列数
y=rbind(x1,x2,x3,x4)#列数相同时横向合并,每个向量一行,增加行数

#转换(类似管道)
transform(`x`,p=m+n,q=m*10)

merge(x=data1,y=date2,
      by.x=c('a','b'),by.y=c('a','b'))#缺点速度慢
library(plyr)
join(x=data1,y=date2,
     by=c('a','b'))#缺点列名要相同,类型为type = "left (default), right, inner or full."

library(dummies)
dummy()
dummy.data.frame()#所有变量均创建亚变量并重构数据框
#处理字符
library(stringr)
str_replace(string=data1,pattern='要替换字符',replacement='替换内容')#替换首字母,\\d表示数字
str_replace_all(string=data1,pattern='要替换字符',replacement='替换内容')#替换全部
#########################################################################################
#循环
for (i in 1:10) {
  if (i==3)
  {
    next#用于跳出循环,即i=3不输出,跳到最开始从i=4继续
  }
  print('hello')
}
#####################
i=1
while (i<10) {
  print(i);
  i=i+1
}
#####################
i=1
repeat{
  i=i+2
  if (i>6) break  #repeat必须用break设定结束条件
}
#####################
#r不支持非向量的循环，要用get
u=matrix(1:8,ncol=2)
v=matrix(9:16,ncol=2) #pmin(u[,1],u[,2])输出每一行的最小值
for (i in c('u','v')){
  z=get(i)
  print(lm(z[,2]~z[,1]))
}
#########################################################################################
#条件,switch不常用
ifelse(a>1,print('>1'),print('<=1'))#本质true替换为>1,false替换为<=1

if(i>1) {print('>1')
} else { #标大括号更好读，不标也可以,注意括号的断句位置
  print('<=1')
}

use_switch=function(x){#x='a'输出first...
  switch (x,
    'a'='first',
    'b'='second',
    'not a or b'
  )
}
centre=function(x, type) {
  switch(type,
         mean = mean(x),
         median = median(x),
         trimmed = mean(x, trim = .1))
}
#########################################################################################
#函数
cov=function(x,...){
  print(summary(...))
  return(mean(x)/sd(x))
}#变异系数,...代表可变参数,...会作为参数全部进入summary
cov(1:100,1:10,digits=2)

m=function(x) x+1#简单写法类似m=lambda x:x+1

stat=function(x,func=mean){
  do.call(func,args=list(x))#构建如包一样的可选参数的函数
}

hello=function(x='jim'){#(x='jim',...)会忽略使用时多输的参数，避免报错
  print(sprintf('hello %s,nice to meet you!',x))
}#sprintf类似于py里面的print函数带'%s'%()

assign('变量名',10)
paste('pp','qq',sep='')#连接字符
#################################################################################################

#字符串操作
grep('搜索内容',c('被搜索内容','aaa','返回的是包含搜索内容字符串的索引'))#返回1,3
grepl('搜索内容',c('被搜索内容','aaa','返回的是返回所有的查询结果'))#返回tff
nchar('返回字符串长度')#中文也是1

substr('返回字符串的指定位置',3,5)#3:5的内容
substr(c("123456789", "abcdefghijklmnopq"),3,5)#返回两个

strsplit('依据-格式-拆分',split='-')

regexpr('搜索内容','返回被搜索内容中首次出现搜索内容的位置,此处返回4')
gregexpr('搜索内容','返回被搜索内容中所有出现搜索内容的位置,此处返回4,13')

sub(pattern="a", replacement="b", 'aacc')#首次替换,bacc,不改变原始文档
gsub(pattern="a", replacement="b", 'aacc')#全部替换,bbcc,不改变原始文档
#################################################################################################
#正则表达式
grep('[搜索内容]',c('被搜索内容','搜索','返回的是包含搜索内容中任一字符的索引'))#返回1,2,3
grep('搜.内容',c('被搜索内容,.代表任一字符','搜索','返回的是包含搜索内容中任一字符的索引'))#返回1,3
grep('\\.内容',c('被搜索.内容','搜索','返回的是包含\'.内容\'的索引'))#返回13,r中转义需要两个反斜杠
grep('[\\(\\)]',c('1','(abc)','(5'))#返回23
grep('[0-9]',c('1','abc','5'))
grep('[^0-9]',c('1','abc','5'))#^非
grep('12+',c('12','1','122','1212'))#1和若干个2，134
grep('12*',c('12','1','122','1212'))#1和大于等于0个2,1234
grep('12?',c('12','1','122','1212'))#1和0或1个2，1234
#################################################################################################
#类的应用
#S3的写法,避免使用
setClass(
  'timeseries',
  representation(
    start_time = 'POSIXct',
    end_time = 'POSIXct',
    time_delta = 'numeric'
  )
)
#S4写法
setClass(
  'timeseries',
  slots = c(
    start_time = 'POSIXct',
    end_time = 'POSIXct',
    time_delta = 'numeric'
  ),#定义参数类型
  prototype = list(
    start_time = as.POSIXct('2000/01/20 00:00:00'),
    end_time = as.POSIXct('2000/01/25 00:00:00'),
    time_delta = 1:5
  )#定义默认值
)
my_timeseries=new('timeseries')
my_timeseries=new('timeseries',
                  start_time=as.POSIXct('2007/01/20 00:00:00'),
                  end_time=as.POSIXct('2007/01/25 00:00:00'),
                  time_delta=1:10)

my_timeseries
my_timeseries@time_delta
#my_timeseries$time_delta#S4不可用









