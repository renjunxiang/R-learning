#mysql应用
library(RODBC)
channel=odbcConnect('mysqlodbc',uid='root',pwd='')#打开一个到DSN为my_dsn的数据库
sqlTables(channel)#查看表
help_topic=sqlFetch(channel,"help_topic")#查询某个表中的数据返回给数据框
name=sqlQuery(channel,'select name from help_topic where help_topic_id<20',stringsAsFactors = FALSE)#条件查询
sqlSave(channel, name, rownames = "name_id", addPK = TRUE)#将数据框中的数据保存到数据库表中,表名为数据框名称
sqlDrop(channel,"name_id")#将刚才添加到mysql数据库中的表删除掉
odbcClose(channel)#关闭连接资源

library(RMySQL)
conn <- dbConnect(MySQL(), dbname = "rmysql", username="rmysql", password="rmysql",
                  client.flag=CLIENT_MULTI_STATEMENTS)#建立本地连接
conn <- dbConnect(MySQL(), dbname = "rmysql", username="rmysql", password="rmysql",
                  host="192.168.1.201",port=3306)#建立远程连接
dbListTables(conn)#查看数据库的表
dbListFields(conn, "t_user")#查看表的字段,表名t_user
dbReadTable(conn, "t_demo")# 获得整个表数据
d0 <- dbGetQuery(conn, "SELECT * FROM t_demo where c>0")#查询数据,为data.frame
dbWriteTable(conn, "t_demo", t_demo)#建表并插入数据
dbWriteTable(conn, "t_demo", t_demo, append=TRUE)#插入新数据
dbDisconnect(conn)
#################################################################################################
#存取table/csv(table的特例)
teacher=read.csv('d:/rstudy/合肥教师问卷.csv')
remove(teacher)#删除变量
data1=read.csv('C:/Users/hasee/Desktop/rstudy/try.csv',
               stringsAsFactors = FALSE,header = TRUE)#默认T,阻止字符型变量变为因子型
data1=read.csv('try.csv',stringsAsFactors = FALSE,
               header=FALSE,col.names=c('a','b'))#首行不作为列名或者即使设定了也会被重新覆盖
write.csv(data1,file = 'try.csv')#写入

pp=readHTMLTable(url)#读取网页上的所有表格

data1=read.table('C:/Users/hasee/Desktop/rstudy/try.csv',header = TRUE,sep=',')
write.table(aaaa,'C:/Users/hasee/Desktop/rstudy/aaaa.csv',sep=",",row.names=FALSE)#保存成表格形式
read.table(aaaa,'C:/Users/hasee/Desktop/rstudy/aaaa.csv',dec='.分隔小数位',col.names=c('变量名'),na.strings=c('什么字符解释为缺失'))

#存为pdf
pdf('aaa.pdf')
plot(rnorm(100))
plot(rnorm(1000))
dev.off()

#存取r格式
data1=1:2
data2=3:4
data3=5:6
data4=7:8
save(data1,data2,file='~/data12.RData')#保存的值,保存的路径(在当前工作目录下)
save(list = c('data3','data4'),file='~/data34.RData')#list保存的变量名,和上面的等价
rm(list = ls())
load(file='~/data12.RData')#导入data1和data2
load(file='~/data34.RData')

x <- stats::runif(20)
y <- list(a = 1, b = TRUE, c = "oops")
save(x, y, file = "xy.RData")
rm(list = ls())
load("xy.RData")


#读取统计软件数据
library(foreign)
read.spss()
read.dta()

library(Hmisc) #更好
data=spss.get("**.sav")

library(memisc)#中文兼容性好
data=as.data.set(spss.system.file("D:/test.sav"))

library(xlsx)
file='C:/Users/hasee/Desktop/rstudy/try.xlsx'
read.xlsx(file,sheetName="Sheet1",as.data.frame=TRUE, header=TRUE)#也可以sheetIndex=2
write.xlsx(x, file, sheetName="Sheet1",
           col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE)#showNA=false则存为空格

library(XML)
theURL='一段网址'
data=readHTMLTable(theURL,which='第几个',header=FALSE,stringsAsFactors = FALSE)


