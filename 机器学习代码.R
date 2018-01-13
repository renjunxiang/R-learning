teacher=read.csv('d:/rstudy/合肥教师问卷.csv')

#lasso回归，用于数据清洗中选择重要变量，适合广义线性模型，因变量不能因子型
library(car)#正态性通过残差正态检验即可
vif(lm(e~.,data=data1))#查看共线性情况决定是否要lasso
#先将数据转化成矩阵,lars函数值用于矩阵型数据
x=as.matrix(data1[,1:4])
y=as.matrix(data1[,5])
library(lars)
la=lars(x,y,type="lar")
plot(la)
laa=summary(la)#cp类似AIC、BIC，越小越好
la#里面有优先顺序，依据cp值确定变量数,在la$actions和la$entry


#KNN算法
library(class)
p=knn(data1[1:80,1:4],data1[81:100,1:4],data1[1:80,5],k,prob=TRUE)#k表示对每个样本，标识k个近邻(使用奇数可以减少票数相等情况)，可以做循环测试最佳k值,标签可以用factor
library(gmodels)
CrossTable(test_label,pred,prop.chisq=FALSE)#用列联表展示判别矩阵

#系统聚类(分层聚类)
kkk=hclust(d=dist(data1),method='average')#联结方法用均值最常见
plot(kkk)
rect.hclust(kkk,k=3,border='red')#分成三组,在原图会标记
rect.hclust(kkk,h=2,border='red')#按高度分组

#kmeans聚类,对异常值敏感
KK=kmeans(x=data1,centers=2)#KK显示各类数量、均值、样本的分类和相似度量，聚类数应该用后面的方法确定
library(useful)#基于ggplot绘图
plot(KK,data=data1,class='e')#将数据投影到二维，如果有一个期望分类e，可以观察实际分类和期望的差别
k_best=FitKMeans(data1,max.clusters=20,nstart =25,seed=10)#nstart算法启发次数(不懂),hartigan准则比较k和k+1类,系数大于10则选k+1
PlotHartigan(k_best)#hartigan系数折线图

library(cluster)
thrGap=clusGap(data1,FUNcluster=pam,K.max=20)#上面的准则可能聚类数过多，替代准则gap取最小,该方法bootsrap抽样比较聚内差异性
gapdf=as.data.frame(thrGap$Tab)#转为数据框
ggplot(gapdf,aes(x=1:nrow(gapdf)))+
  geom_line(aes(y=gap))+#折线
  geom_point(aes(y=gap))+#中心的点
  geom_errorbar(aes(ymin=gap-SE.sim,ymax=gap+SE.sim))#误差棒

#kmedoids聚类,对异常值稳健
library(cluster)
data_pam=pam(x=data1,k=2,keep.diss=TRUE,keep.data=TRUE)#keep.diss把异同放在结果，keep.data=f节约内存
plot(data_pam,which.plots=2)#每条线一个观测值，拟合好的观测值线长(反之短或负),类平均越宽类聚合越好


#朴素贝叶斯,主要用于文本分析
library(tm)
sms_corpus=Corpus(VectorSource(teacher[,1]))#VectorSource将文本传给Corpus，Corpus将信息存为r文件
inspect(sms_corpus)#查看语料库
corpus_clean=tm_map(sms_corpus,tolower)#全部转为小写(避免同一单词字体不同)
corpus_clean=tm_map(corpus_clean,removeNumbers)#剔除数字
corpus_clean=tm_map(corpus_clean,removeWords,stopwords())#剔除停用词(包自带)
corpus_clean=tm_map(corpus_clean,removePunctuation)#剔除标点
corpus_clean=tm_map(corpus_clean,stripWhitespace)#剔除多余空格，只保留词间空格
sms_dtm=DocumentTermMatrix(corpus_clean)#创建稀疏矩阵,不识别中文
findFreqTerms(sms_dtm,5)#找出频数大于5的词
sms_dict=Dictionary(findFreqTerms(sms_dtm,5))#保存频繁词
library(e1071)
m=naiveBayes(data1[1:80,1:4],as.factor(data1[1:80,5]),laplace=1)#标签要用因子否则p会出错，laplace默认0，拉普拉斯估计参数，可以改变来调整准确率
p=predict(m,data1[91:100,1:4],type='class')#type选项class返回类别，raw返回概率
#模型评估和KNN相似
library(wordcloud)#制作词云
wordcloud(teacher[,1],min.freq=40,max.word=40,random.order=FALSE,scale=c(1,0.1))#至少在40个样本中出现，最频繁的40个词,一般设为总样本的10%,scale最大最小字体

#C50决策树
library(C50)
#trial控制自助法循环次数默认1(数字实质就是adaboost的数量,10为行业标准),costs可选矩阵给出各类型错误成本,标签要转为因子,rules=TRUE则可以生成一个规则(默认f)
m=C5.0(data1[1:80,1:4],as.factor(data1[1:80,5]),trials=1,costs=NULL,rules=FALSE)#error=matrix(c(0,1,4,0),nrow=2),costs=error给出两种错误的代价
p=predict(m,data1[81:100,1:4],type='class')#prob为概率
summary(m)
plot(m)

#回归树,分类变为该分支下的均值,通过summary预测值和原始值可以大致了解预测情况，例如最小值没有被预测到
library(rpart.plot)
m1=rpart(data1[,5]~data1[,1]+data1[,2]+data1[,3]+data1[,4])#也可以用变量名,建议e~a+b+c+d,data=data1[1:80,]
p1=predict(m1,data1[91:100,1:4])
rpart.plot(m1,type=4,extra=101)#type、extra影响决策和节点标记方式(外观)
MAE=function(actual,predicted){
  mean(abs(actual-predicted))
}
MAE(p1,(data1[91:100,5]))#平均绝对离差,用于比较不同模型间的优劣

#模型树,用回归替代均值，优于回归树
library(RWeka)
m=M5P(teacher[,3]~teacher[,4]+teacher[,5]+teacher[,6]+teacher[,7]+teacher[,8])
plot(m)
#个人猜想，计算r方应该是通用的
mst=mean((y-mean(y))^2)
mse=mean((y-predicted)^2)
rr=1-mse/mst #计算r方

#分类规则算法(不能用，rjava没装好)
library(RWeka)
m=OneR(y_train~x1+x2+x3,data=mydata)
p=predict(m,x_test)
m=JRip(y_train~x1+x2+x3,data=mydata)#更精密的规则
p=predict(m,x_test)

#神经网络
#自带nnet单层的前向神经网络模型,size隐层结点数,decay表明权值是递减的(可以防止过拟合),skip是否允许跳过隐层,maxit最大迭代次数,rang初始随机权值[-0.1,0.1]
library(nnet)
m=nnet(formula,data, weights, size, Wts, linout = F, entropy = F,
       softmax = F, skip = F, rang = 0.1,decay = 0, maxit = 100,
       trace = T)
m=nnet(data1[1:80,1:4],data1[1:80,5],size=10,rang=0.1,decay=0,maxit=100)
p=predict(m,data1[81:100,1:4],type='class')#不知为何只输出1
#neuralnet包
library(neuralnet)
m=neuralnet(formula, data, hidden = 1, threshold = 0.01,        
            stepmax = 1e+05, rep = 1, startweights = NULL, 
            learningrate.limit = NULL, 
            learningrate.factor = list(minus = 0.5, plus = 1.2), 
            learningrate=NULL, lifesign = "none", 
            lifesign.step = 1000, algorithm = "rprop+", 
            err.fct = "sse", act.fct = "logistic", 
            linear.output = TRUE, exclude = NULL, 
            constant.weights = NULL, likelihood = FALSE)
plot.nn(x, rep = NULL, x.entry = NULL, x.out = NULL,
        radius = 0.15, arrow.length = 0.2, intercept = TRUE, 
        intercept.factor = 0.4, information = TRUE,
        information.pos = 0.1, col.entry.synapse = "black",
        col.entry = "black", col.hidden = "black",
        col.hidden.synapse = "black", col.out = "black",
        col.out.synapse = "black", col.intercept = "blue",
        fontsize = 12, dimension = 6, show.weights = TRUE,
        file = NULL, ...)
m=neuralnet(data1[,5]~data1[,1]+data1[,2]+data1[,3]+data1[,4],data=data1,hidden=1)#隐藏节点数1(hidden=c(5,3)第一层5第二层3),一定要data=data1,可以e~a+b+c+d,linear.output=TRUE为回归否则出类别
plot(m)#输出的圈1为偏差，箭头线为权重,error为SSE
p=compute(m,data1[91:100,1:4])#输出一个表格,net.result列为预测值,neurons为神经元,还原标准化用测试数据的均值方差,训练集最好不包括测试集
#否则训练集要是那一列相同,预测值也相同(已经被训练记忆);不包括的话预测值也会不同


#支持向量机
library(kernlab)
m=ksvm(e~a+b+c+d,data=data1[1:80,],kernel='rbfdot',type="C-bsvc",prob.model=TRUE)#kernel超平面函数(默认rbf高斯函数，也叫径向基，最常用),c违约惩罚(写这个参数会报错？)
p=predict(m,data1[91:100,1:4],type='probabilities')#type预测类型:response类别(训练要选C-svc,默认为插值),probabilities概率(m=ksvm(lable~.,data=data_train[,-1],kernel='rbfdot',type="C-bsvc"))
CrossTable(data1[91:100,5],p)

#关联算法
library(arules)
g=read.transactions('try.csv',sep=',')#创建事务型数据的稀疏矩阵
itemFrequency(g)#查看支持度
itemFrequencyPlot(g,support=0.1)#画出支持度不低于0.1的直方图,topN=20则画出从高到低20个
image(g)#交易图
image(sample(g,100))#随机抽样的100次交易
m=apriori(data=g,parameter=list(support=0.1,confidence=0.8,minlen=1))#minlen规则最低项数
inspect(g)#显示规则,通过inspect(sort(g,by='lift或者support或者confidence'))可以排序规则,lift指规则B-A的p(A|B)/p(A),表示一个放大比例
b=subset(g,items %in% 'berries')#存储包含berries的规则，%in%至少一项匹配，%pin%部分匹配(前后键含)，%ain%完全匹配(前后键就是这两个，要用c())  
write(g,file='g.csv',sep=',',quote=TRUE,row.names=FALSE)#存为csv

#k均值,未知情况下k=(n/2)^0.5
#数据清洗技巧
data1.m=ifelse(data1.a==1&!is.na(data1.a),1,0)#假设变量a代表性别且有缺失值
data1.na=ifelse(is.na(data1.a),1,0)#通过哑变量转换把缺失值提出来新一列,0、1、na就被转换为两列
ave_age=ave(data1.b,data1.c,FUN=function(x) meanx,na.rm=TRUE)#类似Python的apply用分组均值再生成一列
data.b=ifelse(is.na(data1.b),ave_age,data1.b)#再用均值分组均值填补缺失值，类似np.where
aggregate(data=data1,data1$a~data1$e,mean,na.rm=TRUE)#分组计算,类似group by
data1_z=as.data.frame(lapply(data1, scale))#数据标准化，得到矩阵通过frame转为数据框
m=kmeans(data1,centers=3)#m数据框重要的有centers每个质心坐标,size每个质心数量,cluster分类结果

#模型评估==========================================================================================================

#kappa
library(vcd)#只有在行列数一样才行，如果预测值某个不出现报错,要用factor(data1[91:100,5],levels=c(1,2,3))处理
p=factor(p,levels=c(1,2,3))
dd=factor(data1[91:100,5],levels=c(1,2,3))
Kappa(table(data1[91:100,5],p))#处理为Kappa(table(dd,p))
library(caret)
confusionMatrix(data1[91:100,5],p)#预测值某个不出现会警告,和factor处理后一样,为负；0-0.2很差,0.2-0.4尚可,0.4-0.6中等,0.6-0.8不错,0.8-1很好,左开右闭
#一般要设置positive=3为阳，左上真阴TN,右下真阳TP,灵敏度TP/(TP+FN),特异性TN/(TN+FP),精确度TP/(TP+FP),回溯精确TP/(TP+FN)就是recall

#ROC,真阳性比例要高(患者被测出比例)，假阳性要低(健康被当成患者比例)，ROC下面积称为AUC，1-0.9优秀,0.9-0.8良好,0.8-0.7一般,0.7-0.6很差,0.6-0.5无法区分,0.5-0没有价值
library(ROCR)
l=data1[81:100,5]
p=as.integer(p)
result=data.frame(p,l)#还是要放一起，一开始就应该这么做,p和l要同类型
pred=prediction(predictions=result$p,labels=result$l)
perf=performance(pred,measure='tpr',x.measure='fpr')#ROC曲线

plot(perf,main='ROC',col='blue',lwd=3)
abline(a=0,b=1,lwd=2,lty=2)#y=a+bx

perf.auc=performance(pred,measure='auc')
unlist(perf.auc@y.values)#消除列表结构

perf=performance(pred,measure='prec',x.measure='rec')#PR曲线

#kfold
folds=createFolds(data1$e,k=10)#返回一个数据框，列名为Fold01,...按照因变量等比例分隔
cv_results=lapply(folds,function(x){
  data1_train=data1[x,]
  data1_test=data1[-x,]
  data1_model=C5.0(e~a+b+c+d,data=data1_train)
  data1_pred=predict(data1_model,data1_test)
})

#参数调优
library(caret)
set.seed(300)#自选种子
m=train(data1[,1:4],as.factor(data1[,5]),data=data1,method='C5.0')
p=predict(m,data1[81:100,1:4])#会自动按照最佳参数的模型预测,加入type='prob'输出概率

#bagging
library(ipred)#bagging决策树
set.seed(300)
m=bagging(e~a+b+c+d,data=data1,nbagg=25)
p=predict(m,data1[81:100,1:4])

#randomForest
library(randomForest)
m=randomForest(data1[1:80,1:4],as.factor(data1[1:80,5]),ntree=500,mtry=sqrt(4),importance=TRUE)#mtry选择特征的数目,默认取开方,importance=TRUE可以增加#MeanDecreaseAccuracy
p=predict(m,data1[81:100,1:4],type='response')#type='response','prob','votes'投票矩阵
varImpPlot(m)#变量重要性,具体数值可以看importance(m)
#MeanDecreaseAccuracy描述的是当把一个变量变成随机数时,随机森林预测准确度的降低程度，该值越大表示该变量的重要性越大。
#MeanDecreaseGini通过基尼指数计算每个变量对分类树上每个节点的观测值的异质性影响。该值越大表示该变量的重要性越大。
print(m)#输出模型在训练集上的效果，可以看出错误率
library(party)#与randomForest包不同之处在于，party可以处理缺失值，而这个包可以
set.seed(42)
m=cforest(e~.,control = cforest_unbiased(mtry = 2, ntree = 50), data=data1[1:80,])
varimpt=data.frame(varimp(m))#varimp代表重要性函数
p=predict(m,data1[81:100,1:4],type='prob')#不知原因报错

#adaboost和bagging
library(adabag)
data1[,5]=as.factor(data1[,5])
m=boosting(e~.,data=data1[1:80,],type="class")#adaboost.M1,本质为GBDT
p=predict(m,data1[81:100,1:4],type="prob")
z0=table(data1[81:100,5],p$class)#p$prob为概率矩阵
E0=(sum(z0)-sum(diag(z0)))/sum(z0)#计算误差率
barplot(m$importance)#画出变量重要性图
m_b=errorevol(m,data1) #计算全体的误差演变
plot(m_b$error,type="l",main="AdaBoost error vs number of trees") #对误差演变进行画图

m=bagging(e~.,data=data1[1:80,]) #建立bagging分类模型
p=predict(m,data1[81:100,1:4])
z0=table(data1[81:100,5],p$class)
E0=(sum(z0)-sum(diag(z0)))/sum(z0)#计算误差率
barplot(m$importance)#画出变量重要性图



