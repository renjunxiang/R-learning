#编写应用于R软件的GM(1,1)模型
#评价标准：
#                   a相对残差 b残差 C均方差比值 P小误差概率
#        一级(好)     0.01    0.10     0.35        0.95
#        二级(合格)   0.05    0.20     0.50        0.80
#        三级(勉强)   0.10             0.65        0.70
#        四级(不合格) 0.20             0.80        0.60

gm11=function(x0,t){ #x0为输入学列，t为预测个数
  x1=cumsum(x0) #一次累加生成序列1-AG0序列
  b=numeric(length(x0)-1)
  n=length(x0)-1
  for(i in 1:n){ #生成x1的紧邻均值生成序列
    b[i]=-(x1[i]+x1[i+1])/2 
    b} #得序列b，即为x1的紧邻均值生成序列
  D=numeric(length(x0)-1)
  D[]=1
  B=cbind(b,D)
  BT=t(B)#做逆矩阵
  M=solve(BT%*%B)
  YN=numeric(length(x0)-1)
  YN=x0[2:length(x0)]
  alpha=M%*%BT%*%YN  #模型的最小二乘估计参数列满足alpha尖
  alpha2=matrix(alpha,ncol=1)
  a=alpha2[1]
  u=alpha2[2]
  cat("GM(1,1)参数估计值：",'\n',"发展系数-a=",-a,"  ","灰色作用量u=",u,'\n','\n') #利用最小二乘法求得参数估计值a,u
  y=numeric(length(c(1:t)))
  y[1]=x1[1]
  for(w in 1:(t-1)){  #将a,u的估计值代入时间响应序列函数计算x1拟合序列y
    y[w+1]=(x1[1]-u/a)*exp(-a*w)+u/a 
  }
  cat("x(1)的模拟值：",'\n',y,'\n')
  xy=numeric(length(y))
  xy[1]=y[1]
  for(o in 2:t){ #运用后减运算还原得模型输入序列x0预测序列
    xy[o]=y[o]-y[o-1] 
  } 
  cat("x(0)的模拟值：",'\n',xy,'\n','\n')                       
  
  #计算残差e
  e=numeric(length(x0))
  for(l in 1:length(x0)){
    e[l]=x0[l]-xy[l] #得残差
  }
  cat("残差：",'\n',e,'\n')
  #计算相对误差
  e2=numeric(length(x0))
  for(s in 1:length(x0)){
    e2[s]=(abs(e[s])/x0[s]) #得相对误差
  }
  cat("相对残差：",'\n',e2,'\n','\n')
  cat("残差平方和=",sum(e^2),'\n')
  cat("平均相对误差=",sum(e2)/(length(e2)-1)*100,"%",'\n')
  cat("相对精度=",(1-(sum(e2)/(length(e2)-1)))*100,"%",'\n','\n')
  
  #后验差比值检验
  avge=mean(abs(e));esum=sum((abs(e)-avge)^2);evar=esum/(length(e)-1);se=sqrt(evar)  #计算残差的方差se
  avgx0=mean(x0);x0sum=sum((x0-avgx0)^2);x0var=x0sum/(length(x0));sx=sqrt(x0var)  #计算原序列x0的方差sx
  cv=se/sx  #得验差比值
  cat("后验差比值检验:",'\n',"C值=",cv,'\n')#对后验差比值进行检验，与一般标准进行比较判断预测结果好坏。
  if(cv < 0.35){     
    cat("C值<0.35, GM(1,1)预测精度等级为：好",'\n','\n')
  }else{
    if(cv<0.5){
      cat("C值属于[0.35,0.5), GM(1,1)模型预测精度等级为：合格",'\n','\n')
    }else{
      if(cv<0.65){
        cat("C值属于[0.5,0.65), GM(1,1)模型预测精度等级为：勉强合格",'\n','\n')
      }else{
        cat("C值>=0.65, GM(1,1)模型预测精度等级为：不合格",'\n','\n')
      }
    }
  }
  #画出输入序列x0的预测序列及x0的比较图像
  par(mar=c(6,6,6,6))
  plot(xy,col='blue',type='b',pch=16,xlab='时间序列',ylab='值')
  points(x0,col='red',type='b',pch=4)
  legend('topright',c('预测价格','原始价格'),pch=c(16,4),lty=l,col=c('blue','red'))
}


a=c(1.95,2.23,2.4,2.15,1.8,1.95)

gm11(a,length(a)+6)