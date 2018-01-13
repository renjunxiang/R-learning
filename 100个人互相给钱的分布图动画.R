library(ggplot2)
library(grid)
total=data.frame("id"=paste0('i',1:100),'money'=100)#初始化金钱=100

vplayout = function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
library(animation)
saveGIF({ani.options(interval=.1,
                     convert = shQuote('D:/Program Files/ImageMagick-7.0.6-Q16/convert.exe'),
                     movie.name = "animation3.gif")
  for(j in 1:200){
    for(i in 1:100){ 
      total[i,2]=total[i,2]-5 #一轮中第i个人把钱随机给其余的99人之一
      n=(1:100)[-i][sample(99,1)] #得到钱的人id=n
      total[n,2]=total[n,2]+5
    }
    p1=ggplot(data = total,aes(x=id,y=money))+
      geom_bar(stat = 'identity',width=0.3)+
      labs(title=paste0('round ',j))
    p2=ggplot(data = total,aes(x=reorder(id,money),y=money))+
      geom_bar(stat = 'identity',width=0.3)+
      labs(title=paste0('round ',j))
    
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(2, 1)))
    print(p1, vp = vplayout(1, 1))
    print(p2, vp = vplayout(2, 1))
    
  }
})

p1=ggplot(data = total,aes(x=id,y=money))+
  geom_bar(stat = 'identity',width=0.3)+
  labs(title=paste0('round ',j))
p2=ggplot(data = total,aes(x=reorder(id,money),y=money))+
  geom_bar(stat = 'identity',width=0.3)+
  labs(title=paste0('round ',j))
library(grid)
vplayout = function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 1)))
print(p1, vp = vplayout(1, 1))
print(p2, vp = vplayout(2, 1))
dev.off()
