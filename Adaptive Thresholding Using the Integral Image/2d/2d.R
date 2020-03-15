library('jpeg')
setwd("F:\\Desktop\\")
pict<-readJPEG("example.jpg")
w<-length(pict[,1])
h<-length(pict[1,])
out<-matrix(NA,w,h)
pic<-matrix(NA,w,h)
s<-h/8
t<-15
for (j in 1:h) #积分
{
  sum<-0
  for (i in 1:w)
  {
    sum<-sum+pict[i,j]
    if (j==1)
      pic[i,j]<-sum
    else
      pic[i,j]<-pic[i,j-1]+sum
  }
}

for (j in 1:h)
{
  for (i in 1:w)
  {
    x1 = max(j-floor(s/2), 2)
    x2 = min(j+floor(s/2), h)
    y1 = max(i-floor(s/2), 2)
    y2 = min(i+floor(s/2), w)
    count<-(x2-x1)*(y2-y1)
    sum = pic[y2,x2] - pic[(y1-1),x2] - pic[y2,(x1-1)] + pic[(y1-1),(x1-1)]
    if (pict[i,j]*count <= sum*(100-t)/100)
      out[i,j] <- 255 #black
    else
      out[i,j] <- 999 #white
  }
}

writeJPEG(out,'out.jpg')


