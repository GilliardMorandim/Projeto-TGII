str(iris)
set.seed(9850)
install.packages("entropy")
library(entropy)
g<-runif(nrow(iris))
irisr <- iris[order(g),]
str(irisr)
bd<-irisr[1:100,]
bd
vet_for_sepalL<-seq(min(bd$Sepal.Length),max(bd$Sepal.Length),0.1)
p1<-as.numeric(length(bd[bd=='setosa',5]))
p2<-as.numeric(length(bd[bd=='versicolor',5]))
p3<-as.numeric(length(bd[bd=='virginica',5]))
entropia_pai<-entropy(c(p1,p2,p3))
GI<-rep(0,length(vet_for_sepalL))
n<-as.numeric(nrow(bd))
for (i in 1:length(vet_for_sepalL)){
  yf<-bd[bd$Sepal.Length<=vet_for_sepalL[i],5]
  p1_fl<-as.numeric(length(yf[yf=='setosa']))
  p2_fl<-as.numeric(length(yf[yf=='versicolor']))
  p3_fl<-as.numeric(length(yf[yf=='virginica']))
  yfr<-bd[bd$Sepal.Length>vet_for_sepalL[i],5]
  p1_fr<-as.numeric(length(yfr[yfr=='setosa']))
  p2_fr<-as.numeric(length(yfr[yfr=='versicolor']))
  p3_fr<-as.numeric(length(yfr[yfr=='virginica']))
  pl<-sum(c(p1_fl,p2_fl,p3_fl))/n
  pr<-sum(c(p1_fr,p2_fr,p3_fr))/n
  entropia_filho<-(pr*entropy(c(p1_fr,p2_fr,p3_fr))+pl*entropy(c(p1_fl,p2_fl,p3_fl)))
  GI[i]<-entropia_pai-entropia_filho
}
for (i in 1:(as.numeric(length(vet_for_sepalL))-1)){
  as.numeric(length(vet_for_sepalL))
  i_i=i*1
  j=i_i-1
  if (i==1){
    max_pos = 1
  }else if(GI[max_pos] < GI[i_i]){
    max_pos = i_i
  }
}
GI[max_pos]
#0.3460552#
vet_for_sepalL[max_pos]
#5.8
Sepal.Length

vet_for_sepalL<-seq(min(bd$Sepal.Width),max(bd$Sepal.Width),0.1)
p1<-as.numeric(length(bd[bd=='setosa',5]))
p2<-as.numeric(length(bd[bd=='versicolor',5]))
p3<-as.numeric(length(bd[bd=='virginica',5]))
entropia_pai<-entropy(c(p1,p2,p3))
GI<-rep(0,length(vet_for_sepalL))
n<-as.numeric(nrow(bd))
for (i in 1:length(vet_for_sepalL)){
  yf<-bd[bd$Sepal.Width<=vet_for_sepalL[i],5]
  p1_fl<-as.numeric(length(yf[yf=='setosa']))
  p2_fl<-as.numeric(length(yf[yf=='versicolor']))
  p3_fl<-as.numeric(length(yf[yf=='virginica']))
  yfr<-bd[bd$Sepal.Width>vet_for_sepalL[i],5]
  p1_fr<-as.numeric(length(yfr[yfr=='setosa']))
  p2_fr<-as.numeric(length(yfr[yfr=='versicolor']))
  p3_fr<-as.numeric(length(yfr[yfr=='virginica']))
  pl<-sum(c(p1_fl,p2_fl,p3_fl))/n
  pr<-sum(c(p1_fr,p2_fr,p3_fr))/n
  entropia_filho<-(pr*entropy(c(p1_fr,p2_fr,p3_fr))+pl*entropy(c(p1_fl,p2_fl,p3_fl)))
  GI[i]<-entropia_pai-entropia_filho
}
for (i in 1:(as.numeric(length(vet_for_sepalL))-1)){
  as.numeric(length(vet_for_sepalL))
  i_i=i*1
  j=i_i-1
  if (i==1){
    max_pos = 1
  }else if(GI[max_pos] < GI[i_i]){
    max_pos = i_i
  }
}
Sepal.Width
GI[max_pos]
#.2410113#
vet_for_sepalL[max_pos]
#3



vet_for_sepalL<-seq(min(bd$Petal.Length),max(bd$Petal.Length),0.1)
p1<-as.numeric(length(bd[bd=='setosa',5]))
p2<-as.numeric(length(bd[bd=='versicolor',5]))
p3<-as.numeric(length(bd[bd=='virginica',5]))
entropia_pai<-entropy(c(p1,p2,p3))
GI<-rep(0,length(vet_for_sepalL))
n<-as.numeric(nrow(bd))
for (i in 1:length(vet_for_sepalL)){
  yf<-bd[bd$Petal.Length<=vet_for_sepalL[i],5]
  p1_fl<-as.numeric(length(yf[yf=='setosa']))
  p2_fl<-as.numeric(length(yf[yf=='versicolor']))
  p3_fl<-as.numeric(length(yf[yf=='virginica']))
  yfr<-bd[bd$Petal.Length>vet_for_sepalL[i],5]
  p1_fr<-as.numeric(length(yfr[yfr=='setosa']))
  p2_fr<-as.numeric(length(yfr[yfr=='versicolor']))
  p3_fr<-as.numeric(length(yfr[yfr=='virginica']))
  pl<-sum(c(p1_fl,p2_fl,p3_fl))/n
  pr<-sum(c(p1_fr,p2_fr,p3_fr))/n
  entropia_filho<-(pr*entropy(c(p1_fr,p2_fr,p3_fr))+pl*entropy(c(p1_fl,p2_fl,p3_fl)))
  GI[i]<-entropia_pai-entropia_filho
}
for (i in 1:(as.numeric(length(vet_for_sepalL))-1)){
  as.numeric(length(vet_for_sepalL))
  i_i=i*1
  j=i_i-1
  if (i==1){
    max_pos = 1
  }else if(GI[max_pos] < GI[i_i]){
    max_pos = i_i
  }
}
GI[max_pos]
#.64010355
vet_for_sepalL[max_pos]
#1.9
Petal.Length



vet_for_sepalL<-seq(min(bd$Petal.Width),max(bd$Petal.Width),0.1)
p1<-as.numeric(length(bd[bd=='setosa',5]))
p2<-as.numeric(length(bd[bd=='versicolor',5]))
p3<-as.numeric(length(bd[bd=='virginica',5]))
entropia_pai<-entropy(c(p1,p2,p3))
GI<-rep(0,length(vet_for_sepalL))
n<-as.numeric(nrow(bd))
for (i in 1:length(vet_for_sepalL)){
  yf<-bd[bd$Petal.Width<=vet_for_sepalL[i],5]
  p1_fl<-as.numeric(length(yf[yf=='setosa']))
  p2_fl<-as.numeric(length(yf[yf=='versicolor']))
  p3_fl<-as.numeric(length(yf[yf=='virginica']))
  yfr<-bd[bd$Petal.Width>vet_for_sepalL[i],5]
  p1_fr<-as.numeric(length(yfr[yfr=='setosa']))
  p2_fr<-as.numeric(length(yfr[yfr=='versicolor']))
  p3_fr<-as.numeric(length(yfr[yfr=='virginica']))
  pl<-sum(c(p1_fl,p2_fl,p3_fl))/n
  pr<-sum(c(p1_fr,p2_fr,p3_fr))/n
  entropia_filho<-(pr*entropy(c(p1_fr,p2_fr,p3_fr))+pl*entropy(c(p1_fl,p2_fl,p3_fl)))
  GI[i]<-entropia_pai-entropia_filho
}
for (i in 1:(as.numeric(length(vet_for_sepalL))-1)){
  as.numeric(length(vet_for_sepalL))
  i_i=i*1
  j=i_i-1
  if (i==1){
    max_pos = 1
  }else if(GI[max_pos] < GI[i_i]){
    max_pos = i_i
  }
}
GI[max_pos]
plot(GI)
#0.6410355
vet_for_sepalL[max_pos]
#0.6
Petal.Width


#1.9 Petal.Length ou 0.6 Petal.Width
iris
bd[bd$Petal.Length <=1.9,5]
length(bd[bd$Petal.Length <=1.9,5])
bd[bd$Petal.Length >1.9,5]
length(bd[bd$Petal.Length >1.9,5])

bd[bd$Petal.Width<=0.6,5]
length(bd[bd$Petal.Width<=0.6,5])
bd[bd$Petal.Width>0.6,5]
length(bd[bd$Petal.Width>0.6,5])
bd<-irisr[1:100,]
bd<-bd[bd$Petal.Length>1.9,]
nrow(bd)


#nova ramifica????o
vet_for_sepalL<-seq(min(bd$Sepal.Length),max(bd$Sepal.Length),0.1)
p1<-as.numeric(length(bd[bd=='setosa',5]))
p2<-as.numeric(length(bd[bd=='versicolor',5]))
p3<-as.numeric(length(bd[bd=='virginica',5]))
entropia_pai<-entropy(c(p1,p2,p3))
GI<-rep(0,length(vet_for_sepalL))
n<-as.numeric(nrow(bd))
for (i in 1:length(vet_for_sepalL)){
  yf<-bd[bd$Sepal.Length<=vet_for_sepalL[i],5]
  p1_fl<-as.numeric(length(yf[yf=='setosa']))
  p2_fl<-as.numeric(length(yf[yf=='versicolor']))
  p3_fl<-as.numeric(length(yf[yf=='virginica']))
  yfr<-bd[bd$Sepal.Length>vet_for_sepalL[i],5]
  p1_fr<-as.numeric(length(yfr[yfr=='setosa']))
  p2_fr<-as.numeric(length(yfr[yfr=='versicolor']))
  p3_fr<-as.numeric(length(yfr[yfr=='virginica']))
  pl<-sum(c(p1_fl,p2_fl,p3_fl))/n
  pr<-sum(c(p1_fr,p2_fr,p3_fr))/n
  entropia_filho<-(pr*entropy(c(p1_fr,p2_fr,p3_fr))+pl*entropy(c(p1_fl,p2_fl,p3_fl)))
  GI[i]<-entropia_pai-entropia_filho
}
for (i in 1:(as.numeric(length(vet_for_sepalL))-1)){
  as.numeric(length(vet_for_sepalL))
  i_i=i*1
  j=i_i-1
  if (i==1){
    max_pos = 1
  }else if(GI[max_pos] < GI[i_i]){
    max_pos = i_i
  }
}
bd$Sepal.Length
GI[max_pos]
#0.1937845
vet_for_sepalL[max_pos]
#6.6


vet_for_sepalL<-seq(min(bd$Sepal.Width),max(bd$Sepal.Width),0.1)
p1<-as.numeric(length(bd[bd=='setosa',5]))
p2<-as.numeric(length(bd[bd=='versicolor',5]))
p3<-as.numeric(length(bd[bd=='virginica',5]))
entropia_pai<-entropy(c(p1,p2,p3))
GI<-rep(0,length(vet_for_sepalL))
n<-as.numeric(nrow(bd))
for (i in 1:length(vet_for_sepalL)){
  yf<-bd[bd$Sepal.Width<=vet_for_sepalL[i],5]
  p1_fl<-as.numeric(length(yf[yf=='setosa']))
  p2_fl<-as.numeric(length(yf[yf=='versicolor']))
  p3_fl<-as.numeric(length(yf[yf=='virginica']))
  yfr<-bd[bd$Sepal.Width>vet_for_sepalL[i],5]
  p1_fr<-as.numeric(length(yfr[yfr=='setosa']))
  p2_fr<-as.numeric(length(yfr[yfr=='versicolor']))
  p3_fr<-as.numeric(length(yfr[yfr=='virginica']))
  pl<-sum(c(p1_fl,p2_fl,p3_fl))/n
  pr<-sum(c(p1_fr,p2_fr,p3_fr))/n
  entropia_filho<-(pr*entropy(c(p1_fr,p2_fr,p3_fr))+pl*entropy(c(p1_fl,p2_fl,p3_fl)))
  GI[i]<-entropia_pai-entropia_filho
}
for (i in 1:(as.numeric(length(vet_for_sepalL))-1)){
  as.numeric(length(vet_for_sepalL))
  i_i=i*1
  j=i_i-1
  if (i==1){
    max_pos = 1
  }else if(GI[max_pos] < GI[i_i]){
    max_pos = i_i
  }
}
bd$Sepal.Width
GI[max_pos]
#0.06583
vet_for_sepalL[max_pos]
#2.9

vet_for_sepalL<-seq(min(bd$Petal.Length),max(bd$Petal.Length),0.1)
p1<-as.numeric(length(bd[bd=='setosa',5]))
p2<-as.numeric(length(bd[bd=='versicolor',5]))
p3<-as.numeric(length(bd[bd=='virginica',5]))
entropia_pai<-entropy(c(p1,p2,p3))
GI<-rep(0,length(vet_for_sepalL))
n<-as.numeric(nrow(bd))
for (i in 1:length(vet_for_sepalL)){
  yf<-bd[bd$Petal.Length<=vet_for_sepalL[i],5]
  p1_fl<-as.numeric(length(yf[yf=='setosa']))
  p2_fl<-as.numeric(length(yf[yf=='versicolor']))
  p3_fl<-as.numeric(length(yf[yf=='virginica']))
  yfr<-bd[bd$Petal.Length>vet_for_sepalL[i],5]
  p1_fr<-as.numeric(length(yfr[yfr=='setosa']))
  p2_fr<-as.numeric(length(yfr[yfr=='versicolor']))
  p3_fr<-as.numeric(length(yfr[yfr=='virginica']))
  pl<-sum(c(p1_fl,p2_fl,p3_fl))/n
  pr<-sum(c(p1_fr,p2_fr,p3_fr))/n
  entropia_filho<-(pr*entropy(c(p1_fr,p2_fr,p3_fr))+pl*entropy(c(p1_fl,p2_fl,p3_fl)))
  GI[i]<-entropia_pai-entropia_filho
}
for (i in 1:(as.numeric(length(vet_for_sepalL))-1)){
  as.numeric(length(vet_for_sepalL))
  i_i=i*1
  j=i_i-1
  if (i==1){
    max_pos = 1
  }else if(GI[max_pos] < GI[i_i]){
    max_pos = i_i
  }
}
bd$Petal.Length
GI[max_pos]
#0.5095998
vet_for_sepalL[max_pos]
#4.7



vet_for_sepalL<-seq(min(bd$Petal.Width),max(bd$Petal.Width),0.1)
p1<-as.numeric(length(bd[bd=='setosa',5]))
p2<-as.numeric(length(bd[bd=='versicolor',5]))
p3<-as.numeric(length(bd[bd=='virginica',5]))
entropia_pai<-entropy(c(p1,p2,p3))
GI<-rep(0,length(vet_for_sepalL))
n<-as.numeric(nrow(bd))
for (i in 1:length(vet_for_sepalL)){
  yf<-bd[bd$Petal.Width<=vet_for_sepalL[i],5]
  p1_fl<-as.numeric(length(yf[yf=='setosa']))
  p2_fl<-as.numeric(length(yf[yf=='versicolor']))
  p3_fl<-as.numeric(length(yf[yf=='virginica']))
  yfr<-bd[bd$Petal.Width>vet_for_sepalL[i],5]
  p1_fr<-as.numeric(length(yfr[yfr=='setosa']))
  p2_fr<-as.numeric(length(yfr[yfr=='versicolor']))
  p3_fr<-as.numeric(length(yfr[yfr=='virginica']))
  pl<-sum(c(p1_fl,p2_fl,p3_fl))/n
  pr<-sum(c(p1_fr,p2_fr,p3_fr))/n
  entropia_filho<-(pr*entropy(c(p1_fr,p2_fr,p3_fr))+pl*entropy(c(p1_fl,p2_fl,p3_fl)))
  GI[i]<-entropia_pai-entropia_filho
}
for (i in 1:(as.numeric(length(vet_for_sepalL))-1)){
  as.numeric(length(vet_for_sepalL))
  i_i=i*1
  j=i_i-1
  if (i==1){
    max_pos = 1
  }else if(GI[max_pos] < GI[i_i]){
    max_pos = i_i
  }
}
bd$Petal.Width
GI[max_pos]
#0.5734234
vet_for_sepalL[max_pos]
#1.6

table(bd[bd$Petal.Width>1.6,5])
table(bd[bd$Petal.Width<=1.6,5])



#nova ramifica????o
bd<-irisr[1:100,]
bd<-bd[bd$Petal.Length>1.9 &  bd$Petal.Width<=1.6,]
nrow(bd)
vet_for_sepalL<-seq(min(bd$Sepal.Length),max(bd$Sepal.Length),0.1)
p1<-as.numeric(length(bd[bd=='setosa',5]))
p2<-as.numeric(length(bd[bd=='versicolor',5]))
p3<-as.numeric(length(bd[bd=='virginica',5]))
entropia_pai<-entropy(c(p1,p2,p3))
GI<-rep(0,length(vet_for_sepalL))
n<-as.numeric(nrow(bd))
for (i in 1:length(vet_for_sepalL)){
  yf<-bd[bd$Sepal.Length<=vet_for_sepalL[i],5]
  p1_fl<-as.numeric(length(yf[yf=='setosa']))
  p2_fl<-as.numeric(length(yf[yf=='versicolor']))
  p3_fl<-as.numeric(length(yf[yf=='virginica']))
  yfr<-bd[bd$Sepal.Length>vet_for_sepalL[i],5]
  p1_fr<-as.numeric(length(yfr[yfr=='setosa']))
  p2_fr<-as.numeric(length(yfr[yfr=='versicolor']))
  p3_fr<-as.numeric(length(yfr[yfr=='virginica']))
  pl<-sum(c(p1_fl,p2_fl,p3_fl))/n
  pr<-sum(c(p1_fr,p2_fr,p3_fr))/n
  entropia_filho<-(pr*entropy(c(p1_fr,p2_fr,p3_fr))+pl*entropy(c(p1_fl,p2_fl,p3_fl)))
  GI[i]<-entropia_pai-entropia_filho
}
for (i in 1:(as.numeric(length(vet_for_sepalL))-1)){
  as.numeric(length(vet_for_sepalL))
  i_i=i*1
  j=i_i-1
  if (i==1){
    max_pos = 1
  }else if(GI[max_pos] < GI[i_i]){
    max_pos = i_i
  }
}
bd$Sepal.Length
GI[max_pos]
#0.04386235
vet_for_sepalL[max_pos]
#5.9


vet_for_sepalL<-seq(min(bd$Sepal.Width),max(bd$Sepal.Width),0.1)
p1<-as.numeric(length(bd[bd=='setosa',5]))
p2<-as.numeric(length(bd[bd=='versicolor',5]))
p3<-as.numeric(length(bd[bd=='virginica',5]))
entropia_pai<-entropy(c(p1,p2,p3))
GI<-rep(0,length(vet_for_sepalL))
n<-as.numeric(nrow(bd))
for (i in 1:length(vet_for_sepalL)){
  yf<-bd[bd$Sepal.Width<=vet_for_sepalL[i],5]
  p1_fl<-as.numeric(length(yf[yf=='setosa']))
  p2_fl<-as.numeric(length(yf[yf=='versicolor']))
  p3_fl<-as.numeric(length(yf[yf=='virginica']))
  yfr<-bd[bd$Sepal.Width>vet_for_sepalL[i],5]
  p1_fr<-as.numeric(length(yfr[yfr=='setosa']))
  p2_fr<-as.numeric(length(yfr[yfr=='versicolor']))
  p3_fr<-as.numeric(length(yfr[yfr=='virginica']))
  pl<-sum(c(p1_fl,p2_fl,p3_fl))/n
  pr<-sum(c(p1_fr,p2_fr,p3_fr))/n
  entropia_filho<-(pr*entropy(c(p1_fr,p2_fr,p3_fr))+pl*entropy(c(p1_fl,p2_fl,p3_fl)))
  GI[i]<-entropia_pai-entropia_filho
}
for (i in 1:(as.numeric(length(vet_for_sepalL))-1)){
  as.numeric(length(vet_for_sepalL))
  i_i=i*1
  j=i_i-1
  if (i==1){
    max_pos = 1
  }else if(GI[max_pos] < GI[i_i]){
    max_pos = i_i
  }
}
bd$Sepal.Width
GI[max_pos]
#0.04735577
vet_for_sepalL[max_pos]
#2.6

vet_for_sepalL<-seq(min(bd$Petal.Length),max(bd$Petal.Length),0.1)
p1<-as.numeric(length(bd[bd=='setosa',5]))
p2<-as.numeric(length(bd[bd=='versicolor',5]))
p3<-as.numeric(length(bd[bd=='virginica',5]))
entropia_pai<-entropy(c(p1,p2,p3))
GI<-rep(0,length(vet_for_sepalL))
n<-as.numeric(nrow(bd))
for (i in 1:length(vet_for_sepalL)){
  yf<-bd[bd$Petal.Length<=vet_for_sepalL[i],5]
  p1_fl<-as.numeric(length(yf[yf=='setosa']))
  p2_fl<-as.numeric(length(yf[yf=='versicolor']))
  p3_fl<-as.numeric(length(yf[yf=='virginica']))
  yfr<-bd[bd$Petal.Length>vet_for_sepalL[i],5]
  p1_fr<-as.numeric(length(yfr[yfr=='setosa']))
  p2_fr<-as.numeric(length(yfr[yfr=='versicolor']))
  p3_fr<-as.numeric(length(yfr[yfr=='virginica']))
  pl<-sum(c(p1_fl,p2_fl,p3_fl))/n
  pr<-sum(c(p1_fr,p2_fr,p3_fr))/n
  entropia_filho<-(pr*entropy(c(p1_fr,p2_fr,p3_fr))+pl*entropy(c(p1_fl,p2_fl,p3_fl)))
  GI[i]<-entropia_pai-entropia_filho
}
for (i in 1:(as.numeric(length(vet_for_sepalL))-1)){
  as.numeric(length(vet_for_sepalL))
  i_i=i*1
  j=i_i-1
  if (i==1){
    max_pos = 1
  }else if(GI[max_pos] < GI[i_i]){
    max_pos = i_i
  }
}
bd$Petal.Length
GI[max_pos]
#0.5095998
vet_for_sepalL[max_pos]
#4.9
table(bd[bd$Petal.Length>4.9,5])
table(bd[bd$Petal.Length<=4.9,5])
plot(GI)

vet_for_sepalL<-seq(min(bd$Petal.Width),max(bd$Petal.Width),0.1)
p1<-as.numeric(length(bd[bd=='setosa',5]))
p2<-as.numeric(length(bd[bd=='versicolor',5]))
p3<-as.numeric(length(bd[bd=='virginica',5]))
entropia_pai<-entropy(c(p1,p2,p3))
GI<-rep(0,length(vet_for_sepalL))
n<-as.numeric(nrow(bd))
for (i in 1:length(vet_for_sepalL)){
  yf<-bd[bd$Petal.Width<=vet_for_sepalL[i],5]
  p1_fl<-as.numeric(length(yf[yf=='setosa']))
  p2_fl<-as.numeric(length(yf[yf=='versicolor']))
  p3_fl<-as.numeric(length(yf[yf=='virginica']))
  yfr<-bd[bd$Petal.Width>vet_for_sepalL[i],5]
  p1_fr<-as.numeric(length(yfr[yfr=='setosa']))
  p2_fr<-as.numeric(length(yfr[yfr=='versicolor']))
  p3_fr<-as.numeric(length(yfr[yfr=='virginica']))
  pl<-sum(c(p1_fl,p2_fl,p3_fl))/n
  pr<-sum(c(p1_fr,p2_fr,p3_fr))/n
  entropia_filho<-(pr*entropy(c(p1_fr,p2_fr,p3_fr))+pl*entropy(c(p1_fl,p2_fl,p3_fl)))
  GI[i]<-entropia_pai-entropia_filho
}
for (i in 1:(as.numeric(length(vet_for_sepalL))-1)){
  as.numeric(length(vet_for_sepalL))
  i_i=i*1
  j=i_i-1
  if (i==1){
    max_pos = 1
  }else if(GI[max_pos] < GI[i_i]){
    max_pos = i_i
  }
}
bd$Petal.Width
GI[max_pos]
#0.05944019
vet_for_sepalL[max_pos]
#1.3

table(bd[bd$Petal.Width>1.3,5])
table(bd[bd$Petal.Width<=1.3,5])






