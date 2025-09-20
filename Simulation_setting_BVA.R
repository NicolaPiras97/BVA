#Code for simulation BVA
library(Rcpp)
library(RcppEigen)
library(RcppArmadillo)
Rcpp::sourceCpp("MCCLC_categorical_data.cpp") 

L <- 3
R <- H <- 2
K <- 30
Q <- 15
ph <- c(0.8,0.2)
pr <- c(0.4,0.6)

pxwz<-matrix(c(0.5,0.3,0.2,0.1,0.6,0.3,0.2,0.1,0.7,0.6,0.1,0.3),nrow=H*R,ncol=L,byrow=T)

c1=2
nk<-10 
nq<-10

n=K*nk*Q

p1<-matrix(c(0.8,0.2,0.9,0.2,0.8,0.1),nrow=c1,ncol=L,byrow=T)
p2<-matrix(c(0.7,0.3,0.8,0.3,0.7,0.2),nrow=c1,ncol=L,byrow=T)
p3<-matrix(c(0.9,0.8,0.3,0.1,0.2,0.7),nrow=c1,ncol=L,byrow=T) 
#Scenario 1
p4<-matrix(c(0.615384615,0.153846154,0.153846154,0.076923077,0.449542052,0.192640907,0.192640907,0.165137025,0.024390244,0.097560976,0.097560976,0.780487805),nrow=4,ncol=L,byrow=F)
#Scenario 2
#p4<-matrix(c(0.622568004,0.155642057,0.155642057,0.066147882,0.460960239,0.197553965,0.197553965,0.143931831,0.027624332,0.110497287,0.110497287,0.751381094),nrow=4,ncol=L,byrow=F)

S <- 100
for(s in 1:S){

components<-rep(0,K)
while(length(which(components==1))!=round(K*ph[1])){ #remove for simulation scheme i. (random sampling of memberships of level-2 units)
  components <- sample(1:H,prob=ph,size=K,replace=TRUE)      
}

components2<-rep(0,Q)
while(length(which(components2==1))!=round(Q*pr[1])){ #remove for simulation scheme i. (random sampling of memberships of level-2 units)
  components2 <- sample(1:R,prob=pr,size=Q,replace=TRUE)      
}

data <- matrix(nrow=n,ncol=3) 
data[,1] <- seq(1:n)
d1<-NULL
for(k in 1:K){
  d1<-c(d1,rep(k,nk*Q))
}

data[,2]<-d1
d2<-NULL
for(k in 1:K){
  d2<-c(d2,rep(seq(1:Q),nq))
}

data[,3]<-d2

colr<-NULL
colr<-c(colr,rep(components2,nq*K))
colh<-NULL
for(i in 1:(K)){
  colh<-c(colh,rep(components[i],nk*Q))
}
datac<-cbind(data,colh,colr)

datacc<-datac[order(datac[,4],datac[,5]),]
data<-datacc[,1:3]

count<-c(length(which(daticc[,4]==1 & daticc[,5]==1)),length(which(daticc[,4]==1 & daticc[,5]==2)),length(which(daticc[,4]==2 & daticc[,5]==1)),length(which(daticc[,4]==2 & daticc[,5]==2)))

data2<-NULL
samples2 <- NULL
w <- 1
for(j in (1:H)){
  for(m in (1:R)){
    samples <- sample(1:L,prob=pxwz[w,],size=count[w],replace=TRUE) 
    for(i in (1:count[w])){
      data2p = cbind(sample(0:(c1-1),prob=p1[,samples[i]],size=1),sample(0:(c1-1),prob=p2[,samples[i]],size=1),sample(0:(c1-1),prob=p3[,samples[i]],size=1),sample(1:4,prob=p4[,samples[i]],size=1))
      data2=rbind(data2,data2p)
    } 
    samples2<-c(samples2,samples)  
    w=w+1    
  }
}
data<-cbind(data,data2)  
datac<-cbind(data,samples2)

dnew<-matrix(NA,nrow=n,ncol=2)
for(j in 1:n){
  if(dati[j,7]==1){
    dnew[j,]=c(0,0)
  }
  if(dati[j,7]==2){
    dnew[j,]=c(0,1)
  }
  if(dati[j,7]==3){
    dnew[j,]=c(1,0)
  }
  if(dati[j,7]==4){
    dnew[j,]=c(1,1)
  }
}
data<-cbind(data[,1:6],dnew)


a<-order(data[,2],data[,3])
s<-NULL
for(i in 1:(dim(data)[1])){
  s<-rbind(s,data[a[i],])
}
s<-as.matrix(s)
data<-s


y<-list(s[1,])
for(i in (2:dim(s)[1])){
  y[[i]]<-s[i,]
}

main2(y)


}
