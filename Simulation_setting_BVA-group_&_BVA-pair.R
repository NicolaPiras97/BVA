#Code for simulation BVA-group and BVA-pair
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

p1<-matrix(c(0.88888844,0.823529696,0.333333373,0.666665738,0.538462115,0.111111165,0.11111156,0.176470304,0.666666627,0.333334262,0.461537885,0.888888835),nrow=c1,ncol=L*H,byrow=T) 
p2<-matrix(c(0.7,0.8,0.3,0.3,0.2,0.7),nrow=c1,ncol=L,byrow=T)
p3<-matrix(c(0.8,0.2,0.9,0.2,0.8,0.1),nrow=c1,ncol=L,byrow=T)
p4<-matrix(c(0.7,0.3,0.8,0.3,0.7,0.2),nrow=c1,ncol=L,byrow=T)
p5<-matrix(c(0.4,0.2,0.3,0.6,0.8,0.7),nrow=c1,ncol=L,byrow=T)

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
      #Scenario 1
      dati2p = cbind(sample(0:(c1-1),prob=p1[,samples[i]+(j-1)*L],size=1),sample(0:(c1-1),prob=p2[,samples[i]],size=1),sample(0:(c1-1),prob=p3[,samples[i]],size=1),sample(0:(c1-1),prob=p4[,samples[i]],size=1),sample(0:(c1-1),prob=p5[,samples[i]],size=1))
      #Scenario 2
      #dati2p = cbind(sample(0:(c1-1),prob=p1[,samples[i]+(m-1)*L],size=1),sample(0:(c1-1),prob=p2[,samples[i]],size=1),sample(0:(c1-1),prob=p3[,samples[i]],size=1),sample(0:(c1-1),prob=p4[,samples[i]],size=1),sample(0:(c1-1),prob=p5[,samples[i]],size=1))
      data2=rbind(data2,data2p)
    } 
    samples2<-c(samples2,samples)  
    w=w+1    
  }
}
data<-cbind(data,data2)  
datac<-cbind(data,samples2)


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
