
####################### Jackknife EMpirical Likeliihood ######################
rm(list=ls())
library(emplik)
MC<-1000# no. of repition
type11<-rep()
type15<-rep()
sam=c(25,50,100,200)
for(d in 1:4){
  n<-sam[d]
  Jkn<-rep()
  k1=(n+1)/(n*(n-2))
  k2=1/(n*(n-1)*(n-2))
  k11=(n)/((n-1)*(n-3))
  k21=1/((n-1)*(n-2)*(n-3))
  for(r in 1:MC){
    #x <- rnorm(n, 1, 1)
    #x <- rlaplace(n, 0, 1)
    #x <- rlogis(n, 1, 1)
    #x<-rexp(n,1)
    x<-rmixnorm(n,0,1,0,1,0.5)
    #x<-rsl(n,xi=0,omega=1,alpha=1,dp=NULL)
    xs=sort(x)
    s1<-0
    s2<-0
    for(i in 1:n){
      s1<-s1+xs[i]
      s2<-s2+(2*i*((n+1)-i)+2)*xs[i]
    }
    t<- (4*k1*s1)-(6*k2*s2)  
    v<-rep()  
    tl<-rep()  
    for(l in 1:n){
      x1<-x[-l]
      xs1=sort(x1)
      s11<-0
      s21<-0
      for(i in 1:(n-1)){
        s11<-s11+xs1[i]
        s21<-s21+(2*i*(n-i)+2)*xs1[i]
      }
      tl[l]<-(4*k11*s11)-(6*k21*s21)
      v[l]=n*t-(n-1)*tl[l]                            ##### Pseduo Value   ######
    }
    Jkn[r]<- el.test(v,mu=0)$'-2LLR'    
  }
  type15[d]<-mean(1*(abs(Jkn[Jkn!='NaN'])>3.84))
  type11[d] <-mean(1*(abs(Jkn[Jkn!='NaN'])>6.63))
}
type15
type11
mean(v)
t
