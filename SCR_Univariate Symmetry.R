
MC<-1000 #no of repetition
set.seed(111)
power<-rep()
deltas= rep()
deltap= rep()
sam=c(20,40,60,80,100) #sample size
for(d in 1:5){
n<-sam[d]
k1=(n+4)/(2*n*(n-2))
k2=1/(n*(n-1)*(n-2))
for(r in 1:MC){ 
  #x<-rnorm(n,0,1)
  x <- rlaplace(n, 0, 1)
  #x <- rlogis(n, 1, 1)
  xs=sort(x)
  s1<-0
  s2<-0
  for(i in 1:n){
    s1<-s1+xs[i]
    s2<-s2+1*((i*(n+1-i))-1)*xs[i]
  }
  t<- (k1*s1)-(3*k2*s2)  
  SCR.Test<-function(x){ # here X is an array
    n = length(x)
    xs=sort(x)
    s1<-0
    s2<-0
    for(i in 1:n){
      s1<-s1+xs[i]
      s2<-s2+1*((i*(n+1-i))-1)*xs[i]
    }
    t<- (k1*s1)-(3*k2*s2)  
  }
  for(r in 1:MC){
    x <- rlaplace(n, 0, 1)
  deltas[r]= SCR.Test(x)
   }
  deltas=sort(deltas)
  c1= quantile(deltas, 0.025)
  c2= quantile(deltas, 0.975)
  #print(r)
  for (e in 1:MC) {
    # Generate random variables x and y
    x <- rlaplace(n, 0, 1)
    deltap[e]= SCR.Test(x)
    #print(e)
     }
   }
  
  power[d]<- mean(as.integer(deltap< c1))+mean(as.integer(deltap> c2))
}
power
  
  
  
  
  
  
  
  
  
 