
####################### JEL for data set Tensile Strength ######################
####################### Test staistic calculation #########
rm(list=ls())
library(emplik)
MC<-1000# no. of repition
type11<-rep()
type15<-rep()
#sam=c(25,50,75,100,150,200)
#for(d in 1:6){
# n<-sam[d]
n=20
Jkn<-rep()
k1=(n+4)/(2*n*(n-2))
k2=1/(n*(n-1)*(n-2))
k11=(n+3)/(2*(n-1)*(n-3))
k21=1/((n-1)*(n-2)*(n-3))
for(r in 1:MC){
  #mean <- 3076.88 ## given 
  #sd <- 344.362  ## given 
    x1 <- c(2944, 3206, 2751, 3089, 3406, 3275, 2606, 2723, 2475, 2930, 2530, 2399, 2806, 2827, 2951, 2854, 2930, 2565, 2799, 3102, 3454, 4185, 3095, 3247, 3371, 3302, 3544, 3454, 3468, 3233, 2571, 3268, 2792, 2916, 3006, 3523, 2958, 3123, 2930, 2689, 3075, 3337, 3468, 3254, 3061, 3647, 3295, 2971, 3068, 3371)
    #x1<-c(15.5, 23.75, 8.0, 17.0, 5.5, 19.0, 24.0, 2.5, 7.5, 11.0, 13.0, 3.75, 25.0, 9.75, 22.0,18.0, 6.0, 12.5, 2.0, 21.5)
     x=(x1-mean(x1))/sd(x1)
     n = length(x)
     xs=sort(x)
     s1<-0
     s2<-0
     for(i in 1:n){
       s1<-s1+xs[i]
       s2<-s2+1*((i*(n+1-i))-1)*xs[i]
     }
     t<- (k1*s1)-(3*k2*s2)  
     v<-rep()  
     tl<-rep() 
     jkn<-rep()
     for(l in 1:n){
       x1<-x[-l]
       xs1=sort(x1)
       s11<-0
       s21<-0
       for(i in 1:(n-1)){
         s11<-s11+xs1[i]
         s21<-s21+1*((i*(n-i))-1)*xs1[i]
       }
       tl[l]<-(k11*s11)-(3*k21*s21)
       v[l]=n*t-(n-1)*tl[l]                   ##### Pseduo Value   ######
     }
     Jkn[r]<- el.test(v,mu=0)$'-2LLR'    
}
type15<-mean(1*(abs(Jkn[Jkn!='NaN'])>3.84))
type11<-mean(1*(abs(Jkn[Jkn!='NaN'])>6.63))
   
Jkn    
     

#############################################
################# P value calculation  #############
#x1 <- c(2944, 3206, 2751, 3089, 3406, 3275, 2606, 2723, 2475, 2930, 2530, 2399, 2806, 2827, 2951, 2854, 2930, 2565, 2799, 3102, 3454, 4185, 3095, 3247, 3371, 3302, 3544, 3454, 3468, 3233, 2571, 3268, 2792, 2916, 3006, 3523, 2958, 3123, 2930, 2689, 3075, 3337, 3468, 3254, 3061, 3647, 3295, 2971, 3068, 3371)
x1<-c(15.5, 23.75, 8.0, 17.0, 5.5, 19.0, 24.0, 2.5, 7.5, 11.0, 13.0, 3.75, 25.0, 9.75, 22.0,18.0, 6.0, 12.5, 2.0, 21.5)
x=(x1-mean(x1))/sd(x1)
n = length(x)
k1=(n+4)/(2*n*(n-2))
k2=1/(n*(n-1)*(n-2))
k11=(n+3)/(2*(n-1)*(n-3))
k21=1/((n-1)*(n-2)*(n-3))
xs=sort(x)
s1<-0
s2<-0
for(i in 1:n){
  s1<-s1+xs[i]
  s2<-s2+1*((i*(n+1-i))-1)*xs[i]
}
t_dataset<- (k1*s1)-(3*k2*s2) 
t_dataset
###### For simuated test statistic
n=45
mean(x)
sd(x)
x2<-rnorm (n, mean(x) ,sd(x) )
# here X is an array
n = length(x2)
k1=(n+1)/(n*(n-2))
k2=1/(n*(n-1)*(n-2))
xs=sort(x2)
s1<-0
s2<-0
for(i in 1:n){
  s1<-s1+xs[i]
  s2<-s2+1*(i*(n+1-i)-1)*xs[i]
}
t_simulated<- (k1*s1)-(3*k2*s2)  
#### p value calculation ####
p_value <- mean(t_simulated >= t_dataset)
p_value





####################### Data set Qiu 2018 ##########
####################### JEL test########
rm(list=ls())
library(emplik)
MC<-1000# no. of repition
type11<-rep()
type15<-rep()
#sam=c(25,50,75,100,150,200)
#for(d in 1:6){
 # n<-sam[d]
n=45
Jkn<-rep()
k1=(n+4)/(2*n*(n-2))
k2=1/(n*(n-1)*(n-2))
k11=(n+3)/(2*(n-1)*(n-3))
k21=1/((n-1)*(n-2)*(n-3))
  for(r in 1:MC){
    x1<- c(0.2, 0.3, 0.5, 0.5, 0.5, 0.5, 0.6, 0.6, 0.7, 0.7, 0.7, 0.8, 0.8, 1.0, 1.0, 1.0, 1.0, 1.1, 1.3, 1.5,1.5, 1.5, 1.5, 2.0, 2.0, 2.2, 2.5, 3.0, 3.0, 3.3, 3.3, 4.0, 4.0, 4.5, 4.7, 5.0, 5.4, 5.4, 7.0, 7.5,8.8, 9.0, 10.3, 22.0, 24.5)
    x=(x1-mean(x1))/sd(x1)
    x
     n = length(x)
     xs=sort(x)
     s1<-0
     s2<-0
     for(i in 1:n){
       s1<-s1+xs[i]
       s2<-s2+1*((i*(n+1-i))-1)*xs[i]
     }
     t<- (k1*s1)-(3*k2*s2)  
     v<-rep()  
     tl<-rep() 
     jkn<-rep()
     for(l in 1:n){
       x1<-x[-l]
       xs1=sort(x1)
       s11<-0
       s21<-0
       for(i in 1:(n-1)){
         s11<-s11+xs1[i]
         s21<-s21+1*((i*(n-i))-1)*xs1[i]
       }
       tl[l]<-(k11*s11)-(3*k21*s21)
       v[l]=(n*t)-(n-1)*tl[l]                   ##### Pseduo Value   ######
     }
     Jkn[r]<- el.test(v,mu=0)$'-2LLR'    
  }
type15<-mean(1*(abs(Jkn[Jkn!='NaN'])>3.84))
type11<-mean(1*(abs(Jkn[Jkn!='NaN'])>6.63))

Jkn    

#############################################
  ################# SCR test #############
  x1<- c(0.2, 0.3, 0.5, 0.5, 0.5, 0.5, 0.6, 0.6, 0.7, 0.7, 0.7, 0.8, 0.8, 1.0, 1.0, 1.0, 1.0, 1.1, 1.3, 1.5,1.5, 1.5, 1.5, 2.0, 2.0, 2.2, 2.5, 3.0, 3.0, 3.3, 3.3, 4.0, 4.0, 4.5, 4.7, 5.0, 5.4, 5.4, 7.0, 7.5,8.8, 9.0, 10.3, 22.0, 24.5)
 hist(x) #plot(x1)
x=(x1-mean(x1))/sd(x1)  
  n = length(x)
  k1=(n+4)/(2*n*(n-2))
  k2=1/(n*(n-1)*(n-2))
  xs=sort(x)
  s1<-0
  s2<-0
  for(i in 1:n){
    s1<-s1+xs[i]
    s2<-s2+1*((i*(n+1-i))-1)*xs[i]
  }
  t_dataset<- (k1*s1)-(3*k2*s2)  
  t_dataset
  
  n=45
  mu=3.61;lambda=1.588
  # Define your own normal density function
  IGD <- function(x, mu,lambda) {
    exp(-(lambda)*(x - mean)^2 / (2 * x * mu^2)) * (sqrt(lambda /2 * pi * x^3) )
  }
  # Generate random numbers using your custom normal density function
  generate_random_numbers <- function(n,mu,lambda) {
    u <- runif(n)  # Generate uniform random numbers
    z <- qnorm(u, mean = 0, sd = 1)  # Inverse cumulative distribution function of standard normal
    x <- mu + lambda * z  # Transform to desired mean and standard deviation
    return(x)
  }
  x <- generate_random_numbers(n, mu,lambda)
  # here X is an array
  n = length(x)
  k1=(n+1)/(n*(n-2))
  k2=1/(n*(n-1)*(n-2))
  xs=sort(x)
  s1<-0
  s2<-0
  for(i in 1:n){
    s1<-s1+xs[i]
    s2<-s2+1*((i*(n+1-i))-1)*xs[i]
  }
  t_simulated<- (k1*s1)-(3*k2*s2)  
  
  p_value <- mean(t_simulated >= t_dataset)
  p_value
  
  
