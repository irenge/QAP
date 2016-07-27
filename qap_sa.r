library("combinat")
#create distance matrix 
D <- matrix(c(0,24,26,24,17,25,15,24,0,2,5,13,23,18,26,2,0,6,14,24,19,24,5,6,0,10,28,22,17,13,14,10,0,30,21,25,23,24,28,30,0,9,15,18,19,22,21,9,0),7,7)
#create distance matrix
Fl <- matrix(c(0,0,54,88,88,0,53,0,0,51,68,75,80,0,54,51,0,0,86,74,0,88,68,0,0,72,0,0,88,75,86,72,0,87,72,0,80,74,0,87,0,55,53,0,0,0,72,55,0),7,7)

n <- dim(D)[1]

fac <- factorial(n)

nfac<-fac-1

calculate.cost <- function(p){
  cost<-0
  for(i in 1:n){
    for( j in 1:n){
      cost <-cost + Fl[p[i],p[j]]*D[i,j]
    }
  }
  cost
}
swap<-function(a,b) {
temp<-a
a <- b
b <-temp
}
### 1. Generate random solution
g<-sample(fac,1)
Sr <- permn(1:n)[[g]]
### 2. Cost of the solution
CSr <- calculate.cost(Sr)

### 3. Set initial temperature
t<-1000

result <- vector(length=t)

while((t>0)&(g<fac)){
  testSolution <- Sr
  #for(nb in 1:10){
  g<-g+1
  Sn <-permn(1:n)[[g]]
  CSn <-calculate.cost(Sn)
  delta<-CSn-CSr
    if((delta<0)||(exp(-delta/t)>(runif(1)))){
      #swap(Sr,Sn)
      #swap(ESr,ESn)
      Sr<-Sn
      CSr<-CSn
      }
   
  #}
  result[[t]]<-CSr
  t<-t-1
  #g<-g+1
}
print(CSr)
print(Sr)
min(result)
