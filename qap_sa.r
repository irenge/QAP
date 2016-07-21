set.seed(888)
library("permute")

## Calculate a solution at a position
calculate.solution <- function(w){
  sol[w]<-0
  for(i in 1:n){
    for( j in 1:n){
      sol[w] <-sol[w] + d1[i,j]*f1[pm[w,i],pm[w,j]]
    }
  }
  sol[w]
}

#create distance matrix 
d1 <- matrix(c(0,24,26,24,17,25,15,24,0,2,5,13,23,18,26,2,0,6,14,24,19,24,5,6,0,10,28,22,17,13,14,10,0,30,21,25,23,24,28,30,0,9,15,18,19,22,21,9,0),7,7)
#create flow matrix
f1 <- matrix(c(0,0,54,88,88,0,53,0,0,51,68,75,80,0,54,51,0,0,86,74,0,88,68,0,0,72,0,0,88,75,86,72,0,87,72,0,80,74,0,87,0,55,53,0,0,0,72,55,0),7,7)

#calculate the size of the problem
n <- dim(d1)[1]

# calculate possible number of permutation
fac <- factorial(n)
nfac <- fac - 1

#calculate number of permutation
pm<- allPerms(n)

# initialise solution vector
sol <- vector(mode="numeric", length=nfac)

#current position indices
cp<-20
## next position
np <- cp+1

sol[cp]<-position(cp)

#solution <-sol[cp]
sol[np]<- position(np)

## Step 2 Determine difference of current and next position
delta <- position(cp)-position(np)

## Step 3 swap if

if(delta<0){ 
  #swap(sol[cp],sol[np])
  temp <-sol[cp]
  sol[cp]<-sol[np]
  sol[np]<- temp 
  }

## step 4
if(delta>0){
  r<-runif(1,0,1)
  p <- exp(delta/20)
  if(r<p){
    temp <-sol[cp]
    sol[cp]<-sol[np]
    sol[np]<- temp 
  }
  
}

