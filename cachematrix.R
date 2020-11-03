## The function makeCacheMatrix creates a matrix, computes its inverse and stores it.
makeCacheMatrix<-function(m){
  m<<-matrix(rnorm(4),2,2)
  s<<-matrix(,2,2)
  set<-function(m){
    m<<-matrix(rnorm(4),2,2)
    s<<-matrix(,2,2)
  }
  get<-function() m
  setinverse<-solve(m)
  getinverse<<-matrix(setinverse,2,2)
  cat("New matrix:","\n")
  print(m)
}
  makeCacheMatrix(m)
## The cacheSolve function evaluates if the inverse of the matrix of the previous function was already computed, if yes then returns the inverse. 
## If not, cacheSolve computes the inverse. 
cacheSolve<- function(m,...){
  #x<-getinverse
  if(exists("getinverse")){
    message("getting cached data")
    return(getinverse)
  }
  else {
  get<-function() m
  data<-get()
  setinverse<-solve(m)
  getinverse<<-matrix(setinverse,2,2)
  cat("New inverse matrix computed:","\n")
  print(getinverse)
  
  }
}
cacheSolve(m)
#test
m%*%getinverse
