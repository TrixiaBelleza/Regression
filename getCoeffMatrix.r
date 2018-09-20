x <- c(50,50,50,70,70,70,80,80,80,90,90,90,100,100,100)
#x <- c(1,2,3)
#y <- c(2,3,4)
y <- c(3.3,2.8,2.9,2.3,2.6,2.1,2.5,2.9,2.4,3.0,3.1,2.8,3.3,3.5,3.0)

getRegColNames <- function(size) {
  colnames <- c()
  for (i in 1:(size+1)) {
    #concat x and number i
    colname = paste("a",i, sep="")
    colnames <- c(colnames, colname)  
  }
  colnames <- c(colnames, "RHS") #adds RHS at the last index of the vector
  return(colnames)
}
#returns -> c(1, 2, 3, ..., n)
getRegRowNames <- function(size) {
  rownames <- c()
  for (i in 1:(size+1)) {
    rownames <- c(rownames, i)      
  }
  return(rownames)
}
getACM <- function() {
  size = 3
  if((length(x) != length(y)) | (size < 1)){
    return(NA)
  }
  else{
    #declare matrix
    vars = getRegColNames(size)[1:(size+1)]
    augcoeffmatrix = matrix(data=NA, nrow=size+1, ncol=size+2, dimnames=list(getRegRowNames(size), getRegColNames(size)))
    RHScount = 0
    for(i in 1:(size+1)) { #for each row
      count = i-1
      for(j in 1:(size+2)){ #for each column
        if(j == (size+2)) {
          augcoeffmatrix[i,j] = sum((x**RHScount) * y)
          RHScount = RHScount + 1
        }
        else {
          augcoeffmatrix[i,j] = sum(x**count)
          count = count + 1
        }
      }
    }
    #print(augcoeffmatrix)
    return(list(variables = vars, augcoeffmatrix = augcoeffmatrix))
  }
}
result = getACM()
#print(result)

