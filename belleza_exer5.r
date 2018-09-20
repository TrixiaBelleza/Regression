this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

source("belleza_exer4.r")
size = length(GaussianResult$solutionSet)
#x <- c(50,50,50,70,70,70,80,80,80,90,90,90,100,100,100)
#y <- c(3.3,2.8,2.9,2.3,2.6,2.1,2.5,2.9,2.4,3.0,3.1,2.8,3.3,3.5,3.0)
PolynomialRegression <- function(size) {
  polynomial = ""
  f = "f <- function(x)"
  for(i in 1:size) {
    if(i!=size) {
      polynomial = paste(polynomial, " ", GaussianResult$solutionSet[[i]]," * x**", (i-1), " + ", sep="")
    }else{
      polynomial = paste(polynomial, " ", GaussianResult$solutionSet[[i]]," * x**", (i-1), "", sep="")
    }
  
  }
  #print(polynomial)
  eval(parse(text = paste(f,polynomial, sep=" ")))
  #print(f(10))
  
}

print(PolynomialRegression())