this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

source("belleza_exer4.r")
size = length(GaussianResult$solutionSet)
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
  print(polynomial)
  s = eval(parse(text = paste(f,polynomial, sep=" ")))
  return(list(coefficients = GaussianResult$solutionSet, func = s))     
}
a = PolynomialRegression(size)
print(a$func(10))