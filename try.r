

x <- c(50,50,50,70,70,70,80,80,80,90,90,90,100,100,100)
y <- c(3.3,2.8,2.9,2.3,2.6,2.1,2.5,2.9,2.4,3.0,3.1,2.8,3.3,3.5,3.0)

#quadraticModel = lm(dist ~ poly(speed, 2, raw=TRUE), data = cars)
quadraticModel = lm(y ~ x)
q = poly(x, 2, raw=TRUE)
print(q)