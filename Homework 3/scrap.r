
error_func <- function(v){
  return(abs(qbeta(.5,v[1],v[2]) - .3) + abs((1 - pbeta(.75, v[1], v[2])) - .1))
}
nlm.out <- nlm(error_func, c(2,2), stepmax = 1, iterlim = 1000, gradtol = .000000001)



x <- seq(.0001,.9999, length.out = 1000)
plot(x, dbeta(x, nlm.out$est[1], nlm.out$est[2]), type = "l")






num.int <- function(fun, a = 0, b = 1, n = 20, type = "left"){
  ## plots the function and provides a numerical estimation using left sums
  ## The function then outputs the approximate sum for the integral
  ## NOTE: functions are to be input in quotes.
  
  ## Set the gounds for left-hand, midpoint, and right-hand sums.
  if(type == as.character("right")){
    k = 2
  }else if(type == as.character("mid")){
    k=1
  }else{
    k=0
  }
  
  ## Provide the x and y values for the input function
  x <- seq(a, b, length = 10000)
  y <- eval(parse(text = fun))
  
  x.fun <- x
  y.fun <- y
  
  ## Provide the function values used for the numerical integration and rectangle construction.
  x <- seq(a, b, length = 2*n + 1)
  dx = x[3] - x[1]
  y <- eval(parse(text = fun))
  
  ## empty vector for computing the approximate sum.
  approx.int <- rep(0, 2*n + 1)
  
  plot(c(a,b), c(min(0,y.fun), max(0,y.fun)), xlab = "", ylab = "")
  
  ## Plot each rectangle
  for (i in 1:(2*n + 1)){
    rect(0 + x[2*i-1], min(0, y[2*i-1 + k]), x[2*i + 1], max(0, y[2*i-1 + k]), col = "chocolate")
    approx.int[i] <- dx*y[2*i-1 + k]
  }
  
  ## Plot the function over the top of the rectangles.
  par(new = T)
  plot(x.fun, y.fun, ylim = c(min(y.fun, 0) ,max(y.fun, 0)), type = "l", lwd = 2)
  par(new = F)
  
  ## return the approximate integral
  return(sum(approx.int[1:n]))
}

Megan's Prior Function

prior.fun <- function(theta){
if(theta <= 0.385 | theta >= 0.585){
out <- 0.5
} else if(theta > 0.385 & theta <= 0.485){
out <- 50*(theta - 0.385) + 0.5
} else if(theta > 0.485 & theta < 0.585){
out <- -50*(theta - 0.485) + 5.5
}

options(digits = 10)
return(out)
}

Examples:

Note that the function defaults to endpoints of 0 and 1, left hand sums and a partition of 20 rectangles.

num.int("x^2 - 3*x -9")

Plots the the above polynomial function between 0 and 1

num.int("log(x)", a = 1, b = 10)

Plots the natural log function between 0 and 10

num.int("exp(sin(2*(x-pi/4)))", a = 0, b = 2*pi, n = 50, type = "mid")

Plots exp(sin(2*(x - pi/4))) between -pi and pi. n changes the number of rectangles to 50, type changes the type of sums used.

num.int("apply(cbind(x), 1, prior.fun)", type = "right")

Plots Megan's Prior Function using right-hand sums



integrate