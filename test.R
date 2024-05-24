alpha <- 225.61302
beta <- 3.5031918
lambda <- 0.01405405
T <- 400
tp <- 10
cc <- 30
ci <- 20
h <- 300

f <- function(l) {
    return(pexp(h-l, rate = lambda) * pweibull(l, scale = alpha, shape = beta))
}

integrate(f, lower = 0, upper = T)
