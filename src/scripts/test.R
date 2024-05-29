alpha <- 225.61302
beta <- 3.5031918
lambda <- 0.8432432
T <- 400
tp <- 10
cc <- 30
ci <- 20

f <- function(t) {dexp(t, rate = lambda)}
h <- function(t) {dweibull(t, shape = beta, scale = alpha)}

dSumXY <- function(dX, dY){
	
	# Create convolution of distributions.
	dReturn <- function(z){
		integrate( function(x,z){
			dX(x) * dY(z - x) },
			-Inf, Inf, z)$value
	}
	
	# Vectorize convolution.
	dReturn <- Vectorize(dReturn)
	
	return(dReturn)  
}

t_set <- seq(100,200,0.1)
ggplot() +
	geom_line(aes(x = t_set, y = f(t_set))) +
	geom_line(aes(x = t_set, y = h(t_set))) +
	geom_line(aes(x = t_set,
				  y = dSumXY(f,h)(t_set)))

integrate(function(z){z * dSumXY(z)}, -Inf, Inf, z)
