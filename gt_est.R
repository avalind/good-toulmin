good_toumin <- function(phi, t) {
	is <- seq(from=1, to=length(phi))
	return(-sum( ((-t)^is) * phi ))
}

smooth_good_toumin <- function(phi, t) {
	n <- sum(phi*(1:length(phi)))
	k <- floor(1/2 * log2((n * t^2)/(t-1)))
	q <- 1/(t+1)
	is <- seq(from=1, to=length(phi))
	ts <- (-t)^is
	tail_prob <- 1 - pbinom(is, k, q)
	return(-sum(ts*tail_prob*phi))
}
