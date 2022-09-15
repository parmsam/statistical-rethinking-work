w <- 6
n <- 9
p <- seq(from=0, to=1, len=1000)
prw <- dbinom(w,n,p)
prp <- dunif(p,0,1)
posterior <- prw * prp

posterior
plot(posterior)

library(rethinking)
