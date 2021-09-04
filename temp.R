B <- 94
lambda <- 30
mu <- 1/3
a <- 0

# p_0 <- function(a,lambda,mu,B) {
#   for (i in 1:10) {
#     a <- a + 1
#   }
#   return(a)
# }
p_0 <- function(a,lambda,mu,B) {
  for (i in 0:B) {
    a <- a + (lambda^i)/(mu^i*factorial(i))
  }
  return(1/a)
}
p_i <- function(p_0,lambda,mu,B){
  return(p_0*(lambda^B)/(mu^B*factorial(B)))
}

p <- p_0(a,lambda,mu,B)
answer <- p_i(p,lambda,mu,B)
answer
answer<0.05

