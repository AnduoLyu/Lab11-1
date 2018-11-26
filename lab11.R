# Q1
rbinomial <- function (n,p) {
  x <- runif(n, min=0, max=1)
  return(sum(x < p))
}
rbinomial(10, 0.4)

# Q2
library(microbenchmark)
microbenchmark(rbinomial(10, 0.4), rbinom(1000, 10, 0.4))


# Q3
set.seed(123)
xi <- runif(50, min = 20, max = 40)
beta0 <- 15
beta1 <- 0.4
eps <- rnorm(50, 0, 3)
yi <- 15 + 0.4 * xi + eps
fit <- lm(yi ~ xi)
ggplot(data = fit, aes(x = fit$fitted.values, y = fit$residuals)) +
  geom_point() +
  labs(x =  'fitted values', y = 'residuals')


# Q4
q4 <- function(x){
  u1 = runif(x, min = 0, max = 1)
  u2 = runif(x, min = 0, max = 1)
  r = sqrt(-2 * log(u1))
  theta = 2 * pi * u2
  x = r * cos(theta)
  y = r * sin(theta)
return(data.frame(x, y))
}

plotdata <- q4(1000)

ggplot(data = plotdata)+
  geom_histogram(aes(x = x, y=..density..), bins = 20, col = 1, fill = NA)+
  stat_density(aes(x = rnorm(1000, 0,1)), col = 3, fill = NA)+
  labs(title = "X vs Normal Distribution")+
  theme_bw()

ggplot(data = plotdata)+
  geom_histogram(aes(x = y, y=..density..), bins = 20, col = 1, fill = NA)+
  stat_density(aes(x = rnorm(1000, 0,1)), col = 3, fill = NA)+
  labs(title = "Y vs Normal Distribution")
  theme_bw()
