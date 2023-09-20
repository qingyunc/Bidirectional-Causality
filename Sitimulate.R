library(tidyverse)
library(tidybayes)
library(rstan)
library(brms)
library(patchwork)
library(purrr)
library(tidyr)
library(dplyr)

set.seed(123)
w1 = 1
w2 = 1
eta = 1
mean_x = 500
mean_y = 500
sd_x = 20
sd_y = 20
sd_epsilon1 = 1
sd_epsilon2 = 1
b0 = 0
b1 = 0.35
a0 = 0
a1 = 0.5
beta0 = 0
alpha0 = 0
size = 6000
n = 100
t = 2000
t0 = 500

z1 <- runif(size, 0, 1)
data <- as.data.frame(z1)
data$z2 <- runif(size, 0, 1)
data$epsilon1 <- rnorm(size, mean=0, sd=sd_epsilon1)
data$epsilon2 <- rnorm(size, mean=0, sd=sd_epsilon2)
data$ex <- rnorm(size, mean=mean_x, sd=sd_x)
data$x_ex <- data$ex + data$epsilon1
data$beta1 <- b0 + b1 * data$z1
data$y_add <- data$beta1 * data$x_ex
data$y_ex <- eta * data$ex + data$epsilon2
data$alpha1 <- a0 + a1 * data$z2
data$x_add <- data$alpha1 * data$y_ex
data$y <- data$y_ex + data$y_add
data$x <- data$x_ex + data$x_add
data$xz1 <- data$x * data$z1

mod <- lm(y ~ x + z1 + z2 + xz1, data = data)
summary(mod)
plot(data$x, data$y)

B <- c()
z1 <- c()
z2 <- c()
for (i in 1:t) {
  sample <- sample(size, n)
  mod <- lm(y ~ x + z1 + z2, data = data[sample,])
  B <- append(B, mod$coefficients[2])
  z1 <- append(z1, mean(data$z1[sample]))
  z2 <- append(z2, mean(data$z2[sample]))
}
bootstrap <- as.data.frame(B)
bootstrap$z1 <- z1
bootstrap$z2 <- z2

pdf_B <- function(par){
  var1 = abs(par[1])
  var2 = abs(par[2])
  k1 = abs(par[3])
  m1 = par[4]
  k2 = abs(par[5])
  m2 = par[6]
  B = bootstrap$B
  mean1 = k1 * bootstrap$z1 + m1
  mean2 = k2 * bootstrap$z2 + m2
  a = (B ^ 2 / var1 + 1 / var2) ^ 0.5
  b = mean1 * B / var1 + mean2 / var2
  c = mean1 ^ 2 / var1 + mean2 ^ 2 / var2
  d = exp((b ^ 2 - c * a ^ 2) / (2 * a ^ 2))
  p = ((b * d / a ^ 3) / ((2 * pi * var1 * var2) ^ 0.5) * (pnorm(b / a) - pnorm(-b / a)) 
       + exp(-c / 2) / (a ^ 2 * pi * (var1 * var2) ^ 0.5))
  log_likelihood = sum(log(p)) + w1 * log(var1 ^ 0.5) + w2 * log(var2 ^ 0.5)
  return(-log_likelihood)
}

par <- c(1,1,1,1,1,1)
optim <- optim(par, pdf_B)
par_hat_total <- optim$par

par_hat = as.data.frame(1:6)
for (j in 1:100){
  sample_b <- sample(t, t0)
  pdf_B <- function(par){
    var1 = abs(par[1])
    var2 = abs(par[2])
    k1 = abs(par[3])
    m1 = par[4]
    k2 = abs(par[5])
    m2 = par[6]
    B = bootstrap[sample_b,]$B
    mean1 = k1 * bootstrap[sample_b,]$z1 + m1
    mean2 = k2 * bootstrap[sample_b,]$z2 + m2
    a = (B ^ 2 / var1 + 1 / var2) ^ 0.5
    b = mean1 * B / var1 + mean2 / var2
    c = mean1 ^ 2 / var1 + mean2 ^ 2 / var2
    d = exp((b ^ 2 - c * a ^ 2) / (2 * a ^ 2))
    p = ((b * d / a ^ 3) / ((2 * pi * var1 * var2) ^ 0.5) * (pnorm(b / a) - pnorm(-b / a)) 
         + exp(-c / 2) / (a ^ 2 * pi * (var1 * var2) ^ 0.5))
    log_likelihood = sum(log(p)) + w1 * log(var1 ^ 0.5) + w2 * log(var2 ^ 0.5)
    return(-log_likelihood)
  }
  
  par <- c(1,1,1,1,1,1)
  optim <- optim(par, pdf_B)
  par_hat[,j] <- optim$par
}
par_hat <- as.data.frame(t(as.matrix(par_hat)))
var(par_hat[,1]) ^ 0.5
var(par_hat[,2]) ^ 0.5
var(par_hat[,3]) ^ 0.5
var(par_hat[,4]) ^ 0.5
var(par_hat[,5]) ^ 0.5
var(par_hat[,6]) ^ 0.5
2*pt(-abs(par_hat_total[1]/var(par_hat[,1]) ^ 0.5), df = 5993)
2*pt(-abs(par_hat_total[2]/var(par_hat[,2]) ^ 0.5), df = 5993)
2*pt(-abs(par_hat_total[3]/var(par_hat[,3]) ^ 0.5), df = 5993)
2*pt(-abs(par_hat_total[4]/var(par_hat[,4]) ^ 0.5), df = 5993)
2*pt(-abs(par_hat_total[5]/var(par_hat[,5]) ^ 0.5), df = 5993)
2*pt(-abs(par_hat_total[6]/var(par_hat[,6]) ^ 0.5), df = 5993)
par_hat_total

split.screen (c(1, 2))
screen(1)
k1 = 0.38
m1 = 0.05
k2 = 0.55
m2 = 0.05
var1 = seq(0.01 , 1, 0.01)
var2 = seq(0.01 , 1, 0.01)
var <- crossing(
  var1 = var1,
  var2 = var2
)
pdf_B_0 <- function(var1, var2){
  par <- c(var1, var2, k1, m1, k2, m2)
  return(-pdf_B(par))
}
var <- var %>% mutate(log_likelihood = map2_dbl(var$var1, var$var2, pdf_B_0))
var_new <- as.matrix(spread(var, key = 'var2', value = 'log_likelihood'))
persp(var1, var2, var_new[,-1], theta = 30, phi = 20, main = 'Variance under Small Bias',
      xlab = '考1^2', ylab = '考2^2', zlab = 'log_likelihood')

screen(2)
k1 = 1.35
m1 = -1
k2 = -0.5
m2 = 1
var1 = seq(0.01 , 1, 0.01)
var2 = seq(0.01 , 1, 0.01)
var <- crossing(
  var1 = var1,
  var2 = var2
)
pdf_B_0 <- function(var1, var2){
  par <- c(var1, var2, k1, m1, k2, m2)
  return(-pdf_B(par))
}
var <- var %>% mutate(log_likelihood = map2_dbl(var$var1, var$var2, pdf_B_0))
var_new <- as.matrix(spread(var, key = 'var2', value = 'log_likelihood'))
persp(var1, var2, var_new[,-1], theta = 30, phi = 20, main = 'Variance under Great Bias',
      xlab = '考1^2', ylab = '考2^2', zlab = 'log_likelihood')

screen(1)
k1 = seq(0 , 2, 0.02)
m1 = seq(2 , 4, 0.02)
k2 = 0.55
m2 = 0.05
var1 = 10
var2 = 10
par1 <- crossing(
  k1 = k1,
  m1 = m1
)
pdf_B_1 <- function(k1, m1){
  par <- c(var1, var2, k1, m1, k2, m2)
  return(-pdf_B(par))
}
par1 <- par1 %>% mutate(log_likelihood = map2_dbl(par1$k1, par1$m1, pdf_B_1))
par1_new <- as.matrix(spread(par1, key = 'm1', value = 'log_likelihood'))
persp(k1, m1, par1_new[,-1], theta = 30, phi = 20, main = 'Effect of Z and Z on X',
      xlab = '考1^2', ylab = '考2^2', zlab = 'log_likelihood')

      
      