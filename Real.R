library(tidyr)
library(reshape2)
library(readxl)
GDPPer0 <- read_excel("C:/Users/Lenovo/Desktop/project/GDPPer.xls", sheet = "Sheet1", na = "0")
GDPPer <- melt(GDPPer0,
                id.vars = c('Country'),
                measure.vars = colnames(GDPPer0)[-1],
                variable.name='year',
                value.name='GDPPer')
CO2Per0 <- read_excel("C:/Users/Lenovo/Desktop/project/CO2Per.xls", sheet = "Sheet1", na = "0")
CO2Per <- melt(CO2Per0,
               id.vars = c('Country'),
               measure.vars = colnames(CO2Per0)[-1],
               variable.name='year',
               value.name='CO2Per0')
Employment0 <- read_excel("C:/Users/Lenovo/Desktop/project/Employment.xls", sheet = "Sheet1", na = "0")
Employment <- melt(Employment0,
                   id.vars = c('Country'),
                   measure.vars = colnames(Employment0)[-1],
                   variable.name='year',
                   value.name='Employment')
GNI0 <- read_excel("C:/Users/Lenovo/Desktop/project/GNI.xls", sheet = "Sheet1", na = "0")
GNI <- melt(GNI0,
            id.vars = c('Country'),
            measure.vars = colnames(GNI0)[-1],
            variable.name='year',
            value.name='GNI')
CAPITAL0 <- read_excel("C:/Users/Lenovo/Desktop/project/CAPITAL.xls", sheet = "Sheet1", na = "0")
CAPITAL <- melt(CAPITAL0,
                id.vars = c('Country'),
                measure.vars = colnames(CAPITAL0)[-1],
                variable.name='year',
                value.name='CAPITAL')
LABOR0 <- read_excel("C:/Users/Lenovo/Desktop/project/LABOR.xls", sheet = "Sheet1", na = "0")
LABOR <- melt(LABOR0,
              id.vars = c('Country'),
              measure.vars = colnames(LABOR0)[-1],
              variable.name='year',
              value.name='LABOR')
TRADE0 <- read_excel("C:/Users/Lenovo/Desktop/project/TRADE.xls", sheet = "Sheet1", na = "0")
TRADE <- melt(TRADE0,
              id.vars = c('Country'),
              measure.vars = colnames(TRADE0)[-1],
              variable.name='year',
              value.name='TRADE')
URBANLIZATION0 <- read_excel("C:/Users/Lenovo/Desktop/project/URBANLIZATION.xls", sheet = "Sheet1", na = "0")
URBANLIZATION <- melt(URBANLIZATION0,
                      id.vars = c('Country'),
                      measure.vars = colnames(URBANLIZATION0)[-1],
                      variable.name='year',
                      value.name='URBANLIZATION')


data0 <- GDPPer
data0$CO2Per <- CO2Per$CO2Per
data0$Employment <- Employment$Employment
data0$GNI <- GNI$GNI
data0$CAPITAL <- CAPITAL$CAPITAL
data0$LABOR <- LABOR$LABOR
data0$TRADE <- TRADE$TRADE
data0$URBANLIZATION <- URBANLIZATION$URBANLIZATION

data <- na.omit(data0)
data <- scale(data[,-c(1,2)], center = TRUE, scale = TRUE)
data <- as.data.frame(data)
data$CE <- data$CO2Per * data$Employment
data$GG <- data$GDPPer * data$GNI
class(data)

mod <- lm(GDPPer ~ CO2Per + Employment + CE + GNI + CAPITAL + LABOR + TRADE + URBANLIZATION, data = data)
summary(mod)
mod <- lm(CO2Per ~ GDPPer + GNI + GG + Employment + CAPITAL + LABOR + TRADE + URBANLIZATION, data = data)
summary(mod)

set.seed(123)
w1 = 0
w2 = 0
t = 2000
t0 = 500
size = 5618
n = 100
B <- c()
z1 <- c()
z2 <- c()
for (i in 1:t) {
  sample <- sample(size, n)
  mod <- lm(CO2Per ~ GDPPer + GNI + Employment + CAPITAL + LABOR + TRADE + URBANLIZATION, data = data[sample,])
  B <- append(B, mod$coefficients[2])
  z1 <- append(z1, mean(data$GNI[sample]))
  z2 <- append(z2, mean(data$Employment[sample]))
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
2*pt(-abs(par_hat_total[1]/var(par_hat[,1]) ^ 0.5), df = 5611)
2*pt(-abs(par_hat_total[2]/var(par_hat[,2]) ^ 0.5), df = 5611)
2*pt(-abs(par_hat_total[3]/var(par_hat[,3]) ^ 0.5), df = 5611)
2*pt(-abs(par_hat_total[4]/var(par_hat[,4]) ^ 0.5), df = 5611)
2*pt(-abs(par_hat_total[5]/var(par_hat[,5]) ^ 0.5), df = 5611)
2*pt(-abs(par_hat_total[6]/var(par_hat[,6]) ^ 0.5), df = 5611)
par_hat_total

