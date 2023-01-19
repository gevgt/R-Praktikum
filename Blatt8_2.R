# Aufgabe 18
# a)
N = 1000
n = 10
sim = matrix(nrow=N, ncol=2)
colnames(sim) = c('Mean', 'Sample Variance')

set.seed(2007)

for (i in 1:N) {
  vec = rnorm(n, 0, 2)
  sim[i, 1] = mean(vec)
  sim[i, 2] = var(vec)
}


# b)
hist(sim[,1], ylim=c(0, 0.8), xlab='Mittelwert', ylab='Rel. Häufigkeit',
     breaks=seq(-3, 3, 0.2), freq=FALSE, main='1000 simulierte Mittelwerte')


# c)
var_trans = sim[, 2] * (n-1) / 4

hist(var_trans, ylim=c(0, 0.12), xlab='Varianz', ylab='Rel. Häufigkeit',
     breaks=seq(0, 30, 1), freq=FALSE, main='1000 simulierte transformierte Varianzen')

x = seq(0, 30, 0.01)
lines(x, dchisq(x, n-1), type='l', col='green')






# Aufgabe 19
# a)
hist(faithful$eruptions, freq=FALSE, breaks = seq(1.6, 5.2, 0.2), ylim=c(0, 0.8), ylab='Eruptionsdauer (in Minuten)',
     main='Histogramm zu den Eruptionsdauern des Geysirs Old Faithful')


# b)
n = length(faithful$eruptions)
sigma = sd(faithful$eruptions)
bopt = (4/3)**(0.2) * sigma * 1/(n**(1/5))

par(las=1)
kds = density(faithful$eruptions,kernel="gaussian",bw=bopt)
plot(kds,type="l",col="blue",ylim=c(0,0.5),
     xlab="Eruptionsdauer (in Minuten)",
     ylab="Kern-Dichteschaetzung",
     main="Kern-Dichteschaetzung zu den Eruptionsdauern
des Geysirs Old Faithful (Gauss-Kern)")


# c)
plot(1:n,faithful$eruptions,type="o",col="blue",pch=20,
     ylim=c(0,6),ylab="Eruptionsdauer (in Minuten)",
     xlab="Nr. der Eruption in der zeitlichen Abfolge",
     main="Eruptionsdauern des Geysirs Old Faithful
in der zeitlichen Abfolge")






# Aufgabe 20
# a)
my.l = function(beta, y){
  n = length(y)
  return(- n * log(gamma(beta)) + (beta - 1) * sum(log(y)) - sum(y))
}

set.seed(2018)
x = rgamma(10, shape=2)
beta_vec = seq(0.01, 8, 0.01)

par(las=1)
plot(beta_vec, my.l(beta_vec, x), type='l', ylim=c(-70, -10), main='Grafik')


# b)
my.newton = function(y, beta.0=1, iter=20){
  beta = beta.0
  n = length(y)
  for (i in 1:iter){
    beta = beta - (digamma(beta) - sum(log(y))/n) / trigamma(beta) 
  }
  return(beta)
}


# c)
my.newton(x)
my.newton(x, beta.0=4)
my.newton(x, beta.0=6)


# d)
N = 1000
set.seed(2018)

sim = 1:N * 0
for (i in 1:N){
  random_vec = rgamma(10,2)
  sim[i] = my.newton(random_vec)
}

beta.newton = mean(sim)
beta.newton
