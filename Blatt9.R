# Aufgabe 21
# a)
papier = c(184.2 , 182.6 , 185.3 , 184.5 , 186.2 , 183.9 , 185.0 , 187.1 , 184.4)
t.test(papier,conf.level=0.9)


# b)
n = length(papier)
mean = mean(papier)
alpha = 0.1
quant = qt(1-alpha/2, n-1)
sigma = sd(papier)

gr.u = mean - quant * sigma / n**0.5
gr.o = mean + quant * sigma / n**0.5

gr.u
gr.o


# c)
t.test(papier, conf.level = 0.95)


# d)
t.test(papier, conf.level = 0.95, alternative = 'less')





# Aufgabe 22
# a)
KI = function(p, n, alpha){
  quant = qnorm(1-alpha/2) 
  ki_u = p - quant * (p*(1-p) / n)**0.5
  ki_o = p + quant * (p*(1-p) / n)**0.5
  return(c(ki_u, ki_o))
}


# b)
alpha = 0.1
p = 0.05
N = 10000
n = 2000
set.seed(4711)
x = rbinom(N, n, p)

kis = Vectorize(KI)(x/n, n, alpha)
anteil = mean(kis[1,] <= p & kis[2,] >= p)
