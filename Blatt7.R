# Aufgabe 15
n1 = 10
n2 = 20
n3 = 50
x = seq(0, 100, 0.1)

plot(x, dchisq(x, n1), type='l', lty=1, col=1, main='Dichten der Chi-Quadrat-Verteilungen
mit Freiheitsgraden n=10, n=20, n=50')
lines(x, dchisq(x, n2),  lty=2, col=2)
lines(x, dchisq(x, n3),  lty=3, col=3)
legend('topright', legend=c('n=10', 'n=20', 'n=50'), col=1:3, lty=1:3)





# Aufgabe 16
n1 = 1
n2 = 5
n3 = 25
x = seq(-5, 5, 0.01)

plot(x, ylim=c(0, 0.4),
dnorm(x), type='l', lty=1, col=1, main='Unterschiedliche t-Verteilungen im Vergleich zu Standard Normalverteilung')
lines(x, dt(x, n1),  lty=2, col=2)
lines(x, dt(x, n2),  lty=3, col=3)
lines(x, dt(x, n3),  lty=4, col=4)
legend('topright', legend=c('The Normal Distribution', 'n=10', 'n=20', 'n=50'), col=1:4, lty=1:4)





# Aufgabe 17
n1 = 2
n2 = 5
n3 = 20
m = 30
x = seq(0, 5, 0.01)

plot(x, ylim=c(0, 1.1),
df(x, n1, m), type='l', lty=1, col=1, main='Dichten der F-Verteilungen
mit Freiheitsgraden n=2, n=5, n=20')
lines(x, df(x, n2, m),  lty=2, col=2)
lines(x, df(x, n3, m),  lty=3, col=3)
legend('topright', legend=c('n=2', 'n=5', 'n=20'), col=1:3, lty=1:3)
