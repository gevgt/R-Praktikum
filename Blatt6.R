# Aufgabe 13
# a)
nachfrage = read.table('/Users/gedeonvogt/Desktop/RWTH Aachen/4. Semester/R Praktikum/Datensätze/nachfrage.dat', header=TRUE)


# b)
Menge = nachfrage$Menge
Preis = nachfrage$Preis
Preis_quad = Preis ** 2

regpar = lm(Menge ~ Preis + I(Preis^2))
regpar$coefficients

# c)
plot(Preis, Menge, xlim=c(0, 10), ylim=c(0, 50), 
     xlab = 'Verkaufspreis (in Euro)', ylab='Nachgefragte Menge', 
     main='Nachgefragte Menge in Äbhängigkeit von Verkaufspreisen')

nachfr.pred = data.frame(Preis=seq(0,10,0.01))
y = predict(regpar,newdata=nachfr.pred)
lines(nachfr.pred$Preis,y,col="red")


# d)
summary(regpar)





# Aufgabe 14
# a)
luft = read.csv2('/Users/gedeonvogt/Desktop/RWTH Aachen/4. Semester/R Praktikum/Datensätze/luft.csv', header=TRUE)
luft


# b)
luft.vollst = na.omit(luft)
regpar = lm(O3 ~ SO2 + NO + NO2 + CO + STAUB, data = luft.vollst)
O3.hut = ifelse(is.na(luft$O3) == TRUE, round(predict(regpar, luft), 5), luft$O3)
luft.pred = cbind(luft, O3.hut)

# HIER WIRD EIN DATAFRAME SORTIERT!!!!!
luft.pred.sort = luft.pred[order(luft.pred$NAME), ]


# c)
write.csv2(luft.pred.sort, 'Ozonpred.csv', na='', row.names=FALSE)
