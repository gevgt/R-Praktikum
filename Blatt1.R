# Aufgabe 1:
Kurse = c(43.50 , 46.70 , 51.20 , 49.90 , 55.50 , 60.30 , 63.50 , 61.00 , 59.20 , 62.30 , 65.40 , 67.80)
Monate = 1:12
plot(Monate,Kurse, type='l', col="blue",
     ylim=c(-10,max(Kurse)),
     ylab="Kurse (in Euro)",
     main="Monatliche Durchschnittskurse einer Aktie im Verlauf eines Jahres")

Kurse.prozentuale_veraenderung = (Kurse[2:length(Kurse)] / Kurse[1:length(Kurse)-1] - 1) *100
lines(Monate, c(0,Kurse.prozentuale_veraenderung), type='h', col='red')

print((Kurse[length(Kurse)] / Kurse[1])**(1/11)) - 1





# Aufgabe 2:
zeiten = scan('/Users/gedeonvogt/Desktop/RWTH Aachen/4. Semester/R Praktikum/Datensätze/bearbzeit.dat')
zeiten.sort = sort(zeiten)
write.table(zeiten.sort, '/Users/gedeonvogt/Desktop/RWTH Aachen/4. Semester/R Praktikum/Datensätze/bearbzeit_sort.dat',
            row.names=FALSE, col.names=FALSE)

x = summary(zeiten)

spannweite = max(zeiten) - min(zeiten)
quantile(zeiten,prob=0.75,type=2) - quantile(zeiten,prob=0.25,type=2)

var(zeiten)





# Aufgabe 3:
verweildauer = c(2, 3, 7, 10, 14, 21)
anzahl_gaeste = c(6, 2, 12, 6, 10, 4)

rel_haeuf = anzahl_gaeste / sum(anzahl_gaeste)
rel_haeuf.cum_sum = c(0,cumsum(rel_haeuf))

F_n = function(n){
  index = -1
  for (i in 1:length(verweildauer)){
    if(n < verweildauer[i]){
      index = i
      print(index)
      break
    } else if (i == length(verweildauer)){
      index = i + 1
    }
  }
  return(rel_haeuf.cum_sum[index])
}

x = seq(0,25,1)
y = Vectorize(F_n)(x)

plot(x,y, xlab = 'x (Tage)', ylab = 'Fn(x)', main = 'Titel')
