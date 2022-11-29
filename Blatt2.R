# Aufgabe 4:
zeiten = scan('/Users/gedeonvogt/Desktop/RWTH Aachen/4. Semester/R Praktikum/Datensätze/bearbzeit.dat')
boxplot(zeiten, border='blue', main='Titel')

box.erg = boxplot(zeiten, plot=FALSE)
box.erg$stats





# Aufgabe 5:
groessen = read.table("/Users/gedeonvogt/Desktop/RWTH Aachen/4. Semester/R Praktikum/Datensätze/koerpergroessen.dat",header=TRUE)

boxplot(groessen$Groesse, 
        main='Box-Plot saemtlicher gemessener Koerpergroessen 
        von 10 Frauen und 10 Maennern',
        ylab='Groesse (in cm)')

groessen.w = groessen[c(groessen$Geschlecht == 'w'), 2]
groessen.m = groessen[c(groessen$Geschlecht == 'm'), 2]

boxplot(groessen.m, groessen.w, names=c('Männlich', 'Weiblich'), border=c('blue', 'red'))





# Aufgabe 6:
hist(zeiten, breaks=seq(60,150,10), ylab='Bearbeitungszeiten (in s)', xlab='Absolute Klassenhaeufigkeiten', main='Histogramm der Bearbeitungszeiten zu den
     Patientendaten aus Aufgabe P 2', freq=TRUE, ylim=c(0, 20))


hist(zeiten, breaks=seq(60,150,10) + 2, ylab='Bearbeitungszeiten (in s)', xlab='Absolute Klassenhaeufigkeiten', main='Histogramm der Bearbeitungszeiten zu den
     Patientendaten aus Aufgabe P 2', freq=TRUE, ylim=c(0, 20))

hist(zeiten, breaks=c(60,80,90,100,105,110,115,120,130,150), 
      ylab='Bearbeitungszeiten (in s)', 
      xlab='Absolute Klassenhaeufigkeiten', 
      main='Histogramm der Bearbeitungszeiten zu den
     Patientendaten aus Aufgabe P 2')

zeiten.erg = hist(zeiten, breaks=c(60,80,90,100,105,110,115,120,130,150), plot=FALSE)
zeiten.erg$counts
zeiten.erg$counts / length(zeiten)
