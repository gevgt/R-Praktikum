# Aufgabe 10:
path = '/Users/gedeonvogt/Desktop/RWTH Aachen/4. Semester/R Praktikum/Datensätze/'
kastanie = read.table(paste0(path,'Kastanie.dat'), header=FALSE)
colnames(kastanie) = c('Alter', 'Stamm')
cor(kastanie)[1,2]





# Aufgabe 11:
punkte = read.table(paste0(path, 'punkte.dat'), header=TRUE)

K.Rang = rank(punkte$Klausur)
U.Rang = rank(punkte$Uebung)
Rang.zusammen = rbind(K.Rang, U.Rang)
colnames(Rang.zusammen) = punkte$Stud

cor(t(Rang.zusammen), method='spearman')[1,2]





# Aufgabe 12:
regpar = lm(kastanie$Alter ~ kastanie$Stamm)
plot(kastanie$Stamm, kastanie$Alter,
     xlim = c(0,9),
     ylim = c(0,50),
     xlab = 'Stamm (Erklärende Variable)',
     ylab = 'Alter (Abhängige Variable)',
     main = 'Regressionsgerade')
abline(regpar,col="red")

# Berechnung und Ausgabe der Regressionswerte > pr = predict(regpar,kastanie)
pr = predict(regpar,kastanie)
# Abküurzende Bezeichnungen > st = kastanie$Stamm
st = kastanie$Stamm
al = kastanie$Alter
ka = (al-pr)*9/50
# Ergäazung (ausgefüllter) Residuen-Quadrate in der Graphik 
# zur Veranschaulichung der "Methode der kleinsten Quadrate" 
# unter Verwendung der Funktion rect()
rect(st-ka,pr,st,al,density=50,col="green")

summary(regpar)
