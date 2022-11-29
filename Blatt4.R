# Aufgabe 9:
unibew = read.table('/Users/gedeonvogt/Desktop/RWTH Aachen/4. Semester/R Praktikum/Datensätze/unibew.dat', header=TRUE)
table(unibew$Fach)
table(unibew$Geschlecht)
table(unibew$Zulassung)

Tab.ges = table(unibew[, c(2, 3)])
Tab.ges.bed = addmargins(prop.table(Tab.ges, 1), 2)

Tab.ges.f1 = table(unibew[unibew$Fach == 1, c(2, 3)])
Tab.ges.bed.f1 = addmargins(prop.table(Tab.ges.f1, 1), 2)

Tab.ges.f2 = table(unibew[unibew$Fach == 2, c('Geschlecht', 'Zulassung')])
Tab.ges.bed.f2 = addmargins(prop.table(Tab.ges.f2, 1), 2)
