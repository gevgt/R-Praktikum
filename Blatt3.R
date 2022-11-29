# Aufgabe 7:
Farben = matrix(c(94, 84, 17, 20, 7, 119, 26, 68, 16, 29, 14, 5, 10, 54, 14, 15), nrow=4, byrow=TRUE)

colnames(Farben) = c('Blond', 'Braun', 'Rot', 'Schwarz')
rownames(Farben) = c('Blau', 'Braun', 'Grün', 'Haselnuss')

Farben.Randvert = addmargins(Farben)

write.csv2(Farben.Randvert, '/Users/gedeonvogt/Desktop/RWTH Aachen/4. Semester/R Praktikum/Ausgabe/Farben.csv')

chi_quad = Farben.Randvert[5,5] * (sum(Farben**2 / (Farben.Randvert[1:4, 5] %*% t(Farben.Randvert[5, 1:4]))) - 1)





# Aufgabe 8:
Raub = read.csv2('/Users/gedeonvogt/Desktop/RWTH Aachen/4. Semester/R Praktikum/Datensätze/raub.csv', header = TRUE)
Raub.tab = table(Raub)
prop.table(Raub.tab, 1)  # Relative Häufigkeiten bzgl Zeilen (mit 2 wäre bezüglich Spalten)

my.odds = function(m){
  
  if (((class(m)[1] != 'table') & (class(m)[1] != 'matrix')) 
      || (sum(dim(m) == c(2,2)) != 2)) {
      print(stop("Argument ist keine (2x2)-Matrix oder (2x2)-Kontigenztabelle"))
    }
  
  odds1 = Raub.tab[1,1] / Raub.tab[1,2]
  odds2 = Raub.tab[2,1] / Raub.tab[2,2]
  odds_ratio = odds1 / odds2
  return(c(odds1, odds2, odds_ratio, log(odds_ratio)))
}
