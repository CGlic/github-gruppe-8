source("funktionen-R-skript-1.R")

titanic = readRDS("titanic_clean.rds")

# (i) Eine Funktion, die verschiedene geeignete deskriptive Statistiken für 
#     metrische Variablen berechnet und ausgibt. 

#     (1) Deskriptive Analyse des Alters der Passagiere und visualiserung in 
#         einem Boxplot 

          deskr_age <- deskr_metrisch(titanic$Age)
          boxplot(titanic_clean$Age, ylim = c(0,100))

#     (2) Deskriptive Analyse der Ticketpreise, die die Passagiere gezahlt haben
#         und visualiserung in einem Boxplot

          deskr_fare <- deskr_metrisch(titanic$Fare)
          boxplot(titanic$Fare, ylim = c(0,100))


# (ii) Eine Funktion, die verschiedene geeignete deskriptive Statistiken für 
#      kategoriale Variablen berechnet und ausgibt.

#      (1) Dekripitve Analyse der Passagierklasse der Passagiere und 
#          visualisierung in einem Balkendiagramm

          deskr_Pclass <- deskr_kategorial(titanic$Pclass)

          # Balkendiagramm für absolute Häufigkeiten
          barplot(deskr_Pclass$abs_hfgk, 
                  main = "Absolute Häufigkeiten der Passagierklasse", 
                  xlab = "Passagierklasse", 
                  ylab = "Absolute Häufigkeit", 
                  names.arg = c("1.Klasse", "2.Klasse", "3.Klasse"),
                  col = "skyblue", # Farbe der Balken
                  border = "black", # Farbe der Balkenränder
                  ylim = c(0, 800), # Begrenzung der y-Achse von 0 bis 1
                  beside = TRUE, # Nebeneinander angeordnete Balken
                  legend.text = "Absolute Häufigkeit", # Beschriftung der Legende
                  args.legend = list(x = "topleft")) # Legende oben links anzeigen
          
          # Linie für den Median
          abline(v = deskr_Pclass$median, col = "red", lwd = 2)
          
          # Hinzufügen einer Legende für den Median
          legend("topleft", legend = "Median", col = "red", lwd = 2, bty = "n")
          
                    
          # Balkendiagramm für relative Häufigkeiten
          barplot(deskr_Pclass$rel_hfgk, 
                  main = "Relative Häufigkeiten der Passagierklasse", 
                  xlab = "Passagierklasse", 
                  ylab = "Relative Häufigkeit", 
                  names.arg = c("1.Klasse", "2.Klasse", "3.Klasse"),
                  col = "skyblue", # Farbe der Balken
                  border = "black", # Farbe der Balkenränder
                  ylim = c(0, 1), # Begrenzung der y-Achse von 0 bis 1
                  beside = TRUE, # Nebeneinander angeordnete Balken
                  legend.text = "Relative Häufigkeit", # Beschriftung der Legende
                  args.legend = list(x = "topleft")) # Legende oben links anzeigen
          
          # Linie für den Median
          abline(v = deskr_Pclass$median, col = "red", lwd = 2)
          
          # Hinzufügen einer Legende für den Median
          legend("topleft", legend = "Median", col = "red", lwd = 2, bty = "n")
          
          

