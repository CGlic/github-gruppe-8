source("funktionen-R-skript-1.R")

titanic <- readRDS("titanic_clean.rds")



# (i) Eine Funktion, die verschiedene geeignete deskriptive Statistiken für 
#     metrische Variablen berechnet und ausgibt. 

#     (1) Deskriptive Analyse des Alters der Passagiere und visualiserung in 
#         einem Boxplot 

          deskr_age <- deskr_metrisch(titanic$Age)
          boxplot(titanic$Age, ylim = c(0,100))

#     (2) Deskriptive Analyse der Ticketpreise, die die Passagiere gezahlt haben
#         und visualiserung in einem Boxplot

          deskr_fare <- deskr_metrisch(titanic$Fare)
          boxplot(titanic$Fare, ylim = c(0,100))

          
          
# (ii) Eine Funktion, die verschiedene geeignete deskriptive Statistiken für 
#      kategoriale Variablen berechnet und ausgibt.

#      (1) Dekripitve Analyse der Passagierklasse  und visualisierung
#          in einem Balkendiagramm

          deskr_Pclass <- deskr_kategorial(titanic$Pclass)

          # Balkendiagramm für absolute Häufigkeiten
          barplot(deskr_Pclass$abs_hfgk, 
                  main = "Absolute Häufigkeiten der Passagierklasse", 
                  xlab = "Passagierklasse", 
                  ylab = "Absolute Häufigkeit", 
                  names.arg = c("1.Klasse", "2.Klasse", "3.Klasse"),
                  col = "lightblue", # Farbe der Balken
                  border = "black", # Farbe der Balkenränder
                  ylim = c(0, 800), # Begrenzung der y-Achse von 0 bis 1
                  beside = TRUE) # Nebeneinander angeordnete Balken
          
          # Hinzufügen einer Linie und einer Legende für den Median
          abline(v = deskr_Pclass$median, col = "red", lwd = 2)
          legend("topleft", legend = "Median", col = "red", lwd = 2, bty = "n")
          
                    
          # Balkendiagramm für relative Häufigkeiten
          barplot(deskr_Pclass$rel_hfgk, 
                  main = "Relative Häufigkeiten der Passagierklasse", 
                  xlab = "Passagierklasse", 
                  ylab = "Relative Häufigkeit", 
                  names.arg = c("1.Klasse", "2.Klasse", "3.Klasse"),
                  col = "lightblue", # Farbe der Balken
                  border = "black", # Farbe der Balkenränder
                  ylim = c(0, 1), # Begrenzung der y-Achse von 0 bis 1
                  beside = TRUE) # Nebeneinander angeordnete Balken
          
          # Hinzufügen einer Linie und einer Legende für den Median
          abline(v = deskr_Pclass$median, col = "red", lwd = 2)
          legend("topleft", legend = "Median", col = "red", lwd = 2, bty = "n")
          
         
          
          
#      (2) Dekripitve Analyse des Überlebensstatus der Passagiere und
#          visualiserung in einem Balkendiagramm
          
          deskr_survived <- deskr_kategorial(titanic$Survived)
          
          #Barplot für relative Häufigkeit
          barplot(deskr_survived$rel_hfgk, 
                  main = "Relative Häufigkeiten der Überlebenden", 
                  xlab = "Mortalität", 
                  ylab = "Relative Häufigkeit", 
                  names.arg = c("Nicht Überlebt", "Überlebt"),
                  col = "lightblue", # Farbe der Balken
                  border = "black", # Farbe der Balkenränder
                  ylim = c(0, 1), # Begrenzung der y-Achse von 0 bis 1
                  beside = TRUE) # Nebeneinander angeordnete Balken
          
          
          #Barplot für relative Häufigkeit
          barplot(deskr_survived$abs_hfgk, 
                  main = "Absolute Häufigkeiten der Überlebenden", 
                  xlab = "Mortalität", 
                  ylab = "Absolute Häufigkeit", 
                  names.arg = c("Nicht Überlebt", "Überlebt"),
                  col = "lightblue", # Farbe der Balken
                  border = "black", # Farbe der Balkenränder
                  ylim = c(0, 1000), # Begrenzung der y-Achse von 0 bis 1000
                  beside = TRUE) # Nebeneinander angeordnete Balken
          
          
          
# (iii) Eine Funktion, die geeignete deskriptive bivariate Statistiken für den 
#       Zusammenhang zwischen zwei kategorialen Variablen berechnet ausgibt
          
#       (1) Analyse des Zusammenhangs zwischen Geschlecht (Sex) und dem 
#           Überlebensstatus und visualisierung in einem Mosaikplot
          
          deskr_sex_survived <- deskr_zus_kategorial(titanic$Sex,titanic$Survived)
          
          mosaicplot(deskr_sex_survived$kreuztabelle,
                     main = "Kreuztabelle: Geschlecht vs. Mortalität",
                     xlab = "Geschlecht", 
                     ylab = "Mortalität", 
                     color = c("red", "lightgreen"), # Farben für die Rechtecke
                     legend = TRUE) # Legende anzeige
          
#       (2) Analyse des Zusammenhangs zwischen Geschlecht (Sex) und Aufstiegshafen
#           und visualiserung in einem Mosaikplot
         
            deskr_sex_embarked <- deskr_zus_kategorial(titanic$Sex,titanic$Embarked)          
           
            mosaicplot(deskr_sex_embarked$kreuztabelle,
                       main = "Kreuztabelle: Geschlecht vs. Deck",
                       xlab = "Geschlecht", 
                       ylab = "Deck", 
                       color = c("#F0F8FF","lightblue","darkblue"), # Farben für die Rechtecke
                       legend = TRUE) # Legende anzeige

#       (3) Analyse des Zusammenhangs zwischen Geschlecht und 
#           Passagierklasse (Pclass) und visualiserung in einem ...
            
            deskr_sex_Pclass <- deskr_zus_kategorial(titanic$Sex,titanic$Pclass) 
            
            mosaicplot(deskr_sex_Pclass$kreuztabelle,
                       main = "Kreuztabelle: Geschlecht vs. Passagierklasse",
                       xlab = "Geschlecht", 
                       ylab = "Passagierklasse", 
                       color = c("#F0F8FF","lightblue","darkblue"), # Farben für die Rechtecke
                       legend = TRUE) # Legende anzeige           
            
#       (4) Analyse des Zusammenhangs zwischen Überleben (Survived) und 
#           Passagierklasse (Pclass) und visualiserung in einem ...
          
            deskr_survived_Pclass <- deskr_zus_kategorial(titanic$Survived,
                                                          titanic$Pclass) 
            
            mosaicplot(deskr_survived_Pclass$kreuztabelle,
                       main = "Kreuztabelle: Überlebensstatus vs. Passagierklasse",
                       xlab = "Überlebensstatus", 
                       ylab = "Passagierklasse", 
                       color = c("#F0F8FF","lightblue","darkblue"), # Farben für die Rechtecke
                       legend = TRUE) # Legende anzeige
            
            
            
# (iv) Eine Funktion, die geeignete deskriptive bivariate Statistiken für den 
#      Zusammengang zwischen einer metrischen und einer dichotomen Variablen 
#      berechnet und ausgibt
            
#      (1) Analyse des Zusammenhangs zwischen Überleben (Survived) und Alter und
#          visualisierung in einem ...
            
            metr.age_dichot.survived <- deskr_metr_dichot(titanic$Age,
                                                          titanic$Survived)
            
            # Balkendiagramm für Varianzen
            barplot(c(metr.age_dichot.survived$var_1, metr.age_dichot.survived$var_2), 
                    names.arg = c("No", "Yes"),
                    main = "Varianzvergleich der Alter von Überlebenden und Nicht-Überlebenden",
                    ylab = "Varianz",
                    xlab = "Survived",
                    col = c("blue", "green"),
                    ylim = c(0, max(c(metr.age_dichot.survived$var_1, metr.age_dichot.survived$var_2)) * 1.2))
            
            # Balkendiagramm für Durchschnitt
            barplot(c(metr.age_dichot.survived$mean_1, metr.age_dichot.survived$mean_2), 
                    names.arg = c("No", "Yes"),
                    main = "Durchschnittsvergleich der Alter von Überlebenden und Nicht-Überlebenden",
                    ylab = "Durchschnitt",
                    xlab = "Survived",
                    col = c("blue", "green"),
                    ylim = c(0, max(c(metr.age_dichot.survived$mean_1, metr.age_dichot.survived$mean_2)) * 1.2))
            
#      (2) Analyse des Zusammenhangs zwischen Überleben (Survived) und 
#          Ticketpreis (Fare) visualisierung in einem Balkendiagramm
            
            metr.fare_dichot.survived <- deskr_metr_dichot(titanic$Fare,
                                                              titanic$Survived)
            
            # Balkendiagramm für Varianzen
            barplot(c(metr.fare_dichot.survived$var_1, metr.fare_dichot.survived$var_2), 
                    names.arg = c("No", "Yes"),
                    main = "Varianzvergleich der Ticketpreise von Überlebenden und Nicht-Überlebenden",
                    ylab = "Varianz",
                    xlab = "Überlebensrate",
                    col = c("blue", "green"),
                    ylim = c(0, max(c(metr.fare_dichot.survived$var_1, metr.fare_dichot.survived$var_2)) * 1.2))
            
            # Balkendiagramm für Durchschnitt
            barplot(c(metr.fare_dichot.survived$mean_1, metr.fare_dichot.survived$mean_2), 
                    names.arg = c("No", "Yes"),
                    main = "Durchschnittsvergleich der Ticketpreise von Überlebenden und Nicht-Überlebenden",
                    ylab = "Durchschnitt",
                    xlab = "Überlebensstatus",
                    col = c("blue", "green"),
                    ylim = c(0, max(c(metr.fare_dichot.survived$mean_1, metr.fare_dichot.survived$mean_2)) * 1.2))
            
           
#      (3) Analyse des Zusammenhangs zwischen Geschlecht (Sex) und Anzahl der 
#          Geschwister/Ehepartner an Bord (SibSp) und visualiserung in einem
#          Balkendiagramm
            
            metr.SibSp_dichot.sex <- deskr_metr_dichot(titanic$SibSp,titanic$Sex)
            
            # Balkendiagramm für Varianzen
            barplot(c(metr.SibSp_dichot.sex$var_1, metr.SibSp_dichot.sex$var_2), 
                    names.arg = c("No", "Yes"),
                    main = "Varianzvergleich der SibSp von Überlebenden und Nicht-Überlebenden",
                    ylab = "Varianz",
                    xlab = "Geschlecht",
                    col = c("blue", "green"),
                    ylim = c(0, max(c(metr.SibSp_dichot.sex$var_1, metr.SibSp_dichot.sex$var_2)) * 1.2))
            
            
            # Balkendiagramm für Durchschnitt
            barplot(c(metr.SibSp_dichot.sex$mean_1, metr.SibSp_dichot.sex$mean_2), 
                    names.arg = c("No", "Yes"),
                    main = "Durchschnittsvergleich der Ticketpreise von Überlebenden und Nicht-Überlebenden",
                    ylab = "Durchschnitt",
                    xlab = "Überlebensstatus",
                    col = c("blue", "green"),
                    ylim = c(0, max(c(metr.SibSp_dichot.sex$mean_1, metr.SibSp_dichot.sex$mean_2)) * 1.2))
            
#      (4) Analyse des Zusammenhangs zwischen Geschlecht (Sex) und Anzahl der 
#          Eltern/Kinder an Bord (Parch) und visualiserung in einem Balkendiagramm
            
            metr.Parch_dichot.sex <- deskr_metr_dichot(titanic$Parch,titanic$Sex)
           
            
            # Balkendiagramm für Varianzen
            barplot(c(metr.Parch_dichot.sex$var_1, metr.Parch_dichot.sex$var_2), 
                    names.arg = c("No", "Yes"),
                    main = "Varianzvergleich der Parch von Männern und Frauen",
                    ylab = "Varianz",
                    xlab = "Überlebensrate",
                    col = c("blue", "green"),
                    ylim = c(0, max(c(metr.Parch_dichot.sex$var_1, metr.Parch_dichot.sex$var_2)) * 1.2))
            
            # Balkendiagramm für Durchschnitt
            barplot(c(metr.Parch_dichot.sex$mean_1, metr.Parch_dichot.sex$mean_2), 
                    names.arg = c("No", "Yes"),
                    main = "Durchschnittsvergleich der Parch von Männern und Frauen",
                    ylab = "Durchschnitt",
                    xlab = "Überlebensstatus",
                    col = c("blue", "green"),
                    ylim = c(0, max(c(metr.Parch_dichot.sex$mean_1, metr.Parch_dichot.sex$mean_2)) * 1.2))
            
# (v) Eine Funktion, die eine geeignete Visualisierung von drei oder vier 
#     kategorialen Variablen erstellt
            
#      (1) Visualisierung der Variablen Überleben (Survived) und Geschlecht 
#          (Sex) und Passagierklasse (Pclass) und Deck
            
            # Erstellen einer Liste aus drei kategorialen Variablen
            list_kategorial_var <- list(titanic$Survived,titanic$Sex,
                                        titanic$Pclass)
            
            vis_kategorial(list_kategorial_var)
            
            

