# Soll spaeter mindestens die 6 Funktionen enthalten, die in der Aufgabe
# gefordert sind.

# Eine Funktion, die verschiedene geeignete deskriptive Statistiken für 
# metrische Variablen berechnet und ausgibt

  # Eingabe: dataframe
  # Ausgabe: Min, Max, 1. und 3. Quantil, Median, Mittelwert der metrischen Var.
deskr_metrisch <- function(df) {
  summary(data[sapply(df,is.numeric)])
}
  # Vielleicht macht es auch mehr Sinn die metrischen Variablen direkt
  # zu übergeben anstatt automatisch rausfiltern zu lassen?

# Eine Funktion, die verschiedene geeignete deskriptive Statistiken für 
# kategoriale Variablen berechnet und ausgibt

  # Eingabe: dataframe
  # Ausgabe: Häufigkeiten der einzelnen Kategorien
deskr_kategorial <- function(df) {
  index <- which(sapply(df, is.factor))
  print("Häufigkeiten der Levels")
  for(i in index) {
    print(table(df[i]))
  }
}
  # Welche Statistiken wollen wir hier vielleicht noch bestimmen?

# Eine Funktion, die geeignete deskriptive bivariate Statistiken für 
# den Zusammenhang zwischen zwei kategorialen Variablen berechnet ausgibt

  # Eingabe: zwei kategoriale Variablen (v1, v2)
  # Ausgabe: 
deskr_zus_kategorial <- function(v1, v2) {
  
}

# Eine Funktion, die geeignete deskriptive bivariate Statistiken für 
# den Zusammengang zwischen einer metrischen und einer dichotomen Variablen 
# berechnet und ausgibt

  # Eingabe: einen metrischen (m) und einen dichotomen Vektor (v)
  # Ausgabe:
deskr_zus_metr_dichot <- function(m, v) {
  
}

# Eine Funktion, die eine geeignete Visualisierung von drei oder vier 
# kategorialen Variablen erstellt

  # Eingabe: drei oder vier kategoriale Variablen (v1, v2, v3, (v4))
  # Ausgabe: 
vis_kategorial <- function(v1, v2, v3, ...) {
  
}
# Freiwillig: weitere zur Deskription und Visualisierung geeignete Funktionen