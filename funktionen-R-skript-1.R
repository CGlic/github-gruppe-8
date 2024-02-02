# Soll spaeter mindestens die 6 Funktionen enthalten, die in der Aufgabe
# gefordert sind.

# Eine Funktion, die verschiedene geeignete deskriptive Statistiken fuer 
# metrische Variablen berechnet und ausgibt

  # Eingabe: x  - Vektor mit metrischen Werten
  # Ausgabe: Min, Max, Median, Mittelwert der metrischen Var.
deskr_metrisch <- function(x) {
  stopifnot(is.numeric(x))
  return(list(mean = mean(x), 
              min = min(x), 
              max = max(x), 
              var = var(x), 
              sd = sd(x)),
              length = length(x),
              quantile = quantile(x))
}

# Eine Funktion, die verschiedene geeignete deskriptive Statistiken für 
# kategoriale Variablen berechnet und ausgibt

  # Eingabe:  x         - Vektor mit kategorialer Variable (Factor)
  # Ausgabe:  abs_hfgk  - abs. Häufigkeiten pro Kategorien
  #           rel:hfgk  - rel. Häufigkeiten ""          ""
  #           modus     - alle Werte die am h�ufigsten vorkommen
      # Welche Statistiken wollen wir hier vielleicht noch bestimmen?
deskr_kategorial <- function(x) {
  stopifnot(is.factor(x))
  return(list(abs_hfgk = table(x), 
              rel_hfgk = table(x)/length(x),
              modus = table(x)[which(table(x) == max(table(x)))]))
}

# Eine Funktion, die geeignete deskriptive bivariate Statistiken für 
# den Zusammenhang zwischen zwei kategorialen Variablen berechnet ausgibt

  # Eingabe:  v1, v2        - zwei kategoriale Variablen
  # Ausgabe:  kreuztabelle  - kreuztabelle für v1 und v2
  #           Weitere angemessene Statistiken?
deskr_zus_kategorial <- function(v1, v2) {
  stopifnot(is.factor(v1), is.factor(v2))
  return(list(kreuztabelle = table(v1,v2)))
}

# Eine Funktion, die geeignete deskriptive bivariate Statistiken für 
# den Zusammengang zwischen einer metrischen und einer dichotomen Variablen 
# berechnet und ausgibt

  # Eingabe:  m     - metrischer  Vektor
  #           d     - dichotomer Vektor
  # Ausgabe:  mean1 - Mittelwert der Werte für die d = 0
  #           mean2 - Mittelwert der Werte für die d = 1
deskr_metr_dichot <- function(m, d) {
  stopifnot(is.numeric(m), is.factor(d), nlevels(d) == 2)
  mean_1 = as.numeric( mean(m[which(d == unique(d)[1])]))
  mean_2 = as.numeric( mean(m[which(d == unique(d)[2])]))
  temp_list <- list(mean1 = mean_1, mean2 = mean_2)
  namen <- c(levels(d)[1], levels(d)[2])
  names(temp_list) <- namen
  
  return(temp_list)
}

# Eine Funktion, die eine geeignete Visualisierung von drei oder vier 
# kategorialen Variablen erstellt

  # Eingabe: v1, v2, (v3)   - Vektoren mit kategorialen Variablen
  # Ausgabe: 
vis_kategorial <- function(v1, v2, v3) {
  stopifnot(is.factor(v2), is.factor(v3))
  
  data <- table(v1,v2,v3)
  #temp_values <- data.frame(v1,v2,v3)
  mos_plot <- mosaic(data)
  
  return(mos_plot)
}
# Freiwillig: weitere zur Deskription und Visualisierung geeignete Funktionen