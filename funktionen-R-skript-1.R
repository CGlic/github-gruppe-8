library("grid")
library("vcd")
library("ggplot2")
library("ggmosaic")

# Lade Hilfsfunktionen
source("funktionen-R-skript-2.R")


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
              sd = sd(x),
              length = length(x),
              quantile = quantile(x)))
}

# Eine Funktion, die verschiedene geeignete deskriptive Statistiken fuer 
# kategoriale Variablen berechnet und ausgibt

  # Eingabe:  x         - Vektor mit kategorialer Variable (Factor)
  # Ausgabe:  abs_hfgk  - abs. Haeufigkeiten pro Kategorien
  #           rel_hfgk  - rel. Haeufigkeiten ""          ""
  #           modus     - alle Werte die am haeufigsten vorkommen
  #           median    - Median der Werte (falls ordinal)
      # Welche Statistiken wollen wir hier vielleicht noch bestimmen?
deskr_kategorial <- function(x) {
  stopifnot(is.factor(x))
  
  # Bestimme, ob die Variable ordinal oder nominal ist
  skala <- kat_ord_or_norm(x)
  
  return(list(abs_hfgk = table(x), 
              rel_hfgk = table(x)/length(x),
              modus = table(x)[which(table(x) == max(table(x)))],
              median = ifelse(skala == "ordinal", 
                              median(as.numeric(x)), 
                              "Nicht anwendbar"),
              skala = skala
              )
         )
}

# Eine Funktion, die geeignete deskriptive bivariate Statistiken fuer 
# den Zusammenhang zwischen zwei kategorialen Variablen berechnet ausgibt

  # Eingabe:  v1, v2        - zwei kategoriale Variablen
  # Ausgabe:  kreuztabelle  - kreuztabelle fuer v1 und v2
      # Weitere angemessene Statistiken?
deskr_zus_kategorial <- function(v1, v2) {
  stopifnot(is.factor(v1), is.factor(v2))
  
  # Bestimme, ob die Variablen ordinal oder nominal sind und ob sie auf der
  # gleichen Skala liegen
  skalen <- c(kat_ord_or_norm(v1), kat_ord_or_norm(v2))
  gleiche_skala <- kat_same_scale(v1, v2)
  
  # Output
  output <- list()
  
  # Skalen
  output$skalen <- skalen
  output$gleiche_skala <- gleiche_skala
  
  # Kreuztabelle
  output$kreuztabelle <- table(v1, v2)
  
  # Fuege weitere Statistiken hinzu, die fuer die Kombination der Skalen
  # sinnvoll sind
  if(gleiche_skala) {
    # Chi-Quadrat-test
    chi2_test <- chisq.test(output$kreuztabelle)
    output$chi2_stat <- unname(chi2_test$statistic)
    
    # Nominal und Nominal
    if("nominal" %in% skalen) {
      # Cramers V
      output$cramers_v <-
        sqrt(output$chi2_stat / (sum(output$kreuztabelle) * 
                                   (min(nrow(output$kreuztabelle),
                                        ncol(output$kreuztabelle)) - 1)))
    }
    
    # Ordinal und Ordinal
    if("ordinal" %in% skalen) {
      # Spearmans Rangkorrelationskoeffizient
      output$spearmans_rho <- cor(as.numeric(v1), as.numeric(v2),
                                  method = "spearman")
    }
  }
  
  return(output)
}

# Eine Funktion, die geeignete deskriptive bivariate Statistiken fuer 
# den Zusammengang zwischen einer metrischen und einer dichotomen Variablen 
# berechnet und ausgibt

  # Eingabe:  m     - metrischer Vektor
  #           d     - dichotomer Vektor
  # Ausgabe:  Eine benannte Liste mit den Mittelwerten der metrischen Variablen
  #           fuer die beiden Kategorien der dichotomen Variablen, sowie den
  #           Varianzen, dazu die t-Statistik und den p-Wert des t-Tests
deskr_metr_dichot <- function(m, d) {
  stopifnot(is.numeric(m), is.factor(d), nlevels(d) == 2)
  
  # Mittelwerte fuer die jeweilige Gruppe der dichotomen Variable
  mean_1 <- mean(m[d == levels(d)[1]])
  mean_2 <- mean(m[d == levels(d)[2]])
  
  # Varianzen der Gruppen
  var_1 <- var(m[d == levels(d)[1]])
  var_2 <- var(m[d == levels(d)[2]])
  
  # t-Test
  t_test <- t.test(m ~ d)
  
  # F-Test
  f_test <- var.test(m ~ d)
  
  return(
    list(
      levels = levels(d),
      mean_1 = mean_1,
      mean_2 = mean_2,
      var_1 = var_1,
      var_2 = var_2,
      t_stat = t_test$statistic,
      t_p_value = t_test$p.value,
      f_stat = f_test$statistic,
      f_p_value = f_test$p.value
      )
    )
}

# Eine Funktion, die eine geeignete Visualisierung von drei oder vier 
# kategorialen Variablen erstellt

  # Eingabe: variables - Liste mit 3 oder 4 kategorialen Variablen
  # Ausgabe: Mosaikplot von 3 oder 4 kategorialen Variablen
vis_kategorial <- function(variables) {
  stopifnot(is.list(variables), length(variables) >= 3, length(variables) <= 4,
            all(sapply(variables, is.factor)))
  
  # Es gibt 3 oder 4 Variablen
  if(length(variables) == 3) {
    data <- table(variables[[1]], variables[[2]], variables[[3]])
    data <- as.data.frame(data)
    
    # Mosaicplot
    mos_plot <- ggplot(data) +
      geom_mosaic(aes(weight = Freq, x = product(Var1, Var2), fill = Var3)) +
      theme_bw()
  } else {
    data <-
      table(variables[[1]], variables[[2]], variables[[3]], variables[[4]])
    data <- as.data.frame(data)
    
    # Mosaicplot
    mos_plot <- ggplot(data) +
      geom_mosaic(aes(weight = Freq, x = product(Var1, Var2, Var3),
                      fill = Var4)) +
      theme_bw()
  }
  
  return(mos_plot)
}

# Freiwillig: weitere zur Deskription und Visualisierung geeignete Funktionen