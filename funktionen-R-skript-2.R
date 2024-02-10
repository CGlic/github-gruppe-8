# Eine Funktion, die checkt, ob eine kategorielle Variable ordinal oder
# nominal ist

  # Eingabe:  x - Vektor mit kategorialer Variable (Factor)
  # Ausgabe:  "ordinal" oder "nominal" als (Factor)

kat_ord_or_norm <- function(x) {
  stopifnot(is.factor(x))
  
  if (is.ordered(x)) {
    return("ordinal")
  } else {
    return("nominal")
  }
}


# Eine Funktion, die prueft ob zwei kategoriale Variablen auf der gleichen
# Skala liegen

  # Eingabe:  v1, v2 - zwei kategoriale Variablen
  # Ausgabe:  TRUE, wenn beide auf der gleichen Skala liegen, sonst FALSE

kat_same_scale <- function(v1, v2) {
  stopifnot(is.factor(v1), is.factor(v2))
  
  skalen <- c(kat_ord_or_norm(v1), kat_ord_or_norm(v2))
  gleiche_skala <- all(skalen[1] == skalen)
  
  return(gleiche_skala)
}