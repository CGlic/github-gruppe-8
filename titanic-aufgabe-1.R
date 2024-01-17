# Gruppe 08
# Aufgabe 1

titanic <- read.csv("~/GitHub/github-gruppe-8/titanic.csv")
View(titanic)

# Anrede in Salutation extrahieren
temp_split <- strsplit(titanic$Name, ", ")
temp_split <- unlist(lapply(temp_split, "[", c(2)))
temp_split <- strsplit(temp_split, "\\.")
temp_split <- unlist(lapply(temp_split, "[", c(1)))
titanic$Salutation <- temp_split 
rm(temp_split)

# Codierung der Variablen Sex, Survived,Embarkeds 
titanic$Sex <- as.factor(ifelse(titanic$Sex == "male", 1, 0))
titanic$Survived <- as.factor(titanic$Survived)
titanic$Embarked <- as.factor(titanic$Embarked)

# Pclass in einen ordered-factor überführen
titanic$Pclass <- as.factor(titanic$Pclass)

# fehlende Age-Werte imputieren durch Median der Anrede
  
    # Prüfen, bei welchen Anreden NA's vorhanden sind
  unique(titanic$Salutation[which(is.na(titanic$Age))])
    # Prüfen, welche Anreden existieren
  unique(titanic$Salutation)
  
    #Mediane der gesuchten Anreden finden
  med_age_mr <- median(titanic$Age[which(titanic$Salutation == "Mr")], na.rm = TRUE)
          # hier möglichwerweise auch: Don, Rev, Major, Sir, Col, Capt, Jonkheer?
  med_age_master <- median(titanic$Age[which((titanic$Salutation == "Master"))], na.rm = TRUE)
  med_age_mrs <- median(titanic$Age[which((titanic$Salutation == "Mrs"))], na.rm = TRUE)
          # hier möglicherweise auch: Lady, the Countess?
  med_age_ms <- median(titanic$Age[titanic$Salutation %in% list("Ms", "Mlle", "Miss")], na.rm = TRUE)
  med_age_dr <- median(titanic$Age[titanic$Salutation == "Dr"], na.rm = TRUE)
  
  
    # Index der NAs finden             
  age_na <- which(is.na(titanic$Age))
  
  index_na_mr <- intersect(age_na, which(titanic$Salutation %in% list("Mr")))
  index_na_master <- intersect(age_na, which(titanic$Salutation %in% list("Master")))
  index_na_mrs <- intersect(age_na, which(titanic$Salutation %in% list("Mrs")))
  index_na_ms <- intersect(age_na, which(titanic$Salutation %in% list("Ms", "Mlle", "Miss")))
  index_na_dr <- intersect(age_na, which(titanic$Salutation %in% list("Dr")))
  
    # NAs ersetzen
  titanic$Age[index_na_mr] <- med_age_mr
  titanic$Age[index_na_master] <- med_age_master
  titanic$Age[index_na_mrs] <- med_age_mrs
  titanic$Age[index_na_ms] <- med_age_ms
  titanic$Age[index_na_dr] <- med_age_dr

  # fehlende Werte in Cabin zu NA's ändern
titanic$Cabin[which(titanic$Cabin == "")] <- NA
  # Besondere Handhabung bei mehreren Kabinennummern? (s. z.B. Passagier Nr. 89)

# Steuerbord und Backbord Variable hinzufügen
temp_side <- as.numeric(gsub( ".*?([0-9]+).*", "\\1", titanic$Cabin))
    # Hier gibts ne Warnung, funktioniert aber trotzdem
titanic$Side <- ifelse(temp_side %% 2 == 0, "B", "S")

# Variable für Deck hinzufügen
titanic$Deck <- as.character(gsub("[0-9]", "", titanic$Cabin))

# Variablen entfernen
titanic = titanic[,c(2,3,5,6,7,8,10,12,13,14,15)]
    # Hier gibts sicherlich ne bessere Methode, aber wusste nicht wie
