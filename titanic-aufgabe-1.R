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
´# Codierung der Variablen Sex, Survived,Embarkeds 
titanic$Sex <- as.factor(ifelse(titanic$Sex == "male", 1, 0))
titanic$Survived <- as.factor(titanic$Survived)
titanic$Embarked <- as.factor(titanic$Embarked)

# Pclass in einen ordered-factor überführen
titanic$Pclass <- as.factor(titanic$Pclass)

# fehlende Age-Werte imputieren durch arithmetisches Mittel
    # Welches Verfahren wollen wir hier nutze?
  
# fehlende Werte in Cabin zu NA's ändern
titanic$Cabin[which(titanic$Cabin == "")] <- NA

# Steuerbord und Backbord Variable hinzufügen
temp_side <- as.numeric(gsub( ".*?([0-9]+).*", "\\1", titanic$Cabin))
    # Hier gibts ne Warnung, funktioniert aber trotzdem
titanic$Side <- ifelse(temp_side %% 2 == 0, "B", "S")

# Variable für Deck hinzufügen
titanic$Deck <- as.character(gsub("[0-9]", "", titanic$Cabin))

# Variablen entfernen
titanic = titanic[,c(2,3,5,6,7,8,10,12,13,14,15)]
    # Hier gibts sicherlich ne bessere Methode, aber wusste nicht wie
