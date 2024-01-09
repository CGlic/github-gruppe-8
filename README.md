# Gruppe 8
Das Repository der Gruppe 8, zur GitHub Gruppenarbeit, welche die dritte Teilleistung des Seminars Wissenschaftliches Arbeiten darstellt.

## Abgabe
25.02.2024, 23:59 Uhr, als Link zu diesem Repository per Moodle. (Wichtig: Vorher das Repository auf public stellen)

## Mitglieder
- Paula Lorenz
- Kjell Noack
- Gil Feldhoff
- Emma-Lynn Plümper

## Datensatz
In Moodle ist wird ein Datensatz „titanic.csv“ zur Verfügung gestellt. Dieser enthält die
nachfolgenden Informationen über die Passagiere auf der Titanic:
- PassengerID: ID-Variable
- Survived: Hat den Untergang der Titanic überlebt? Ja (1), Nein (0)
- Pclass: Klasse des Reisenden (ordinal mit 1 > 2 > 3)
- Name: Name des Reisenden
- Sex: Geschlecht (male/female)
- Age: Alter in Jahren beim Untergang (Für Kleinkinder auch in Dezimalzahlen)
- SibSp: Anzahl an Geschwistern und Ehefrauen an Bord
- Parch: Anzahl an Eltern und Kinder an Bord
- Ticket: Ticketnummer
- Fare: Ticketpreis
- Cabin: Kabinennummer
- Embarked: Zustiegshafen (C = Cherbourg; Q = Queenstown; S = Southampton)


## Aufgabenstellung
Über euer GitHub Repository unter Verwendung von Git und in gleichbeteiligter
Gruppenarbeit, bearbeitet bitte die folgenden Aufgaben:

1. **Zwei** Gruppenmitglieder erstellen ein R-Skript, mit welchem der Titanic-Datensatz eingelesen und für eine Analyse vorverarbeitet wird. Am Ende des Skriptes sollen nur noch sinnvolle Variablen im Datensatz vorhanden sein, die auch bei einer Analyse sinnvoll verwendet werden können. Bearbeitet hierfür die folgenden Schritte:
    - Extrahiert aus dem Namen eine Variable mit der Anrede der Person, d.h. „Mr.“, „Mrs.“, „Mse.“ usw., damit später fehlende Werte im Alter ersetzt werden können. Beachtet hierbei, dass gewisse Anreden wie „Ms.“, „Miss.“ oder „Mlle“ inhaltlich gleichbedeutend sind (in diesem Beispiel eine junge, unverheiratete Frau). Die Anrede „Master“ bezeichnet einen kleinen Jungen.
    - Codiert die Variablen „Survived“, „Sex“, „Embarked“ als factor um.
    - Überführt die Variable „Pclass“ in einen ordered-factor.
    - Imputiert fehlende Werte in der Variable „Age“ mithilfe der erzeugten Variable „Anrede“ über ein Imputationsverfahren eurer Wahl (z.B. arithmetisches Mittel, Median, usw.)
    - Extrahiert aus der Variable „Cabin“ die folgenden Informationen und erzeugt neue Variablen hierfür:
      - Backbord oder Steuerbord? Tipp: Kabinen mit einer ungeraden Nummer liegen auf Steuerbord, die anderen auf Backbord.
      - Deck: Vorangehender Buchstabe der Kabinennummer
      - Einträge mit unbekannter Kabinennummer, d.h. „“ setzt ihr auf NA.
    - Entfernt am Ende die Variablen „PassengerID“, „Name“, „Ticket“ und „Cabin“ aus dem Datensatz
    - Speichert das R-Skript, sowie den neuen Datensatz in dem GitHub-Repository ab.

2. **Gemeinsam als ganze Gruppe** erstellt zwei weitere R-Skripte. Im 4. Schritt sollen diejenigen Gruppenmitglieder, die nicht an (1.) gearbeitet haben, den Datensatz analysieren (Deskription und Visualisierung). Hierzu sollen nun Funktionen erstellt werden, die dabei genutzt werden.
   - Funktionen-R-Skript 1 soll (mindestens) folgende Funktionen enthalten:
      - Eine Funktion, die verschiedene geeignete deskriptive Statistiken für metrische Variablen berechnet und ausgibt
      -  Eine Funktion, die verschiedene geeignete deskriptive Statistiken für kategoriale Variablen berechnet und ausgibt
      -  Eine Funktion, die geeignete deskriptive bivariate Statistiken für den Zusammenhang zwischen zwei kategorialen Variablen berechnet ausgibt
      -  Eine Funktion, die geeignete deskriptive bivariate Statistiken für den Zusammengang zwischen einer metrischen und einer dichotomen Variablen berechnet und ausgibt
      -  Eine Funktion, die eine geeignete Visualisierung von drei oder vier kategorialen Variablen erstellt
      -  Freiwillig: weitere zur Deskription und Visualisierung geeignete Funktionen
   - Funktionen-R-Skript 2 soll Helfer-Funktionen enthalten, die nicht selbst zur Deskription und Visualisierung der Daten verwendet werden, sondern die nur in Funktionen-Skript 1 Anwendung finden ( interne Funktionen). Funktionen-R-Skript 2 muss mindestens eine Funktion enthalten.

3. Denkt auch an eine gute Dokumentation aller Funktionen. Nutzt euer GitHub Repository um darüber zu diskutieren, welche Funktionen sinnvoll und notwendig sind.

4. Diejenigen Gruppenmitglieder, die nicht an (1.) gearbeitet haben, sollen mit Hilfe der in (3.) in Funktionen-R-Skript 1 erstellten Funktionen den aufgeräumten Datensatz aus (1.) analysieren (Deskription und Visualisierung). Hierzu soll ein weiteres Skript im Repository erstellt werden. Hierbei sollte das R-Skript im Minimum jede der Funktionen (i) bis (vi) aus Funktionen-R-Skript 1 einmal anwenden. Denkt bei der Aufgabe über sinnvolle Analysen nach, z.B. Überlebensrate gegen andere Variablen. Wie verhält sich der Ticketpreis?

5. Diskutiert anschließend im GitHub Repository als ganze Gruppe die Ergebnisse. Möglicherweise haben die Gruppenmitglieder, die an (1.) gearbeitet haben noch weitere Ideen.