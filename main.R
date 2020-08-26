# Umfrage zur Evaluation der digitalen Lehre am Institut für Digital
# aktiv: 08.07.2020 — 02.08.2020
# letzte Antwort am 24.07.2020
# 22 Fragen
# 8 Gruppen


library(tidyverse)

#### IMPORT ####

import <- read.csv2("data/results-allgemein-dl.csv",header=T,strip.white=TRUE)

# inspect data
nrow(import) # 64 participants
ncol(import) # 76 columns


#### PREPROCESSING ####

# remove empty rows
dataset <- import[!(is.na(import$TT1) | import$TT1==""), ]

# inspect data again
nrow(dataset) # 49 rows
# summary(dataset)


##### ANALYSIS #####

get_percentage <- function(x, y){
  y/x*100
}


#### GRUPPE 1: Tools und Technik ####

#### TT — FRAGE 1 ####
# Haben Sie für Ihr Studium eigenständig zusätzliche Tools benutzt, 
# die in den Lehrveranstaltungen nicht verwendet wurden? Falls ja, welche?
# TT1; TT1.comment.

count(filter(dataset, TT1 == "Ja")) # 17
get_percentage(46, 17) # 34,7%
count(filter(dataset, TT1 == "Nein")) # 29
get_percentage(46, 29) # 59,2%
count(filter(dataset, TT1 == "Keine Antwort")) # 3
count(dataset[!(is.na(dataset$TT1.comment.) | dataset$TT1.comment.==""), ]) #16

TT1_answers <- subset(trimws(tolower(dataset$TT1.comment.)), 
                      dataset$TT1.comment. != "" & 
                        !is.na(dataset$TT1.comment.))
TT1_answers

tools <- vector()
for (TT1_answer in TT1_answers) {
 for (tool in strsplit(TT1_answer, ", ")) {
   tools <- c(tools, tool)
 }
}

tool_freq <- sort(table(tools), decreasing = TRUE)
tool_freq


#### TT — FRAGE 2 ####
# Wie wichtig sind Ihnen Sicherheits- und Datenschutzaspekte 
# bei der Auswahl von Tools für die digitale Lehre?
# TT2

count(filter(dataset, TT2 == "Keine Antwort")) # 1
TT2_answered <- filter(dataset, TT2 != "Keine Antwort")
count(TT2_answered) # 48

count(filter(dataset, TT2 == "Überhaupt nicht wichtig")) # 0
count(filter(dataset, TT2 == "Eher nicht wichtig")) # 9
get_percentage(48, 9) # 18,8%
count(filter(dataset, TT2 == "Eher wichtig")) # 22
get_percentage(48, 22) # 45,8%
count(filter(dataset, TT2 == "Sehr wichtig")) # 17
get_percentage(48, 17) # 35,4%

TT2_answered$TT2[TT2_answered$TT2 == "Überhaupt nicht wichtig"] <- 0
TT2_answered$TT2[TT2_answered$TT2 == "Eher nicht wichtig"] <- 1
TT2_answered$TT2[TT2_answered$TT2 == "Eher wichtig"] <- 2
TT2_answered$TT2[TT2_answered$TT2 == "Sehr wichtig"] <- 3

TT2_mean <- mean(as.numeric(TT2_answered$TT2)) # 2,2 / 0-3


#### TT — FRAGE 3 ####
# Bitte geben Sie an, in welchem Ausmaß jedes der aufgeführten 
# technischen Probleme Ihre Lernerfahrung beeinträchtigt hat.

TT3 <- dataset
TT3[ , 6:11 ]

TT3[6:11][TT3[6:11] == "Überhaupt nicht"] <- 0
TT3[6:11][TT3[6:11] == "Eher wenig"] <- 1
TT3[6:11][TT3[6:11] == "Eher viel"] <- 2
TT3[6:11][TT3[6:11] == "Sehr viel"] <- 3


# TT3.SQ003.: Sehr schlechte Audioqualität bei Live-Streams

count(filter(TT3, TT3.SQ003. == "Keine Antwort")) # 1
TT3.SQ003._answered <- filter(TT3, TT3.SQ003. != "Keine Antwort")
count(TT3.SQ003._answered) # 48
TT3.SQ003._mean <- mean(as.numeric(TT3.SQ003._answered$TT3.SQ003.)) # 1,4
get_percentage(3, TT3.SQ003._mean) # 45,1%


# TT3.SQ002.: Sehr schlechte Videoqualität bei Live-Streams

count(filter(TT3, TT3.SQ002. == "Keine Antwort")) # 2
TT3.SQ002._answered <- filter(TT3, TT3.SQ002. != "Keine Antwort")
count(TT3.SQ002._answered) # 47
TT3.SQ002._mean <- mean(as.numeric(TT3.SQ002._answered$TT3.SQ002.)) # 1,1
get_percentage(3, TT3.SQ002._mean) # 37,6%


# TT3.SQ005.: Benachrichtigungen via Anwendungen kommen nicht rechtzeitig an

count(filter(TT3, TT3.SQ005. == "Keine Antwort")) # 2
TT3.SQ005._answered <- filter(TT3, TT3.SQ005. != "Keine Antwort")
count(TT3.SQ005._answered) # 47
TT3.SQ005._mean <- mean(as.numeric(TT3.SQ005._answered$TT3.SQ005.)) # 0,6
get_percentage(3, TT3.SQ005._mean) # 21,3%


# TT3.SQ001.: Anwendungen reagieren nur langsam oder bleiben hängen

count(filter(TT3, TT3.SQ001. == "Keine Antwort")) # 0
TT3.SQ001._answered <- filter(TT3, TT3.SQ001. != "Keine Antwort")
count(TT3.SQ001._answered) # 49
TT3.SQ001._mean <- mean(as.numeric(TT3.SQ001._answered$TT3.SQ001.)) # 1
get_percentage(3, TT3.SQ001._mean) # 34,7%


# TT3.SQ006.: Das Herunterladen von Dateien ist extrem langsam oder schlägt fehl

count(filter(TT3, TT3.SQ006. == "Keine Antwort")) # 0
TT3.SQ006._answered <- filter(TT3, TT3.SQ006. != "Keine Antwort")
count(TT3.SQ006._answered) # 49
TT3.SQ006._mean <- mean(as.numeric(TT3.SQ006._answered$TT3.SQ006.)) # 0,9
get_percentage(3, TT3.SQ006._mean) # 29,9%


# TT3.SQ004.: Das Hochladen von Dateien ist extrem langsam oder schlägt fehl

count(filter(TT3, TT3.SQ004. == "Keine Antwort")) # 1
TT3.SQ004._answered <- filter(TT3, TT3.SQ004. != "Keine Antwort")
count(TT3.SQ004._answered) # 48
TT3.SQ004._mean <- mean(as.numeric(TT3.SQ004._answered$TT3.SQ004.)) # 0,8
get_percentage(3, TT3.SQ004._mean) #26,4%


#### TT — FRAGE 4 ####
# Falls es andere technischen Probleme gab, die ihre Lernerfahrung 
# beeinträchtigt haben, geben Sie diese bitte unten an.
# TT4

TT4_df <- dataset[!(is.na(dataset$TT4) | dataset$TT4==""), ]
count(TT4_df) # 11
answers_TT4 <- vector()
for (answer_TT4 in TT4_df$TT4) {
  answer_TT4 <- trimws(str_replace_all(answer_TT4, "\n", " "))
  answers_TT4 <- c(answers_TT4, answer_TT4)
}
answers_TT4


#### GRUPPE 2: Kommunikation mit Dozierenden ####

#### KD — FRAGE 1 ####
# Hat sich die Verfügbarkeit/Erreichbarkeit der Dozierenden im Vergleich
# zu in der Zeit der Präsenzlehre insgesamt verändert?
# KD1

count(filter(dataset, KD1 == "Keine Antwort")) # 6
KD1_answered <- dataset[!(is.na(dataset$KD1) | dataset$KD1=="" | dataset$KD1=="Keine Antwort"), ]
count(KD1_answered) # 43

count(filter(dataset, KD1 == "Die Verfügbarkeit hat sich stark verringert")) # 2
get_percentage(43, 2) # 4,7%
count(filter(dataset, KD1 == "Die Verfügbarkeit hat sich etwas verringert")) # 4
get_percentage(43, 4) # 9,3%
count(filter(dataset, KD1 == "Die Verfügbarkeit hat sich nicht verändert")) # 22
get_percentage(43, 22) # 51,2%
count(filter(dataset, KD1 == "Die Verfügbarkeit hat sich etwas erhöht")) # 13
get_percentage(43, 13) # 30%
count(filter(dataset, KD1 == "Die Verfügbarkeit hat sich stark erhöht")) # 2
get_percentage(43, 2) # 4,7%

KD1_answered$KD1[KD1_answered$KD1 == "Die Verfügbarkeit hat sich stark verringert"] <- -2
KD1_answered$KD1[KD1_answered$KD1 == "Die Verfügbarkeit hat sich etwas verringert"] <- -1
KD1_answered$KD1[KD1_answered$KD1 == "Die Verfügbarkeit hat sich nicht verändert"] <- 0
KD1_answered$KD1[KD1_answered$KD1 == "Die Verfügbarkeit hat sich etwas erhöht"] <- 1
KD1_answered$KD1[KD1_answered$KD1 == "Die Verfügbarkeit hat sich stark erhöht"] <- 2

KD1_mean <- mean(as.numeric(KD1_answered$KD1)) # 0,2


#### GRUPPE 3: Semesterplanung ####

#### SP — FRAGE 1 ####
# Haben Sie in allen Lehrveranstaltungen vom Institut für Digital 
# Humanities, die Sie besuchen wollten, einen Platz erhalten?
# SP1

count(filter(dataset, SP1 == "Keine Antwort")) # 1
SP1_answered <- dataset[!(is.na(dataset$SP1) | dataset$SP1=="" | dataset$SP1=="Keine Antwort"), ]
count(SP1_answered) # 48

count(filter(dataset, SP1 == "Ich habe in keiner der Veranstaltungen einen Platz erhalten")) # 1
get_percentage(48, 1) # 2,1%
count(filter(dataset, SP1 == "Ich habe in den meisten der Veranstaltungen keinen Platz erhalten")) # 1
get_percentage(48, 1) # 2,1%
count(filter(dataset, SP1 == "Ich habe in den meisten der Veranstaltungen einen Platz erhalten")) # 9
get_percentage(48, 9) # 18,8%
count(filter(dataset, SP1 == "Ich habe in allen der Veranstaltungen einen Platz erhalten")) # 37
get_percentage(48, 37) # 77,1%


#### GRUPPE 4: Arbeitsbelastung ####

#### AB — FRAGE 1 ####
# Hatten Sie genug Zeit, um alle geplanten Lehrveranstaltungen zu besuchen?
# AB1 

count(filter(dataset, AB1 == "Keine Antwort")) # 0
AB1_answered <- dataset[!(is.na(dataset$AB1) | dataset$AB1=="" | dataset$AB1=="Keine Antwort"), ]
count(AB1_answered) # 48

count(filter(dataset, AB1 == "Ich konnte keine der Veranstaltungen besuchen")) # 1
get_percentage(48, 1) # 2,1%
count(filter(dataset, AB1 == "Ich konnte die meisten der Veranstaltungen nicht besuchen")) # 2
get_percentage(48, 2) # 4,2%
count(filter(dataset, AB1 == "Ich konnte die meisten der Veranstaltungen besuchen")) # 17
get_percentage(48, 17) # 35,4%
count(filter(dataset, AB1 == "Ich konnte alle der Veranstaltungen besuchen")) # 28
get_percentage(48, 28) # 58,3%


#### AB — FRAGE 2 ####
# Hat sich Ihre Arbeitsbelastung im Vergleich zu in der Zeit der Präsenzlehre verändert?
# AB2

count(filter(dataset, AB2 == "Keine Antwort")) # 0
AB2_answered <- dataset[!(is.na(dataset$AB2) | dataset$AB2=="" | dataset$AB2=="Keine Antwort"), ]
count(AB2_answered) # 48

count(filter(dataset, AB2 == "Ja, sie hat sich stark verringert")) # 2
get_percentage(48, 2) # 4,2%
count(filter(dataset, AB2 == "Ja, sie hat sich etwas verringert")) # 4
get_percentage(48, 4) # 8,3%
count(filter(dataset, AB2 == "Nein, sie hat sich nicht verändert")) # 10
get_percentage(48, 10) # 20,8%
count(filter(dataset, AB2 == "Ja, sie hat sich etwas erhöht")) # 24
get_percentage(48, 24) # 50%
count(filter(dataset, AB2 == "Ja, sie hat sich stark erhöht")) # 8
get_percentage(48, 8) # 16,7%

AB2_answered$AB2[AB2_answered$AB2 == "Ja, sie hat sich stark verringert"] <- -2
AB2_answered$AB2[AB2_answered$AB2 == "Ja, sie hat sich etwas verringert"] <- -1
AB2_answered$AB2[AB2_answered$AB2 == "Nein, sie hat sich nicht verändert"] <- 0
AB2_answered$AB2[AB2_answered$AB2 == "Ja, sie hat sich etwas erhöht"] <- 1
AB2_answered$AB2[AB2_answered$AB2 == "Ja, sie hat sich stark erhöht"] <- 2

AB2_mean <- mean(as.numeric(AB2_answered$AB2)) # 0,7


#### AB — FRAGE 3 ####
# Hat sich bei Ihnen der Bedarf, Abgabefristen zu verlängern, 
# im Vergleich zu in der Zeit der Präsenzlehre verändert?
# AB3

count(filter(dataset, AB3 == "Keine Antwort")) # 4
AB3_answered <- dataset[!(is.na(dataset$AB3) | dataset$AB3=="" | dataset$AB3=="Keine Antwort"), ]
count(AB3_answered) # 44

count(filter(dataset, AB3 == "Ja, er hat sich stark verringert")) # 0
count(filter(dataset, AB3 == "Ja, er hat sich etwas verringert")) # 1
get_percentage(44, 1) # 2,3%
count(filter(dataset, AB3 == "Nein, er ist gleich geblieben")) # 22
get_percentage(44, 22) # 50%
count(filter(dataset, AB3 == "Ja, er hat sich etwas erhöht")) # 17
get_percentage(44, 17) # 38,6%
count(filter(dataset, AB3 == "Ja, er hat sich stark erhöht")) # 4
get_percentage(44, 4) # 9,1%

AB3_answered$AB3[AB3_answered$AB3 == "Ja, er hat sich stark verringert"] <- -2
AB3_answered$AB3[AB3_answered$AB3 == "Ja, er hat sich etwas verringert"] <- -1
AB3_answered$AB3[AB3_answered$AB3 == "Nein, er ist gleich geblieben"] <- 0
AB3_answered$AB3[AB3_answered$AB3 == "Ja, er hat sich etwas erhöht"] <- 1
AB3_answered$AB3[AB3_answered$AB3 == "Ja, er hat sich stark erhöht"] <- 2

AB3_mean <- mean(as.numeric(AB3_answered$AB3)) # 0,55


#### AB — FRAGE 4 ####
# Wie schätzen Sie das Verhältnis zwischen der angesetzten und 
# der tatsächlichen Arbeitsbelastung insgesamt ein?
# AB4

count(filter(dataset, AB4 == "Keine Antwort")) # 3
AB4_answered <- dataset[!(is.na(dataset$AB4) | dataset$AB4=="" | dataset$AB4=="Keine Antwort"), ]
count(AB4_answered) # 45

count(filter(dataset, AB4 == "Die tatsächliche Arbeitsbelastung ist viel geringer als die angesetzte")) # 0
count(filter(dataset, AB4 == "Die tatsächliche Arbeitsbelastung ist etwas geringer als die angesetzte")) # 6
get_percentage(45, 6) # 13,3%
count(filter(dataset, AB4 == "Die tatsächliche und die angesetzte Arbeitsbelastung sind genau gleich")) # 11
get_percentage(45, 11) # 24,4%
count(filter(dataset, AB4 == "Die tatsächliche Arbeitsbelastung ist etwas höher als die angesetzte")) # 20
get_percentage(45, 20) # 44,4%
count(filter(dataset, AB4 == "Die tatsächliche Arbeitsbelastung ist viel höher als die angesetzte")) # 8
get_percentage(45, 8) # 17,8%

AB4_answered$AB4[AB4_answered$AB4 == "Die tatsächliche Arbeitsbelastung ist viel geringer als die angesetzte"] <- -2
AB4_answered$AB4[AB4_answered$AB4 == "Die tatsächliche Arbeitsbelastung ist etwas geringer als die angesetzte"] <- -1
AB4_answered$AB4[AB4_answered$AB4 == "Die tatsächliche und die angesetzte Arbeitsbelastung sind genau gleich"] <- 0
AB4_answered$AB4[AB4_answered$AB4 == "Die tatsächliche Arbeitsbelastung ist etwas höher als die angesetzte"] <- 1
AB4_answered$AB4[AB4_answered$AB4 == "Die tatsächliche Arbeitsbelastung ist viel höher als die angesetzte"] <- 2

AB4_mean <- mean(as.numeric(AB4_answered$AB4)) # 0,67


#### GRUPPE 5: Studien- und Prüfungsleistungen ####

#### SPL — FRAGE 1 ####
# Welche Erwartungen haben/hatten Sie bezüglich der Studien- und Prüfungsleistungen?
# SPL1 

SPL1_df <- dataset[!(is.na(dataset$SPL1) | dataset$SPL1==""), ]
count(SPL1_df) #15
answers_SPL1 <- vector()
for (answer_SPL1 in SPL1_df$SPL1) {
  answer_SPL1 <- trimws(str_replace_all(answer_SPL1, "\n", " "))
  answers_SPL1 <- c(answers_SPL1, answer_SPL1)
}
answers_SPL1


#### SPL — FRAGE 2 ####
# Haben sich Ihre Erwartungen bezüglich der Studien- und Prüfungsleistungen bisher bestätigt?
# SPL2

count(filter(dataset, SPL2 == "Keine Antwort")) # 11
SPL2_answered <- dataset[!(is.na(dataset$SPL2) | dataset$SPL2=="" | dataset$SPL2=="Keine Antwort"), ]
count(SPL2_answered) # 34

count(filter(dataset, SPL2 == "Nein, die Leistungen waren viel schlechter als erwartet")) # 0
count(filter(dataset, SPL2 == "Nein, die Leistungen waren etwas schlechter als erwartet")) # 6
get_percentage(34, 6) # 17,7%
count(filter(dataset, SPL2 == "Ja, die Leistungen waren genau so wie erwartet")) # 23
get_percentage(34, 23) # 67,7%
count(filter(dataset, SPL2 == "Nein, die Leistungen waren etwas besser als erwartet")) # 5
get_percentage(34, 5) # 14,7%
count(filter(dataset, SPL2 == "Nein, die Leistungen waren viel besser als erwartet")) # 0

SPL2_answered$SPL2[SPL2_answered$SPL2 == "Nein, die Leistungen waren viel schlechter als erwartet"] <- -2
SPL2_answered$SPL2[SPL2_answered$SPL2 == "Nein, die Leistungen waren etwas schlechter als erwartet"] <- -1
SPL2_answered$SPL2[SPL2_answered$SPL2 == "Ja, die Leistungen waren genau so wie erwartet"] <- 0
SPL2_answered$SPL2[SPL2_answered$SPL2 == "Nein, die Leistungen waren etwas besser als erwartet"] <- 1
SPL2_answered$SPL2[SPL2_answered$SPL2 == "Nein, die Leistungen waren viel besser als erwartet"] <- 2

SPL2_mean <- mean(as.numeric(SPL2_answered$SPL2)) # -0,03


#### GRUPPE 6: Zugänglichkeit ####

#### Z — FRAGE 1 ####
# Geben Sie bitte an, inwiefern Sie Zugang zu der aufgeführten Ausstattung 
# für die Teilnahme am digitalen Unterricht haben.

Z1 <- dataset[!(is.na(dataset$Z1.SQ001.) | dataset$Z1.SQ001.==""), ]
count(Z1) # 45
Z1[ , 21:25 ]

Z1[21:26][Z1[21:26] == "Habe nie Zugang dazu"] <- 0
Z1[21:26][Z1[21:26] == "Habe selten Zugang dazu"] <- 1
Z1[21:26][Z1[21:26] == "Habe meistens Zugang dazu"] <- 2
Z1[21:26][Z1[21:26] == "Habe immer Zugang dazu"] <- 3

# Z1.SQ002.: Stabile Internetverbindung

Z1.SQ002._mean <- mean(as.numeric(Z1$Z1.SQ002.)) # 2,3
get_percentage(3, Z1.SQ002._mean) # 76,3%

# Z1.SQ001.: Funktionsfähiger Computer (Laptop oder PC)

Z1.SQ001._mean <- mean(as.numeric(Z1$Z1.SQ001.)) # 2,8
get_percentage(3, Z1.SQ001._mean) # 92,6%

# Z1.SQ004.: Kopfhörer

Z1.SQ004._mean <- mean(as.numeric(Z1$Z1.SQ004.)) # 2,9
get_percentage(3, Z1.SQ004._mean) # 95,6%

# Z1.SQ005.: Mikrofon

Z1.SQ005._mean <- mean(as.numeric(Z1$Z1.SQ005.)) # 2,8
get_percentage(3, Z1.SQ005._mean) # 92,6

# Z1.SQ003.: Webcam

Z1.SQ003._mean <- mean(as.numeric(Z1$Z1.SQ003.)) # 2,6
get_percentage(3, Z1.SQ003._mean) # 88,2


#### Z — FRAGE 2 ####
# Inwiefern werden Sie anderweitig beeinträchtigt oder behindert, am digitalen Unterricht teilzunehmen?
# Z2

Z2_df <- dataset[!(is.na(dataset$Z2) | dataset$Z2==""), ]
count(Z2_df) # 6
answers_Z2 <- vector()
for (answer_Z2 in Z2_df$Z2) {
  answer_Z2 <- trimws(str_replace_all(answer_Z2, "\n", " "))
  answers_Z2 <- c(answers_Z2, answer_Z2)
}
answers_Z2


#### GRUPPE 7: Psychosoziale Situation ####

#### PSS — FRAGE 1 ####
# Werden Sie durch persönliche Umstände verhindert, regelmäßig am 
# digitalen Unterricht teilzunehmen? Wenn ja, durch welche?

PSS1_answered <- filter(dataset, PSS1.SQ001. != "N/A")
count(PSS1_answered) # 44

# PSS1.SQ001.: Nein

count(filter(dataset, PSS1.SQ001. == "Ja")) # 21
get_percentage(44, 21) # 47,7%

# PSS1.SQ003.: Ja, kein/eingeschränkter Zugang zu einem geeigneten Arbeitsplatz

count(filter(dataset, PSS1.SQ003. == "Ja")) # 7
get_percentage(44, 7) # 15,9%

# PSS1.SQ002.: Ja, Sorgeverantwortung (Kinder, Pflege)

count(filter(dataset, PSS1.SQ002. == "Ja")) # 1
get_percentage(44, 1) # 2,3%

# PSS1.SQ004.: Ja, finanzielle Belastungen

count(filter(dataset, PSS1.SQ004. == "Ja")) # 8
get_percentage(44, 8) # 18,2%

# PSS1.SQ005.: Ja, körperliche Belastungen

count(filter(dataset, PSS1.SQ005. == "Ja")) # 5
get_percentage(44, 5) # 11,4%

# PSS1.SQ006.: Ja, psychische Belastungen

count(filter(dataset, PSS1.SQ006. == "Ja")) # 13
get_percentage(44, 13) #29,5%

# PSS1.SQ007.: Keine Antwort

count(filter(dataset, PSS1.SQ007. == "Ja")) # 0

# PSS1.other.

PSS1.other._df <- dataset[!(is.na(dataset$PSS1.other.) | dataset$PSS1.other.==""), ]
count(PSS1.other._df) # 5
answers_PSS1.other. <- vector()
for (answer_PSS1.other. in PSS1.other._df$PSS1.other.) {
  answer_PSS1.other. <- trimws(str_replace_all(answer_PSS1.other., "\n", " "))
  answers_PSS1.other. <- c(answers_PSS1.other., answer_PSS1.other.)
}
answers_PSS1.other.


#### PSS — FRAGE 2 ####
# Geben Sie bitte an, inwiefern sich die folgenden Aspekte bei Ihnen
# hinsichtlich des Studiums im digitalen Semester verändert haben.

PSS2 <- dataset[!(is.na(dataset$PSS2.SQ001.) | dataset$PSS2.SQ001.==""), ]
count(PSS2) # 44
PSS2[ , 35:38 ]

PSS2[35:38][PSS2[35:38] == "Hat sich stark verringert"] <- -2
PSS2[35:38][PSS2[35:38] == "Hat sich etwas verringert"] <- -1
PSS2[35:38][PSS2[35:38] == "Hat sich nicht verändert"] <- 0
PSS2[35:38][PSS2[35:38] == "Hat sich etwas erhöht"] <- 1
PSS2[35:38][PSS2[35:38] == "Hat sich stark erhöht"] <- 2


# PSS2.SQ001.: Konzentrationsfähigkeit

count(filter(PSS2, PSS2.SQ001. == "Keine Antwort")) # 0

count(filter(dataset, PSS2.SQ001. == "Hat sich stark verringert")) # 6
get_percentage(44, 6) # 13,6%
count(filter(dataset, PSS2.SQ001. == "Hat sich etwas verringert")) # 20
get_percentage(44, 20) # 45,5%
count(filter(dataset, PSS2.SQ001. == "Hat sich nicht verändert")) # 9
get_percentage(44, 9) # 20,5%
count(filter(dataset, PSS2.SQ001. == "Hat sich etwas erhöht")) # 6
get_percentage(44, 6) # 13,6%
count(filter(dataset, PSS2.SQ001. == "Hat sich stark erhöht")) # 3
get_percentage(44, 3) # 6,8%

PSS2.SQ001._mean <- mean(as.numeric(PSS2$PSS2.SQ001.)) # -0,46


# PSS2.SQ002.: Motivation

count(filter(PSS2, PSS2.SQ002. == "Keine Antwort")) # 0

count(filter(dataset, PSS2.SQ002. == "Hat sich stark verringert")) # 9
get_percentage(44, 9) # 20,5%
count(filter(dataset, PSS2.SQ002. == "Hat sich etwas verringert")) # 14
get_percentage(44, 14) # 31,8%
count(filter(dataset, PSS2.SQ002. == "Hat sich nicht verändert")) # 9
get_percentage(44, 9) # 20,5%
count(filter(dataset, PSS2.SQ002. == "Hat sich etwas erhöht")) # 10
get_percentage(44, 10) # 22,7%
count(filter(dataset, PSS2.SQ002. == "Hat sich stark erhöht")) # 2
get_percentage(44, 2) # 4,6%

PSS2.SQ002._mean <- mean(as.numeric(PSS2$PSS2.SQ002.)) # -0,41


# PSS2.SQ004.: Energie

count(filter(PSS2, PSS2.SQ004. == "Keine Antwort")) # 0

count(filter(dataset, PSS2.SQ004. == "Hat sich stark verringert")) # 7
get_percentage(44, 7) # 15,9%
count(filter(dataset, PSS2.SQ004. == "Hat sich etwas verringert")) # 18
get_percentage(44, 18) # 40,9%
count(filter(dataset, PSS2.SQ004. == "Hat sich nicht verändert")) # 9
get_percentage(44, 9) # 20,5%
count(filter(dataset, PSS2.SQ004. == "Hat sich etwas erhöht")) # 10
get_percentage(44, 10) # 22,7%
count(filter(dataset, PSS2.SQ004. == "Hat sich stark erhöht")) # 0

PSS2.SQ004._mean <- mean(as.numeric(PSS2$PSS2.SQ004.)) # -0,5


# PSS2.SQ003.: Zeitliche Verfügbarkeit

count(filter(PSS2, PSS2.SQ003. == "Keine Antwort")) # 1
PSS2.SQ003._answered <- filter(PSS2, PSS2.SQ003. != "Keine Antwort")
count(PSS2.SQ003._answered) # 43

count(filter(dataset, PSS2.SQ003. == "Hat sich stark verringert")) # 3
get_percentage(43, 3) # 7%
count(filter(dataset, PSS2.SQ003. == "Hat sich etwas verringert")) # 5
get_percentage(43, 5) # 11,6%
count(filter(dataset, PSS2.SQ003. == "Hat sich nicht verändert")) # 8
get_percentage(43, 8) # 18,6%
count(filter(dataset, PSS2.SQ003. == "Hat sich etwas erhöht")) # 16
get_percentage(43, 16) # 37,2%
count(filter(dataset, PSS2.SQ003. == "Hat sich stark erhöht")) # 11 
get_percentage(43, 11) # 25,6%

PSS2.SQ003._mean <- mean(as.numeric(PSS2.SQ003._answered$PSS2.SQ003.)) # 0,63


#### PSS — FRAGE 3 ####
# Geben Sie bitte an, wo Sie sich in der Zeit der digitalen Lehre auf der Skala einordnen würden.
# PSS3

count(filter(dataset, PSS3 == "Keine Antwort")) # 0
PSS3_answered <- dataset[!(is.na(dataset$PSS3) | dataset$PSS3=="" | dataset$PSS3=="Keine Antwort"), ]
count(PSS3_answered) # 44

count(filter(dataset, PSS3 == "Ich bin mit dem Studium völlig überfordert")) # 0
count(filter(dataset, PSS3 == "Ich bin mit dem Studium etwas überfordert")) # 20
get_percentage(44, 20) # 45,5%
count(filter(dataset, PSS3 == "Ich bin im Studium weder überfordert, noch unterfordert")) # 23
get_percentage(44, 23) # 52,3%
count(filter(dataset, PSS3 == "Ich bin im Studium etwas unterfordert")) # 1
get_percentage(44, 1) # 2,3%
count(filter(dataset, PSS3 == "Ich bin im Studium völlig unterfordert")) # 0

PSS3_answered$PSS3[PSS3_answered$PSS3 == "Ich bin mit dem Studium völlig überfordert"] <- 2
PSS3_answered$PSS3[PSS3_answered$PSS3 == "Ich bin mit dem Studium etwas überfordert"] <- 1
PSS3_answered$PSS3[PSS3_answered$PSS3 == "Ich bin im Studium weder überfordert, noch unterfordert"] <- 0
PSS3_answered$PSS3[PSS3_answered$PSS3 == "Ich bin im Studium etwas unterfordert"] <- -1
PSS3_answered$PSS3[PSS3_answered$PSS3 == "Ich bin im Studium völlig unterfordert"] <- -2

PSS3_mean <- mean(as.numeric(PSS3_answered$PSS3)) # 0,43


#### PSS — FRAGE 4 ####
# Geben Sie bitte an, inwiefern die folgenden Aussagen auf Sie zutreffen.

PSS4 <- dataset[!(is.na(dataset$PSS4.SQ001.) | dataset$PSS4.SQ001.==""), ]
count(PSS4) # 44

PSS4[ , 40:41 ]

PSS4[40:41][PSS4[40:41] == "Trifft überhaupt nicht zu"] <- 0
PSS4[40:41][PSS4[40:41] == "Trifft eher nicht zu"] <- 1
PSS4[40:41][PSS4[40:41] == "Weder noch"] <- 2
PSS4[40:41][PSS4[40:41] == "Trifft eher zu"] <- 3
PSS4[40:41][PSS4[40:41] == "Trifft vollkommen zu"] <- 4

# PSS4.SQ001.: Die Dozierenden haben auf meine persönliche Situation Rücksicht genommen.

count(filter(dataset, PSS4.SQ001. == "Keine Antwort")) # 16
PSS4.SQ001._answered <- filter(PSS4, PSS4.SQ001. != "Keine Antwort")
count(PSS4.SQ001._answered) # 28

PSS4.SQ001._mean <- mean(as.numeric(PSS4.SQ001._answered$PSS4.SQ001.)) # 2,8 / 4
get_percentage(4, PSS4.SQ001._mean) # 69,6%


# PSS4.SQ002.: Mir wurde von den Dozierenden (oder vom Studienberater) bei Bedarf Hilfe angeboten 
# (z. B. Weiterleitung an eine geeignete Beratungs-/Hilfestelle, Verweis auf hilfreiche Informationen etc.)

count(filter(dataset, PSS4.SQ002. == "Keine Antwort")) # 20
PSS4.SQ002._answered <- filter(PSS4, PSS4.SQ002. != "Keine Antwort")
count(PSS4.SQ002._answered) # 24

PSS4.SQ002._mean <- mean(as.numeric(PSS4.SQ002._answered$PSS4.SQ002.)) # 2 / 4
get_percentage(4, PSS4.SQ002._mean) # 50%


#### GRUPPE 8: Allgemein ####

#### A — FRAGE 1 ####
# Wie zufrieden sind Sie insgesamt mit der digitalen Lehre am Institut für Digital Humanities?
# A1
count(filter(dataset, A1 == "Keine Antwort")) # 1
A1_answered <- dataset[!(is.na(dataset$A1) | dataset$A1 == "" | dataset$A1 == "Keine Antwort"), ]
count(A1_answered) # 43

count(filter(dataset, A1 == "Vollkommen unzufrieden")) # 0
count(filter(dataset, A1 == "Eher unzufrieden")) # 5
get_percentage(43, 5) # 11,6%
count(filter(dataset, A1 == "Weder noch")) # 2
get_percentage(43, 2) # 4,7%
count(filter(dataset, A1 == "Eher zufrieden")) # 24
get_percentage(43, 24) # 55,8%
count(filter(dataset, A1 == "Vollkommen zufrieden")) # 12
get_percentage(43, 12) # 27,9%

A1_answered$A1[A1_answered$A1 == "Vollkommen unzufrieden"] <- 0
A1_answered$A1[A1_answered$A1 == "Eher unzufrieden"] <- 1
A1_answered$A1[A1_answered$A1 == "Weder noch"] <- 2
A1_answered$A1[A1_answered$A1 == "Eher zufrieden"] <- 3
A1_answered$A1[A1_answered$A1 == "Vollkommen zufrieden"] <- 4

A1_mean <- mean(as.numeric(A1_answered$A1)) # 3


#### A — FRAGE 2 ####
# Haben sich Ihre allgemeinen Erwartungen an das Semester bisher bestätigt?
# A2

count(filter(dataset, A2 == "Keine Antwort")) # 2
A2_answered <- dataset[!(is.na(dataset$A2) | dataset$A2 == "" | dataset$A2 == "Keine Antwort"), ]
count(A2_answered) # 42

count(filter(dataset, A2 == "Nein, das Semester war viel schlechter als erwartet")) # 1
get_percentage(42, 1) # 2,4%
count(filter(dataset, A2 == "Nein, das Semester war etwas schlechter als erwartet")) # 13
get_percentage(42, 13) # 31%
count(filter(dataset, A2 == "Ja, das Semester war genau so wie erwartet")) # 9
get_percentage(42, 9) # 21,4%
count(filter(dataset, A2 == "Nein, das Semester war etwas besser als erwartet")) # 13
get_percentage(42, 13) # 31%
count(filter(dataset, A2 == "Nein, das Semester war viel besser als erwartet")) # 6
get_percentage(42, 6) # 14,3%

A2_answered$A2[A2_answered$A2 == "Nein, das Semester war viel schlechter als erwartet"] <- -2
A2_answered$A2[A2_answered$A2 == "Nein, das Semester war etwas schlechter als erwartet"] <- -1
A2_answered$A2[A2_answered$A2 == "Ja, das Semester war genau so wie erwartet"] <- 0
A2_answered$A2[A2_answered$A2 == "Nein, das Semester war etwas besser als erwartet"] <- 1
A2_answered$A2[A2_answered$A2 == "Nein, das Semester war viel besser als erwartet"] <- 2

A2_mean <- mean(as.numeric(A2_answered$A2)) # 0,24


#### A — FRAGE 3 ####
# Welche Hoffnungen und Befürchtungen haben Sie für das kommende 
# (digitale) Semester (Wintersemester 2020/21)?
# A3

A3_df <- dataset[!(is.na(dataset$A3) | dataset$A3==""), ]
count(A3_df) # 21
answers_A3 <- vector()
for (answer_A3 in A3_df$A3) {
  answer_A3 <- trimws(str_replace_all(answer_A3, "\n", " "))
  answers_A3 <- c(answers_A3, answer_A3)
}
answers_A3


#### A — FRAGE 4 ####
# Haben Sie weitere Anmerkungen oder Vorschläge über die digitale Lehre am Institut für Digital Humanities?
# A4

A4_df <- dataset[!(is.na(dataset$A4) | dataset$A4==""), ]
count(A4_df) # 14
answers_A4 <- vector()
for (answer_A4 in A4_df$A4) {
  answer_A4 <- trimws(str_replace_all(answer_A4, "\n", " "))
  answers_A4 <- c(answers_A4, answer_A4)
}
answers_A4

