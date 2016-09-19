# Acceptance --------------------------------------------------------------

# Variable names in data
itemset_acc <- 
  paste("acc", sprintf("%02d", seq(1:9)), sep = "_")

# Labels in questionnaire
labels_acc <- data.frame(
  eng = c("useless - useful", 
          "unpleasant - pleasant", 
          "bad - good",
          "annoying - nice",
          "superfluous - effective",
          "irritating - likable",
          "worthless - assisting",
          "undesirable - desirable",
          "sleep-inducing - rasing alertness"),
  ger = c("nutzlos - nützlich",
          "unangenehm - angenehm",
          "schlecht - gut",
          "nervig - nett",
          "unnötig - effizient",
          "ärgerlich - erfreulich",
          "wertlos - hilfreich",
          "nicht wünschenswert - wünschenswert",
          "einschläfernd - aktivierend"))


# AttrakDiff 2 ------------------------------------------------------------

# Labels in questionnaire
labels_attrakdiff <- c(
  "technical - human",
  "complicated - simple",
  "impractical - practical",
  "cumbersome - straightforward",
  "confusing - clearly structured",
  "unpredictable - predictable",
  "isolating - connective",
  "unprofessional - professional",
  "tacky - stylish",
  "cheap - premium",
  "alienating - integrating",
  "separates me - brings me closer",
  "unpresentable - presentable",
  "conventional - inventive",
  "unimaginative - creative",
  "cautious - bold",
  "conservative - innovative",
  "dull - captivating",
  "undemanding - challenging",
  "unpleasant - pleasant",
  "ugly - attractive",
  "disagreeable - likeable",
  "rejecting - inviting",
  "bad - good",
  "repelling - appealing",
  "discouraging - motivating",
  "ordinary - novel",
  "unruly - manageable")

labels_attrakdiff <- data.frame(eng = labels_attrakdiff,
                                ger = labels_attrakdiff)

itemnr_attrakdiff_pq  <- c(1:4, 6, 5, 28)
itemnr_attrakdiff_hqi <- c(7:13)
itemnr_attrakdiff_hqs <- c(14:19, 27)
itemnr_attrakdiff_hq  <- c(itemnr_attrakdiff_hqi,
                           itemnr_attrakdiff_hqs)
itemnr_attrakdiff_att <- c(20:26)

itemnr_attrakdiff_ordered <- c(itemnr_attrakdiff_pq,
                               itemnr_attrakdiff_hqi,
                               itemnr_attrakdiff_hqs,
                               itemnr_attrakdiff_att)

# Variable names in data
itemset_attrakdiff     <- paste("attrakdiff", sprintf("%02d", seq(1:28)), sep = "_")
itemset_attrakdiff_pq  <- itemset_attrakdiff[itemnr_attrakdiff_pq]
itemset_attrakdiff_hqi <- itemset_attrakdiff[itemnr_attrakdiff_hqi]
itemset_attrakdiff_hqs <- itemset_attrakdiff[itemnr_attrakdiff_hqs]
itemset_attrakdiff_hq  <- itemset_attrakdiff[itemnr_attrakdiff_hq]
itemset_attrakdiff_att <- itemset_attrakdiff[itemnr_attrakdiff_att]

# Settings for matrix rectangles
attrakdiff_rect <- rbind(
  tl = data.frame(xmin = -3, xmax = -1, ymin =  1, ymax =  3, col = "grey70"),
  tm = data.frame(xmin = -1, xmax =  1, ymin =  1, ymax =  3, col = "grey85"),
  tr = data.frame(xmin =  1, xmax =  3, ymin =  1, ymax =  3, col = "grey85"),
  ml = data.frame(xmin = -3, xmax = -1, ymin = -1, ymax =  1, col = "grey55"),
  mm = data.frame(xmin = -1, xmax =  1, ymin = -1, ymax =  1, col = "grey70"),
  mr = data.frame(xmin =  1, xmax =  3, ymin = -1, ymax =  1, col = "grey85"),
  bl = data.frame(xmin = -3, xmax = -1, ymin = -3, ymax = -1, col = "grey40"),
  bm = data.frame(xmin = -1, xmax =  1, ymin = -3, ymax = -1, col = "grey55"),  
  br = data.frame(xmin =  1, xmax =  3, ymin = -3, ymax = -1, col = "grey70"))

# Settings for matrix labels
labels_attrakdiff_matrix <- rbind(
  tl = data.frame(x = -2, y =  2, 
                  label_ger = "zu\nselbst-\norientiert", 
                  label_eng = "too\nself-\noriented"),
  tm = data.frame(x =  0, y =  2, 
                  label_ger = "selbst-\norientiert", 
                  label_eng = "self-\noriented"),
  tr = data.frame(x =  2, y =  2, 
                  label_ger = "begehrt", 
                  label_eng = "desired"),
  mm = data.frame(x =  0, y =  0, 
                  label_ger = "neutral", 
                  label_eng = "neutral"),
  mr = data.frame(x =  2, y =  0, 
                  label_ger = "handlungs-\norientiert", 
                  label_eng = "task-\noriented"),
  bl = data.frame(x = -2, y = -2, 
                  label_ger = "überflüssig", 
                  label_eng = "superfluous"),
  br = data.frame(x =  2, y = -2, 
                  label_ger = "zu\nhandlungs-\norientiert", 
                  label_eng = "too\ntask-\noriented"))



# Feedback ----------------------------------------------------------------
itemset_fb <- paste("fb", sprintf("%02d", seq(1:13)), sep = "_")
# labels_fb <-data.frame(
#   eng = c("Ich finde das Feedback angemessen",
#           "Die Rückmeldung vermittelt mir \nalle nötigen Informationen.",
#           "Das Feedback hilft mir, \ndie Oberfläche besser zu bedienen.",
#           "Durch die Rückmeldung weiß genau, \nwas ich tue.",
#           "Die Rückmeldung zeigt mir an, \nwas die Leap Motion erkannt hat.",
#           "Durch das Feedback weiß ich, \nob ich die richtige Handlung ausgeführt habe.",
#           "Durch das Feedback weiß ich genau, \nwas als nächstes passieren wird.",
#           "Ich würde mir mehr Informationen \ndurch das Feedback wünschen.",
#           "Das Feedback lenkt mich zu sehr ab.",
#           "Ich habe die Rückmeldung regelmäßig beobachtet.",
#           "Die Rückmeldung würde mir helfen, \nGrenzen des Systems besser zu verstehen.",
#           "Das Feedback würde mir helfen, \ndie Erkennungsleistung einschätzen zu können.",
#           "Die Rückmeldung würde mir helfen, \nzu verstehen, warum ein Fehler aufgetreten ist."),
#   ger = itemset_fb)

labels_fb <- data.frame(
  eng = c("... Feedback angemessen",
          "... vermittelt nötige Infos",
          "... hilft Oberfläche bedienen",
          "... weiß was ich tue",
          "... zeigt was Leap erkennt",
          "... richtige Handlung ausgeführt",
          "... weiß was passieren wird",
          "... mehr Infos gewünscht",
          "... lenkt sehr ab",
          "... regelmäßig angeschaut",
          "... hilft Systemgrenzen verstehen",
          "... hilft Erkennungsleistung einschätzen",
          "... hilft Fehler verstehen"),
  ger = itemset_fb)

# NASA-TLX ----------------------------------------------------------------

itemset_nasatlx <- paste("nasatlx", sprintf("%02d", seq(1:6)), sep = "_")
itemset_nasatlx[4] <- paste(itemset_nasatlx[4], "i", sep = "")
labels_nasatlx <- 
  c("mental", 
    "physical", 
    "time", 
    "success", 
    "effort", 
    "negative")
labels_nasatlx <- data.frame(eng = labels_nasatlx,
                            ger = labels_nasatlx)

# Naturalness -------------------------------------------------------------

itemset_nat <- paste("nat", sprintf("%02d", seq(1:35)), sep = "_")
labels_nat <-data.frame(
  eng = c("... Umwelt vergessen",
          "... bemerke kaum, was herum passiert",
          "... tauche völlig in Interaktion ein",
          "... Inhalte fühlen sich real an",
          "... konzentriere mich nur auf Interface",
          "... bekomme ausreichend Feedback vom System",
          "... weiß, ob Aufgabe erfolgreicht ausgeführt",
          "... Gefühl, jede Aktion fehlerfrei ausführen",
          "... konnte Bedienung schnell erlernen",
          "... muss stark auf Teilschritte konzentrieren",
          "... macht mir Spaß System zu bedienen",
          "... finde [Interaktionstyp] ästhetisch",
          "... gesamte Bedienung des Systems attraktiv",
          "... würde System gerne öfter nutzen",
          "... fällt schwer, Bedienung zu beenden",
          "... Bedienung des Systems zu komplex",
          "... Bedienung des Systemsn zu reduziert",
          "... Dauer des Bedienschrittes angemessen",
          "... Ausführung Aktionen wie ich mir vorstelle",
          "... Ausführung ... fühlt sich natürlich an",
          "... Anzahl nötiger Bedienschritte zu hoch",
          "... dauert zu lange, Aufgabe zu bewältigen",
          "... zu viel Zeit mit Blick auf Bildschirm",
          "... Kommunikation mit System scheint kompliziert",
          "... Kommunikation mit System zu langwierig",
          "... vorstellen, System zusammen mit Kollegen",
          "... wäre Bereicherung für Teamarbeit",
          "... würde Kommunikation mit Kollegen erleichtern",
          "... fördert Kommunikation mit Kollegen",
          "... kann von mehreren Personen gemeinsam ...",
          "... Zuordnung [Interaktionstyp]Aktionen systematisch",
          "... [Interaktionstyp] leicht verständlich",
          "... mit [Interaktionstyp] Aufgabe leicht umsetzen",
          "... fällt schwer, eine Aktion umzusetzen",
          "... Zuordnung [Interaktionstyp-]Aktionen nachvollziehbar"),
    ger = itemset_nat)
# labels_nat <-data.frame(
#  eng = itemset_nat,
#  ger = itemset_nat)

itemnr_nat_seamless <- c(1:5)
itemnr_nat_learning <- c(6:10)
itemnr_nat_joy <- c(11:15)
itemnr_nat_context <- c(16:20)
itemnr_nat_comminterface <- c(21:25)
itemnr_nat_interpersonal <- c(26:30)
itemnr_nat_simplicity <- c(31:35)

itemnr_attrakdiff_ordered <- c(itemnr_nat_seamless,
                               itemnr_nat_learning,
                               itemnr_nat_joy,
                               itemnr_nat_context,
                               itemnr_nat_comminterface,
                               itemnr_nat_interpersonal,
                               itemnr_nat_simplicity)