cat("* Labels for questionnaire: AttrakDiff \n")

## Variable names in data
itemsets$attrakdiff <- paste("attrakdiff", sprintf("%02d", seq(1:28)), sep = "_")

itemlabels$attrakdiff <- data.frame(
  eng = c(
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
    "unruly - manageable"),
  ger = c(
    "technisch - menschlich",
    "kompliziert - einfach",
    "unpraktisch - praktisch",
    "umständlch - direkt",
    "verwirrend - übersichtlich",
    "unberechenbar - voraussagbar",
    "isolierend - verbindend",
    "laienhaft - fachmännisch",
    "stillos - stilvoll",
    "minderwertig - wertvoll",
    "ausgrenzend - einbeziehend",
    "trennt micht - bringt mich näher",
    "nicht vorzeigbar - vorzeigbar",
    "konventionell - originell",
    "fantasielos - kreativ",
    "vorsichtig - mutig",
    "konservativ - innovativ",
    "lahm - fesselnd",
    "harmlos - herausfordernd",
    "unangenehm - angenehm",
    "hässlich - schön",
    "unympathisch - sympatisch",
    "zurückweisend - einladend",
    "schlecht - gut",
    "abstoßend - anziehend",
    "entmutigend - motivierend",
    "herkömmlich - neuartig",
    "widerspenstig - handhabbar")
  )


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