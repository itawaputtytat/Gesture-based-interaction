
# Version info ------------------------------------------------------------

library(puttytat4R)
puttytat4R::.outputFunProc_status(F)
outputSectionTitle("Project: Gesture-based interaction", char_aes = "#", sepline_char = "=")
outputString("* Framework V3", type = "message")



# Libraries ---------------------------------------------------------------

outputString("Attaching libraries ...")
attachLibraries("init-framework/libraries.R")


# Database ----------------------------------------------------------------

outputSectionTitle("Database")

outputDone(step = T)

outputString("Initialise set4db")
dbInitSettings(dir_name = "settings")
outputDone(step = T)

outputString("Connecting to database ...")
dbConnectOperator()
outputDone(step = T)

sourceWithEcho("fun/dbGetQuery_batch.R")
sourceWithEcho("fun/dbCreateQueryString.R")
sourceWithEcho("fun/createVector_var_sxx.R")
sourceWithEcho("settings/sett_id_names.R")
sourceWithEcho("fun/renameVar_sxx_exx.R")
sourceWithEcho("fun/intrpldf_batch.R")
sourceWithEcho("fun/deparseDataFunArg.R")

sourceWithEcho("fun/codePedalActivity.R")

