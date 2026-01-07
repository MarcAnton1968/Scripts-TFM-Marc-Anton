#
#  Anàlisi de la sensibilitat del LPI
#
#  Ho fem amb els z-scores relatius al darrer any

library(dplyr)
library(tidyr)

#
# preparem un dataframe per acumular valors pel test
#

testFile <- data.frame(
  index = numeric(),
  SE = numeric(),
  stringsAsFactors = FALSE
)

# carreguem el valor del MSI original

setwd("C:/Marc/Indicadors/LPI/recerca/Sensitivity_analisis/MSI_trend1")

LPIResults <- read.csv("./LPI_CAT_TRENDS.csv", header = TRUE, sep = ";")


change <- as.numeric(gsub(",",".",LPIResults$value[LPIResults$X == "% change"]))
SEchange <- as.numeric(gsub(",",".",LPIResults$value[LPIResults$X == "SE % change"]))

test1 <- data.frame(change, SEchange)
names(test1) <- names(testFile)

testFile <- rbind(testFile, test1)

# carreguem el valor del MSI que volem testar

setwd("C:/Marc/Indicadors/LPI/recerca/Sensitivity_analisis/MSI_trend10")

LPIResults <- read.csv("./LPI_CAT_TRENDS.csv", header = TRUE, sep = ";")

change <- as.numeric(gsub(",",".",LPIResults$value[LPIResults$X == "% change"]))
SEchange <- as.numeric(gsub(",",".",LPIResults$value[LPIResults$X == "SE % change"]))

test1 <- data.frame(change, SEchange)
names(test1) <- names(testFile)

testFile <- rbind(testFile, test1)

#
#  Fem el test amb els z scores. Aquest test el fem considerant que el z score ens dona un valor de la distància 
#     entre dos valors. Per a això, ho fem sobre els valors finals de canvi
#

diff      <- testFile$index[1] - testFile$index[2]
se_diff   <- sqrt(testFile$SE[1]^2 + testFile$SE[2]^2)
z         <- diff / se_diff
p_value   <- 2*pnorm(-abs(z))

