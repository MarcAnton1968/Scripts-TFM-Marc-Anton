#
#   Proces per generar un bucle sobre distribució que sembla la variable clau i ho farem 
#    amb els valors més alts i els més baixos. Si ens situem en els extrems, podrem explicar ç
#    coses. No ho farem sobre les tendències extremes ja que això és obvi que ha de sortir. 
#    Per tant, anem a buscar els llindars de distribució.
#
#   A oartir d'aquí hem de fer:
#     1. Executem el MSI per generar un LPI base.
#     2. Fem un bucle que fa:
#        2.1. Cerquem els que estan per sota de la mitjana del percentil 5, 10, 15, 20, 25. Està al final de l'script
#        2.2. S'executa el MSI eliminant el que es troba en el punt 2.1.
#        2.3. Es fa el test dels z_scores
#        2.4. Si el test no és significatiu es passa al proper percentil
#        2.5. Quan es troba una significació, s'atura el procés
#
#

library(dplyr)
library(tidyr)

# Comencem per carregar el fitxer amb els indexs anuals i generem el txt que necessitem per
#    l'entrada del msi

setwd("C:/Marc/indicadors/LPI/recerca/Sensitivity_analisis")
      
species_trends <- read.csv("./lpi_cat2024_lpi_cat_species.csv", header = TRUE, sep = ";")

# generem el txt pel MSI

species_trends_out <- species_trends %>%
  dplyr::select(species_code, year, index, std_error) %>%      # canviem l'ordre
  rename(species = species_code,
         year = year,
         index = index,
         se = std_error
         ) %>%
  filter(year >= 2002) %>%
  drop_na()

#
#   Correm el MSI de base sobre el que comparar  
#

#  Creem un nou directori base 0 en el que hi tindrem l'execució de base

new_wd <- paste0("./MSI_dist0")
dir.create(new_wd)

# Generem un fitxer de sortida amb les tendències

outputFileName <- paste0(new_wd,"/species_trends_LPI.txt")
readr::write_tsv(species_trends_out, outputFileName)
setwd(new_wd)

## Es passa el MSI que hi ha a la carpeta d'anàlisi

source("../MSI_23_TFM.R", echo = TRUE)

#
# preparem un dataframe per acumular valors per realitzar el test z_score
#

testFile <- data.frame(
  index = numeric(),
  SE = numeric(),
  stringsAsFactors = FALSE
)

#
# I un altre per guardar els resultats dels tests
#

testResults <- data.frame(
  percentil = numeric(),
  index1 = numeric(),
  se1 = numeric(),
  index2 = numeric(),
  se2 = numeric(),
  nSpecies = numeric(),
  indexDiff = numeric(),
  seIndexDiff = numeric(),
  z_score = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)


# carreguem els resultats del MSI original

LPIResults <- read.csv("./LPI_CAT_TRENDS.csv", header = TRUE, sep = ";")
change <- as.numeric(gsub(",",".",LPIResults$value[LPIResults$X == "% change"]))
SEchange <- as.numeric(gsub(",",".",LPIResults$value[LPIResults$X == "SE % change"]))

test1 <- data.frame(change, SEchange)
names(test1) <- names(testFile)

testFile <- rbind(testFile, test1)

setwd("..")

#
#  Generem el bucle per distribució on eliminem les espècies per percentils
#

# ens generem uns fitxers de treball per poder fer el bucle

w_species_trends_out <- species_trends_out
w_inputDf <- inputDf  #  ens generem un nou inputDf per poder anar eliminat espècies

#
#  Bucle d'anàlisi de les distribucions més petites
#

for (X in 1:10){

  # Es treuen les espècies per percentils. Ja hem vist que traient les 10 menys 
  #   distribuides estem a prop de la significació, però no hi arribem. 
  #   Amb el percentil traiem 17 espècies. Per anar amb més calma, ho farem sobre la
  #   la mitjana del percentil. És una situació que vol ser conservadora
  #

  # Anem fet amb percentils de 5 en 5

  prob <- 0.05*X
  perc <- quantile(inputDf$Distribution, probs = prob, na.rm = TRUE)
  result <- inputDf[inputDf$Distribution < perc, ]
  meanDist <- mean(result$Distribution)
  result2 <- inputDf[inputDf$Distribution < meanDist, ]

  #
  # Ara hem d'eliminar les espècies de  result2 als dos working files
  #
  llista_eliminar <- unique(result2$SpeciesCode)
  w_species_trends_out <- 
    w_species_trends_out[ ! w_species_trends_out$species %in% llista_eliminar , ]
  w_inputDf <- w_inputDf[ ! w_inputDf$SpeciesCode %in% llista_eliminar , ]
  
  
  #  Inici codi_MSI
  #  Aquest codi ens permet crear un directori i executar el MSI de manera que deixi els 
  #     resultats en aquell directori
  
  # Creem un nou directori
  
  new_wd <- paste0("./MSI_dist",X)
  dir.create(new_wd)
  
  # Generem un fitxer de sortida amb les tendències

  outputFileName <- paste0(new_wd,"/species_trends_LPI.txt")
  readr::write_tsv(w_species_trends_out, outputFileName)
  setwd(new_wd)

  
  ## Es passa el MSI que hi ha a la carpeta d'anàlisi
  
  source("../MSI_23_TFM.R", echo = TRUE)
  
  # carreguem els resultats del nou MSI
  
  LPIResults <- read.csv("./LPI_CAT_TRENDS.csv", header = TRUE, sep = ";")
  change <- as.numeric(gsub(",",".",LPIResults$value[LPIResults$X == "% change"]))
  SEchange <- as.numeric(gsub(",",".",LPIResults$value[LPIResults$X == "SE % change"]))
  
  test1 <- data.frame(change, SEchange)
  names(test1) <- names(testFile)
  
  testFile <- rbind(testFile, test1)
  
    #  Tornem al directori pare
  
  setwd("..")
  
  #
  #  Fem el test d'aquesta execució
  #
  
  diff      <- testFile$index[1] - testFile$index[nrow(testFile)]
  se_diff   <- sqrt(testFile$SE[1]^2 + testFile$SE[nrow(testFile)]^2)
  z         <- diff / se_diff
  p_value   <- 2*pnorm(-abs(z))
  
  
  print(p_value)

  # Guardem els resultats del test per poder-los ensenyar després
  
  testResult <- data.frame(prob,
                           testFile$index[1],
                           testFile$SE[1],
                           testFile$index[nrow(testFile)],
                           testFile$SE[nrow(testFile)],
                           length(unique(w_species_trends_out$species)),
                           diff,
                           se_diff,
                           z,
                           p_value)
  
  names(testResult) <- names(testResults)
  
  testResults <- rbind(testResults, testResult)
  
  
  if (p_value < 0.05) {
    break
  }
  
}

#
#   Fem un segon bucle per veure si les espècies més ben distribuides tenen impacte sobre
#     l'indicador
#

# ens generem uns fitxers de treball per poder fer el bucle

w_species_trends_out <- species_trends_out
w_inputDf <- inputDf  #  ens generem un nou inputDf per poder anar eliminat espècies

#
#  Bucle d'anàlisi de les distribucions més grans
#

for (X in 1:10){
  
  # Es treuen les espècies per percentils, però en aquest cas, pels percentils més alts
  
  # Anem fet amb percentils de 5 en 5
  
  prob_max <- 1 - 0.05*X
  perc_max <- quantile(inputDf$Distribution, probs = prob_max, na.rm = TRUE)
  result_max <- inputDf[inputDf$Distribution > perc_max, ]
  meanDist_max <- mean(result_max$Distribution)
  result2_max <- inputDf[inputDf$Distribution > meanDist_max, ]
  
  #
  # Ara hem d'eliminar les espècies de  result2 als dos working files
  #
  llista_eliminar <- unique(result2_max$SpeciesCode)
  w_species_trends_out <- 
    w_species_trends_out[ ! w_species_trends_out$species %in% llista_eliminar , ]
  w_inputDf <- w_inputDf[ ! w_inputDf$SpeciesCode %in% llista_eliminar , ]
  
  
  #  Inici codi_MSI
  #  Aquest codi ens permet crear un directori i executar el MSI de manera que deixi els 
  #     resultats en aquell directori
  
  # Creem un nou directori
  
  new_wd <- paste0("./MSI_dist_max",X)
  dir.create(new_wd)
  
  # Generem un fitxer de sortida amb les tendències
  
  outputFileName <- paste0(new_wd,"/species_trends_LPI.txt")
  readr::write_tsv(w_species_trends_out, outputFileName)
  setwd(new_wd)
  
  
  ## Es passa el MSI que hi ha a la carpeta d'anàlisi
  
  source("../MSI_23_TFM.R", echo = TRUE)
  
  # carreguem els resultats del nou MSI
  
  LPIResults <- read.csv("./LPI_CAT_TRENDS.csv", header = TRUE, sep = ";")
  change <- as.numeric(gsub(",",".",LPIResults$value[LPIResults$X == "% change"]))
  SEchange <- as.numeric(gsub(",",".",LPIResults$value[LPIResults$X == "SE % change"]))
  
  test1 <- data.frame(change, SEchange)
  names(test1) <- names(testFile)
  
  testFile <- rbind(testFile, test1)
  
  #  Tornem al directori pare
  
  setwd("..")
  
  #
  #  Fem el test d'aquesta execució
  #
  
  diff      <- testFile$index[1] - testFile$index[nrow(testFile)]
  se_diff   <- sqrt(testFile$SE[1]^2 + testFile$SE[nrow(testFile)]^2)
  z         <- diff / se_diff
  p_value   <- 2*pnorm(-abs(z))
  
  
  print(p_value)
  
  # Guardem els resultats del test per poder-los ensenyar després
  
  testResult <- data.frame(prob_max,
                           testFile$index[1],
                           testFile$SE[1],
                           testFile$index[nrow(testFile)],
                           testFile$SE[nrow(testFile)],
                           length(unique(w_species_trends_out$species)),
                           diff,
                           se_diff,
                           z,
                           p_value)
  
  names(testResult) <- names(testResults)
  
  testResults <- rbind(testResults, testResult)
  
  
  if (p_value < 0.05) {
    break
  }
  
}

#
#   Fem un tercer bucle per eliminar els valors extrems de tendèmcia i testar  per tant 
#      las sensibilitat de l'indicador a tendències extremes amb independencia d'on vinguin
#

#  Anem al directori del MSIoriginal per recollir les dades d'ell i poder fer els tests 

setwd("./MSI_dist0")

#
# preparem un dataframe per acumular valors per realitzar el test z_score
#

testFile_abs <- data.frame(
  index = numeric(),
  SE = numeric(),
  stringsAsFactors = FALSE
)

#
# I un altre per guardar els resultats dels tests
#

testResults_abs <- data.frame(
  percentil = numeric(),
  index1 = numeric(),
  se1 = numeric(),
  index2 = numeric(),
  se2 = numeric(),
  nSpecies = numeric(),
  indexDiff = numeric(),
  seIndexDiff = numeric(),
  z_score = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

# carreguem els resultats del MSI original

LPIResults <- read.csv("./LPI_CAT_TRENDS.csv", header = TRUE, sep = ";")
change <- as.numeric(gsub(",",".",LPIResults$value[LPIResults$X == "% change"]))
SEchange <- as.numeric(gsub(",",".",LPIResults$value[LPIResults$X == "SE % change"]))

test1 <- data.frame(change, SEchange)
names(test1) <- names(testFile_abs)

testFile_abs <- rbind(testFile_abs, test1)

setwd("..")

#
#  Generem el bucle pel valor absolut de la distribució on eliminem les espècies per percentils
#

# ens generem uns fitxers de treball per poder fer el bucle

w_species_trends_out <- species_trends_out
w_inputDf <- inputDf  #  ens generem un nou inputDf per poder anar eliminat espècies

#
#  Bucle d'anàlisi de les distribucions més petites
#

for (X in 1:10){
  
  # Es treuen les espècies per percentils. Anem fet amb percentils de 5 en 5
  # Treiem els valors més extrems, és a dir amb els majors valors de la meitat del percentil
  #   i els menors de l'altra meitat
  
  prob <- 0.05*X
  perc <- quantile(inputDf$Trend, probs = prob/2, na.rm = TRUE)
  result <- inputDf[inputDf$Trend < perc, ]
  
  prob_max <- 1 - 0.05*X + prob/2
  perc_max <- quantile(inputDf$Trend, probs = prob_max, na.rm = TRUE)
  result_max <- inputDf[inputDf$Trend > perc_max, ]
  result2 <- rbind(result, result_max)
  
  #
  # Ara hem d'eliminar les espècies de  result2 als dos working files
  #
  llista_eliminar <- unique(result2$SpeciesCode)
  w_species_trends_out <- 
    w_species_trends_out[ ! w_species_trends_out$species %in% llista_eliminar , ]
  w_inputDf <- w_inputDf[ ! w_inputDf$SpeciesCode %in% llista_eliminar , ]
  
  
  #  Inici codi_MSI
  #  Aquest codi ens permet crear un directori i executar el MSI de manera que deixi els 
  #     resultats en aquell directori
  
  # Creem un nou directori
  
  new_wd <- paste0("./MSI_trend",X)
  dir.create(new_wd)
  
  # Generem un fitxer de sortida amb les tendències
  
  outputFileName <- paste0(new_wd,"/species_trends_LPI.txt")
  readr::write_tsv(w_species_trends_out, outputFileName)
  setwd(new_wd)
  
  ## Es passa el MSI que hi ha a la carpeta d'anàlisi
  
  source("../MSI_23_TFM.R", echo = TRUE)
  
  # carreguem els resultats del nou MSI
  
  LPIResults <- read.csv("./LPI_CAT_TRENDS.csv", header = TRUE, sep = ";")
  change <- as.numeric(gsub(",",".",LPIResults$value[LPIResults$X == "% change"]))
  SEchange <- as.numeric(gsub(",",".",LPIResults$value[LPIResults$X == "SE % change"]))
  
  test1 <- data.frame(change, SEchange)
  names(test1) <- names(testFile_abs)
  
  testFile_abs <- rbind(testFile_abs, test1)
  
  #  Tornem al directori pare
  
  setwd("..")
  
  #
  #  Fem el test d'aquesta execució
  #
  
  diff      <- testFile_abs$index[1] - testFile_abs$index[nrow(testFile_abs)]
  se_diff   <- sqrt(testFile_abs$SE[1]^2 + testFile_abs$SE[nrow(testFile_abs)]^2)
  z         <- diff / se_diff
  p_value   <- 2*pnorm(-abs(z))
  
  print(p_value)
  
  # Guardem els resultats del test per poder-los ensenyar després
  
  testResult <- data.frame(prob,
                           testFile_abs$index[1],
                           testFile_abs$SE[1],
                           testFile_abs$index[nrow(testFile_abs)],
                           testFile_abs$SE[nrow(testFile_abs)],
                           length(unique(w_species_trends_out$species)),
                           diff,
                           se_diff,
                           z,
                           p_value)
  
  names(testResult) <- names(testResults_abs)
  
  testResults_abs <- rbind(testResults_abs, testResult)
  
  
  if (p_value < 0.05) {
    break
  }
  
}

#
#   Fem un quart bucle per eliminar els valors extrems de distribució i testar  per tant 
#      las sensibilitat de l'indicador a tendències extremes amb independencia d'on vinguin
#

#  Anem al directori del MSIoriginal per recollir les dades d'ell i poder fer els tests 

setwd("./MSI_dist0")

#
# preparem un dataframe per acumular valors per realitzar el test z_score
#

testFile_dist <- data.frame(
  index = numeric(),
  SE = numeric(),
  stringsAsFactors = FALSE
)

#
# I un altre per guardar els resultats dels tests
#

testResults_dist <- data.frame(
  percentil = numeric(),
  index1 = numeric(),
  se1 = numeric(),
  index2 = numeric(),
  se2 = numeric(),
  nSpecies = numeric(),
  indexDiff = numeric(),
  seIndexDiff = numeric(),
  z_score = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

# carreguem els resultats del MSI original

LPIResults <- read.csv("./LPI_CAT_TRENDS.csv", header = TRUE, sep = ";")
change <- as.numeric(gsub(",",".",LPIResults$value[LPIResults$X == "% change"]))
SEchange <- as.numeric(gsub(",",".",LPIResults$value[LPIResults$X == "SE % change"]))

test1 <- data.frame(change, SEchange)
names(test1) <- names(testFile_dist)

testFile_dist <- rbind(testFile_dist, test1)

setwd("..")

#
#  Generem el bucle pel valor absolut de la distribució on eliminem les espècies per percentils
#

# ens generem uns fitxers de treball per poder fer el bucle

w_species_trends_out <- species_trends_out
w_inputDf <- inputDf  #  ens generem un nou inputDf per poder anar eliminat espècies

#
#  Bucle d'anàlisi de les distribucions més petites
#

for (X in 1:10){
  
  # Es treuen les espècies per percentils. Anem fet amb percentils de 5 en 5
  # Treiem els valors més extrems, és a dir amb els majors valors de la meitat del percentil
  #   i els menors de l'altra meitat
  
  prob <- 0.05*X
  perc <- quantile(inputDf$Distribution, probs = prob/2, na.rm = TRUE)
  result <- inputDf[inputDf$Distribution < perc, ]
  
  prob_max <- 1 - 0.05*X + prob/2
  perc_max <- quantile(inputDf$Distribution, probs = prob_max, na.rm = TRUE)
  result_max <- inputDf[inputDf$Distribution > perc_max, ]
  result2 <- rbind(result, result_max)
  
  #
  # Ara hem d'eliminar les espècies de  result2 als dos working files
  #
  llista_eliminar <- unique(result2$SpeciesCode)
  w_species_trends_out <- 
    w_species_trends_out[ ! w_species_trends_out$species %in% llista_eliminar , ]
  w_inputDf <- w_inputDf[ ! w_inputDf$SpeciesCode %in% llista_eliminar , ]
  
  
  #  Inici codi_MSI
  #  Aquest codi ens permet crear un directori i executar el MSI de manera que deixi els 
  #     resultats en aquell directori
  
  # Creem un nou directori
  
  new_wd <- paste0("./MSI_dist_extrem",X)
  dir.create(new_wd)
  
  # Generem un fitxer de sortida amb les tendències
  
  outputFileName <- paste0(new_wd,"/species_trends_LPI.txt")
  readr::write_tsv(w_species_trends_out, outputFileName)
  setwd(new_wd)
  
  ## Es passa el MSI que hi ha a la carpeta d'anàlisi
  
  source("../MSI_23_TFM.R", echo = TRUE)
  
  # carreguem els resultats del nou MSI
  
  LPIResults <- read.csv("./LPI_CAT_TRENDS.csv", header = TRUE, sep = ";")
  change <- as.numeric(gsub(",",".",LPIResults$value[LPIResults$X == "% change"]))
  SEchange <- as.numeric(gsub(",",".",LPIResults$value[LPIResults$X == "SE % change"]))
  
  test1 <- data.frame(change, SEchange)
  names(test1) <- names(testFile_dist)
  
  testFile_dist <- rbind(testFile_dist, test1)
  
  #  Tornem al directori pare
  
  setwd("..")
  
  #
  #  Fem el test d'aquesta execució
  #
  
  diff      <- testFile_dist$index[1] - testFile_dist$index[nrow(testFile_dist)]
  se_diff   <- sqrt(testFile_dist$SE[1]^2 + testFile_dist$SE[nrow(testFile_dist)]^2)
  z         <- diff / se_diff
  p_value   <- 2*pnorm(-abs(z))
  
  print(p_value)
  
  # Guardem els resultats del test per poder-los ensenyar després
  
  testResult <- data.frame(prob,
                           testFile_dist$index[1],
                           testFile_dist$SE[1],
                           testFile_dist$index[nrow(testFile_dist)],
                           testFile_dist$SE[nrow(testFile_dist)],
                           length(unique(w_species_trends_out$species)),
                           diff,
                           se_diff,
                           z,
                           p_value)
  
  names(testResult) <- names(testResults_dist)
  
  testResults_dist <- rbind(testResults_dist, testResult)
  
  if (p_value < 0.05) {
    break
  }
  
}
