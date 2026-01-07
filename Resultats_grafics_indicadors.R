#
# Grafics d'indicadors
#
#  Comencem per recollir el resultat del LPI original de la  carpeta MSI_dist0
# 

library(dplyr)
library(ggplot2)

setwd("C:/Marc/indicadors/LPI/recerca/Sensitivity_analisis")

LPI_cat <- read.csv2("./MSI_dist0/LPI_CAT_RESULTS.csv", header = TRUE, sep = ";")

#
#  Ara necessitem tenir les carpetes amb més valor de MSI_dist i de MSI_dist_max
#

carpetes <-  list.dirs(path = ".", recursive = FALSE, full.names = FALSE) |> 
  grep("^MSI", x = _, value = TRUE)

# MSI_dist (sense _max)
nums_dist <- as.numeric(
  sub("^MSI_dist(\\d+)$", "\\1",
      grep("^MSI_dist\\d+$", carpetes, value = TRUE))
)

max_dist <- max(nums_dist)

# MSI_dist_max
nums_dist_max <- as.numeric(
  sub("^MSI_dist_max(\\d+)$", "\\1",
      grep("^MSI_dist_max\\d+$", carpetes, value = TRUE))
)

max_dist_max <- max(nums_dist_max)

#
# Generem les dues carpetres i carreguem els valors
#

carpeta_dist <- paste0("./MSI_dist", max_dist)
carpeta_dist_max <- paste0("MSI_dist_max", max_dist_max)

#
#  PEr saber el percentil anem a test_results i tindrem quin d'ells és per a cada carpeta
#

# percentils <- testResults$percentil[testResults$p_value<0.05]
percentils <- c(0.25,0.70)

#
# Generem un nom pel fitxer que ens permetrà graficar
#

nom_percentil_baix <- paste0("LPI_cat_perc",100*min(percentils))
nom_percentil_alt <- paste0("LPI_cat_perc",100*max(percentils))

#
# Agafem el LPI diferent amb el percentil baix
#

setwd(carpeta_dist)
assign(nom_percentil_baix, 
       read.csv2("./LPI_CAT_RESULTS.csv", header = TRUE, sep = ";"))
setwd("..")

#
# Agafem el LPI diferent amb el percentil alt
#

setwd(carpeta_dist_max)
assign(nom_percentil_alt, 
       read.csv2("./LPI_CAT_RESULTS.csv", header = TRUE, sep = ";"))
setwd("..")

#
#  Ajunto tots els LPIs
#

LPIS <- bind_rows(
  LPI_cat = LPI_cat,
  LPI_cat_perc25 = LPI_cat_perc25,
  LPI_cat_perc70 = LPI_cat_perc70,
  .id = "LPI"
)

#
#  Fem els gràfics
#

minyear <- min(LPIS$year)
maxyear <- max(LPIS$year)
jobname <- "Anàlisi de sensibilitat del Living Planet Index Catalunya"
escala <- c("LPI_cat","LPI_cat_perc25","LPI_cat_perc70")
color_principal <- c("orange","olivedrab","blue")
color_fons <- c("orange1","olivedrab3","blue3")
llegenda = guide_legend(title = NULL, direction = "vertical")
dibuix_jpg <- paste("LPI_cat","jpg", sep = ".")
dibuix_vec <- paste("LPI_cat","svg", sep = ".")

ggplot(LPIS, aes(x = year, y = Trend, colour = LPI)) +
  geom_line(size = 1) + 
  geom_ribbon(aes(ymin = lower_CL_trend, ymax = upper_CL_trend, fill = LPI), 
              lty = 0, alpha = 0.3) +  
  labs(title = jobname, x = NULL) +
  scale_color_manual(values=color_principal, labels = escala, guide = llegenda) +
  scale_fill_manual(values=color_fons, labels = escala, guide = llegenda) +
  theme(legend.position ="bottom") +
  scale_x_continuous(breaks = seq(minyear,maxyear,2)) +
  scale_y_continuous(limits = c(60,110), breaks = seq(60,110,10)) +
  theme_minimal()  +
  theme(
    axis.title = element_text(size = 10),    # Títols dels eixos
    axis.text = element_text(size = 10),     # Nombres de marques
    legend.title = element_text(size = 10),  # Títol llegenda
    legend.text = element_text(size = 10),  # Text llegenda
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

ggsave(dibuix_jpg,)
ggsave(dibuix_vec,)

#
#   Ara grafiquem només LPI_cat
#

jobname <- "Living Planet Index Catalunya, versió 2024"
escala <- c("LPI_cat")
color_principal <- c("orange")
color_fons <- c("orange1")
llegenda = guide_legend(title = NULL, direction = "vertical")
dibuix_jpg <- paste("LPI_cat_original","jpg", sep = ".")
dibuix_vec <- paste("LPI_cat_original","svg", sep = ".")

ggplot(LPIS[LPIS$LPI == "LPI_cat",], aes(x = year, y = Trend, colour = LPI)) +
  geom_point(LPIS[LPIS$LPI == "LPI_cat",], mapping = aes(x = year, y = MSI, colour = LPI)) +
  geom_line(size = 1) + 
  geom_ribbon(aes(ymin = lower_CL_trend, ymax = upper_CL_trend, fill = LPI), 
              lty = 0, alpha = 0.3) +  
    labs(title = jobname, x = NULL) +
  scale_color_manual(values=color_principal, labels = escala, guide = llegenda) +
  scale_fill_manual(values=color_fons, labels = escala, guide = llegenda) +
  theme(legend.position ="bottom") +
  scale_x_continuous(breaks = seq(minyear,maxyear,2)) +
  scale_y_continuous(limits = c(60,110), breaks = seq(60,110,10)) +
  theme_minimal()  +
  theme(
    axis.title = element_text(size = 10),    # Títols dels eixos
    axis.text = element_text(size = 10),     # Nombres de marques
    legend.title = element_text(size = 10),  # Títol llegenda
    legend.text = element_text(size = 10),  # Text llegenda
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

ggsave(dibuix_jpg,)
ggsave(dibuix_vec,)

#
#   El mateix, però sense punts
#

jobname <- "Living Planet Index Catalunya, versió 2024"
escala <- c("LPI_cat")
color_principal <- c("orange")
color_fons <- c("orange1")
llegenda = guide_legend(title = NULL, direction = "vertical")
dibuix_jpg <- paste("LPI_cat_original_sense_punts","jpg", sep = ".")
dibuix_vec <- paste("LPI_cat_original_sense_punts","svg", sep = ".")

ggplot(LPIS[LPIS$LPI == "LPI_cat",], aes(x = year, y = Trend, colour = LPI)) +
  geom_line(size = 1) + 
  geom_ribbon(aes(ymin = lower_CL_trend, ymax = upper_CL_trend, fill = LPI), 
              lty = 0, alpha = 0.3) +  
  labs(title = jobname, x = NULL) +
  scale_color_manual(values=color_principal, labels = escala, guide = llegenda) +
  scale_fill_manual(values=color_fons, labels = escala, guide = llegenda) +
  theme(legend.position ="bottom") +
  scale_x_continuous(breaks = seq(minyear,maxyear,2)) +
  scale_y_continuous(limits = c(60,110), breaks = seq(60,110,10)) +
  theme_minimal()  +
  theme(
    axis.title = element_text(size = 10),    # Títols dels eixos
    axis.text = element_text(size = 10),     # Nombres de marques
    legend.title = element_text(size = 10),  # Títol llegenda
    legend.text = element_text(size = 10),  # Text llegenda
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

ggsave(dibuix_jpg,)
ggsave(dibuix_vec,)

#
#   Anàlisi de sensibilitat de les distribucions menors
#


# percentils <- testResults$percentil[testResults$p_value<0.05]
percentils <- c(0.15,0.25)

#
# Generem un nom pel fitxer que ens permetrà graficar
#

nom_percentil_baix <- paste0("LPI_cat_perc",100*min(percentils))
nom_percentil_alt <- paste0("LPI_cat_perc",100*max(percentils))

#
# Agafem el LPI diferent amb el percentil baix
#

setwd("./MSI_dist3")
assign(nom_percentil_baix, 
       read.csv2("./LPI_CAT_RESULTS.csv", header = TRUE, sep = ";"))
setwd("..")

#
# Agafem el LPI diferent amb el percentil alt
#

setwd("./MSI_dist5")
assign(nom_percentil_alt, 
       read.csv2("./LPI_CAT_RESULTS.csv", header = TRUE, sep = ";"))
setwd("..")

#
#  Ajunto tots els LPIs
#

LPIS <- bind_rows(
  LPI_cat = LPI_cat,
  LPI_cat_perc10 = LPI_cat_perc15,
  LPI_cat_perc25 = LPI_cat_perc25,
  .id = "LPI"
)

#
#  Grafiquem
#

minyear <- min(LPIS$year)
maxyear <- max(LPIS$year)
jobname <- "Anàlisi de sensibilitat del Living Planet Index Catalunya"
escala <- c("LPI_cat: 355 espècies",
            "LPI_cat_perc15:332 espècies",
            "LPI_cat_perc25:314 espècies")
color_principal <- c("orange","olivedrab","blue")
color_fons <- c("orange1","olivedrab3","blue3")
llegenda = guide_legend(title = NULL, direction = "vertical")
dibuix_jpg <- paste("LPI_cat_4","jpg", sep = ".")
dibuix_vec <- paste("LPI_cat_4","svg", sep = ".")

ggplot(LPIS, aes(x = year, y = Trend, colour = LPI)) +
  geom_line(size = 1) + 
  geom_ribbon(aes(ymin = lower_CL_trend, ymax = upper_CL_trend, fill = LPI), 
              lty = 0, alpha = 0.3) +  
  labs(title = jobname, x = NULL) +
  scale_color_manual(values=color_principal, labels = escala, guide = llegenda) +
  scale_fill_manual(values=color_fons, labels = escala, guide = llegenda) +
  theme(legend.position ="bottom") +
  scale_x_continuous(breaks = seq(minyear,maxyear,2)) +
  scale_y_continuous(limits = c(60,110), breaks = seq(60,110,10)) +
  theme_minimal()  +
  theme(
    axis.title = element_text(size = 10),    # Títols dels eixos
    axis.text = element_text(size = 10),     # Nombres de marques
    legend.title = element_text(size = 10),  # Títol llegenda
    legend.text = element_text(size = 10),  # Text llegenda
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

ggsave(dibuix_jpg,)
ggsave(dibuix_vec,)
