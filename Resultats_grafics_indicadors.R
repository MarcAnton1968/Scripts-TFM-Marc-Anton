#
# Generació de la taula de resultats del MSI i grafics dels casos extrems
#
#  Comencem per recollir el resultat del LPI original de la  carpeta MSI_dist0
# 

library(dplyr)

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

percentils <- testResults$percentil[testResults$p_value<0.05]

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

library(ggplot2)
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
  scale_y_continuous(limits = c(50,110), breaks = seq(50,110,20)) +
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

