#
#  En aquest script posarem elsouputs per l'informe
#

#
#  carrega de  llibreries
#

library(gt)
library(flextable)
library(officer)
library(dplyr)
library(DataExplorer)

#
# calcul especies per taxon
#

taula_taxon <- inputDf %>%
  group_by(Taxon) %>%
  summarise(num_especies = n_distinct(SpeciesCode)) %>%
  arrange(desc(num_especies))   

taula_taxon

#
#  Taula 1
#

taula_resum <- inputDf %>%
  group_by(Taxon) %>%
  summarise(
    num_especies = n_distinct(SpeciesCode),
    
    tendencia = paste0(
      round(mean(Trend, na.rm=TRUE), 2), " (",
      round(min(Trend, na.rm=TRUE), 2), "/", 
      round(max(Trend, na.rm=TRUE), 2), ")"
    ),
    
    error_estandard = paste0(
      round(mean(SETrend, na.rm=TRUE), 2), " (",
      round(min(SETrend, na.rm=TRUE), 2), "/", 
      round(max(SETrend, na.rm=TRUE), 2), ")"
    ),
    
    distribucio = paste0(
      round(mean(Distribution, na.rm=TRUE), 2), " (",
      round(min(Distribution, na.rm=TRUE), 2), "/", 
      round(max(Distribution, na.rm=TRUE), 2), ")"
    ),
    
    cobertura = paste0(
      round(mean(SamplingCoverage, na.rm=TRUE), 2), " (",
      round(min(SamplingCoverage, na.rm=TRUE), 2), "/", 
      round(max(SamplingCoverage, na.rm=TRUE), 2), ")"
    )
  ) %>%
  arrange(desc(num_especies))

ft <- flextable(taula_resum) %>%
  autofit() %>%
  theme_box() %>%
  bold(part = "header") %>%
  color(color = "black", part = "header") %>%
  bg(bg = "#73EDFF", part = "header")  # capçalera blava

# Crear document Word

doc <- read_docx()
doc <- body_add_par(doc, "Resum d'espècies i estadístiques per taxon", style = "heading 1")
doc <- body_add_flextable(doc, ft)
print(doc, target = "Informe_TaulaResumCompacte.docx")

#
#  Annex II. Llista d'espècies  amb les seves caracteristiques
#

df <- inputDf[,-c(1, ((ncol(inputDf)-1):ncol(inputDf)))]

ft <- flextable(df)
ft <- theme_box(ft)      # estil bonic
ft <- color(ft, color = "black", part = "header") |> 
  bg(bg = "#73EDFF", part = "header")          # capçalera blava
ft <- align(ft, align = "center", part = "all")
ft <- autofit(ft)

doc <- read_docx()
doc <- body_add_par(doc, "Especies analitzades", 
                    style = "heading 1")
doc <- body_add_flextable(doc, ft)
print(doc, target = "informe_taula.docx")

#
#   Taula de resultats del test de sensibilitat del LPI-cat
#

gt(testResults_dist) |>
  tab_header(
    title = "Resultats del test de sensibilitat a totes les tendències extremes",
    subtitle = "Test de z scores"
  ) |>
  fmt_number(columns = everything()) |>
  opt_row_striping()
re

#
#  Gràfics per avaluar la normalitat de les variables
#

plot_intro(inputDf)
plot_bar(inputDf, ggtheme = theme_minimal())
inputDf_hist <- data.frame(inputDf[,5],abs(inputDf$Trend),inputDf[,4])
colnames(inputDf_hist) <- c("SE","Absolute Trend", "Trend")
plot_histogram(inputDf_hist)
plot_qq(log(inputDf_hist))

inputDf_trans <- inputDf_hist[,1:2]
plot_histogram(log(inputDf_trans))
plot_qq(log(inputDf_trans))



