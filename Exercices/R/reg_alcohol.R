# ------------------------- #
# Tableau de regression - consommation d'alcool #
# ------------------------- #

library(tidyverse)
library(readxl)
library(gt)
library(modelsummary)

# Import ------------------------------------------------------------------
df_alcool <- read_xlsx("F:/Exercices/data/base_alcools_pays.xlsx")

# Regression --------------------------------------------------------------
df_alcool_reg <- lm(`Part de la mortalité attribuable à l'alcool (en %)` ~ 
                      `Part des buveurs ayant connu une alcoolisation massive au cours des 30 derniers jours (en %)` +
                      `Part des hommes ayant bu au cours des 12 derniers mois (en %)` +
                      `Consommation de vin (en litres d'alcool pur)` + 
                      `Consommation de bière (en litres d'alcool pur)` + 
                      `Consommation de spiritueux (en litres d'alcool pur)`, 
                    data = df_alcool)

# Tableau -----------------------------------------------------------------
msummary(df_alcool_reg)
