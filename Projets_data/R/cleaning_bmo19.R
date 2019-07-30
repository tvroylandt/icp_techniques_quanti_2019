# --------------------------------- #
# Mise en forme BMO - projets ICP #
# --------------------------------- #

# Ventilation présente sous le requeteur
# On ne fait que rajouter les codes pour plus de simplicité
  
library(tidyverse)
library(writexl)

# Chargement --------------------------------------------------------------
bmo19 <- read_rds("T:/DSEE/DOE/_Nouvelle_Arborescence/BMO/Bases_principale/Propre/Fichier_projets/bmo_2019.rds")

# Regroupement ------------------------------------------------------------
bmo19_export <- bmo19 %>% 
  group_by(fap87_lib) %>% 
  summarise(nb_proj_tot = sum(nb_proj_tot),
            nb_proj_diff = sum(nb_proj_diff),
            nb_proj_sais = sum(nb_proj_sais)) %>% 
  ungroup() %>% 
  mutate_if(is.numeric, function(x){round(x / 10, 0) * 10}) %>% 
  rename("Métier" = fap87_lib,
         "Grand secteur" = gd_secteur_bmo,
         "Nombre de projets" = nb_proj_tot,
         "Nombre de projets difficiles" = nb_proj_diff,
         "Nombre de projets saisonniers" = nb_proj_sais)

# Export ------------------------------------------------------------------
write_xlsx(bmo19_export, "data/pole_emploi_bmo_2019_metier_secteur.xlsx")
