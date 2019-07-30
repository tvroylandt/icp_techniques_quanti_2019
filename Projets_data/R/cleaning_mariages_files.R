# ----------------------------------- #
# Nettoyage fichiers mariages #
# ----------------------------------- #

library(tidyverse)
library(foreign)

# Import ------------------------------------------------------------------
df_mariages <- read.dbf("data_raw/MAR2017.dbf") %>%
  mutate_all(as.character) %>%
  as_tibble()

# Uniformiser les variables -----------------------------------------------
df_mariages_new <- df_mariages %>%
  select(-ANAISH,
         -ANAISF,
         -DEPNAISH,
         -DEPNAISF,
         -ETAMATH,
         -ETAMATF,
         -INDNATH,
         -INDNATF) %>%
  mutate(ind_inversion = case_when(SEXE1 != SEXE2 &
                                     SEXE1 == "F" ~ "1"))

# Gestion des cas d'inversion conjoint 1 / conjoint 2
df_mariages_new_inversion <- df_mariages_new %>%
  filter(ind_inversion == "1") %>%
  mutate(
    ANAIS1_bis = ANAIS1,
    DEPNAIS1_bis = DEPNAIS1,
    ETAMAT1_bis = ETAMAT1,
    INDNAT1_bis = INDNAT1,
    ANAIS1 = ANAIS2,
    DEPNAIS1 = DEPNAIS2,
    ETAMAT1 = ETAMAT2,
    INDNAT1 = INDNAT2,
    ANAIS2 = ANAIS1_bis,
    DEPNAIS2 = DEPNAIS1_bis,
    ETAMAT2 = ETAMAT1_bis,
    INDNAT2 = INDNAT1_bis
  ) %>%
  select(-ANAIS1_bis, -DEPNAIS1_bis, -ETAMAT1_bis, -INDNAT1_bis)

# Fusion
df_mariages_unif <- df_mariages_new_inversion %>%
  bind_rows(df_mariages_new %>%
              filter(is.na(ind_inversion)))

# Mise en forme des données -----------------------------------------------
# Type des variables
# Indicatrice de mariage meme sexe
# Ecart d'age
# Remplacer les codes par les modalités
# Nom des variables en minuscules
df_mariages_full <- df_mariages_unif %>%
  mutate_at(vars(ANAIS1, ANAIS2, AMAR), as.numeric) %>%
  mutate(
    type_mariage = case_when(SEXE1 == SEXE2 ~ "Même sexe",
                             TRUE ~ "Sexe différent"),
    age1 = AMAR - ANAIS1,
    age2 = AMAR - ANAIS2,
    ecart_age = age1 - age2,
    ETAMAT1 = fct_recode(
      ETAMAT1,
      "Célibataire" = "1",
      "Veuf" = "3",
      "Divorcé" = "4"
    ),
    ETAMAT2 = fct_recode(
      ETAMAT2,
      "Célibataire" = "1",
      "Veuf" = "3",
      "Divorcé" = "4"
    ),
    INDNAT1 = fct_recode(INDNAT1,
                         "Française" = "1",
                         "Etrangère" = "2"),
    INDNAT2 = fct_recode(INDNAT2,
                         "Française" = "1",
                         "Etrangère" = "2"),
    NBENFCOM = fct_recode(
      NBENFCOM,
      "Enfants en commun" = "O",
      "Pas d'enfants en commun" = "N"
    ),
    SEXE1 = fct_recode(SEXE1,
                       "Femme" = "F",
                       "Homme" = "M"),
    SEXE2 = fct_recode(SEXE2,
                       "Femme" = "F",
                       "Homme" = "M"),
    TUDOM = fct_recode(
      TUDOM,
      "Commune rurale" = "0",
      "Unité urbaine de 2 000 à 4 999 habitants" = "1",
      "Unité urbaine de 5 000 à 9 999 habitants" = "2",
      "Unité urbaine de 10 000 à 19 999 habitants" = "3",
      "Unité urbaine de 20 000 à 49 999 habitants" = "4",
      "Unité urbaine de 50 000 à 99 999 habitants" = "5",
      "Unité urbaine de 100 000 à 199 999 habitants" = "6",
      "Unité urbaine de 200 000 à 1 999 999 habitants" = "7",
      "Agglomération de Paris" = "8",
      "COM ou étranger" = "9"
    ),
    TUCOM = fct_recode(
      TUCOM,
      "Commune de plus de 10 000 habitants" = "P",
      "Commune de moins de 10 000 habitants" = "M",
      "Terres australes et antarctiques, COM non précisé" = "A"
    ),
    JSEMAINE = fct_recode(
      JSEMAINE,
      "Lundi" = "1",
      "Mardi" = "2",
      "Mercredi" = "3",
      "Jeudi" = "4",
      "Vendredi" = "5",
      "Samedi" = "6",
      "Dimanche" = "7"
    ),
    MMAR = fct_recode(
      MMAR,
      "Janvier" = "01",
      "Février" = "02",
      "Mars" = "03",
      "Avril" = "04",
      "Mai" = "05",
      "Juin" = "06",
      "Juillet" = "07",
      "Août" = "08",
      "Septembre" = "09",
      "Octobre" = "10",
      "Novembre" = "11",
      "Décembre" = "12"
    )
  ) %>%
  replace_na(list(
    ind_inversion = "0",
    tudom = "Indéterminé",
    tucom = "Indéterminé ou étranger"
  )) %>%
  rename_all(tolower)

# Fichier réduit pour projet ----------------------------------------------
df_mariages_reduit_2017_idf <- df_mariages_full %>%
  filter(
    depmar %in% c("75", "77", "78", "91", "92", "93", "94", "95") &
      type_mariage == "Sexe différent"
  ) %>%
  select(amar,
         mmar,
         jsemaine,
         age1,
         age2,
         etamat1,
         etamat2,
         indnat1,
         indnat2,
         depmar) %>%
  rename(
    "Année" = amar,
    "Mois" = mmar,
    "Jour de la semaine" = jsemaine,
    "Âge de l'homme" = age1,
    "Âge de la femme" = age2,
    "Etat matrimonial antérieur de l'homme" = etamat1,
    "Etat matrimonial antérieur de la femme" = etamat2,
    "Nationalité de l'homme" = indnat1,
    "Nationalité de la femme" = indnat2,
    "Département du mariage" = depmar
  )

# Export ------------------------------------------------------------------
writexl::write_xlsx(df_mariages_reduit_2017_idf,
                    "data/insee_mariage_ile-de-france_2017.xlsx")
