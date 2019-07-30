# ------------------------- #
# Nettoyage fichier electiones 2017 presidentielle 1er tour #
# ------------------------- #

library(tidyverse)

# Chargement --------------------------------------------------------------
df_elections <-
  readxl::read_xls(
    "data-raw/Presidentielle_2017_Resultats_Tour_1.xls",
    sheet = "Départements Tour 1",
    skip = 4,
    col_types = c(
      "text",
      "text",
      "numeric",
      "numeric",
      "skip",
      "numeric",
      "skip",
      "numeric",
      "skip",
      "skip",
      "numeric",
      "skip",
      "skip",
      "numeric",
      "skip",
      "skip",
      rep(c(
        "skip", "text", "skip", "numeric", "skip", "skip"
      ), 11)
    ),
    col_names = c(
      "code_dep",
      "lib_dep",
      "inscrits",
      "abstentions",
      "votants",
      "blancs",
      "nuls",
      "exprimes",
      "nom1",
      "voix1",
      "nom2",
      "voix2",
      "nom3",
      "voix3",
      "nom4",
      "voix4",
      "nom5",
      "voix5",
      "nom6",
      "voix6",
      "nom7",
      "voix7",
      "nom8",
      "voix8",
      "nom9",
      "voix9",
      "nom10",
      "voix10",
      "nom11",
      "voix11"
    )
  )


# Nettoyage ----------------------------------------------------------------
# Gestion du format
df_gather <- df_elections %>%
  select(-lib_dep,-inscrits,-abstentions,-votants,-blancs,-nuls,-exprimes) %>%
  gather(variable, var2, -code_dep) %>%
  mutate(
    type_var = case_when(str_detect(variable, "nom") ~ "nom",
                         TRUE ~ "voix"),
    id = str_remove(variable, "nom"),
    id = str_remove(id, "voix")
  ) %>%
  select(-variable)

df_gather_propre <- df_gather %>%
  filter(type_var == "nom") %>%
  left_join(df_gather %>% filter(type_var == "voix"), by = c("code_dep", "id")) %>%
  rename(nom = var2.x,
         voix = var2.y) %>%
  select(code_dep, nom, voix)

# Mise au propre des variables
# Source de la typologie : http://www.lecompas.fr/doc/compasetudes10_novembre2013.pdf
df_elections_propre <- df_elections %>%
  select(code_dep,
         lib_dep,
         inscrits,
         abstentions,
         votants,
         blancs,
         nuls,
         exprimes) %>%
  left_join(df_gather_propre, by = c("code_dep")) %>%
  mutate(
    code_dep = case_when(nchar(code_dep) == 1 ~ paste0("0", code_dep),
                         TRUE ~ code_dep),
    typologie = fct_collapse(
      code_dep,
      "Départements franciliens à niveaux de vie très élevés et fortes disparités" = c("75", "77", "78", "91", "92", "94", "95"),
      "Départements aux fonctions métropolitaines supérieures et niveaux de vie élevés" = c("01", "31", "33", "35", "38", "44", "64", "69", "73", "74"),
      "Départements à forte mixité socio-économique" = c(
        "14",
        "21",
        "25",
        "27",
        "28",
        "37",
        "42",
        "45",
        "51",
        "54",
        "57",
        "60",
        "63",
        "67",
        "68",
        "72",
        "76",
        "86",
        "87",
        "90"
      ),
      "Départements à population jeune et à forte précarité" = c("02", "08", "59", "62", "80", "93"),
      "Départements méditerranéens vieillissants à tendance précaire et fortes disparités" = c("06", "13", "2A", "2B", "30", "34", "66", "83", "84"),
      "Départements ruraux à faible dynamisme démographique" = c(
        "03",
        "10",
        "16",
        "18",
        "23",
        "36",
        "52",
        "55",
        "58",
        "61",
        "70",
        "71",
        "88",
        "89"
      ),
      "Départements ruraux très attractifs à forte instabilité de l'emploi" = c(
        "04",
        "05",
        "07",
        "09",
        "11",
        "17",
        "24",
        "26",
        "40",
        "46",
        "47",
        "65",
        "81",
        "82"
      ),
      "Départements ruraux attractifs à faible taux de chômage et fort taux d'emploi dans le secteur primaire" = c(
        "12",
        "15",
        "19",
        "22",
        "29",
        "32",
        "39",
        "41",
        "43",
        "48",
        "49",
        "50",
        "53",
        "56",
        "79",
        "85"
      )
    )
  ) %>%
  spread(nom, voix) %>%
  filter(str_sub(code_dep, 1, 1) != "Z") %>%
  rename(
    "Code du département" = code_dep,
    "Nom du département" = lib_dep,
    "Inscrits" = inscrits,
    "Abstentions" = abstentions,
    "Votants" = votants,
    "Blancs" = blancs,
    "Nuls" = nuls,
    "Exprimés" = exprimes,
    "Type de département" = typologie
  )

# Export ------------------------------------------------------------------
writexl::write_xlsx(df_elections_propre,
                    "data/interieur_elections_presidentielles_2017_departements.xlsx")
