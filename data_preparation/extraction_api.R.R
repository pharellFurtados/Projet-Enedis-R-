# --- 0. Dépendances et Constantes ---
library(dplyr)
library(readr)
library(janitor)
library(rlang)
library(here) # <-- Package pour trouver les fichiers

# --- Configuration des chemins et périmètre ---
# here() trouve la racine de ton projet (Projet_Enedis_FINAL)
# Il va chercher les CSV bruts dans le dossier "data_preparation"
CHEMIN_SOURCE_NEUFS <- here("data_preparation", "dpe-v2-logements-neufs.csv")
CHEMIN_SOURCE_EXISTANTS <- here("data_preparation", "dpe-v2-logements-existants.csv")

# Il va sauvegarder le fichier final au bon endroit
CHEMIN_FINAL <- here("app", "data", "dpe_clean.csv")

# Vérification du dossier de sortie
if (!dir.exists(here("app", "data"))) {
  dir.create(here("app", "data"), recursive = TRUE)
  cat("Dossier 'app/data/' créé.\n")
}

# Définition du périmètre géographique (Lyon 1-9)
CODES_POSTAUX_LYON <- c("69001", "69002", "69003", "69004", "69005", "69006", "69007", "69008", "69009")

# --- 1. Fonctions Utilitaires ---
find_col_name <- function(noms_colonnes, possibilites) {
  for (nom in possibilites) {
    if (nom %in% noms_colonnes) {
      return(nom)
    }
  }
  return(NA_character_)
}

# --- 2. Fonction Principale de Traitement ---
process_dpe_file <- function(chemin_fichier, type_logement) {
  cat(paste("\n--- Traitement de :", chemin_fichier, "---\n"))
  
  # ===================================================================
  # CORRECTION : Remplacement du tryCatch cassé par une vérification claire
  # ===================================================================
  if (!file.exists(chemin_fichier)) {
    cat(paste("  ERREUR FATALE: Fichier introuvable:", chemin_fichier, "\n"))
    cat("  Vérifiez qu'il est bien dans le dossier 'data_preparation/'\n")
    return(tibble()) # Arrête la fonction PROPREMENT
  }
  
  cat("  Fichier trouvé. Lecture en cours...\n")
  data_brut <- read_csv(
    chemin_fichier,
    col_types = cols(.default = "c"),
    progress = FALSE,
    show_col_types = FALSE
  )
  # ===================================================================
  
  data_clean <- data_brut %>% 
    janitor::clean_names() 
  
  noms_nets <- names(data_clean)
  
  col_postal <- find_col_name(noms_nets, c("code_postal_ban", "code_postal_brut"))
  col_lat <- find_col_name(noms_nets, c("latitude", "coordonnee_cartographique_y_ban"))
  col_lon <- find_col_name(noms_nets, c("longitude", "coordonnee_cartographique_x_ban"))
  col_dept <- find_col_name(noms_nets, c("n_departement_ban", "nn_departement_ban"))
  col_conso <- find_col_name(noms_nets, c("conso_5_usages_m2_e_finale", "consommation_energie_m2"))
  
  if (is.na(col_postal) || is.na(col_lat) || is.na(col_lon) || is.na(col_conso)) {
    cat("  ERREUR: Colonnes vitales (postal, geo, conso) introuvables.\n")
    return(tibble())
  }
  
  if (!("annee_construction" %in% noms_nets)) {
    data_clean$annee_construction <- NA_character_
  }
  
  data_transformee <- data_clean %>%
    filter(!!sym(col_postal) %in% CODES_POSTAUX_LYON) %>%
    transmute(
      dpe_classe = as.factor(etiquette_dpe),
      consommation_kwh_m2 = as.numeric(!!sym(col_conso)),
      surface_m2 = as.numeric(surface_habitable_logement),
      annee_construction = as.numeric(annee_construction),
      type_logement_immeuble = type_batiment,
      departement = !!sym(col_dept),
      code_postal = !!sym(col_postal),
      longitude = as.numeric(!!sym(col_lon)),
      latitude = as.numeric(!!sym(col_lat)),
      source_type = type_logement
    ) %>%
    filter(
      !is.na(latitude) & !is.na(longitude),
      !is.na(consommation_kwh_m2),
      dpe_classe %in% LETTERS[1:7]
    )
  
  cat(paste("  ->", nrow(data_transformee), "lignes valides extraites pour Lyon.\n"))
  return(data_transformee)
}

# --- 3. Exécution et Sauvegarde ---
data_neuf <- process_dpe_file(CHEMIN_SOURCE_NEUFS, "Neuf")
data_existant <- process_dpe_file(CHEMIN_SOURCE_EXISTANTS, "Ancien")

logements_complet_lyon <- bind_rows(data_neuf, data_existant)

if (nrow(logements_complet_lyon) > 0) {
  write_csv(logements_complet_lyon, CHEMIN_FINAL)
  cat(paste("\n--- SUCCÈS ---"))
  cat(paste("\nTotal logements fusionnés :", nrow(logements_complet_lyon), "lignes.\n"))
  cat(paste("Fichier final généré :", CHEMIN_FINAL, "\n"))
} else {
  cat("\n--- ÉCHEC ---")
  cat("\nAucune donnée générée. Le fichier final est vide. Vérifiez que vos CSV bruts contiennent bien des données pour Lyon (69001-69009).\n")
}
