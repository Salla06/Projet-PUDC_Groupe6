# ========================================
# FICHIER: performance.R
# Description: Module de traitement des données Excel pour l'analyse de performance
# ========================================

# Chargement des bibliothèques nécessaires
if (!require("readxl")) install.packages("readxl", dependencies = TRUE)
if (!require("dplyr")) install.packages("dplyr", dependencies = TRUE)
if (!require("stringr")) install.packages("stringr", dependencies = TRUE)

library(readxl)
library(dplyr)
library(stringr)

# ========================================
# FONCTIONS DE TRAITEMENT
# ========================================
EXCEL_FILE_PATH <- "data/PUDC_SUIVI TRIMESTRE  DU PTBA 2025 VF.xlsx"  # Ajustez le chemin

# Fonction pour extraire les données de performance du fichier Excel
extract_performance_data <- function(excel_file, sheet_name = "PERFORMANCE GLOBALE 2025") {
  
  # Vérifier l'existence du fichier
  if (!file.exists(excel_file)) {
    stop("Fichier Excel non trouvé: ", excel_file)
  }
  
  # Lire le fichier Excel
  tryCatch({
    data <- read_excel(excel_file, sheet = sheet_name, col_names = TRUE)
  }, error = function(e) {
    stop("Erreur lors de la lecture du fichier Excel: ", e$message)
  })
  
  # Fonction pour extraire région et valeur d'une cellule
  parse_region_cell <- function(cell_content) {
    if (is.na(cell_content) || cell_content == "") {
      return(list(regions = character(), values = numeric()))
    }
    
    # Pattern pour extraire toutes les occurrences de région (valeur)
    pattern <- "([^(\\s]+)\\s*\\(([^)]+)\\)"
    matches <- str_match_all(as.character(cell_content), pattern)[[1]]
    
    if (nrow(matches) > 0) {
      regions <- str_trim(matches[, 2])
      values <- sapply(matches[, 3], function(x) {
        # Nettoyer et convertir en numérique
        cleaned <- str_replace_all(x, "[^0-9.,]", "")
        as.numeric(str_replace(cleaned, ",", "."))
      })
      return(list(regions = regions, values = values))
    } else {
      return(list(regions = character(), values = numeric()))
    }
  }
  
  # Identifier les colonnes importantes
  available_cols <- names(data)
  cat("Colonnes disponibles:\n")
  print(available_cols)
  
  # Fonction pour trouver les colonnes par mots-clés
  find_column <- function(keywords) {
    for (keyword in keywords) {
      matches <- grep(keyword, available_cols, ignore.case = TRUE)
      if (length(matches) > 0) {
        return(available_cols[matches[1]])
      }
    }
    return(NULL)
  }
  
  # Mapping des colonnes
  col_mapping <- list(
    cible_globale = find_column(c("Cible globale", "cible.*globale", "PTBA", "globale.*PTBA")),
    cible_annuelle = find_column(c("Cible annuelle", "cible.*annuelle", "annuelle.*2025")),
    realisation_globale = find_column(c("Réalisation globale", "réalisation.*globale", "globale.*31", "31/12")),
    realisation_annuelle = find_column(c("Réalisation annuelle", "réalisation.*annuelle", "annuelle.*réalisation")),
    taux_realisation = find_column(c("Taux", "taux.*réalisation", "pourcentage", "réalisation.*globale"))
  )
  
  cat("Mapping des colonnes:\n")
  print(col_mapping)
  
  # Collecter les données par région
  region_data_list <- list()
  
  for (col_type in names(col_mapping)) {
    col_name <- col_mapping[[col_type]]
    if (!is.null(col_name) && col_name %in% names(data)) {
      for (i in 1:nrow(data)) {
        cell_content <- data[[col_name]][i]
        if (!is.na(cell_content) && cell_content != "") {
          parsed <- parse_region_cell(cell_content)
          if (length(parsed$regions) > 0) {
            for (j in 1:length(parsed$regions)) {
              region <- parsed$regions[j]
              value <- parsed$values[j]
              
              if (!region %in% names(region_data_list)) {
                region_data_list[[region]] <- list(
                  cible_globale = NA,
                  cible_annuelle = NA,
                  realisation_globale = NA,
                  realisation_annuelle = NA,
                  taux_realisation = NA
                )
              }
              
              region_data_list[[region]][[col_type]] <- value
            }
          }
        }
      }
    }
  }
  
  # Créer le dataframe final
  if (length(region_data_list) == 0) {
    warning("Aucune donnée régionale extraite")
    return(data.frame(
      region = character(),
      cible_globale = numeric(),
      cible_annuelle = numeric(),
      realisation_globale = numeric(),
      realisation_annuelle = numeric(),
      taux_realisation = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  
  performance_data <- data.frame(
    region = names(region_data_list),
    cible_globale = sapply(region_data_list, function(x) ifelse(is.na(x$cible_globale), 0, x$cible_globale)),
    cible_annuelle = sapply(region_data_list, function(x) ifelse(is.na(x$cible_annuelle), 0, x$cible_annuelle)),
    realisation_globale = sapply(region_data_list, function(x) ifelse(is.na(x$realisation_globale), 0, x$realisation_globale)),
    realisation_annuelle = sapply(region_data_list, function(x) ifelse(is.na(x$realisation_annuelle), 0, x$realisation_annuelle)),
    taux_realisation = sapply(region_data_list, function(x) ifelse(is.na(x$taux_realisation), 0, x$taux_realisation)),
    stringsAsFactors = FALSE
  )
  
  # Calculer le taux de réalisation si manquant
  performance_data$taux_realisation <- ifelse(
    performance_data$taux_realisation == 0 & performance_data$cible_globale != 0,
    round((performance_data$realisation_globale / performance_data$cible_globale) * 100, 2),
    performance_data$taux_realisation
  )
  
  return(performance_data)
}

# ========================================
# FONCTION PRINCIPALE D'INITIALISATION
# ========================================

# Fonction principale pour charger et traiter les données
load_performance_data <- function(excel_file_path, sheet_name = "PERFORMANCE GLOBALE 2025") {
  
  cat("=== CHARGEMENT DES DONNÉES DE PERFORMANCE ===\n")
  cat("Fichier:", excel_file_path, "\n")
  cat("Feuille:", sheet_name, "\n\n")
  
  # Vérifier l'existence du fichier
  if (!file.exists(excel_file_path)) {
    cat("ATTENTION: Fichier non trouvé, utilisation des données de test\n")
    return(get_test_data())
  }
  
  # Extraire les données
  tryCatch({
    data <- extract_performance_data(excel_file_path, sheet_name)
    
    if (nrow(data) == 0) {
      cat("ATTENTION: Aucune donnée extraite, utilisation des données de test\n")
      return(get_test_data())
    }
    
    cat("SUCCESS: Données extraites avec succès\n")
    cat("Régions trouvées:", paste(data$region, collapse = ", "), "\n")
    cat("Nombre de régions:", nrow(data), "\n\n")
    
    # Afficher un résumé des données
    print("=== RÉSUMÉ DES DONNÉES ===")
    print(data)
    
    return(data)
    
  }, error = function(e) {
    cat("ERREUR lors de l'extraction:", e$message, "\n")
    cat("Utilisation des données de test\n")
    return(get_test_data())
  })
}

# ========================================
# DONNÉES DE TEST (FALLBACK)
# ========================================

get_test_data <- function() {
  return(data.frame(
    region = c("Thiès", "Louga", "Kolda", "Sédhiou", "Fatick", "Kaffrine"),
    cible_globale = c(105.62, 74.5, 166.42, 159.5, 0, 0),
    cible_annuelle = c(90, 98, 0, 0, 45, 0),
    realisation_globale = c(166.42, 159.5, 0, 0, 0, 0),
    realisation_annuelle = c(0.3321, 0.531666667, 0, 0, 0, 0),
    taux_realisation = c(83, 53, 0, 0, 0, 0),
    stringsAsFactors = FALSE
  ))
}

# ========================================
# FONCTIONS UTILITAIRES
# ========================================

# Fonction pour valider les données
validate_performance_data <- function(data) {
  required_cols <- c("region", "cible_globale", "cible_annuelle", "realisation_globale", "realisation_annuelle", "taux_realisation")
  
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Colonnes manquantes dans les données: ", paste(missing_cols, collapse = ", "))
  }
  
  if (nrow(data) == 0) {
    stop("Aucune donnée de performance trouvée")
  }
  
  cat("Validation OK: Données conformes\n")
  return(TRUE)
}

# Fonction pour nettoyer et formater les données
clean_performance_data <- function(data) {
  # Nettoyer les valeurs manquantes
  data[is.na(data)] <- 0
  
  # Nettoyer les noms de régions
  data$region <- str_trim(data$region)
  data$region <- str_to_title(data$region)
  
  # Arrondir les valeurs numériques
  numeric_cols <- c("cible_globale", "cible_annuelle", "realisation_globale", "realisation_annuelle", "taux_realisation")
  data[numeric_cols] <- lapply(data[numeric_cols], function(x) round(as.numeric(x), 2))
  
  return(data)
}

# ========================================
# MESSAGE DE CHARGEMENT DU MODULE
# ========================================

cat("Module performance.R chargé avec succès ✓\n")
cat("Fonctions disponibles:\n")
cat("  - load_performance_data(excel_file_path, sheet_name)\n")
cat("  - validate_performance_data(data)\n")
cat("  - clean_performance_data(data)\n")
cat("  - get_test_data()\n\n")
