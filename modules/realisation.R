# ========================================
# IMPORT DIRECT DES DONNÉES EXCEL
# Approche directe avec volets configurables
# ========================================

library(readxl)
library(dplyr)
library(stringr)

# Import direct de la feuille Excel
realisation <- read_excel("data/PUDC_SUIVI.xlsx", sheet = "PERFORMANCE GLOBALE 2025")
realisation <- as.data.frame(realisation, row.names = TRUE)

# Configuration des volets (basée sur vos données)
volets_config <- data.frame(
  id = c("PISTES RURALES", "ELECTRIFICATION RURALE", "Volet Hydraulique", "Volet Education",
         "Volet Santé", "Volet équipements post-récolte", "Volet chaine de valeur Agricole",
         "Volet chaine de Lait", "Renforcement de capacités des acteurs",
         "Environnement et Economie verte"),
  name = c("PISTES RURALES", "ELECTRIFICATION RURALE", "Volet Hydraulique", "Volet Education",
           "Volet Santé", "Volet équipements post-récolte", "Volet chaine de valeur Agricole",
           "Volet chaine de Lait", "Renforcement de capacités des acteurs",
           "Environnement et Economie verte"),
  color = c("#3B82F6", "#10B981", "#F59E0B", "#8B5CF6", "#EF4444", "#06B6D4", 
            "#F97316", "#84CC16", "#EC4899", "#6366F1"),
  stringsAsFactors = FALSE
)

# Fonction pour extraire les données d'un volet spécifique
extract_volet_data <- function(data, volet_name) {
  
  cat("Extraction des données pour le volet:", volet_name, "\n")
  
  # Afficher la structure des données pour debug
  cat("Colonnes disponibles:\n")
  print(colnames(data))
  cat("Premières lignes:\n")
  print(head(data))
  
  # Rechercher les lignes correspondant au volet
  volet_rows <- which(str_detect(data[,1], volet_name, ignore.case = TRUE))
  
  if (length(volet_rows) == 0) {
    cat("Aucune ligne trouvée pour le volet:", volet_name, "\n")
    return(data.frame())
  }
  
  cat("Lignes trouvées pour", volet_name, ":", volet_rows, "\n")
  
  # Extraire les données de performance pour ce volet
  volet_data <- data.frame()
  
  for (row_idx in volet_rows) {
    # Examiner la ligne
    row_data <- data[row_idx, ]
    cat("Ligne", row_idx, ":\n")
    print(row_data)
    
    # Extraire les valeurs en supposant une structure standard
    # Adapter selon la structure réelle de vos données
    if (ncol(data) >= 10) {
      region_info <- extract_region_from_cells(row_data)
      
      if (length(region_info) > 0) {
        volet_data <- rbind(volet_data, region_info)
      }
    }
  }
  
  return(volet_data)
}

# Fonction pour extraire les informations de région depuis une ligne
extract_region_from_cells <- function(row_data) {
  
  regions_data <- data.frame()
  
  # Parcourir les colonnes pour trouver les informations de région
  for (col_idx in 1:ncol(row_data)) {
    cell_value <- as.character(row_data[1, col_idx])
    
    if (!is.na(cell_value) && cell_value != "") {
      
      # Vérifier si c'est une cellule de région (format "Region(valeur)")
      region_match <- str_match(cell_value, "^(.+?)\\(([0-9.]+).*\\)$")
      
      if (!is.na(region_match[1])) {
        region_name <- str_trim(region_match[2])
        region_value <- as.numeric(region_match[3])
        
        cat("Région trouvée:", region_name, "- Valeur:", region_value, "\n")
        
        # Déterminer le type de données (globale ou annuelle) selon la colonne
        data_type <- determine_data_type(col_idx, colnames(row_data)[col_idx])
        
        region_row <- data.frame(
          region = region_name,
          value = region_value,
          type = data_type,
          stringsAsFactors = FALSE
        )
        
        regions_data <- rbind(regions_data, region_row)
      }
    }
  }
  
  return(regions_data)
}

# Fonction pour déterminer le type de données selon la colonne
determine_data_type <- function(col_idx, col_name) {
  
  # Adapter selon votre structure Excel
  # Par exemple, colonnes H = Région 2025 (annuelle), J = Région (globale)
  
  if (col_idx == 8 || str_detect(col_name, "2025|annuelle|Région 2025", ignore.case = TRUE)) {
    return("annuelle")
  } else if (col_idx == 10 || str_detect(col_name, "globale|Région$", ignore.case = TRUE)) {
    return("globale")
  } else {
    return("autre")
  }
}

# Fonction pour extraire toutes les données de performance
extract_all_performance_data <- function(data, selected_volet = "PISTES RURALES") {
  
  cat("=== EXTRACTION DES DONNÉES DE PERFORMANCE ===\n")
  cat("Volet sélectionné:", selected_volet, "\n\n")
  
  # Extraire les données du volet sélectionné
  volet_data <- extract_volet_data(data, selected_volet)
  
  if (nrow(volet_data) == 0) {
    cat("Aucune donnée trouvée. Utilisation des données de test.\n")
    return(create_test_data_for_volet(selected_volet))
  }
  
  # Restructurer les données par région
  performance_by_region <- volet_data %>%
    group_by(region) %>%
    summarise(
      realisation_globale = sum(value[type == "globale"], na.rm = TRUE),
      realisation_annuelle = sum(value[type == "annuelle"], na.rm = TRUE),
      .groups = "drop"
    )
  
  # Ajouter les cibles (à adapter selon vos données)
  performance_by_region <- performance_by_region %>%
    mutate(
      cible_globale = case_when(
        selected_volet == "PISTES RURALES" ~ c(200, 300, 256, 34, 200, 300)[1:n()],
        TRUE ~ rep(100, n())
      ),
      cible_annuelle = case_when(
        selected_volet == "PISTES RURALES" ~ c(30, 35, 45, 34, 30, 35)[1:n()],
        TRUE ~ rep(50, n())
      ),
      taux_realisation = round((realisation_globale / cible_globale) * 100, 1)
    )
  
  return(performance_by_region)
}

# Fonction pour créer des données de test selon le volet
create_test_data_for_volet <- function(volet_name) {
  
  regions <- c("Dakar", "Thiès", "Diourbel", "Fatick", "Kaolack", "Kaffrine", 
               "Louga", "Saint-Louis", "Matam", "Tambacounda", "Kédougou", 
               "Kolda", "Sédhiou", "Ziguinchor")
  
  # Données de test adaptées au volet
  if (volet_name == "PISTES RURALES") {
    test_data <- data.frame(
      region = regions[1:14],
      cible_globale = c(200, 300, 256, 34, 200, 300, 256, 34, 200, 300, 256, 34, 200, 300),
      cible_annuelle = c(30, 35, 45, 34, 30, 35, 45, 34, 30, 35, 45, 34, 30, 35),
      realisation_globale = c(105.62, 74.5, 0, 0, 105.62, 74.5, 0, 0, 105.62, 74.5, 0, 0, 105.62, 74.5),
      realisation_annuelle = c(28.9, 0, 0, 0, 41.5, 0, 0, 0, 28.9, 0, 0, 0, 41.5, 0)
    )
  } else if (volet_name == "ELECTRIFICATION RURALE") {
    test_data <- data.frame(
      region = regions[1:14],
      cible_globale = rep(c(42, 60, 35, 25), length.out = 14),
      cible_annuelle = rep(c(15, 20, 12, 8), length.out = 14),
      realisation_globale = rep(c(38, 45, 20, 15), length.out = 14),
      realisation_annuelle = rep(c(12, 18, 8, 5), length.out = 14)
    )
  } else {
    # Données génériques pour autres volets
    test_data <- data.frame(
      region = regions[1:14],
      cible_globale = rep(100, 14),
      cible_annuelle = rep(25, 14),
      realisation_globale = rep(75, 14),
      realisation_annuelle = rep(20, 14)
    )
  }
  
  test_data$taux_realisation <- round((test_data$realisation_globale / test_data$cible_globale) * 100, 1)
  
  return(test_data)
}

# Fonction pour charger les données avec volet sélectionnable
load_data_with_volet <- function(file_path = "data/PUDC_SUIVI.xlsx", 
                                 sheet_name = "PERFORMANCE GLOBALE 2025",
                                 selected_volet = "PISTES RURALES") {
  
  cat("=== CHARGEMENT DES DONNÉES ===\n")
  cat("Fichier:", file_path, "\n")
  cat("Feuille:", sheet_name, "\n")
  cat("Volet:", selected_volet, "\n\n")
  
  tryCatch({
    # Import direct comme vous l'avez spécifié
    realisation <- read_excel(file_path, sheet = sheet_name)
    realisation <- as.data.frame(realisation, row.names = TRUE)
    
    cat("Données importées avec succès!\n")
    cat("Dimensions:", nrow(realisation), "lignes,", ncol(realisation), "colonnes\n\n")
    
    # Extraire les données de performance
    performance_data <- extract_all_performance_data(realisation, selected_volet)
    
    return(performance_data)
    
  }, error = function(e) {
    cat("Erreur lors du chargement:", e$message, "\n")
    cat("Utilisation des données de test pour le volet:", selected_volet, "\n")
    return(create_test_data_for_volet(selected_volet))
  })
}

# Fonction pour obtenir les volets disponibles
get_available_volets <- function() {
  return(volets_config$id)
}

# Fonction pour obtenir la couleur d'un volet
get_volet_color <- function(volet_name) {
  color_idx <- which(volets_config$id == volet_name)
  if (length(color_idx) > 0) {
    return(volets_config$color[color_idx])
  } else {
    return("#666666")  # Couleur par défaut
  }
}

# ========================================
# UTILISATION ET TESTS
# ========================================

# Test avec différents volets
cat("=== TESTS D'EXTRACTION ===\n\n")

# Test 1: Pistes rurales
cat("1. Test PISTES RURALES:\n")
data_pistes <- load_data_with_volet(selected_volet = "PISTES RURALES")
print(head(data_pistes))

cat("\n2. Test ELECTRIFICATION RURALE:\n")
data_electrification <- load_data_with_volet(selected_volet = "ELECTRIFICATION RURALE")
print(head(data_electrification))

cat("\n3. Liste des volets disponibles:\n")
print(get_available_volets())

cat("\n4. Couleurs des volets:\n")
for (volet in volets_config$id[1:5]) {  # Premier 5 volets
  cat(volet, ":", get_volet_color(volet), "\n")
}

# Exporter les données pour utilisation dans Shiny
performance_data_global <<- data_pistes  # Par défaut, utiliser les pistes rurales
volets_config <<- volets_config



senegal_coords <- data.frame(
  region = c("Dakar", "Thiès", "Diourbel", "Fatick", "Kaolack", "Kaffrine", 
             "Louga", "Saint-Louis", "Matam", "Tambacounda", "Kédougou", 
             "Kolda", "Sédhiou", "Ziguinchor"),
  lat = c(14.6928, 14.7886, 14.6522, 14.3344, 14.1514, 14.1067,
          15.6186, 16.0200, 15.6557, 13.7671, 12.5571, 
          12.8839, 12.7081, 12.5847),
  lon = c(-17.4467, -16.9246, -16.2581, -16.4034, -16.0724, -15.5503,
          -15.6144, -16.5119, -13.2553, -13.6681, -12.1754,
          -14.9709, -15.5581, -16.2681)
)
