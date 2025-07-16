suivi_tech <- read_excel("data/PUDC_SUIVI.xlsx", sheet= "Execution technique  PTBA 2025")
suivi_tech <- as.data.frame(suivi_tech,row.names = T)
suivi_tech

# Configuration des volets (basée sur les données visibles dans vos images)
volets_config <- data.frame(
  id = c("Pistes rurales", "Hydraulique", "Santé", "Equipements post-récolte", 
         "Ingénierie sociale", "Communication", "Genre et inclusion sociale", 
         "PUDC-Academy", "Périmètre agricole communautaire", "Mise en œuvre du PCGES", 
         "SIG", "Renforcement de capacités des acteurs", "Suivi Evaluation et études socio-économiques",
         "Gestion et Coordination du PUDC", "Electrification rurale", "Education",
         "Chaîne de valeur agricole", "Chaîne de valeur laitière", 
         "Plateforme de Transformation Agroalimentaire", "Biodigesteurs", "Environment",
         "Electrification de villages", "Dorsale Electrique"),
  name = c("Pistes rurales", "Hydraulique", "Santé", "Equipements post-récolte", 
           "Ingénierie sociale", "Communication", "Genre et inclusion sociale", 
           "PUDC-Academy", "Périmètre agricole communautaire", "Mise en œuvre du PCGES", 
           "SIG", "Renforcement de capacités des acteurs", "Suivi Evaluation et études socio-économiques",
           "Gestion et Coordination du PUDC", "Electrification rurale", "Education",
           "Chaîne de valeur agricole", "Chaîne de valeur laitière", 
           "Plateforme de Transformation Agroalimentaire", "Biodigesteurs", "Environment",
           "Electrification de villages", "Dorsale Electrique"),
  color = c("#3B82F6", "#10B981", "#F59E0B", "#8B5CF6", "#EF4444", "#06B6D4", 
            "#F97316", "#84CC16", "#EC4899", "#6366F1", "#14B8A6", "#F43F5E", 
            "#8B5A2B", "#64748B", "#10B981", "#F59E0B", "#8B5CF6", "#EF4444",
            "#06B6D4", "#F97316", "#84CC16", "#EC4899", "#6366F1"),
  stringsAsFactors = FALSE
)

