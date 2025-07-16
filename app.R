###############################################################
#                                                             #
#  Titre : TP sur les nombres complexes                       #
#  Cours : R SHINY                                            #
#  Dernière mise à jour : 28/05/2025                          #
#  Auteur : Ndeye Salla TOURE & Gilbert OUMSAORE              #
#  Contact : (ndeyesallatoure0@gmail.com)                     #
#                                                             #
###############################################################

# ========================================
# INSTALLATION DES LIBRARY
# ========================================
install_if_missing <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}

# List of required packages
packages <- c(
  "shiny",
  "shinydashboard", 
  "shinyWidgets",
  "htmltools",
  "DT",
  "shinyjs",
  "plotly",
  "readxl",
  "dplyr",
  "shinycssloaders",
  "leaflet",
  "sf",
  "ggplot2",
  "scales",
  "tidyr",
  "httr",
  "jsonlite",
  "rsconnect"
)

# Install and load all packages
for (pkg in packages) {
  install_if_missing(pkg)
}

print("All packages have been checked and loaded successfully!")


# ========================================
# CHARGEMENT DES LIBRARY
# ========================================
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(htmltools)
library(DT)
library(shinyjs)
library(plotly)
library(readxl)
library(dplyr)
library(shinycssloaders)
library(leaflet)
library(sf)
library(ggplot2)
library(scales)
library(tidyr) 
library(httr)
library(jsonlite)
library(readxl)
library(rsconnect)
# ========================================
# CHARGEMENT DES FICHIERS R DE TRAITEMENT
# ========================================
source("modules/suivi_techinique.R")
source("modules/realisation.R")
source("modules/assistante_ia.R")

# ========================================
# INTERFACE UTILISATEUR (UI)
# ========================================
ui <- navbarPage(
  id = "main_navbar",
  title = div(
    img(src = "https://pudc.gouv.sn/local/cache-vignettes/L117xH90/pudc-logo-9ca22.png?1698834831", 
        height = "40px", 
        style = "margin-right: 10px; margin-top: -5px;"),
    "Tableau de bord PUDC"
  ),
  
  # CSS et JS externes
  useShinyjs(),
  tags$head(
    # Font Awesome
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"),
    
    # Google Fonts
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = ""),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap"),
    
    # Fichiers CSS personnalisés
    tags$link(rel = "stylesheet", type = "text/css", href = "all-styles.css"),
  ),
  
  
  tags$script(HTML("
      let currentTheme = 'light';
      let isDropdownOpen = false;
      
      const themes = {
        light: { icon: '☀️', label: 'Clair' },
        dark: { icon: '🌙', label: 'Sombre' },
        system: { icon: '💻', label: 'Système' }
      };
      
      function toggleDropdown() {
        isDropdownOpen = !isDropdownOpen;
        const button = document.querySelector('.dropdown-button');
        const menu = document.querySelector('.dropdown-menu');
        
        if (isDropdownOpen) {
          button.classList.add('open');
          menu.classList.add('show');
        } else {
          button.classList.remove('open');
          menu.classList.remove('show');
        }
      }
      
      function selectTheme(theme) {
        currentTheme = theme;
        
        // Mettre à jour l'affichage du bouton
        updateButtonDisplay();
        
        // Appliquer le thème
        applyTheme();
        
        // Mettre à jour les items sélectionnés
        updateSelectedItems();
        
        // Fermer le dropdown
        isDropdownOpen = false;
        document.querySelector('.dropdown-button').classList.remove('open');
        document.querySelector('.dropdown-menu').classList.remove('show');
      }
      
      function updateButtonDisplay() {
        const theme = themes[currentTheme];
        document.querySelector('.theme-icon').textContent = theme.icon;
        document.querySelector('.theme-label').textContent = theme.label;
      }
      
      function updateSelectedItems() {
        document.querySelectorAll('.dropdown-item').forEach(item => {
          item.classList.remove('selected');
        });
        document.querySelector(`[data-theme='${currentTheme}']`).classList.add('selected');
      }
      
      function applyTheme() {
        const body = document.body;
        const button = document.querySelector('.dropdown-button');
        const menu = document.querySelector('.dropdown-menu');
        const items = document.querySelectorAll('.dropdown-item');
        
        if (currentTheme === 'dark') {
          body.classList.add('dark-theme');
          button.classList.add('dark');
          menu.classList.add('dark');
          items.forEach(item => item.classList.add('dark'));
        } else if (currentTheme === 'light') {
          body.classList.remove('dark-theme');
          button.classList.remove('dark');
          menu.classList.remove('dark');
          items.forEach(item => item.classList.remove('dark'));
        } else { // system
          const prefersDark = window.matchMedia('(prefers-color-scheme: dark)').matches;
          body.classList.toggle('dark-theme', prefersDark);
          button.classList.toggle('dark', prefersDark);
          menu.classList.toggle('dark', prefersDark);
          items.forEach(item => item.classList.toggle('dark', prefersDark));
        }
      }
      
      // Fermer le dropdown si on clique ailleurs
      document.addEventListener('click', function(e) {
        if (!e.target.closest('.custom-dropdown')) {
          if (isDropdownOpen) {
            isDropdownOpen = false;
            document.querySelector('.dropdown-button').classList.remove('open');
            document.querySelector('.dropdown-menu').classList.remove('show');
          }
        }
      });
      
      // Écouter les changements de thème système
      window.matchMedia('(prefers-color-scheme: dark)').addEventListener('change', function() {
        if (currentTheme === 'system') {
          applyTheme();
        }
      });
      
      // Initialisation
      document.addEventListener('DOMContentLoaded', function() {
        updateButtonDisplay();
        updateSelectedItems();
        applyTheme();
      });
    ")),
  
  # Sélecteur de thème fixe en haut à droite
  div(class = "theme-selector-wrapper",
      div(class = "custom-dropdown",
          # Bouton principal
          tags$button(
            class = "dropdown-button",
            onclick = "toggleDropdown()",
            HTML('
          <span class="theme-icon">💻</span>
          <span class="theme-label">Système</span>
          <i class="fas fa-chevron-down dropdown-arrow"></i>
        ')
          ),
          
          # Menu dropdown
          div(class = "dropdown-menu",
              tags$button(
                class = "dropdown-item selected",
                `data-theme` = "light",
                onclick = "selectTheme('light')",
                HTML('
            <span class="item-icon">☀️</span>
            <span class="item-label">Clair</span>
            <i class="fas fa-check check-icon"></i>
          ')
              ),
              tags$button(
                class = "dropdown-item",
                `data-theme` = "dark", 
                onclick = "selectTheme('dark')",
                HTML('
            <span class="item-icon">🌙</span>
            <span class="item-label">Sombre</span>
            <i class="fas fa-check check-icon"></i>
          ')
              ),
              tags$button(
                class = "dropdown-item",
                `data-theme` = "system",
                onclick = "selectTheme('system')",
                HTML('
            <span class="item-icon">💻</span>
            <span class="item-label">Système</span>
            <i class="fas fa-check check-icon"></i>
          ')
              )
          )
      )
  ),
  
  # ========================================
  # UI ONGLET ACCEUIL
  # ========================================
  
  tabPanel("Acceuil",
           # Header
           div(class = "main-header",
               div(class = "header-content",
                   div(
                     h1(class = "header-title", "Tableau de Bord PTBA"),
                     p(class = "header-subtitle", "Programme d'Urgence de Développement Communautaire 2025")
                   ),
                   div(class = "year-badge", "Année: 2025")
               )
           ),
           
           # Main Content
           div(class = "main-content",
               # Carousel
               div(class = "carousel-container",
                   # Slides
                   div(class = "carousel-slide active", id = "slide-1",
                       tags$img(src = "https://pudc.gouv.sn/local/cache-gd2/90/5d88ed6703372fb8eddb3a0ca6f78e.jpg?1699465839", 
                                class = "carousel-image", alt = "Infrastructures"),
                       div(class = "carousel-overlay"),
                       div(class = "carousel-content",
                           h3(class = "carousel-title", "Infrastructures"),
                           p(class = "carousel-description", "Renforcement des infrastuctures")
                       )
                   ),
                   
                   div(class = "carousel-slide", id = "slide-2",
                       tags$img(src = "https://pudc.gouv.sn/local/cache-gd2/61/f3c885160da884b4805ee390f13dd0.jpg?1702906783", 
                                class = "carousel-image", alt = "Transformation Agricole"),
                       div(class = "carousel-overlay"),
                       div(class = "carousel-content",
                           h3(class = "carousel-title", "Transformation Agricole"),
                           p(class = "carousel-description", "Appui à la transformation agricole")
                       )
                   ),
                   
                   div(class = "carousel-slide", id = "slide-3",
                       tags$img(src = "https://pudc.gouv.sn/local/cache-gd2/a7/739a543d4e71fc82039e94dad5a490.jpg?1699465476", 
                                class = "carousel-image", alt = "Services Sociaux"),
                       div(class = "carousel-overlay"),
                       div(class = "carousel-content",
                           h3(class = "carousel-title", "Services Sociaux"),
                           p(class = "carousel-description", "Amélioration de l’accès aux services sociaux de base")
                       )
                   ),
                   
                   div(class = "carousel-slide", id = "slide-4",
                       tags$img(src = "https://pudc.gouv.sn/local/cache-gd2/aa/566f35bd153cd2b83887ffc3d0df2a.jpg?1699465480", 
                                class = "carousel-image", alt = "Environnement"),
                       div(class = "carousel-overlay"),
                       div(class = "carousel-content",
                           h3(class = "carousel-title", "Environnement"),
                           p(class = "carousel-description", "Environnement et économie verte")
                       )
                   ),
                   
                   div(class = "carousel-slide", id = "slide-5",
                       tags$img(src = "https://pudc.gouv.sn/local/cache-gd2/e2/113475ac4b26c08b018ed0ebafbd7c.jpg?1702987165", 
                                class = "carousel-image", alt = "Gestion Sociale"),
                       div(class = "carousel-overlay"),
                       div(class = "carousel-content",
                           h3(class = "carousel-title", "Gestion Sociale"),
                           p(class = "carousel-description", "Ingénierie sociale et gestion des connaissances")
                       )
                   ),
                   
                   # Navigation buttons
                   actionButton("prevSlide", "", class = "carousel-nav prev",
                                icon = icon("chevron-left")),
                   actionButton("nextSlide", "", class = "carousel-nav next",
                                icon = icon("chevron-right")),
                   
                   # Dots
                   div(class = "carousel-dots",
                       actionButton("dot1", "", class = "carousel-dot active"),
                       actionButton("dot2", "", class = "carousel-dot"),
                       actionButton("dot3", "", class = "carousel-dot"),
                       actionButton("dot4", "", class = "carousel-dot"),
                       actionButton("dot5", "", class = "carousel-dot")
                   )
               ),
               
               div(class = "nav-grid",
                   div(class = "nav-card",
                       id = "nav-suivi-technique",
                       div(class = "nav-icon", icon("tools")),
                       h3(class = "nav-title", "Suivi Technique"),
                       p(class = "nav-description", "Accéder aux données et analyses détaillées"),
                       div(class = "nav-link", 
                           span("Voir plus"),
                           icon("arrow-right")
                       )
                   ),
                   
                   div(class = "nav-card",
                       id = "nav-suivi-financier",
                       div(class = "nav-icon", icon("chart-line")),
                       h3(class = "nav-title", "Suivi Financier"),
                       p(class = "nav-description", "Accéder aux données et analyses détaillées"),
                       div(class = "nav-link", 
                           span("Voir plus"),
                           icon("arrow-right")
                       )
                   ),
                   
                   div(class = "nav-card",
                       id = "nav-performance-marches",
                       div(class = "nav-icon", icon("handshake")),
                       h3(class = "nav-title", "Performance Passation Marchés"),
                       p(class = "nav-description", "Accéder aux données et analyses détaillées"),
                       div(class = "nav-link", 
                           span("Voir plus"),
                           icon("arrow-right")
                       )
                   ),
                   
                   div(class = "nav-card",
                       id = "nav-realisation-globale",
                       div(class = "nav-icon", icon("globe")),
                       h3(class = "nav-title", "Réalisation Globale"),
                       p(class = "nav-description", "Accéder aux données et analyses détaillées"),
                       div(class = "nav-link", 
                           span("Voir plus"),
                           icon("arrow-right")
                       )
                   ),
                   
                   div(class = "nav-card",
                       id = "nav-resume",
                       div(class = "nav-icon", icon("file-alt")),
                       h3(class = "nav-title", "Résumé"),
                       p(class = "nav-description", "Accéder aux données et analyses détaillées"),
                       div(class = "nav-link", 
                           span("Voir plus"),
                           icon("arrow-right")
                       )
                   ),
                   
                   div(class = "nav-card",
                       id = "nav-assistant-ia",
                       div(class = "nav-icon", icon("robot")),
                       h3(class = "nav-title", "Assistante IA"),
                       p(class = "nav-description", "Accéder aux données et analyses détaillées"),
                       div(class = "nav-link", 
                           span("Voir plus"),
                           icon("arrow-right")
                       )
                   )
               ),
               
               # Overview Section
               div(class = "overview-section",
                   h2(class = "overview-title", "Aperçu du Programme"),
                   div(class = "overview-grid",
                       div(class = "overview-item",
                           div(class = "overview-icon infrastructure",
                               icon("road")
                           ),
                           h3(class = "overview-item-title", "Infrastructure"),
                           p(class = "overview-item-description", "Développement des infrastructures")
                       ),
                       
                       div(class = "overview-item",
                           div(class = "overview-icon energy",
                               icon("bolt")
                           ),
                           h3(class = "overview-item-title", "Énergie"),
                           p(class = "overview-item-description", "Électrification des zones rurales")
                       ),
                       
                       div(class = "overview-item",
                           div(class = "overview-icon health",
                               icon("heartbeat")
                           ),
                           h3(class = "overview-item-title", "Santé"),
                           p(class = "overview-item-description", "Construction de postes de santé")
                       )
                   )
               )
           ),
           
           # JavaScript pour le carousel
           tags$script(HTML("
    let currentSlide = 0;
    const slides = document.querySelectorAll('.carousel-slide');
    const dots = document.querySelectorAll('.carousel-dot');
    
    function showSlide(index) {
      slides.forEach((slide, i) => {
        slide.classList.toggle('active', i === index);
      });
      dots.forEach((dot, i) => {
        dot.classList.toggle('active', i === index);
      });
      currentSlide = index;
    }
    
    function nextSlide() {
      const next = (currentSlide + 1) % slides.length;
      showSlide(next);
    }
    
    function prevSlide() {
      const prev = (currentSlide - 1 + slides.length) % slides.length;
      showSlide(prev);
    }
    
    // Auto-slide
    setInterval(nextSlide, 3000);
    
    // Navigation event listeners
    document.getElementById('nextSlide').addEventListener('click', nextSlide);
    document.getElementById('prevSlide').addEventListener('click', prevSlide);
    
    // Dots event listeners
    dots.forEach((dot, index) => {
      dot.addEventListener('click', () => showSlide(index));
    });
  "))
  ),
  # ========================================
  # UI ONGLET SUIVI TECHNIQUE
  # ========================================
  tabPanel("Suivi Technique",
           tabsetPanel(
             # Sous-onglet : Visualisation (code original)
             tabPanel("Visualisation",
                      fluidRow(
                        div(class = "page-content",
                            style = "background-color: #f0f8ff; padding: 20px; border-radius: 8px;",
                            
                            h2("Suivi Technique - Visualisation"),
                            
                            hr(),
                            sidebarLayout(
                              sidebarPanel(
                                uiOutput("choix_financement"),
                                uiOutput("choix_variable"),
                                uiOutput("choix_volet")
                              ),
                              mainPanel(
                                plotlyOutput("graph_suivi_tech")
                              )
                            )
                        )
                      ),
                      # Section des cartes de résumé
                      fluidRow(
                        box(
                          title = "Résumé par Financement", 
                          status = "warning", solidHeader = TRUE,
                          width = 12,
                          withSpinner(uiOutput("summary_cards"))
                        )
                      )
             ),
             
             # Sous-onglet : Comparaison Barres
             tabPanel("Comparaison par Barres",
                      div(class = "page-content",
                          style = "background-color: #ffffff; padding: 20px; border-radius: 8px;",
                          
                          h2("Suivi Technique - Comparaison Objectifs vs Réalisations"),
                          # Section des filtres
                          fluidRow(
                            box(
                              title = "Filtres", status = "primary", solidHeader = TRUE,
                              width = 12, height = 120,
                              fluidRow(
                                column(3,
                                       selectInput("selectedFinancement", "Financement:",
                                                   choices = c("Tous les financements" = "all"),
                                                   selected = "all"
                                       )
                                ),
                                column(3,
                                       selectInput("selectedVolet", "Volet:",
                                                   choices = c("Tous les volets" = "all"),
                                                   selected = "all"
                                       )
                                ),
                                column(3,
                                       selectInput("selectedTrimestre", "Trimestre:",
                                                   choices = c("Trimestre 1" = "T1",
                                                               "Trimestre 2" = "T2", 
                                                               "Trimestre 3" = "T3",
                                                               "Trimestre 4" = "T4"),
                                                   selected = "T1"
                                       )
                                )
                              )
                            )
                          ),
                          
                          # Section des indicateurs de performance
                          fluidRow(
                            box(
                              title = "Indicateurs de Performance", 
                              status = "success", solidHeader = TRUE,
                              width = 12,
                              withSpinner(uiOutput("performance_indicators"))
                            )
                          ),
                          
                          # Section du graphique principal
                          fluidRow(
                            box(
                              title = "Graphique de Comparaison par Volet", 
                              status = "primary", solidHeader = TRUE,
                              width = 8,
                              withSpinner(uiOutput("comparison_chart"))
                            ),
                            box(
                              title = "Légende et Informations", 
                              status = "info", solidHeader = TRUE,
                              width = 4,
                              div(style = "padding: 10px;",
                                  h5("Code couleurs:", style = "margin-bottom: 15px;"),
                                  div(style = "margin-bottom: 10px;",
                                      span(style = "display: inline-block; width: 20px; height: 20px; background-color: #10B981; border-radius: 3px; margin-right: 8px; vertical-align: middle;"),
                                      span("≥ 100% - Objectif atteint", style = "color: #10B981; font-weight: 500;")
                                  ),
                                  div(style = "margin-bottom: 10px;",
                                      span(style = "display: inline-block; width: 20px; height: 20px; background-color: #F59E0B; border-radius: 3px; margin-right: 8px; vertical-align: middle;"),
                                      span("75-99% - En cours", style = "color: #F59E0B; font-weight: 500;")
                                  ),
                                  div(style = "margin-bottom: 15px;",
                                      span(style = "display: inline-block; width: 20px; height: 20px; background-color: #EF4444; border-radius: 3px; margin-right: 8px; vertical-align: middle;"),
                                      span("< 75% - En retard", style = "color: #EF4444; font-weight: 500;")
                                  )
                              )
                            )
                          )
                      )
             ),
             
             tabPanel("Tableau des Données",
                      box(status = "info", solidHeader = TRUE,
                          h2("Suivi Technique - Données Détaillées"),
                          width = 12,
                          collapsible = TRUE, collapsed = TRUE,
                          div(style = "margin-top: 10px;",
                              withSpinner(DTOutput("detailed_table"))
                          )
                      )
             )
           )
  ),
  
  # ========================================
  # UI ONGLET SUIVI FINANCIER
  # ========================================
  
  tabPanel(
    "Suivi Financier",
    
    tags$head(
      tags$style(HTML("
      .suivi-financier-bg {
        background-image: url('https://images.unsplash.com/photo-1565372912237-77fc7f2f70fd?ixlib=rb-4.0.3&auto=format&fit=crop&w=1470&q=80');
        background-size: cover;
        background-position: center;
        background-repeat: no-repeat;
        padding: 20px;
        border-radius: 15px;
        position: relative;
        z-index: 1;
      }
      .suivi-financier-bg h4,
      .suivi-financier-bg .box,
      .suivi-financier-bg .loading-message {
        background-color: rgba(255, 255, 255, 0.85);
        padding: 10px;
        border-radius: 10px;
      }
    "))
    ),
    
    div(
      class = "suivi-financier-bg",
      fluidRow(
        box(
          width = 12,
          title = "Suivi Financier",
          status = "primary",
          solidHeader = TRUE,
          
          fluidRow(
            column(
              6,
              h4("Répartition du Budget par Projet"),
              
              # Les deux listes déroulantes
              uiOutput("ui_var_repartition_x"),
              uiOutput("ui_var_repartition_y"),
              
              conditionalPanel(
                condition = "output.plot_repartition_loaded == false",
                tags$div(class = "loading-message", "Chargement du graphique...")
              ),
              plotlyOutput("plot_repartition")
            ),
            
            column(
              6,
              h4("Exécution Budgétaire"),
              uiOutput("ui_var_execution"),
              conditionalPanel(
                condition = "output.plot_execution_loaded == false",
                tags$div(class = "loading-message", "Chargement du graphique...")
              ),
              plotlyOutput("plot_execution")
            )
          ),
          
          fluidRow(
            column(
              12,
              h4("Décaissements Globaux 2025"),
              uiOutput("ui_var_decaissement"),
              conditionalPanel(
                condition = "output.plot_decaissement_loaded == false",
                tags$div(class = "loading-message", "Chargement du graphique...")
              ),
              plotlyOutput("plot_decaissement")
            )
          )
        )
      )
    )
  ),
  
  
  # ========================================
  # UI ONGLET PERFORMANCE
  # ========================================
  
  tabPanel("Performance Marchés",
           fluidRow(
             column(4, uiOutput("ui_var_physique")),
             column(8,
                    div(style="background-color: rgba(255,255,255,0.5); padding: 10px; border-radius: 10px;",
                        plotlyOutput("plot_physique")))
           ),
           fluidRow(
             column(4, uiOutput("ui_var_budget")),
             column(8,
                    div(style="background-color: rgba(255,255,255,0.5); padding: 10px; border-radius: 10px;",
                        plotlyOutput("plot_budget")))
           ),
           fluidRow(
             column(4, uiOutput("ui_var_passation")),
             column(8,
                    div(style="background-color: rgba(255,255,255,0.5); padding: 10px; border-radius: 10px;",
                        plotlyOutput("plot_passation")))
           )
  ),
  
  
  # ========================================
  # UI ONGLET REALISATION
  # ========================================
  tabPanel("Réalisation Globale",
           # En-tête avec sélecteurs
           # Titre principal
           fluidRow(
             column(12,
                    h1("Réalisation par Volet PTBA 2025", style = "color: #333; margin-bottom: 10px;")
             )
           ),
           br()
           ,
           # Première ligne de filtres
           fluidRow(
             column(6,
                    selectInput("selected_volet", "Sélectionner un volet:",
                                choices = setNames(volets_config$id, volets_config$name),
                                selected = "education")
             ),
             column(6,radioButtons("type_realisation", "Type de réalisation:",
                                   choices = list("Globale" = "globale", "Annuelle" = "annuelle"),
                                   selected = "globale",
                                   inline = TRUE)
                    
             )
           ),
           
           br(),
           
           # Statistiques globales adaptées au volet sélectionné
           fluidRow(
             column(3,
                    div(class = "stat-card",
                        icon("target", style = "color: #3B82F6; font-size: 2rem;"),
                        div(class = "stat-value", textOutput("total_cible")),
                        div(class = "stat-label", textOutput("label_cible"))
                    )
             ),
             column(3,
                    div(class = "stat-card",
                        icon("chart-line", style = "color: #10B981; font-size: 2rem;"),
                        div(class = "stat-value", textOutput("total_realisation")),
                        div(class = "stat-label", textOutput("label_realisation"))
                    )
             ),
             column(3,
                    div(class = "stat-card",
                        icon("percentage", style = "color: #8B5CF6; font-size: 2rem;"),
                        div(class = "stat-value", textOutput("taux_global")),
                        div(class = "stat-label", "Taux Global")
                    )
             ),
             column(3,
                    div(class = "stat-card",
                        icon("map-marker-alt", style = "color: #F59E0B; font-size: 2rem;"),
                        div(class = "stat-value", textOutput("nb_regions")),
                        div(class = "stat-label", "Nombre de Régions")
                    )
             )
           ),
           
           br(),
           
           # Contenu principal avec carte et graphique
           fluidRow(
             # Carte interactive adaptée
             column(6,
                    div(class = "upload-area",
                        h3(icon("map"), " Carte Interactive du Sénégal"),
                        withSpinner(leafletOutput("map_regions", height = "500px")),
                        br(),
                        div(style = "background: #f8f9fa; padding: 15px; border-radius: 8px;",
                            h4("Légende Performance"),
                            div(style = "display: flex; flex-wrap: wrap; gap: 15px;",
                                div(style = "display: flex; align-items: center;",
                                    div(style = "width: 16px; height: 16px; background: #10B981; border-radius: 50%; margin-right: 8px;"),
                                    span("Excellent (≥80%)")
                                ),
                                div(style = "display: flex; align-items: center;",
                                    div(style = "width: 16px; height: 16px; background: #F59E0B; border-radius: 50%; margin-right: 8px;"),
                                    span("Satisfaisant (50-79%)")
                                ),
                                div(style = "display: flex; align-items: center;",
                                    div(style = "width: 16px; height: 16px; background: #EF4444; border-radius: 50%; margin-right: 8px;"),
                                    span("À améliorer (<50%)")
                                )
                            )
                        )
                    )
             ),
             
             # Graphique de performance
             column(6,
                    div(class = "upload-area",
                        h3(icon("chart-bar"), " Graphique de Performance"),
                        withSpinner(plotlyOutput("chart_performance", height = "500px")),
                        br(),
                        div(style = "text-align: center;",
                            p("Comparaison entre les cibles et réalisations par région", 
                              style = "color: #666; font-size: 0.9rem;")
                        )
                    )
             )
           ),
           
           br(),
           
           # Tableau détaillé
           fluidRow(
             column(12,
                    div(class = "upload-area",
                        h3(icon("table"), " Tableau Détaillé par Région"),
                        withSpinner(DT::dataTableOutput("table_details")),
                        br(),
                        div(style = "background: #f8f9fa; padding: 10px; border-radius: 8px;",
                            p("Tableau interactif avec possibilité d'exporter les données (CSV, Excel, PDF)",
                              style = "color: #666; font-size: 0.9rem; margin: 0;")
                        )
                    )
             )
           )
  ),
  
  
  # ========================================
  # UI ONGLET RESUME
  # ========================================
  tabPanel("Résumé",
           
           # Ajouter le JavaScript pour les barres de progression
           tags$head(
             tags$script(HTML("
             Shiny.addCustomMessageHandler('updateProgressBar', function(message) {
               var element = document.getElementById(message.id);
               if (element) {
                 element.style.width = message.width;
               }
             });
           "))
           ),
           
           fluidPage(    
             # Sous-onglets 
             tabsetPanel(
               br(),
               # Premier sous-onglet : Vue d'ensemble des performances
               tabPanel("Vue d'ensemble",
                        value = "overview",
                        
                        # Filtres avec design amélioré
                        fluidRow(
                          column(width = 12,
                                 div(
                                   title = "Filtres et Contrôles", 
                                   status = "primary", 
                                   solidHeader = TRUE, 
                                   width = 12,
                                   height = 100,
                                   background = "light-blue",
                                   fluidRow(
                                     column(4,
                                            div(class = "form-group",
                                                selectInput("selectedProject_overview", "Projet:",
                                                            choices = c("Tous" = "all"),
                                                            selected = "all",
                                                            width = "100%")
                                            )
                                     ),
                                     column(4,
                                            div(class = "form-group",
                                                selectInput("selectedTrimester_overview", "Trimestre:",
                                                            choices = list("Tous" = "all", "Q1" = "1", "Q2" = "2", "Q3" = "3", "Q4" = "4"),
                                                            selected = "all",
                                                            width = "100%")
                                            )
                                     ),
                                     column(4,
                                            div(class = "form-group",
                                                selectInput("selectedIndicator_overview", "Indicateur:",
                                                            choices = list("Tous" = "all", "TECHNIQUE" = "TECHNIQUE", 
                                                                           "BUDGETAIRE" = "BUDGETAIRE", "PPM" = "PPM"),
                                                            selected = "all",
                                                            width = "100%")
                                            )
                                     )
                                   )
                                 )
                          )
                        ),
                        
                        # Section des KPIs principaux
                        fluidRow(
                          column(width = 12,
                                 h3("Indicateurs Clés de Performance", class = "text-center", 
                                    style = "color: #34495e; margin-bottom: 30px; font-weight: 300;")
                          )
                        ),
                        
                        fluidRow(
                          # Indicateurs Techniques
                          column(width = 4,
                                 box(
                                   title = "Performance Technique", 
                                   status = "primary", 
                                   solidHeader = TRUE, 
                                   width = NULL,
                                   height = "380px",
                                   icon = icon("cogs"),
                                   background = "light-blue",
                                   div(class = "performance-box", style = "padding: 15px;",
                                       div(class = "metric-row", style = "display: flex; justify-content: space-between; margin-bottom: 10px;",
                                           div(class = "metric-label", style = "font-weight: bold; color: #34495e;", "Objectif Moyen"),
                                           div(class = "metric-value", style = "color: navy; font-size: 18px; font-weight: bold;", 
                                               textOutput("tech_objectif_moyen", inline = TRUE))
                                       ),
                                       hr(style = "margin: 10px 0; border-color: #bdc3c7;"),
                                       div(class = "metric-row", style = "display: flex; justify-content: space-between; margin-bottom: 10px;",
                                           div(class = "metric-label", style = "font-weight: bold; color: #34495e;", "Réalisation Moyenne"),
                                           div(class = "metric-value", style = "color: navy; font-size: 18px; font-weight: bold;",
                                               textOutput("tech_realisation_moyenne", inline = TRUE))
                                       ),
                                       hr(style = "margin: 10px 0; border-color: #bdc3c7;"),
                                       div(class = "metric-row", style = "display: flex; justify-content: space-between; margin-bottom: 15px;",
                                           div(class = "metric-label", style = "font-weight: bold; color: #34495e;", "Écart Moyen"),
                                           div(class = "metric-value", style = "color: navy; font-size: 18px; font-weight: bold;",
                                               textOutput("tech_ecart_moyen", inline = TRUE))
                                       ),
                                       div(class = "progress-section", style = "margin-top: 20px;",
                                           div(class = "progress-label", style = "text-align: center; margin-bottom: 10px; font-weight: bold; color: #34495e;", "Taux de Réussite"),
                                           div(class = "progress", style = "height: 25px; margin: 10px 0; background-color: #ecf0f1; border-radius: 12px; overflow: hidden;",
                                               div(class = "progress-bar", id = "tech_progress_bar",
                                                   style = "background-color: navy; height: 100%; border-radius: 12px; transition: width 0.5s ease; width: 0%;")
                                           ),
                                           div(class = "progress-badge", style = "text-align: center; margin-top: 10px;",
                                               span(class = "badge", style = "background-color: navy; color: white; padding: 8px 16px; border-radius: 20px; font-size: 14px;", 
                                                    textOutput("tech_taux_reussite", inline = TRUE))
                                           )
                                       )
                                   )
                                 )
                          ),
                          
                          # Indicateurs Budgétaires
                          column(width = 4,
                                 box(
                                   title = "Performance Budgétaire", 
                                   status = "success", 
                                   solidHeader = TRUE, 
                                   width = NULL,
                                   height = "380px",
                                   icon = icon("chart-line"),
                                   background = "light-blue",
                                   div(class = "performance-box", style = "padding: 15px;",
                                       div(class = "metric-row", style = "display: flex; justify-content: space-between; margin-bottom: 10px;",
                                           div(class = "metric-label", style = "font-weight: bold; color: #34495e;", "Objectif Moyen"),
                                           div(class = "metric-value", style = "color: darkgreen; font-size: 18px; font-weight: bold;",
                                               textOutput("budget_objectif_moyen", inline = TRUE))
                                       ),
                                       hr(style = "margin: 10px 0; border-color: #bdc3c7;"),
                                       div(class = "metric-row", style = "display: flex; justify-content: space-between; margin-bottom: 10px;",
                                           div(class = "metric-label", style = "font-weight: bold; color: #34495e;", "Réalisation Moyenne"),
                                           div(class = "metric-value", style = "color: darkgreen; font-size: 18px; font-weight: bold;",
                                               textOutput("budget_realisation_moyenne", inline = TRUE))
                                       ),
                                       hr(style = "margin: 10px 0; border-color: #bdc3c7;"),
                                       div(class = "metric-row", style = "display: flex; justify-content: space-between; margin-bottom: 15px;",
                                           div(class = "metric-label", style = "font-weight: bold; color: #34495e;", "Écart Moyen"),
                                           div(class = "metric-value", style = "color: darkgreen; font-size: 18px; font-weight: bold;",
                                               textOutput("budget_ecart_moyen", inline = TRUE))
                                       ),
                                       div(class = "progress-section", style = "margin-top: 20px;",
                                           div(class = "progress-label", style = "text-align: center; margin-bottom: 10px; font-weight: bold; color: #34495e;", "Taux de Réussite"),
                                           div(class = "progress", style = "height: 25px; margin: 10px 0; background-color: #ecf0f1; border-radius: 12px; overflow: hidden;",
                                               div(class = "progress-bar", id = "budget_progress_bar",
                                                   style = "background-color: darkgreen; height: 100%; border-radius: 12px; transition: width 0.5s ease; width: 0%;")
                                           ),
                                           div(class = "progress-badge", style = "text-align: center; margin-top: 10px;",
                                               span(class = "badge", style = "background-color: darkgreen; color: white; padding: 8px 16px; border-radius: 20px; font-size: 14px;", 
                                                    textOutput("budget_taux_reussite", inline = TRUE))
                                           )
                                       )
                                   )
                                 )
                          ),
                          
                          # Performance PPM
                          column(width = 4,
                                 box(
                                   title = "Performance PPM", 
                                   status = "warning", 
                                   solidHeader = TRUE, 
                                   width = NULL,
                                   height = "380px",
                                   icon = icon("handshake"),
                                   background = "yellow",
                                   div(class = "performance-box", style = "padding: 15px;",
                                       div(class = "metric-row", style = "display: flex; justify-content: space-between; margin-bottom: 10px;",
                                           div(class = "metric-label", style = "font-weight: bold; color: #34495e;", "Objectif Moyen"),
                                           div(class = "metric-value", style = "color: #f39c12; font-size: 18px; font-weight: bold;",
                                               textOutput("ppm_objectif_moyen", inline = TRUE))
                                       ),
                                       hr(style = "margin: 10px 0; border-color: #bdc3c7;"),
                                       div(class = "metric-row", style = "display: flex; justify-content: space-between; margin-bottom: 10px;",
                                           div(class = "metric-label", style = "font-weight: bold; color: #34495e;", "Réalisation Moyenne"),
                                           div(class = "metric-value", style = "color: #f39c12; font-size: 18px; font-weight: bold;",
                                               textOutput("ppm_realisation_moyenne", inline = TRUE))
                                       ),
                                       hr(style = "margin: 10px 0; border-color: #bdc3c7;"),
                                       div(class = "metric-row", style = "display: flex; justify-content: space-between; margin-bottom: 15px;",
                                           div(class = "metric-label", style = "font-weight: bold; color: #34495e;", "Écart Moyen"),
                                           div(class = "metric-value", style = "color: #f39c12; font-size: 18px; font-weight: bold;",
                                               textOutput("ppm_ecart_moyen", inline = TRUE))
                                       ),
                                       div(class = "progress-section", style = "margin-top: 20px;",
                                           div(class = "progress-label", style = "text-align: center; margin-bottom: 10px; font-weight: bold; color: #34495e;", "Taux de Réussite"),
                                           div(class = "progress", style = "height: 25px; margin: 10px 0; background-color: #ecf0f1; border-radius: 12px; overflow: hidden;",
                                               div(class = "progress-bar", id = "ppm_progress_bar",
                                                   style = "background-color: #f39c12; height: 100%; border-radius: 12px; transition: width 0.5s ease; width: 0%;")
                                           ),
                                           div(class = "progress-badge", style = "text-align: center; margin-top: 10px;",
                                               span(class = "badge", style = "background-color: #f39c12; color: white; padding: 8px 16px; border-radius: 20px; font-size: 14px;", 
                                                    textOutput("ppm_taux_reussite", inline = TRUE))
                                           )
                                       )
                                   )
                                 )
                          )
                        ),
                        
                        # Espace entre les sections
                        div(style = "margin: 30px 0;"),
                        
                        # Section Performance Globale
                        fluidRow(
                          column(width = 12,
                                 box(
                                   title = "Tableau de Bord Global", 
                                   status = "info", 
                                   solidHeader = TRUE, 
                                   width = NULL,
                                   height = "200px",
                                   icon = icon("bullseye"),
                                   background = "aqua",
                                   div(class = "global-performance",
                                       fluidRow(
                                         column(4, 
                                                div(class = "global-metric-card",
                                                    div(class = "global-metric-icon", 
                                                        icon("cogs", class = "fa-2x"),
                                                        style = "color: navy;"),
                                                    div(class = "global-metric-value", 
                                                        style = "color: #2c3e50; font-size: 24px; font-weight: bold;", 
                                                        textOutput("global_technique", inline = TRUE)),
                                                    div(class = "global-metric-label", 
                                                        style = "color: #7f8c8d; font-size: 12px;",
                                                        "Performance Technique")
                                                )
                                         ),
                                         column(4,
                                                div(class = "global-metric-card",
                                                    div(class = "global-metric-icon", 
                                                        icon("chart-line", class = "fa-2x"),
                                                        style = "color: darkgreen;"),
                                                    div(class = "global-metric-value", 
                                                        style = "color: #2c3e50; font-size: 24px; font-weight: bold;", 
                                                        textOutput("global_budgetaire", inline = TRUE)),
                                                    div(class = "global-metric-label", 
                                                        style = "color: #7f8c8d; font-size: 12px;",
                                                        "Performance Budgétaire")
                                                )
                                         ),
                                         column(4,
                                                div(class = "global-metric-card",
                                                    div(class = "global-metric-icon", 
                                                        icon("handshake", class = "fa-2x"),
                                                        style = "color: #f39c12;"),
                                                    div(class = "global-metric-value", 
                                                        style = "color: #2c3e50; font-size: 24px; font-weight: bold;", 
                                                        textOutput("global_ppm", inline = TRUE)),
                                                    div(class = "global-metric-label", 
                                                        style = "color: #7f8c8d; font-size: 12px;",
                                                        "Performance PPM")
                                                )
                                         )
                                       )
                                   )
                                 )
                          )
                        )
               ),
               
               # Deuxième sous-onglet : Analyses et Tendances
               tabPanel("Analyses & Tendances",
                        value = "analytics",
                        
                        # Filtres avec design amélioré
                        fluidRow(
                          column(width = 12,
                                 div(
                                   title = "Filtres et Contrôles", 
                                   status = "primary", 
                                   solidHeader = TRUE, 
                                   width = 12,
                                   height = 100,
                                   background = "light-blue",
                                   fluidRow(
                                     column(4,
                                            div(class = "form-group",
                                                selectInput("selectedProject_analytics", "Projet:",
                                                            choices = c("Tous" = "all"),
                                                            selected = "all",
                                                            width = "100%")
                                            )
                                     ),
                                     column(4,
                                            div(class = "form-group",
                                                selectInput("selectedTrimester_analytics", "Trimestre:",
                                                            choices = list("Tous" = "all", "Q1" = "1", "Q2" = "2", "Q3" = "3", "Q4" = "4"),
                                                            selected = "all",
                                                            width = "100%")
                                            )
                                     ),
                                     column(4,
                                            div(class = "form-group",
                                                selectInput("selectedIndicator_analytics", "Indicateur:",
                                                            choices = list("Tous" = "all", "TECHNIQUE" = "TECHNIQUE", 
                                                                           "BUDGETAIRE" = "BUDGETAIRE", "PPM" = "PPM"),
                                                            selected = "all",
                                                            width = "100%")
                                            )
                                     )
                                   )
                                 )
                          )
                        ),
                        
                        # Section des graphiques
                        fluidRow(
                          column(width = 12,
                                 h3("Analyses et Tendances", class = "text-center", 
                                    style = "color: #34495e; margin-bottom: 30px; font-weight: 300;")
                          )
                        ),
                        
                        fluidRow(
                          # Graphique d'évolution
                          column(width = 12,
                                 box(
                                   title = "Évolution des Indicateurs par Trimestre", 
                                   status = "info", 
                                   solidHeader = TRUE, 
                                   width = NULL,
                                   height = "500px",
                                   icon = icon("chart-area"),
                                   collapsible = TRUE,
                                   withSpinner(plotlyOutput("trendChart", height = "400px"),
                                               type = 4, color = "#3498db")
                                 )
                          )
                        ),
                        
                        # Espace entre les graphiques
                        div(style = "margin: 20px 0;"),
                        
                        fluidRow(
                          # Graphique comparatif
                          column(width = 12,
                                 box(
                                   title = "Comparaison Objectifs vs Réalisations", 
                                   status = "primary", 
                                   solidHeader = TRUE, 
                                   width = NULL,
                                   height = "900px",
                                   icon = icon("balance-scale"),
                                   collapsible = TRUE,
                                   withSpinner(plotlyOutput("comparisonChart", height = "800px"),
                                               type = 4, color = "#2980b9")
                                 )
                          )
                        )
               ),
               
               # Troisième sous-onglet : Détails par Projet
               tabPanel("Détails Projets",
                        value = "projects",
                        
                        # Filtres pour le tableau
                        fluidRow(
                          column(width = 12,
                                 div(
                                   title = "Filtres et Contrôles", 
                                   status = "primary", 
                                   solidHeader = TRUE, 
                                   width = 12,
                                   height = 100,
                                   background = "light-blue",
                                   fluidRow(
                                     column(4,
                                            div(class = "form-group",
                                                selectInput("selectedProject_table", "Projet:",
                                                            choices = c("Tous" = "all"),
                                                            selected = "all",
                                                            width = "100%")
                                            )
                                     ),
                                     column(4,
                                            div(class = "form-group",
                                                selectInput("selectedTrimester_table", "Trimestre:",
                                                            choices = list("Tous" = "all", "Q1" = "1", "Q2" = "2", "Q3" = "3", "Q4" = "4"),
                                                            selected = "all",
                                                            width = "100%")
                                            )
                                     ),
                                     column(4,
                                            div(class = "form-group",
                                                selectInput("selectedIndicator_table", "Indicateur:",
                                                            choices = list("Tous" = "all", "TECHNIQUE" = "TECHNIQUE", 
                                                                           "BUDGETAIRE" = "BUDGETAIRE", "PPM" = "PPM"),
                                                            selected = "all",
                                                            width = "100%")
                                            )
                                     )
                                   )
                                 )
                          )
                        ),
                        
                        # Section du tableau
                        fluidRow(
                          column(width = 12,
                                 h3("Détails par Projet", class = "text-center", 
                                    style = "color: #34495e; margin-bottom: 30px; font-weight: 300;")
                          )
                        ),
                        
                        fluidRow(
                          # Tableau des projets
                          column(width = 12,
                                 box(
                                   title = "Tableau Détaillé des Projets", 
                                   status = "danger", 
                                   solidHeader = TRUE, 
                                   width = NULL,
                                   icon = icon("table"),
                                   collapsible = TRUE,
                                   footer = "Cliquez sur les colonnes pour trier. Utilisez la barre de recherche pour filtrer.",
                                   withSpinner(DT::dataTableOutput("projectTable"),
                                               type = 4, color = "#e74c3c")
                                 )
                          )
                        )
               )
             )
           )
  ),
  
  
  
  # ========================================
  # UI ONGLET ASSISTANT IA
  # ========================================
  
  tabPanel("Assistante IA", assistant_ia_ui()
  )
  
  
  
)


server <- function(input, output, session) {
  
  # ========================================
  # SERVER Onglet Acceuil
  # ========================================
  # Navigation vers les différents onglets
  # Navigation avec les noms EXACTS de la navbar
  shinyjs::onclick("nav-suivi-technique", {
    updateNavbarPage(session, "main_navbar", selected = "Suivi Technique")
  })
  
  shinyjs::onclick("nav-suivi-financier", {
    updateNavbarPage(session, "main_navbar", selected = "Suivi Financier")
  })
  
  shinyjs::onclick("nav-performance-marches", {
    updateNavbarPage(session, "main_navbar", selected = "Performance Marchés")
  })
  
  shinyjs::onclick("nav-realisation-globale", {
    updateNavbarPage(session, "main_navbar", selected = "Réalisation Globale")
  })
  
  shinyjs::onclick("nav-resume", {
    updateNavbarPage(session, "main_navbar", selected = "Résumé")
  })
  
  shinyjs::onclick("nav-assistant-ia", {
    updateNavbarPage(session, "main_navbar", selected = "Assistante IA")
  })
  
  # Gestion des événements du carousel
  observeEvent(input$nextSlide, {
    runjs("nextSlide();")
  })
  
  observeEvent(input$prevSlide, {
    runjs("prevSlide();")
  })
  
  observeEvent(input$dot1, {
    runjs("showSlide(0);")
  })
  
  observeEvent(input$dot2, {
    runjs("showSlide(1);")
  })
  
  observeEvent(input$dot3, {
    runjs("showSlide(2);")
  })
  
  observeEvent(input$dot4, {
    runjs("showSlide(3);")
  })
  
  observeEvent(input$dot5, {
    runjs("showSlide(4);")
  })
  
  # ========================================
  #  SERVER Onglet Suivi Technique
  # ========================================
  # Charger les données (avec gestion d'erreur)
  df <- reactive({
    tryCatch({
      read_excel("data/PUDC_SUIVI.xlsx", sheet = "Execution technique  PTBA 2025") %>%
        mutate(across(where(is.character), ~ trimws(.)))
    }, error = function(e) {
      # Si le fichier n'existe pas, utiliser des données d'exemple
      sample_data
    })
  })
  # UI dynamique pour filtrage
  output$choix_financement <- renderUI({
    df_data <- df()
    if("Financement" %in% names(df_data)) {
      selectInput("financement", "Choisir le financement :",
                  choices = unique(df_data$Financement),
                  selected = unique(df_data$Financement)[1])
    } else {
      selectInput("financement", "Choisir le financement :",
                  choices = c("Financement 1", "Financement 2"),
                  selected = "Financement 1")
    }
  })
  
  output$choix_variable <- renderUI({
    df_data <- df()
    var_choices <- names(df_data)[grepl("Objectif|Réalisation", names(df_data))]
    if(length(var_choices) == 0) {
      var_choices <- c("objectif", "realisation")
    }
    selectInput("variable_y", "Choisir la variable à afficher :",
                choices = var_choices,
                selected = var_choices[1])
  })
  
  output$choix_volet <- renderUI({
    req(input$financement)
    df_data <- df()
    if("Volet" %in% names(df_data) && "Financement" %in% names(df_data)) {
      volets <- df_data %>%
        filter(Financement == input$financement) %>%
        pull(Volet) %>% unique()
    } else {
      volets <- unique(df_data$volet)
    }
    selectInput("volets_choisis", "Choisir les volets :", choices = volets,
                multiple = TRUE, selected = volets)
  })
  
  # Données filtrées pour la visualisation pyrex
  data_filtered <- reactive({
    req(input$financement, input$variable_y, input$volets_choisis)
    df_data <- df()
    
    if("Financement" %in% names(df_data)) {
      df_data %>%
        filter(Financement == input$financement) %>%
        filter(Volet %in% input$volets_choisis) %>%
        select(Volet, !!sym(input$variable_y))
    } else {
      df_data %>%
        filter(volet %in% input$volets_choisis) %>%
        select(volet, !!sym(input$variable_y)) %>%
        rename(Volet = volet)
    }
  })
  
  # Graphique pyrex
  output$graph_suivi_tech <- renderPlotly({
    df_plot <- data_filtered() %>%
      mutate(Valeur = .data[[input$variable_y]] * 100) %>%
      arrange(desc(Valeur)) %>%
      mutate(Volet = factor(Volet, levels = Volet))
    
    p <- ggplot(df_plot) +
      geom_bar(aes(x = Volet, y = 120),
               stat = "identity", width = 0.6,
               fill = "grey90", color = "grey70") +
      geom_bar(aes(x = Volet, y = Valeur, fill = Volet,
                   text = paste0("Volet : ", Volet,
                                 "<br>", input$variable_y, " : ", round(Valeur,1), "%")),
               stat = "identity", width = 0.4, color = "white", show.legend = FALSE) +
      geom_point(aes(x = Volet, y = Valeur, color = Volet), size = 6, show.legend = FALSE) +
      geom_text(aes(x = Volet, y = Valeur + 5, 
                    label = paste0(round(Valeur,1), "%")), 
                hjust = 0, size = 4) +
      coord_flip() + 
      ylim(0, 130) +
      labs(x = "Volet", y = "Pourcentage",
           title = paste("Suivi Technique -", input$financement)) +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size=14, face="bold", hjust=0.5)
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  
  # Traitement des données réactives
  data_barres <- reactiveVal(suivi_tech)
  
  # Debugging - afficher la structure des données
  observe({
    cat("Structure des données:\n")
    cat("Colonnes:", paste(names(suivi_tech), collapse = ", "), "\n")
    cat("Nombre de lignes:", nrow(suivi_tech), "\n")
    if("Financement" %in% names(suivi_tech)) {
      cat("Financements uniques:", paste(unique(suivi_tech$Financement), collapse = ", "), "\n")
    }
    if("Volet" %in% names(suivi_tech)) {
      cat("Volets uniques:", paste(unique(suivi_tech$Volet), collapse = ", "), "\n")
    }
  })
  
  # Mise à jour des choix de financement
  observe({
    current_data <- data_barres()
    if(nrow(current_data) > 0 && "Financement" %in% names(current_data)) {
      financements <- unique(current_data$Financement)
      financements <- financements[!is.na(financements) & financements != ""]
      choices <- c("Tous les financements" = "all")
      if(length(financements) > 0) {
        choices <- c(choices, setNames(financements, financements))
      }
      updateSelectInput(session, "selectedFinancement", choices = choices)
    }
  })
  
  # Mise à jour des choix de volets
  observe({
    current_data <- data_barres()
    if(nrow(current_data) > 0 && "Volet" %in% names(current_data)) {
      volets <- unique(current_data$Volet)
      volets <- volets[!is.na(volets) & volets != ""]
      choices <- c("Tous les volets" = "all")
      if(length(volets) > 0) {
        choices <- c(choices, setNames(volets, volets))
      }
      updateSelectInput(session, "selectedVolet", choices = choices)
    }
  })
  
  # Mise à jour des choix de trimestres
  observe({
    trimestres <- c("T1", "T2", "T3", "T4")
    choices <- setNames(trimestres, paste("Trimestre", 1:4))
    updateSelectInput(session, "selectedTrimestre", choices = choices)
  })
  
  # Données filtrées
  filtered_data <- reactive({
    current_data <- data_barres()
    
    # Debug
    cat("Données initiales:", nrow(current_data), "lignes\n")
    
    # Vérifier que les colonnes existent
    if(!"Financement" %in% names(current_data) || !"Volet" %in% names(current_data)) {
      cat("Colonnes manquantes: Financement ou Volet\n")
      return(data.frame())
    }
    
    # Filtre par financement
    if(!is.null(input$selectedFinancement) && input$selectedFinancement != "all") {
      current_data <- current_data[current_data$Financement == input$selectedFinancement, ]
      cat("Après filtre financement:", nrow(current_data), "lignes\n")
    }
    
    # Filtre par volet
    if(!is.null(input$selectedVolet) && input$selectedVolet != "all") {
      current_data <- current_data[current_data$Volet == input$selectedVolet, ]
      cat("Après filtre volet:", nrow(current_data), "lignes\n")
    }
    
    return(current_data)
  })
  
  # Données transformées selon le trimestre sélectionné
  transformed_data <- reactive({
    df <- filtered_data()
    if(nrow(df) == 0) {
      cat("Aucune donnée filtrée\n")
      return(data.frame())
    }
    
    trimestre <- input$selectedTrimestre
    if(is.null(trimestre)) trimestre <- "T1"
    
    cat("Trimestre sélectionné:", trimestre, "\n")
    
    # Construire les noms des colonnes selon le trimestre - essayer différents formats
    possible_objectif_cols <- c(
      paste0("Objectif_", trimestre, "(%)"),
      paste0("Objectif_", trimestre, " (%)"),
      paste0("Objectif ", trimestre, "(%)"),
      paste0("Objectif ", trimestre, " (%)"),
      paste0("Objectif_", trimestre),
      paste0("Objectif ", trimestre)
    )
    
    possible_realisation_cols <- c(
      paste0("Realisation_", trimestre, "(%)"),
      paste0("Realisation_", trimestre, " (%)"),
      paste0("Réalisation_", trimestre, "(%)"),
      paste0("Réalisation_", trimestre, " (%)"),
      paste0("Realisation ", trimestre, "(%)"),
      paste0("Realisation ", trimestre, " (%)"),
      paste0("Réalisation ", trimestre, "(%)"),
      paste0("Réalisation ", trimestre, " (%)"),
      paste0("Realisation_", trimestre),
      paste0("Réalisation_", trimestre),
      paste0("Realisation ", trimestre),
      paste0("Réalisation ", trimestre)
    )
    
    # Trouver les bonnes colonnes
    objectif_col <- NULL
    realisation_col <- NULL
    
    for(col in possible_objectif_cols) {
      if(col %in% names(df)) {
        objectif_col <- col
        break
      }
    }
    
    for(col in possible_realisation_cols) {
      if(col %in% names(df)) {
        realisation_col <- col
        break
      }
    }
    
    
    # Créer le dataframe transformé
    result <- data.frame(
      financement = df$Financement,
      volet = df$Volet,
      objectif = as.numeric(df[[objectif_col]]),
      realisation = as.numeric(df[[realisation_col]]),
      stringsAsFactors = FALSE
    )
    
    
    # Convertir en pourcentage si les valeurs sont entre 0 et 1
    if(max(result$objectif, na.rm = TRUE) <= 1 && min(result$objectif, na.rm = TRUE) >= 0) {
      result$objectif <- result$objectif * 100
      result$realisation <- result$realisation * 100
      cat("Conversion en pourcentage effectuée\n")
    }
    
    # Nettoyer les données (supprimer les NA)
    result <- result[complete.cases(result), ]
    
    cat("Après nettoyage:", nrow(result), "lignes\n")
    
    return(result)
  })
  
  # Données agrégées pour les barres
  aggregated_data <- reactive({
    df <- transformed_data()
    if(nrow(df) == 0) {
      cat("Aucune donnée transformée\n")
      return(data.frame())
    }
    
    cat("Agrégation de", nrow(df), "lignes\n")
    
    # Agrégation par volet (moyenne car plusieurs projets peuvent avoir le même volet)
    result <- df %>%
      group_by(volet) %>%
      summarise(
        objectif = mean(objectif, na.rm = TRUE),
        realisation = mean(realisation, na.rm = TRUE),
        nb_projets = n(),
        .groups = 'drop'
      )
    
    cat("Données agrégées:", nrow(result), "volets\n")
    
    return(result)
  })
  
  # Graphique de comparaison par barres
  output$comparison_chart <- renderUI({
    agg_data <- aggregated_data()
    
    cat("Rendu du graphique avec", nrow(agg_data), "volets\n")
    
    if(nrow(agg_data) == 0) {
      return(div(
        div(style = "text-align: center; padding: 60px; color: #666;",
            icon("chart-bar", style = "font-size: 48px; color: #ddd; margin-bottom: 20px;"),
            h4("Aucune donnée disponible", style = "margin: 0; color: #999;"),
            p("Vérifiez vos filtres ou les données source", style = "margin-top: 10px; font-size: 14px;")
        )
      ))
    }
    
    max_value <- max(c(agg_data$objectif, agg_data$realisation), na.rm = TRUE)
    if(max_value == 0 || is.na(max_value)) max_value <- 100
    
    chart_elements <- lapply(1:nrow(agg_data), function(i) {
      volet_data <- agg_data[i, ]
      volet_info <- volets_config[volets_config$id == volet_data$volet, ]
      
      if(nrow(volet_info) == 0) {
        volet_info <- data.frame(
          name = volet_data$volet,
          color = sample(c("#3B82F6", "#10B981", "#F59E0B", "#8B5CF6", "#EF4444"), 1)
        )
      }
      
      objectif_width <- (volet_data$objectif / max_value) * 100
      realisation_width <- (volet_data$realisation / max_value) * 100
      
      # Calculer le pourcentage de réalisation
      percentage <- ifelse(volet_data$objectif > 0, 
                           round((volet_data$realisation / volet_data$objectif) * 100, 1), 
                           0)
      
      # Déterminer la couleur selon le pourcentage
      status_color <- if(percentage >= 100) {
        "#10B981"  # Vert
      } else if(percentage >= 75) {
        "#F59E0B"  # Orange
      } else {
        "#EF4444"  # Rouge
      }
      
      div(style = "margin-bottom: 24px; padding: 16px; border: 1px solid #e5e7eb; border-radius: 8px; background-color: white; box-shadow: 0 1px 3px rgba(0,0,0,0.1);",
          div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 12px;",
              h4(volet_info$name, style = "margin: 0; font-weight: 500; font-size: 16px; color: #374151;"),
              span(paste0(percentage, "%"), 
                   style = paste0("font-size: 18px; color: ", status_color, "; font-weight: bold;"))
          ),
          
          div(style = "margin-bottom: 8px;",
              div(style = "display: flex; align-items: center; margin-bottom: 6px;",
                  span("Objectif", style = "width: 80px; font-size: 12px; color: #6b7280; font-weight: 500;"),
                  div(style = "flex: 1; margin-left: 12px; height: 24px; background-color: #f3f4f6; border-radius: 12px; position: relative; overflow: hidden;",
                      div(style = paste0("width: ", objectif_width, "%; background: linear-gradient(90deg, ", volet_info$color, "60, ", volet_info$color, "80); height: 100%; border-radius: 12px; position: relative; transition: width 0.3s ease;"),
                          span(paste0(round(volet_data$objectif, 1), "%"), 
                               style = "position: absolute; right: 8px; top: 50%; transform: translateY(-50%); font-size: 10px; color: #374151; font-weight: 500;")
                      )
                  )
              ),
              
              div(style = "display: flex; align-items: center;",
                  span("Réalisé", style = "width: 80px; font-size: 12px; color: #6b7280; font-weight: 500;"),
                  div(style = "flex: 1; margin-left: 12px; height: 24px; background-color: #f3f4f6; border-radius: 12px; position: relative; overflow: hidden;",
                      div(style = paste0("width: ", realisation_width, "%; background: linear-gradient(90deg, ", volet_info$color, ", ", volet_info$color, "CC); height: 100%; border-radius: 12px; position: relative; transition: width 0.3s ease;"),
                          span(paste0(round(volet_data$realisation, 1), "%"), 
                               style = "position: absolute; right: 8px; top: 50%; transform: translateY(-50%); font-size: 10px; color: white; font-weight: 500;")
                      )
                  )
              )
          ),
          
          div(style = "margin-top: 8px; font-size: 11px; color: #6b7280; text-align: right;",
              paste( volet_data$nb_projets, "projet(s)")
          )
      )
    })
    
    div(style = "padding: 20px;", chart_elements)
  })
  
  # Données agrégées par financement pour le résumé
  aggregated_data_financing <- reactive({
    df <- transformed_data()
    if(nrow(df) == 0) return(data.frame())
    
    cat("Agrégation par financement de", nrow(df), "lignes\n")
    
    # Agrégation par financement
    result <- df %>%
      group_by(financement) %>%
      summarise(
        objectif = mean(objectif, na.rm = TRUE),
        realisation = mean(realisation, na.rm = TRUE),
        nb_volets = n_distinct(volet),
        nb_projets = n(),
        .groups = 'drop'
      )
    
    cat("Données agrégées par financement:", nrow(result), "financements\n")
    
    return(result)
  })
  
  # Cartes de résumé par financement
  output$summary_cards <- renderUI({
    agg_data <- aggregated_data_financing()
    
    if(nrow(agg_data) == 0) {
      return(div(
        div(style = "text-align: center; padding: 60px; color: #666;",
            icon("cards", style = "font-size: 48px; color: #ddd; margin-bottom: 20px;"),
            h4("Aucune donnée disponible", style = "margin: 0; color: #999;"),
            p("Vérifiez vos filtres ou les données source", style = "margin-top: 10px; font-size: 14px;")
        )
      ))
    }
    
    # Configuration des couleurs pour les financements
    financing_colors <- c(
      "PUDC-Budget État 2025" = "#3B82F6",
      "PA-PUDC-BID" = "#10B981", 
      "PA-PUDC-BAD" = "#F59E0B",
      "PSP-PUDC-FSD" = "#8B5CF6",
      "Projet « Elec. 2000 villages »" = "#EF4444"
    )
    
    cards <- lapply(1:nrow(agg_data), function(i) {
      # Vérification de sécurité
      if(i > nrow(agg_data)) return(NULL)
      
      financing_data <- agg_data[i, ]
      
      # Vérifier que financing_data n'est pas vide
      if(is.null(financing_data$financement) || is.na(financing_data$financement)) {
        return(NULL)
      }
      
      # Obtenir la couleur pour ce financement de manière sécurisée
      financing_color <- "#6B7280"  # Couleur par défaut
      if(!is.null(financing_data$financement) && 
         financing_data$financement %in% names(financing_colors)) {
        financing_color <- financing_colors[[financing_data$financement]]
      } else {
        # Assigner une couleur basée sur la position
        colors_list <- c("#3B82F6", "#10B981", "#F59E0B", "#8B5CF6", "#EF4444")
        financing_color <- colors_list[((i - 1) %% length(colors_list)) + 1]
      }
      
      percentage <- ifelse(financing_data$objectif > 0, 
                           round((financing_data$realisation / financing_data$objectif) * 100, 1), 
                           0)
      
      if(percentage >= 100) {
        status_color <- "#10B981"
        bg_color <- "#ffffff"        # <- Changé en blanc
        border_color <- "#10B981"
        icon_name <- "check-circle"
        status_text <- "Objectif atteint"
      } else if(percentage >= 75) {
        status_color <- "#F59E0B"
        bg_color <- "#ffffff"        # <- Changé en blanc
        border_color <- "#F59E0B"
        icon_name <- "clock"
        status_text <- "En bonne voie"
      } else {
        status_color <- "#EF4444"
        bg_color <- "#ffffff"        # <- Changé en blanc
        border_color <- "#EF4444"
        icon_name <- "exclamation-triangle"
        status_text <- "Attention requise"
      }
      
      # Raccourcir le nom du financement pour l'affichage
      display_name <- financing_data$financement
      if(financing_data$financement == "PUDC-Budget État 2025") {
        display_name <- "Budget État 2025"
      } else if(financing_data$financement == "PA-PUDC-BID") {
        display_name <- "PA-BID"
      } else if(financing_data$financement == "PA-PUDC-BAD") {
        display_name <- "PA-BAD"
      } else if(financing_data$financement == "PSP-PUDC-FSD") {
        display_name <- "PSP-FSD"
      } else if(grepl("Elec.*2000.*villages", financing_data$financement)) {
        display_name <- "Élec. 2000 villages"
      }
      
      column(4,
             div(style = paste0("border: 1px solid ", border_color, "; border-radius: 8px; padding: 16px; margin-bottom: 16px; background-color: ", bg_color, "; box-shadow: 0 1px 3px rgba(0,0,0,0.06); transition: transform 0.2s ease;"),
                 # Header avec nom et statut
                 div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 12px;",
                     h5(display_name, style = "margin: 0; font-weight: 600; font-size: 14px; color: #1f2937;"),
                     div(style = "display: flex; align-items: center;",
                         icon(icon_name, style = paste0("color: ", status_color, "; margin-right: 4px;")),
                         strong(paste0(percentage, "%"), style = paste0("color: #111827; font-size: 16px;"))
                     )
                 ),
                 
                 # Barre de progression
                 div(style = "margin-bottom: 12px;",
                     div(style = "width: 100%; height: 4px; background-color: #f9fafb; border-radius: 2px; overflow: hidden;",
                         div(style = paste0("width: ", min(percentage, 100), "%; height: 100%; background: linear-gradient(90deg, #e5e7eb, ", status_color, "); transition: width 0.3s ease;"))
                     )
                 ),
                 
                 # Détails
                 div(style = "font-size: 12px; color: #4b5563; line-height: 1.5;",
                     div(paste("Objectif:", round(financing_data$objectif, 1), "%")),
                     div(paste("Réalisé:", round(financing_data$realisation, 1), "%")),
                     div(paste("Volets:", financing_data$nb_volets))
                 ),
                 
                 # Statut en bas
                 div(style = paste0("margin-top: 8px; padding: 4px 8px; background-color: ", status_color, "15; border-radius: 3px; text-align: center;"),
                     span(status_text, style = paste0("font-size: 11px; color: #374151; font-weight: 500;"))
                 )
             )
      )
    })
    
    # Filtrer les cartes nulles
    cards <- cards[!sapply(cards, is.null)]
    
    if(length(cards) == 0) {
      return(div("Aucune donnée à afficher", style = "text-align: center; padding: 20px; color: #666;"))
    }
    
    fluidRow(cards)
  })
  
  # Tableau de données détaillé
  output$detailed_table <- renderDT({
    current_data <- transformed_data()
    if(nrow(current_data) == 0) return(NULL)
    
    # Calculer le pourcentage de réalisation
    current_data$pourcentage <- round((current_data$realisation / current_data$objectif) * 100, 1)
    
    # Sélectionner et renommer les colonnes
    display_data <- current_data[, c("financement", "volet", "objectif", "realisation", "pourcentage")]
    names(display_data) <- c("Financement", "Volet", "Objectif (%)", "Réalisation (%)", "Taux de réalisation (%)")
    
    datatable(display_data, 
              options = list(
                pageLength = 15,
                scrollX = TRUE,
                language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json')
              ),
              rownames = FALSE) %>%
      formatStyle("Taux de réalisation (%)",
                  backgroundColor = styleInterval(c(75, 100), c("#FEE2E2", "#FEF3C7", "#D1FAE5"))) %>%
      formatRound(c("Objectif (%)", "Réalisation (%)", "Taux de réalisation (%)"), 1)
  })
  
  # Indicateurs de performance globaux
  output$performance_indicators <- renderUI({
    agg_data <- aggregated_data()
    
    if(nrow(agg_data) == 0) {
      return(div(
        div(style = "text-align: center; padding: 60px; color: #666;",
            icon("chart-line", style = "font-size: 48px; color: #ddd; margin-bottom: 20px;"),
            h4("Aucune donnée disponible", style = "margin: 0; color: #999;"),
            p("Vérifiez vos filtres ou les données source", style = "margin-top: 10px; font-size: 14px;")
        )
      ))
    }
    
    # Calculer les indicateurs globaux
    total_objectif <- sum(agg_data$objectif, na.rm = TRUE)
    total_realisation <- sum(agg_data$realisation, na.rm = TRUE)
    taux_global <- ifelse(total_objectif > 0, round((total_realisation / total_objectif) * 100, 1), 0)
    nb_volets <- nrow(agg_data)
    
    # Compter les volets par statut
    volets_success <- sum(agg_data$realisation >= agg_data$objectif, na.rm = TRUE)
    volets_warning <- sum(agg_data$realisation >= agg_data$objectif * 0.75 & agg_data$realisation < agg_data$objectif, na.rm = TRUE)
    volets_danger <- sum(agg_data$realisation < agg_data$objectif * 0.75, na.rm = TRUE)
    
    fluidRow(
      column(3,
             div(style = "background:navy; color: white; padding: 20px; border-radius: 12px; text-align: center; box-shadow: 0 4px 6px rgba(0,0,0,0.1); transition: transform 0.2s ease;",
                 icon("chart-pie", style = "font-size: 24px; margin-bottom: 8px;"),
                 h3(paste0(taux_global, "%"), style = "margin: 0; font-size: 2.5em; font-weight: bold;"),
                 p("Taux global", style = "margin: 8px 0 0 0; font-size: 14px; opacity: 0.9;")
             )
      ),
      column(3,
             div(style = "background:darkgreen ; color: white; padding: 20px; border-radius: 12px; text-align: center; box-shadow: 0 4px 6px rgba(0,0,0,0.1); transition: transform 0.2s ease;",
                 icon("check-circle", style = "font-size: 24px; margin-bottom: 8px;"),
                 h3(volets_success, style = "margin: 0; font-size: 2.5em; font-weight: bold;"),
                 p("Volets réussis", style = "margin: 8px 0 0 0; font-size: 14px; opacity: 0.9;")
             )
      ),
      column(3,
             div(style = "background: linear-gradient(135deg, #F59E0B, #D97706); color: white; padding: 20px; border-radius: 12px; text-align: center; box-shadow: 0 4px 6px rgba(0,0,0,0.1); transition: transform 0.2s ease;",
                 icon("clock", style = "font-size: 24px; margin-bottom: 8px;"),
                 h3(volets_warning, style = "margin: 0; font-size: 2.5em; font-weight: bold;"),
                 p("Volets en cours", style = "margin: 8px 0 0 0; font-size: 14px; opacity: 0.9;")
             )
      ),
      column(3,
             div(style = "background:darkred; color: white; padding: 20px; border-radius: 12px; text-align: center; box-shadow: 0 4px 6px rgba(0,0,0,0.1); transition: transform 0.2s ease;",
                 icon("exclamation-triangle", style = "font-size: 24px; margin-bottom: 8px;"),
                 h3(volets_danger, style = "margin: 0; font-size: 2.5em; font-weight: bold;"),
                 p("Volets en retard", style = "margin: 8px 0 0 0; font-size: 14px; opacity: 0.9;")
             )
      )
    )
  })
  
  # ========================================
  # SERVER ONGLET SUIVI FINANCIER
  # ========================================
  
  # ------------------- DONNÉES : chaque feuille Excel -------------------
  
  # Feuille REPARTIT - 4 tableaux
  repartition_data <- reactive({
    df <- read_excel("data/PUDC_SUIVI.xlsx", sheet = "Repartit budget PTBA par projet") %>%
      mutate(across(where(is.character), trimws))
    
    df1 <- df[, 1:4] %>% filter(!is.na(.[[1]]))
    df2 <- df[, 5:8] %>% filter(!is.na(.[[1]]))
    df3 <- df[, 9:12] %>% filter(!is.na(.[[1]]))
    df4 <- df[, 13:16] %>% filter(!is.na(.[[1]]))
    
    list(df1 = df1, df2 = df2, df3 = df3, df4 = df4)
  })
  
  # Feuille EXECUTION
  execution_data <- reactive({
    read_excel("data/PUDC_SUIVI.xlsx", sheet = "Execution budgetaire PTBA 2025") %>%
      mutate(across(where(is.character), trimws))
  })
  
  # Feuille DECAISSEMENT
  decaissement_data <- reactive({
    read_excel("data/PUDC_SUIVI.xlsx", sheet = "DECAISSEMENT GLOBAUX 2025") %>%
      mutate(across(where(is.character), trimws))
  })
  
  # ------------------- UI dynamiques -------------------
  
  output$ui_var_repartition_x <- renderUI({
    req(repartition_data())
    dfs <- repartition_data()
    
    x_choices <- c(
      names(dfs$df1)[1],
      names(dfs$df2)[1],
      names(dfs$df3)[1],
      names(dfs$df4)[1]
    )
    
    selectInput("var_repartition_x", "Différentes composantes :", choices = x_choices)
  })
  
  output$ui_var_repartition_y <- renderUI({
    req(input$var_repartition_x, repartition_data())
    dfs <- repartition_data()
    
    if (input$var_repartition_x == names(dfs$df1)[1]) {
      choices <- names(dfs$df1)[-1]
    } else if (input$var_repartition_x == names(dfs$df2)[1]) {
      choices <- names(dfs$df2)[-1]
    } else if (input$var_repartition_x == names(dfs$df3)[1]) {
      choices <- names(dfs$df3)[-1]
    } else {
      choices <- names(dfs$df4)[-1]
    }
    
    selectInput("var_repartition_y", "Variable de visualisation de l'état financier :", choices = choices)
  })
  
  output$ui_var_execution <- renderUI({
    req(execution_data())
    df <- execution_data()
    numeric_vars <- names(df)[sapply(df, is.numeric)]
    selectInput("var_execution", "Variable à comparer (exécution budgétaire) :", choices = numeric_vars)
  })
  
  output$ui_var_decaissement <- renderUI({
    req(decaissement_data())
    df <- decaissement_data()
    numeric_vars <- names(df)[sapply(df, is.numeric)]
    selectInput("var_decaissement", "Métrique financière (décaissements) :", choices = numeric_vars)
  })
  
  # ------------------- Indicateurs chargement -------------------
  
  plot_repartition_loaded <- reactiveVal(FALSE)
  plot_execution_loaded <- reactiveVal(FALSE)
  plot_decaissement_loaded <- reactiveVal(FALSE)
  
  output$plot_repartition_loaded <- reactive({ plot_repartition_loaded() })
  output$plot_execution_loaded <- reactive({ plot_execution_loaded() })
  output$plot_decaissement_loaded <- reactive({ plot_decaissement_loaded() })
  
  outputOptions(output, "plot_repartition_loaded", suspendWhenHidden = FALSE)
  outputOptions(output, "plot_execution_loaded", suspendWhenHidden = FALSE)
  outputOptions(output, "plot_decaissement_loaded", suspendWhenHidden = FALSE)
  
  # ------------------- GRAPHIQUES REPARTITION -------------------
  
  output$plot_repartition <- renderPlotly({
    plot_repartition_loaded(FALSE); on.exit(plot_repartition_loaded(TRUE))
    dfs <- repartition_data()
    req(input$var_repartition_x, input$var_repartition_y)
    
    if (input$var_repartition_x == names(dfs$df1)[1]) {
      df <- dfs$df1
      color <- 'rgba(111,66,193,0.6)'; line_color <- 'rgba(111,66,193,1)'
    } else if (input$var_repartition_x == names(dfs$df2)[1]) {
      df <- dfs$df2
      color <- 'rgba(0,123,255,0.6)'; line_color <- 'rgba(0,123,255,1)'
    } else if (input$var_repartition_x == names(dfs$df3)[1]) {
      df <- dfs$df3
      color <- 'rgba(40,167,69,0.6)'; line_color <- 'rgba(40,167,69,1)'
    } else {
      df <- dfs$df4
      color <- 'rgba(255,193,7,0.6)'; line_color <- 'rgba(255,193,7,1)'
    }
    
    x_var <- input$var_repartition_x
    y_var <- input$var_repartition_y
    
    df <- df %>%
      filter(!is.na(.data[[x_var]]), !is.na(.data[[y_var]])) %>%
      arrange(desc(.data[[y_var]]))
    
    plot_ly(
      data = df,
      x = ~.data[[x_var]],
      y = ~.data[[y_var]],
      type = 'bar',
      marker = list(color = color, line = list(color = line_color, width = 3))
    ) %>%
      layout(
        xaxis = list(title = x_var),
        yaxis = list(title = y_var)
      )
  })
  
  output$list_repartition <- renderPrint({
    dfs <- repartition_data()
    req(input$var_repartition_x, input$var_repartition_y)
    
    if (input$var_repartition_x == names(dfs$df1)[1]) {
      df <- dfs$df1
    } else if (input$var_repartition_x == names(dfs$df2)[1]) {
      df <- dfs$df2
    } else if (input$var_repartition_x == names(dfs$df3)[1]) {
      df <- dfs$df3
    } else {
      df <- dfs$df4
    }
    
    x_var <- input$var_repartition_x
    y_var <- input$var_repartition_y
    
    df <- df %>%
      filter(!is.na(.data[[x_var]]), !is.na(.data[[y_var]])) %>%
      arrange(desc(.data[[y_var]]))
    
    df[[x_var]]
  })
  
  # ------------------- GRAPHIQUES EXECUTION -------------------
  
  output$plot_execution <- renderPlotly({
    plot_execution_loaded(FALSE); on.exit(plot_execution_loaded(TRUE))
    df <- execution_data()
    req(input$var_execution)
    y_var <- names(df)[sapply(df, is.character)][1]
    x_var <- input$var_execution
    
    df <- df %>%
      filter(!is.na(.data[[y_var]]), !is.na(.data[[x_var]])) %>%
      arrange(desc(.data[[x_var]]))
    
    plot_ly(
      data = df,
      x = ~.data[[x_var]],
      y = ~reorder(.data[[y_var]], .data[[x_var]]),
      type = 'bar',
      orientation = 'h',
      marker = list(color = 'rgba(0,123,255,0.6)', line = list(color = 'rgba(0,123,255,1)', width = 3))
    ) %>%
      layout(
        xaxis = list(title = x_var),
        yaxis = list(title = y_var)
      )
  })
  
  output$list_execution <- renderPrint({
    df <- execution_data()
    req(input$var_execution)
    y_var <- names(df)[sapply(df, is.character)][1]
    x_var <- input$var_execution
    
    df <- df %>%
      filter(!is.na(.data[[y_var]]), !is.na(.data[[x_var]])) %>%
      arrange(desc(.data[[x_var]]))
    
    df[[y_var]]
  })
  
  # ------------------- GRAPHIQUES DECAISSEMENT -------------------
  
  output$plot_decaissement <- renderPlotly({
    plot_decaissement_loaded(FALSE); on.exit(plot_decaissement_loaded(TRUE))
    df <- decaissement_data()
    req(input$var_decaissement)
    y_var <- names(df)[sapply(df, is.character)][1]
    x_var <- input$var_decaissement
    
    df <- df %>%
      filter(!is.na(.data[[y_var]]), !is.na(.data[[x_var]])) %>%
      arrange(desc(.data[[x_var]]))
    
    plot_ly(
      data = df,
      x = ~.data[[x_var]],
      y = ~reorder(.data[[y_var]], .data[[x_var]]),
      type = 'bar',
      orientation = 'h',
      marker = list(color = 'rgba(220,53,69,0.6)', line = list(color = 'rgba(220,53,69,1)', width = 3))
    ) %>%
      layout(
        xaxis = list(title = x_var),
        yaxis = list(title = y_var)
      )
  })
  
  output$list_decaissement <- renderPrint({
    df <- decaissement_data()
    req(input$var_decaissement)
    y_var <- names(df)[sapply(df, is.character)][1]
    x_var <- input$var_decaissement
    
    df <- df %>%
      filter(!is.na(.data[[y_var]]), !is.na(.data[[x_var]])) %>%
      arrange(desc(.data[[x_var]]))
    
    df[[y_var]]
  })
  
  
  
  # ========================================
  # SERVER ONGLET PERFORMANCE
  # ========================================
  
  # ========================================
  # SERVER ONGLET PERFORMANCE
  # ========================================
  analyse_data <- reactive({
    read_excel("data/PUDC_SUIVI.xlsx", sheet = "Analyse par Projet") %>%
      mutate(across(where(is.character), trimws))
  })
  
  # --------- Extraction et nettoyage des tableaux
  physique_data <- reactive({
    df <- analyse_data() %>% select(1:9)
    df[-1] <- lapply(df[-1], function(x) as.numeric(gsub("%","",x)))
    df[-1] <- lapply(df[-1], function(x) x*100)
    df
  })
  
  budget_data <- reactive({
    df <- analyse_data() %>% select(10:18)
    df[-1] <- lapply(df[-1], function(x) as.numeric(gsub("%","",x)))
    df[-1] <- lapply(df[-1], function(x) x*100)
    df
  })
  
  passation_data <- reactive({
    df <- analyse_data() %>% select(19:23)
    df[-1] <- lapply(df[-1], function(x) as.numeric(gsub("%","",x)))
    df[-1] <- lapply(df[-1], function(x) x*100)
    df
  })
  
  # --------- UI dynamiques
  output$ui_var_physique <- renderUI({
    df <- physique_data()
    numeric_vars <- names(df)[-1] # enlève la colonne projet
    selectInput("var_physique", "Variable Exécution Physique :", choices = numeric_vars)
  })
  
  output$ui_var_budget <- renderUI({
    df <- budget_data()
    numeric_vars <- names(df)[-1]
    selectInput("var_budget", "Variable Exécution Budgétaire :", choices = numeric_vars)
  })
  
  output$ui_var_passation <- renderUI({
    df <- passation_data()
    numeric_vars <- names(df)[-1]
    selectInput("var_passation", "Variable Passation Marchés :", choices = numeric_vars)
  })
  
  # --------- Graphiques barres verticales
  output$plot_physique <- renderPlotly({
    req(input$var_physique)
    df <- physique_data()
    proj_col <- names(df)[1]
    
    plot_ly(
      data = df,
      x = ~.data[[proj_col]],
      y = ~.data[[input$var_physique]],
      type = 'bar',
      text = ~paste0(round(.data[[input$var_physique]],1), "%"),
      textposition = 'outside',
      marker = list(
        color = 'rgba(40,167,69,0.6)',
        line = list(color = 'rgba(0,80,30,1)', width = 2)
      )
    ) %>%
      layout(
        title = list(text = paste0("Exécution Physique : ", input$var_physique)),
        yaxis = list(title = paste0(input$var_physique, " (%)"), range = c(0,120)),
        xaxis = list(title = "", tickangle = -45),
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(255,255,255,0.3)',
        margin = list(b = 100)
      )
  })
  
  output$plot_budget <- renderPlotly({
    req(input$var_budget)
    df <- budget_data()
    proj_col <- names(df)[1]
    
    plot_ly(
      data = df,
      x = ~.data[[proj_col]],
      y = ~.data[[input$var_budget]],
      type = 'bar',
      text = ~paste0(round(.data[[input$var_budget]],1), "%"),
      textposition = 'outside',
      marker = list(
        color = 'rgba(220,53,69,0.6)',
        line = list(color = 'rgba(120,0,20,1)', width = 2)
      )
    ) %>%
      layout(
        title = list(text = paste0("Exécution Budgétaire : ", input$var_budget)),
        yaxis = list(title = paste0(input$var_budget, " (%)"), range = c(0,120)),
        xaxis = list(title = "", tickangle = -45),
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(255,255,255,0.3)',
        margin = list(b = 100)
      )
  })
  
  output$plot_passation <- renderPlotly({
    req(input$var_passation)
    df <- passation_data()
    proj_col <- names(df)[1]
    
    plot_ly(
      data = df,
      x = ~.data[[proj_col]],
      y = ~.data[[input$var_passation]],
      type = 'bar',
      text = ~paste0(round(.data[[input$var_passation]],1), "%"),
      textposition = 'outside',
      marker = list(
        color = 'rgba(102,16,242,0.6)',
        line = list(color = 'rgba(50,0,100,1)', width = 2)
      )
    ) %>%
      layout(
        title = list(text = paste0("Plan de Passation : ", input$var_passation)),
        yaxis = list(title = paste0(input$var_passation, " (%)"), range = c(0,120)),
        xaxis = list(title = "", tickangle = -45),
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(255,255,255,0.3)',
        margin = list(b = 100)
      )
  })
  
  
  # ========================================
  # SERVER ONGLET REALISATION
  # ========================================
  # Données réactives basées sur le volet et type sélectionnés
  current_data <- reactive({
    req(input$selected_volet)
    
    # Charger les données du volet sélectionné
    data <- load_data_with_volet(selected_volet = input$selected_volet)
    
    # Adapter selon le type de réalisation
    if (input$type_realisation == "annuelle") {
      data$realisation_principale <- data$realisation_annuelle
      data$cible_principale <- data$cible_annuelle
    } else {
      data$realisation_principale <- data$realisation_globale
      data$cible_principale <- data$cible_globale
    }
    
    # Recalculer le taux
    data$taux_principal <- ifelse(data$cible_principale > 0, 
                                  round((data$realisation_principale / data$cible_principale) * 100, 1), 
                                  0)
    
    return(data)
  })
  
  # Description du volet
  output$volet_description <- renderText({
    volet_name <- volets_config$name[volets_config$id == input$selected_volet]
    type_text <- ifelse(input$type_realisation == "globale", "globales", "annuelles")
    paste("Affichage des réalisations", type_text, "pour le volet:", volet_name)
  })
  
  # Labels dynamiques
  output$label_cible <- renderText({
    if (input$type_realisation == "globale") "Cible Globale" else "Cible Annuelle"
  })
  
  output$label_realisation <- renderText({
    if (input$type_realisation == "globale") "Réalisation Globale" else "Réalisation Annuelle"
  })
  
  # Métriques globales
  output$total_cible <- renderText({
    data <- current_data()
    format(sum(data$cible_principale, na.rm = TRUE), big.mark = " ")
  })
  
  output$total_realisation <- renderText({
    data <- current_data()
    format(sum(data$realisation_principale, na.rm = TRUE), big.mark = " ")
  })
  
  output$taux_global <- renderText({
    data <- current_data()
    total_cible <- sum(data$cible_principale, na.rm = TRUE)
    total_real <- sum(data$realisation_principale, na.rm = TRUE)
    taux <- if(total_cible > 0) round((total_real / total_cible) * 100, 1) else 0
    paste0(taux, "%")
  })
  
  output$nb_regions <- renderText({
    data <- current_data()
    nrow(data)
  })
  
  # Carte interactive
  output$map_regions <- renderLeaflet({
    data <- current_data()
    map_data <- merge(data, senegal_coords, by = "region", all.x = TRUE)
    map_data <- map_data[!is.na(map_data$lat), ]
    
    # Couleur du volet
    volet_color <- get_volet_color(input$selected_volet)
    
    # Popups
    map_data$popup_content <- paste(
      "<b>Région:</b>", map_data$region, "<br>",
      "<b>Volet:</b>", input$selected_volet, "<br>",
      "<b>Cible:</b>", format(map_data$cible_principale, big.mark = " "), "<br>",
      "<b>Réalisation:</b>", format(map_data$realisation_principale, big.mark = " "), "<br>",
      "<b>Taux:</b>", map_data$taux_principal, "%"
    )
    
    # Couleurs selon performance
    map_data$color <- ifelse(map_data$taux_principal >= 80, "#10B981",
                             ifelse(map_data$taux_principal >= 50, "#F59E0B", "#EF4444"))
    
    leaflet(map_data) %>%
      addTiles() %>%
      setView(lng = -14.4524, lat = 14.4974, zoom = 6) %>%
      addCircleMarkers(
        lng = ~lon, lat = ~lat,
        radius = ~pmax(8, 8 + (taux_principal/10)),
        color = ~color,
        fillOpacity = 0.7,
        popup = ~popup_content,
        label = ~paste("Région:", region, "- Taux:", taux_principal, "%")
      )
  })
  
  # Graphique de performance
  output$chart_performance <- renderPlotly({
    data <- current_data()
    volet_color <- get_volet_color(input$selected_volet)
    
    p <- plot_ly(data, x = ~region, type = 'bar') %>%
      add_trace(y = ~cible_principale, name = 'Cible',
                marker = list(color = '#94A3B8')) %>%
      add_trace(y = ~realisation_principale, name = 'Réalisation',
                marker = list(color = volet_color)) %>%
      layout(
        title = list(text = paste("Performance -", input$selected_volet), x = 0.5),
        xaxis = list(title = "Régions", tickangle = -45),
        yaxis = list(title = "Valeurs"),
        barmode = 'group',
        legend = list(orientation = "h", x = 0.5, xanchor = "center")
      )
    
    p
  })
  
  # Tableau détaillé
  output$table_details <- DT::renderDataTable({
    data <- current_data() %>%
      select(region, cible_principale, realisation_principale, taux_principal) %>%
      rename(
        "Région" = region,
        !!paste("Cible", ifelse(input$type_realisation == "globale", "Globale", "Annuelle")) := cible_principale,
        !!paste("Réalisation", ifelse(input$type_realisation == "globale", "Globale", "Annuelle")) := realisation_principale,
        "Taux (%)" = taux_principal
      )
    
    DT::datatable(
      data,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json'),
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      ),
      extensions = 'Buttons',
      rownames = FALSE
    ) %>%
      DT::formatRound(columns = 2:3, digits = 2) %>%
      DT::formatStyle(
        "Taux (%)",
        backgroundColor = DT::styleInterval(c(50, 80), c("#fee2e2", "#fef3c7", "#d1fae5")),
        color = DT::styleInterval(c(50, 80), c("#dc2626", "#d97706", "#059669"))
      )
  })
  
  
  # ========================================
  # SERVER ONGLET RESUME
  # ========================================
  # Fonction de formatage des pourcentages
  # Fonction de formatage des pourcentages
  formatPercentage <- function(x) {
    if (is.null(x) || is.na(x) || length(x) == 0) return("0%")
    paste0(round(x, 1), "%")
  }
  
  # Fonction de formatage des nombres
  formatNumber <- function(x) {
    if (is.null(x) || is.na(x) || length(x) == 0) return("0")
    format(round(x, 1), big.mark = " ", decimal.mark = ",")
  }
  
  # Variable réactive pour stocker les données
  resume_data <- reactive({
    tryCatch({
      # Charger les données depuis le fichier Excel
      cat("Tentative de chargement du fichier Excel...\n")
      resume <- read_excel("data/PUDC_SUIVI.xlsx", sheet = "TEC. BUDGETAIRE. PPM ")
      resume <- as.data.frame(resume, row.names = TRUE)
      
      cat("Fichier chargé. Dimensions:", nrow(resume), "x", ncol(resume), "\n")
      cat("Noms de colonnes originaux:\n")
      print(names(resume))
      
      # Nettoyer les noms de colonnes
      names(resume) <- c("Financement", "Projet", 
                         "Objectif_TRIM1", "Realisation_TRIM1",
                         "Objectif_TRIM2", "Realisation_TRIM2", 
                         "Objectif_TRIM3", "Realisation_TRIM3",
                         "Objectif_TRIM4", "Realisation_TRIM4")
      
      cat("Aperçu des premières lignes:\n")
      print(head(resume, 3))
      
      # Supprimer les lignes entièrement vides
      resume <- resume %>%
        filter(!is.na(Financement) | !is.na(Projet)) %>%
        filter(Financement != "" | Projet != "") %>%
        # Supprimer les lignes où Financement est NA ou vide
        filter(!is.na(Financement), Financement != "")
      
      cat("Après nettoyage. Dimensions:", nrow(resume), "x", ncol(resume), "\n")
      
      # Debug: afficher les projets uniques trouvés
      cat("Projets trouvés dans les données:\n")
      cat(paste(unique(resume$Financement), collapse = "\n"))
      cat("\n")
      
      # Convertir les pourcentages en valeurs numériques (diviser par 100)
      percentage_cols <- grep("Objectif_|Realisation_", names(resume), value = TRUE)
      for(col in percentage_cols) {
        resume[[col]] <- as.numeric(gsub("%", "", resume[[col]])) / 100
        resume[[col]] <- ifelse(is.na(resume[[col]]), 0, resume[[col]])
      }
      
      cat("Conversion des pourcentages terminée.\n")
      
      # Transformer les données en format long
      resume_long <- resume %>%
        pivot_longer(
          cols = -c(Financement, Projet),
          names_to = c("Type", "Trimestre"),
          names_pattern = "(.*)_TRIM(.)",
          values_to = "Valeur"
        ) %>%
        mutate(
          Trimestre = as.numeric(Trimestre),
          Type = case_when(
            Type == "Objectif" ~ "objectif",
            Type == "Realisation" ~ "realisation",
            TRUE ~ Type
          )
        ) %>%
        pivot_wider(
          names_from = Type,
          values_from = Valeur
        ) %>%
        mutate(
          # S'assurer que les valeurs manquantes sont traitées correctement
          objectif = ifelse(is.na(objectif), 0, objectif),
          realisation = ifelse(is.na(realisation), 0, realisation),
          ecart = realisation - objectif,
          statut = ifelse(realisation >= objectif, "Atteint", "Non Atteint"),
          performance = Projet,  # La colonne Projet contient le type de performance
          projet = Financement   # La colonne Financement contient le nom du projet
        ) %>%
        select(projet, performance, trimestre = Trimestre, objectif, realisation, ecart, statut) %>%
        filter(!is.na(projet), !is.na(performance), projet != "", performance != "")
      
      cat("Transformation en format long terminée. Dimensions finales:", nrow(resume_long), "x", ncol(resume_long), "\n")
      cat("Colonnes finales:", paste(names(resume_long), collapse = ", "), "\n")
      cat("Types de performance uniques:", paste(unique(resume_long$performance), collapse = ", "), "\n")
      
      # Debug: afficher un échantillon des données PPM
      cat("Échantillon des données PPM transformées:\n")
      ppm_sample <- resume_long %>% filter(performance == "PPM") %>% head(10)
      print(ppm_sample)
      
      return(resume_long)
    }, error = function(e) {
      cat("Erreur lors du chargement des données:", e$message, "\n")
      cat("Trace complète de l'erreur:\n")
      print(traceback())
      return(data.frame())
    })
  })
  
  # Mettre à jour les choix de projets dans tous les selectInput
  observe({
    req(resume_data())
    
    if (nrow(resume_data()) > 0) {
      # Obtenir les projets uniques RÉELLEMENT présents dans les données
      projets <- unique(resume_data()$projet)
      projets <- projets[!is.na(projets) & projets != ""]
      # Trier par ordre alphabétique pour une meilleure lisibilité
      projets <- sort(projets)
      
      cat("Projets disponibles pour les filtres:\n")
      cat(paste(projets, collapse = "\n"))
      cat("\n")
      
      projets_choices <- c("Tous" = "all", setNames(projets, projets))
      
      # Mettre à jour tous les selectInput pour les projets
      updateSelectInput(session, "selectedProject_overview", choices = projets_choices)
      updateSelectInput(session, "selectedProject_analytics", choices = projets_choices)
      updateSelectInput(session, "selectedProject_table", choices = projets_choices)
    } else {
      # Si aucune donnée, afficher un message dans la console
      cat("Aucune donnée trouvée pour mettre à jour les filtres\n")
    }
  })
  
  # Données filtrées pour l'onglet Vue d'ensemble
  donnees_filtrees_overview <- reactive({
    req(resume_data())
    data <- resume_data()
    
    # Filtrer par projet
    if (!is.null(input$selectedProject_overview) && input$selectedProject_overview != "all") {
      data <- data %>% filter(projet == input$selectedProject_overview)
    }
    
    # Filtrer par trimestre
    if (!is.null(input$selectedTrimester_overview) && input$selectedTrimester_overview != "all") {
      data <- data %>% filter(trimestre == as.numeric(input$selectedTrimester_overview))
    }
    
    # Filtrer par indicateur
    if (!is.null(input$selectedIndicator_overview) && input$selectedIndicator_overview != "all") {
      data <- data %>% filter(performance == input$selectedIndicator_overview)
    }
    
    return(data)
  })
  
  # Données filtrées pour l'onglet Analytics
  donnees_filtrees_analytics <- reactive({
    req(resume_data())
    data <- resume_data()
    
    # Filtrer par projet
    if (!is.null(input$selectedProject_analytics) && input$selectedProject_analytics != "all") {
      data <- data %>% filter(projet == input$selectedProject_analytics)
    }
    
    # Filtrer par trimestre
    if (!is.null(input$selectedTrimester_analytics) && input$selectedTrimester_analytics != "all") {
      data <- data %>% filter(trimestre == as.numeric(input$selectedTrimester_analytics))
    }
    
    # Filtrer par indicateur
    if (!is.null(input$selectedIndicator_analytics) && input$selectedIndicator_analytics != "all") {
      data <- data %>% filter(performance == input$selectedIndicator_analytics)
    }
    
    return(data)
  })
  
  # Données filtrées pour l'onglet Table
  donnees_filtrees_table <- reactive({
    req(resume_data())
    data <- resume_data()
    
    # Filtrer par projet
    if (!is.null(input$selectedProject_table) && input$selectedProject_table != "all") {
      data <- data %>% filter(projet == input$selectedProject_table)
    }
    
    # Filtrer par trimestre
    if (!is.null(input$selectedTrimester_table) && input$selectedTrimester_table != "all") {
      data <- data %>% filter(trimestre == as.numeric(input$selectedTrimester_table))
    }
    
    # Filtrer par indicateur
    if (!is.null(input$selectedIndicator_table) && input$selectedIndicator_table != "all") {
      data <- data %>% filter(performance == input$selectedIndicator_table)
    }
    
    return(data)
  })
  
  # Calcul des métriques pour chaque indicateur (Vue d'ensemble)
  metriques_technique <- reactive({
    req(donnees_filtrees_overview())
    data <- donnees_filtrees_overview() %>% filter(performance == "TECHNIQUE")
    
    if (nrow(data) == 0) return(list(obj = 0, real = 0, ecart = 0, taux = 0))
    
    list(
      obj = mean(data$objectif, na.rm = TRUE),
      real = mean(data$realisation, na.rm = TRUE),
      ecart = mean(data$ecart, na.rm = TRUE),
      taux = ifelse(nrow(data) > 0, sum(data$statut == "Atteint", na.rm = TRUE) / nrow(data) * 100, 0)
    )
  })
  
  metriques_budgetaire <- reactive({
    req(donnees_filtrees_overview())
    data <- donnees_filtrees_overview() %>% filter(performance == "BUDGETAIRE")
    
    if (nrow(data) == 0) return(list(obj = 0, real = 0, ecart = 0, taux = 0))
    
    list(
      obj = mean(data$objectif, na.rm = TRUE),
      real = mean(data$realisation, na.rm = TRUE),
      ecart = mean(data$ecart, na.rm = TRUE),
      taux = ifelse(nrow(data) > 0, sum(data$statut == "Atteint", na.rm = TRUE) / nrow(data) * 100, 0)
    )
  })
  
  metriques_ppm <- reactive({
    req(donnees_filtrees_overview())
    data <- donnees_filtrees_overview() %>% filter(performance == "PPM")
    
    if (nrow(data) == 0) return(list(obj = 0, real = 0, ecart = 0, taux = 0))
    
    list(
      obj = mean(data$objectif, na.rm = TRUE),
      real = mean(data$realisation, na.rm = TRUE),
      ecart = mean(data$ecart, na.rm = TRUE),
      taux = ifelse(nrow(data) > 0, sum(data$statut == "Atteint", na.rm = TRUE) / nrow(data) * 100, 0)
    )
  })
  
  # Outputs pour les métriques techniques
  output$tech_objectif_moyen <- renderText({
    formatPercentage(metriques_technique()$obj * 100)
  })
  
  output$tech_realisation_moyenne <- renderText({
    formatPercentage(metriques_technique()$real * 100)
  })
  
  output$tech_ecart_moyen <- renderText({
    ecart <- metriques_technique()$ecart * 100
    paste0(ifelse(ecart >= 0, "+", ""), formatPercentage(abs(ecart)))
  })
  
  output$tech_taux_reussite <- renderText({
    paste0(round(metriques_technique()$taux, 1), "%")
  })
  
  # Mettre à jour la barre de progression technique avec JavaScript
  observe({
    taux <- metriques_technique()$taux
    session$sendCustomMessage(type = "updateProgressBar", message = list(
      id = "tech_progress_bar",
      width = paste0(round(taux, 1), "%")
    ))
  })
  
  # Outputs pour les métriques budgétaires
  output$budget_objectif_moyen <- renderText({
    formatPercentage(metriques_budgetaire()$obj * 100)
  })
  
  output$budget_realisation_moyenne <- renderText({
    formatPercentage(metriques_budgetaire()$real * 100)
  })
  
  output$budget_ecart_moyen <- renderText({
    ecart <- metriques_budgetaire()$ecart * 100
    paste0(ifelse(ecart >= 0, "+", ""), formatPercentage(abs(ecart)))
  })
  
  output$budget_taux_reussite <- renderText({
    paste0(round(metriques_budgetaire()$taux, 1), "%")
  })
  
  # Mettre à jour la barre de progression budgétaire avec JavaScript
  observe({
    taux <- metriques_budgetaire()$taux
    session$sendCustomMessage(type = "updateProgressBar", message = list(
      id = "budget_progress_bar",
      width = paste0(round(taux, 1), "%")
    ))
  })
  
  # Outputs pour les métriques PPM
  output$ppm_objectif_moyen <- renderText({
    formatPercentage(metriques_ppm()$obj * 100)
  })
  
  output$ppm_realisation_moyenne <- renderText({
    formatPercentage(metriques_ppm()$real * 100)
  })
  
  output$ppm_ecart_moyen <- renderText({
    ecart <- metriques_ppm()$ecart * 100
    paste0(ifelse(ecart >= 0, "+", ""), formatPercentage(abs(ecart)))
  })
  
  output$ppm_taux_reussite <- renderText({
    paste0(round(metriques_ppm()$taux, 1), "%")
  })
  
  # Mettre à jour la barre de progression PPM avec JavaScript
  observe({
    taux <- metriques_ppm()$taux
    session$sendCustomMessage(type = "updateProgressBar", message = list(
      id = "ppm_progress_bar",
      width = paste0(round(taux, 1), "%")
    ))
  })
  
  # Métriques globales
  output$global_technique <- renderText({
    paste0(round(metriques_technique()$taux, 1), "%")
  })
  
  output$global_budgetaire <- renderText({
    paste0(round(metriques_budgetaire()$taux, 1), "%")
  })
  
  output$global_ppm <- renderText({
    paste0(round(metriques_ppm()$taux, 1), "%")
  })
  
  # Graphique d'évolution (onglet Analytics)
  output$trendChart <- renderPlotly({
    req(donnees_filtrees_analytics())
    
    tryCatch({
      data <- donnees_filtrees_analytics()
      
      if (nrow(data) == 0) {
        return(plotly_empty() %>%
                 layout(title = "Aucune donnée disponible pour les filtres sélectionnés"))
      }
      
      # Préparer les données pour le graphique d'évolution
      data_chart <- data %>%
        group_by(trimestre, performance) %>%
        summarise(
          objectif_moyen = mean(objectif, na.rm = TRUE) * 100,
          realisation_moyenne = mean(realisation, na.rm = TRUE) * 100,
          .groups = "drop"
        ) %>%
        pivot_longer(cols = c(objectif_moyen, realisation_moyenne), 
                     names_to = "type", 
                     values_to = "valeur") %>%
        mutate(
          type = case_when(
            type == "objectif_moyen" ~ "Objectif",
            type == "realisation_moyenne" ~ "Réalisation",
            TRUE ~ type
          )
        )
      
      # Créer le graphique
      p <- ggplot(data_chart, aes(x = trimestre, y = valeur, color = performance, linetype = type)) +
        geom_line(size = 1.2) +
        geom_point(size = 3) +
        labs(title = "Évolution des Indicateurs par Trimestre",
             x = "Trimestre", y = "Pourcentage (%)",
             color = "Performance", linetype = "Type") +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          legend.position = "bottom"
        ) +
        scale_y_continuous(labels = function(x) paste0(x, "%")) +
        scale_x_continuous(breaks = 1:4, labels = paste0("T", 1:4)) +
        scale_color_manual(values = c("TECHNIQUE" = "navy", "BUDGETAIRE" = "darkgreen", "PPM" = "#f39c12"))
      
      ggplotly(p, tooltip = c("x", "y", "colour", "linetype"))
    }, error = function(e) {
      cat("Erreur dans trendChart:", e$message, "\n")
      plotly_empty() %>%
        layout(title = "Erreur dans le chargement du graphique d'évolution")
    })
  })
  
  # Graphique de comparaison (onglet Analytics)
  output$comparisonChart <- renderPlotly({
    req(donnees_filtrees_analytics())
    
    tryCatch({
      data <- donnees_filtrees_analytics()
      
      if (nrow(data) == 0) {
        return(plotly_empty() %>%
                 layout(title = "Aucune donnée disponible pour les filtres sélectionnés"))
      }
      
      # Debug: afficher les données pour PPM
      ppm_data <- data %>% filter(performance == "PPM")
      cat("Données PPM disponibles:\n")
      print(ppm_data)
      
      # Préparer les données pour le graphique de comparaison
      data_comp <- data %>%
        group_by(projet, performance) %>%
        summarise(
          objectif = mean(objectif, na.rm = TRUE) * 100,
          realisation = mean(realisation, na.rm = TRUE) * 100,
          .groups = "drop"
        ) %>%
        # S'assurer que les données PPM avec objectif = 0 soient incluses
        mutate(
          objectif = ifelse(is.na(objectif), 0, objectif),
          realisation = ifelse(is.na(realisation), 0, realisation)
        ) %>%
        pivot_longer(cols = c(objectif, realisation), 
                     names_to = "type", 
                     values_to = "valeur") %>%
        mutate(
          type = case_when(
            type == "objectif" ~ "Objectif",
            type == "realisation" ~ "Réalisation",
            TRUE ~ type
          ),
          projet_short = ifelse(nchar(projet) > 20, paste0(substr(projet, 1, 20), "..."), projet)
        )
      
      # Debug: afficher les données transformées pour PPM
      ppm_comp <- data_comp %>% filter(performance == "PPM")
      cat("Données PPM pour le graphique:\n")
      print(ppm_comp)
      
      # Créer le graphique
      p <- ggplot(data_comp, aes(x = projet_short, y = valeur, fill = type)) +
        geom_bar(stat = "identity", position = "dodge", alpha = 0.8, width = 0.6) +
        facet_wrap(~ performance, scales = "free_y", ncol = 1) +
        labs(title = "Comparaison Objectifs vs Réalisations par Projet",
             x = "Projet", y = "Pourcentage (%)",
             fill = "Type") +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 18, face = "bold", margin = margin(b = 30)),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "bold"),
          axis.text.y = element_text(size = 11),
          axis.title = element_text(size = 14, face = "bold"),
          axis.title.x = element_text(margin = margin(t = 20)),
          axis.title.y = element_text(margin = margin(r = 20)),
          legend.position = "bottom",
          legend.title = element_text(size = 14, face = "bold"),
          legend.text = element_text(size = 12),
          legend.margin = margin(t = 20),
          strip.text = element_text(size = 14, face = "bold", margin = margin(b = 15, t = 15)),
          panel.spacing = unit(1.5, "lines"),
          plot.margin = margin(30, 30, 30, 30),
          panel.grid.major = element_line(color = "grey90", size = 0.5),
          panel.grid.minor = element_blank()
        ) +
        scale_y_continuous(labels = function(x) paste0(x, "%"), 
                           expand = expansion(mult = c(0, 0.1))) +
        scale_fill_manual(values = c("Objectif" = "navy", "Réalisation" = "lightblue"))
      
      ggplotly(p, tooltip = c("x", "y", "fill"))
    }, error = function(e) {
      cat("Erreur dans comparisonChart:", e$message, "\n")
      plotly_empty() %>%
        layout(title = "Erreur dans le chargement du graphique de comparaison")
    })
  })
  
  # Tableau des projets (onglet Table)
  output$projectTable <- DT::renderDataTable({
    req(donnees_filtrees_table())
    
    tryCatch({
      data <- donnees_filtrees_table()
      
      if (nrow(data) == 0) {
        return(DT::datatable(data.frame(Message = "Aucune donnée disponible pour les filtres sélectionnés")))
      }
      
      # Préparer les données pour le tableau
      table_data <- data %>%
        group_by(projet, performance) %>%
        summarise(
          objectif_moyen = round(mean(objectif, na.rm = TRUE) * 100, 1),
          realisation_moyenne = round(mean(realisation, na.rm = TRUE) * 100, 1),
          ecart_moyen = round(mean(ecart, na.rm = TRUE) * 100, 1),
          taux_reussite = round(sum(statut == "Atteint", na.rm = TRUE) / n() * 100, 1),
          nb_mesures = n(),
          nb_atteints = sum(statut == "Atteint", na.rm = TRUE),
          .groups = "drop"
        ) %>%
        select(
          Projet = projet,
          Performance = performance,
          `Objectif Moyen (%)` = objectif_moyen,
          `Réalisation Moyenne (%)` = realisation_moyenne,
          `Écart Moyen (%)` = ecart_moyen,
          `Taux de Réussite (%)` = taux_reussite,
          `Nb Trimestres` = nb_mesures,
          `Nb Atteints` = nb_atteints
        )
      
      # Créer le datatable avec formatage conditionnel
      DT::datatable(table_data, 
                    options = list(
                      pageLength = 15, 
                      scrollX = TRUE,
                      dom = 'Bfrtip',
                      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                      language = list(
                        search = "Rechercher:",
                        lengthMenu = "Afficher _MENU_ entrées par page",
                        info = "Affichage de _START_ à _END_ sur _TOTAL_ entrées",
                        paginate = list(
                          first = "Premier",
                          last = "Dernier",
                          previous = "Précédent"
                        )
                      ),
                      columnDefs = list(
                        list(targets = c(2, 3, 4, 5), className = "dt-right")
                      )
                    ),
                    rownames = FALSE,
                    class = 'cell-border stripe hover') %>%
        # Formatage conditionnel pour le taux de réussite
        DT::formatStyle(
          "Taux de Réussite (%)",
          backgroundColor = DT::styleInterval(
            cuts = c(50, 80),
            values = c("#ffebee", "#fff3e0", "#e8f5e8")
          )
        ) %>%
        # Formatage conditionnel pour l'écart moyen
        DT::formatStyle(
          "Écart Moyen (%)",
          color = DT::styleInterval(
            cuts = c(-5, 5),
            values = c("#d32f2f", "#f57c00", "#388e3c")
          )
        )
    }, error = function(e) {
      cat("Erreur dans projectTable:", e$message, "\n")
      DT::datatable(data.frame(Erreur = paste("Erreur dans le chargement:", e$message)))
    })
  })
  
  # ========================================
  # SERVER ONGLET ASSISTANT IA
  # ========================================
  ai_values <- reactiveValues(
    messages = list(
      list(
        id = 1,
        type = "assistant",
        content = "Bonjour ! Je suis votre assistant IA pour le PTBA 2025. Comment puis-je vous aider ?",
        timestamp = Sys.time()
      )
    ),
    conversation_history = list(),
    excel_data = NULL,
    excel_summary = "",
    file_loaded = FALSE,
    error_message = "",
    is_loading = FALSE,
    api_key = Sys.getenv("GROQ_API_KEY", "")  # Utiliser variable d'environnement
  )
  
  # Questions suggérées
  suggested_questions <- list(
    "Questions Générales" = c(
      "C'est quoi le PUDC ?",
      "Explique-moi le PTBA 2025",
      "Que peux-tu faire pour m'aider ?"
    ),
    "Performance" = c(
      "Quel est le taux de réalisation global ?",
      "Quels sont les projets en retard ?",
      "Comment évolue la performance financière ?"
    ),
    "Analyse Financière" = c(
      "Quel est le budget total consommé ?",
      "Quels projets dépassent leur budget ?",
      "Analyse des écarts budgétaires"
    )
  )
  
  # =========================================================================
  # FONCTIONS UTILITAIRES POUR L'ASSISTANT IA
  # =========================================================================
  
  # Charger données Excel avec meilleure gestion d'erreurs
  load_pudc_data <- function() {
    tryCatch({
      excel_file_path <- "data/PUDC_SUIVI.xlsx"
      
      if (!file.exists(excel_file_path)) {
        ai_values$error_message <- paste("Fichier non trouvé:", excel_file_path)
        ai_values$file_loaded <- FALSE
        return(FALSE)
      }
      
      if (!requireNamespace("readxl", quietly = TRUE)) {
        ai_values$error_message <- "Package 'readxl' non installé"
        ai_values$file_loaded <- FALSE
        return(FALSE)
      }
      
      sheets <- readxl::excel_sheets(excel_file_path)
      excel_data <- list()
      total_rows <- 0
      
      for (sheet_name in sheets) {
        tryCatch({
          sheet_data <- readxl::read_excel(excel_file_path, sheet = sheet_name)
          if (nrow(sheet_data) > 0) {
            clean_name <- gsub("[^A-Za-z0-9_]", "_", sheet_name)
            excel_data[[clean_name]] <- sheet_data
            total_rows <- total_rows + nrow(sheet_data)
          }
        }, error = function(e) {
          ai_values$error_message <- paste("Erreur lecture feuille", sheet_name, ":", e$message)
        })
      }
      
      if (length(excel_data) > 0) {
        ai_values$excel_data <- excel_data
        ai_values$excel_summary <- paste0(
          "📊 FICHIER PUDC_SUIVI.xlsx CHARGÉ\n",
          "🗂️ Feuilles: ", length(excel_data), "\n",
          "📈 Lignes: ", format(total_rows, big.mark = " ")
        )
        ai_values$file_loaded <- TRUE
        ai_values$error_message <- ""
        return(TRUE)
      }
      
      return(FALSE)
      
    }, error = function(e) {
      ai_values$error_message <- paste("Erreur chargement données:", e$message)
      ai_values$file_loaded <- FALSE
      return(FALSE)
    })
  }
  
  # API Groq améliorée avec gestion d'erreurs détaillée
  call_groq_api <- function(message) {
    tryCatch({
      # Vérifier la clé API
      if (is.null(ai_values$api_key) || ai_values$api_key == "") {
        return("❌ Clé API Groq non configurée. Veuillez définir la variable d'environnement GROQ_API_KEY.")
      }
      
      # Vérifier httr
      if (!requireNamespace("httr", quietly = TRUE)) {
        return("❌ Package 'httr' non installé. Utilisez: install.packages('httr')")
      }
      
      # Vérifier jsonlite
      if (!requireNamespace("jsonlite", quietly = TRUE)) {
        return("❌ Package 'jsonlite' non installé. Utilisez: install.packages('jsonlite')")
      }
      
      url <- "https://api.groq.com/openai/v1/chat/completions"
      
      # Contexte enrichi avec données Excel si disponibles
      system_context <- "Tu es un assistant IA spécialisé dans l'analyse du PTBA 2025 du PUDC (Programme d'Urgence de Développement Communautaire) du Sénégal. Réponds en français de manière professionnelle et précise."
      
      if (ai_values$file_loaded && !is.null(ai_values$excel_data)) {
        system_context <- paste(system_context, 
                                "\n\nTu as accès aux données Excel PUDC_SUIVI.xlsx avec",
                                length(ai_values$excel_data), "feuilles de calcul.",
                                "Utilise ces informations pour répondre aux questions sur le PTBA 2025.")
      }
      
      body <- list(
        model = "llama-3.3-70b-versatile",
        messages = list(
          list(role = "system", content = system_context),
          list(role = "user", content = message)
        ),
        max_tokens = 800,
        temperature = 0.7,
        top_p = 0.9
      )
      
      headers <- list(
        "Authorization" = paste("Bearer", ai_values$api_key),
        "Content-Type" = "application/json"
      )
      
      ai_values$is_loading <- TRUE
      
      response <- httr::POST(
        url = url,
        body = jsonlite::toJSON(body, auto_unbox = TRUE),
        httr::add_headers(.headers = headers),
        encode = "raw",
        timeout = 30
      )
      
      ai_values$is_loading <- FALSE
      
      status_code <- httr::status_code(response)
      
      if (status_code == 200) {
        response_content <- httr::content(response, "text", encoding = "UTF-8")
        response_data <- jsonlite::fromJSON(response_content)
        
        if (!is.null(response_data$choices) && length(response_data$choices) > 0) {
          return(response_data$choices[[1]]$message$content)
        } else {
          return("❌ Réponse API invalide")
        }
        
      } else if (status_code == 401) {
        return("❌ Clé API invalide. Vérifiez votre clé Groq.")
      } else if (status_code == 429) {
        return("❌ Limite de taux dépassée. Attendez quelques minutes.")
      } else if (status_code == 500) {
        return("❌ Erreur serveur Groq. Réessayez plus tard.")
      } else {
        error_content <- httr::content(response, "text", encoding = "UTF-8")
        return(paste("❌ Erreur API (", status_code, "):", error_content))
      }
      
    }, error = function(e) {
      ai_values$is_loading <- FALSE
      if (grepl("timeout", e$message)) {
        return("❌ Timeout de l'API. Réessayez.")
      } else if (grepl("network", e$message, ignore.case = TRUE)) {
        return("❌ Problème de connexion réseau.")
      } else {
        return(paste("❌ Erreur technique:", e$message))
      }
    })
  }
  
  # Simulation locale améliorée avec analyse des données
  simulate_ai_response <- function(user_message) {
    lower_msg <- tolower(user_message)
    
    # Réponses contextuelles selon les données chargées
    if (grepl("bonjour|salut|hello", lower_msg)) {
      if (ai_values$file_loaded) {
        return("👋 Bonjour ! Je suis votre assistant IA pour le PTBA 2025. J'ai chargé les données PUDC_SUIVI.xlsx. Comment puis-je vous aider ?")
      } else {
        return("👋 Bonjour ! Je suis votre assistant IA pour le PTBA 2025. Je fonctionne en mode local (sans données Excel). Comment puis-je vous aider ?")
      }
    }
    
    if (grepl("pudc", lower_msg)) {
      return("📋 Le PUDC est le Programme d'Urgence de Développement Communautaire du Sénégal, lancé pour améliorer les conditions de vie des populations dans les zones rurales et périurbaines.")
    }
    
    if (grepl("ptba", lower_msg)) {
      return("📊 Le PTBA 2025 est le Plan de Travail et Budget Annuel pour l'année 2025 du Programme PUDC, détaillant les activités planifiées et leur financement.")
    }
    
    if (grepl("données|excel|fichier", lower_msg)) {
      if (ai_values$file_loaded) {
        return(paste("✅ Données chargées:", ai_values$excel_summary))
      } else {
        return("❌ Aucune donnée Excel chargée. Vérifiez que le fichier PUDC_SUIVI.xlsx est dans le dossier 'data/'.")
      }
    }
    
    if (grepl("performance|réalisation|taux", lower_msg)) {
      if (ai_values$file_loaded) {
        return("📈 Pour analyser la performance, j'aurais besoin d'examiner les colonnes de réalisation dans vos données Excel. Pouvez-vous me préciser quel type de performance vous intéresse ?")
      } else {
        return("📈 Pour analyser la performance, j'aurais besoin d'accéder aux données Excel PUDC_SUIVI.xlsx.")
      }
    }
    
    if (grepl("budget|financier|coût", lower_msg)) {
      if (ai_values$file_loaded) {
        return("💰 Je peux analyser les aspects financiers une fois les données chargées. Quel type d'analyse budgétaire vous intéresse ?")
      } else {
        return("💰 Pour l'analyse budgétaire, j'aurais besoin d'accéder aux données Excel PUDC_SUIVI.xlsx.")
      }
    }
    
    if (grepl("aide|aider|fonction", lower_msg)) {
      capabilities <- c(
        "📊 Analyser les données du PTBA 2025",
        "📈 Calculer les taux de réalisation",
        "💰 Analyser les budgets et dépenses",
        "🎯 Identifier les projets en retard",
        "📋 Générer des rapports de performance",
        "❓ Répondre à vos questions sur le PUDC"
      )
      return(paste("🔧 Je peux vous aider avec:\n", paste(capabilities, collapse = "\n")))
    }
    
    # Réponse par défaut
    return("💬 Merci pour votre question. Je peux vous aider avec l'analyse des données PTBA 2025. N'hésitez pas à me poser des questions spécifiques sur le PUDC, les performances, ou les budgets.")
  }
  
  # Ajouter message avec validation
  add_message <- function(content, type = "assistant") {
    if (is.null(content) || content == "") {
      content <- "Erreur: message vide"
    }
    
    new_message <- list(
      id = length(ai_values$messages) + 1,
      type = type,
      content = content,
      timestamp = Sys.time()
    )
    ai_values$messages <- append(ai_values$messages, list(new_message))
  }
  
  # =========================================================================
  # ÉVÉNEMENTS POUR L'ASSISTANT IA
  # =========================================================================
  
  # Initialisation avec feedback
  observe({
    load_result <- load_pudc_data()
  })
  
  # Bouton Envoyer avec validation
  observeEvent(input$send_message, {
    req(input$user_message)
    user_msg <- trimws(input$user_message)
    
    if (nchar(user_msg) == 0) {
      showNotification("⚠️ Message vide", type = "warning")
      return()
    }
    
    if (nchar(user_msg) > 1000) {
      showNotification("⚠️ Message trop long (max 1000 caractères)", type = "warning")
      return()
    }
    
    # Ajouter message utilisateur
    add_message(user_msg, "user")
    
    # Générer réponse IA
    ai_response <- if (nchar(ai_values$api_key) > 0) {
      call_groq_api(user_msg)
    } else {
      simulate_ai_response(user_msg)
    }
    
    add_message(ai_response, "assistant")
    
    # Vider champ
    updateTextAreaInput(session, "user_message", value = "")
  })
  
  # Effacer chat avec confirmation
  observeEvent(input$clear_chat, {
    ai_values$messages <- list(list(
      id = 1,
      type = "assistant",
      content = "🔄 Conversation effacée. Comment puis-je vous aider ?",
      timestamp = Sys.time()
    ))
    showNotification("🗑️ Conversation effacée", type = "message")
  })
  
  # Questions suggérées
  lapply(names(suggested_questions), function(category) {
    lapply(seq_along(suggested_questions[[category]]), function(i) {
      question <- suggested_questions[[category]][i]
      button_id <- paste0("suggestion_", gsub("[^A-Za-z0-9]", "_", category), "_", i)
      
      observeEvent(input[[button_id]], {
        add_message(question, "user")
        
        ai_response <- if (nchar(ai_values$api_key) > 0) {
          call_groq_api(question)
        } else {
          simulate_ai_response(question)
        }
        
        add_message(ai_response, "assistant")
      }, ignoreInit = TRUE)
    })
  })
  
  # =========================================================================
  # OUTPUTS POUR L'ASSISTANT IA
  # =========================================================================
  
  # Indicateur de statut amélioré
  output$ai_status_indicator <- renderUI({
    if (ai_values$is_loading) {
      status_text <- "🔄 Traitement en cours..."
      status_class <- "status-loading"
    } else if (ai_values$file_loaded) {
      status_text <- "✅ Données PUDC_SUIVI chargées"
      status_class <- "status-success"
    } else {
      status_text <- "⚠️ Mode local (sans données Excel)"
      status_class <- "status-warning"
    }
    
    div(style = "margin-top: 8px;",
        span(class = paste("status-indicator", status_class)),
        span(status_text, style = "font-size: 14px; color: #374151;")
    )
  })
  
  # Messages du chat amélioré
  output$chat_messages_display <- renderUI({
    if (length(ai_values$messages) == 0) {
      return(div(class = "no-messages", "Aucun message"))
    }
    
    messages_ui <- lapply(ai_values$messages, function(msg) {
      time_str <- format(msg$timestamp, "%H:%M")
      
      if (msg$type == "user") {
        div(class = "message user",
            div(class = "message-avatar", tags$i(class = "fas fa-user")),
            div(
              div(class = "message-content", 
                  style = "white-space: pre-wrap; word-wrap: break-word;",
                  msg$content),
              div(class = "message-time", style = "text-align: right;", time_str)
            )
        )
      } else {
        div(class = "message assistant",
            div(class = "message-avatar", tags$i(class = "fas fa-robot")),
            div(
              div(class = "message-content", 
                  style = "white-space: pre-wrap; word-wrap: break-word;",
                  msg$content),
              div(class = "message-time", time_str)
            )
        )
      }
    })
    
    do.call(tagList, messages_ui)
  })
  
  # Questions suggérées avec catégories
  output$suggested_questions_display <- renderUI({
    categories <- names(suggested_questions)
    
    category_elements <- lapply(categories, function(category) {
      questions <- suggested_questions[[category]]
      
      question_buttons <- lapply(seq_along(questions), function(i) {
        question <- questions[i]
        button_id <- paste0("suggestion_", gsub("[^A-Za-z0-9]", "_", category), "_", i)
        
        actionButton(
          inputId = button_id,
          label = question,
          class = "suggestion-btn btn-sm",
          style = "width: 100%; margin-bottom: 6px; text-align: left;"
        )
      })
      
      div(class = "suggestion-category",
          h6(category, style = "color: #6b7280; font-size: 12px; margin-bottom: 8px; font-weight: bold;"),
          do.call(tagList, question_buttons),
          br()
      )
    })
    
    do.call(tagList, category_elements)
  })
  
  # Historique des conversations
  output$conversation_history_display <- renderUI({
    if (length(ai_values$conversation_history) == 0) {
      div(class = "empty-state",
          tags$i(class = "fas fa-history", style = "color: #9ca3af; font-size: 24px;"),
          p("Aucune conversation sauvegardée", style = "color: #6b7280; text-align: center; margin-top: 8px;")
      )
    } else {
      p("📜 Historique disponible", style = "color: #6b7280;")
    }
  })
  
  # Statut des fichiers détaillé
  output$files_status_display <- renderUI({
    if (ai_values$file_loaded) {
      div(class = "files-status-success",
          div(style = "display: flex; align-items: center; margin-bottom: 12px;",
              tags$i(class = "fas fa-file-excel", style = "color: #10b981; margin-right: 8px;"),
              strong("PUDC_SUIVI.xlsx")
          ),
          div(style = "margin-left: 20px;",
              p(tags$strong("📁 Localisation: "), "data/"),
              p(tags$strong("📊 Feuilles: "), length(ai_values$excel_data)),
              p(tags$strong("⏱️ Chargé: "), format(Sys.time(), "%H:%M"))
          )
      )
    } else {
      div(class = "files-status-error",
          div(style = "display: flex; align-items: center; margin-bottom: 12px;",
              tags$i(class = "fas fa-exclamation-triangle", style = "color: #ef4444; margin-right: 8px;"),
              strong("Fichier non trouvé")
          ),
          div(style = "margin-left: 20px;",
              p("📁 Cherche: data/PUDC_SUIVI.xlsx", style = "color: #6b7280;"),
              if (ai_values$error_message != "") {
                p(paste("❌ Erreur:", ai_values$error_message), style = "color: #ef4444; font-size: 12px;")
              } else {
                NULL
              }
          )
      )
    }
  })
}



# Lancement de l'application
shinyApp(ui = ui, server = server)











