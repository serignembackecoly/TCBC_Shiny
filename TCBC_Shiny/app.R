# Les librairies
library(shiny)
library(raster)
library(shiny.semantic)
library(ggplot2)
library(dplyr)
library(leaflet)
library(plotly)
library(tidyr)
library(stringr)
library(janitor)
library(lubridate)
library(sf)
library(gt)


#### Chargement des données ####
# Données historiques
precip_hist <- read.csv2("data/daily_precip_regions.csv", stringsAsFactors = FALSE) %>%
  clean_names() %>%
  mutate(date = as.Date(year, format = "%Y-%m-%d")) %>%
  select(-year) %>%
  pivot_longer(cols = -date, names_to = "regions", values_to = "precipitation") %>%
  mutate(regions = gsub("_", " ", str_to_sentence(regions)),
         period = "Historique",
         scenario = "Historique")

# Données futures SSP2
precip_ssp2 <- read.csv2("data/daily_precip_regions_ssp2.csv", stringsAsFactors = FALSE) %>%
  clean_names() %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  pivot_longer(cols = -date, names_to = "regions", values_to = "precipitation") %>%
  mutate(regions = gsub("_", " ", str_to_sentence(regions)),
         period = "Futur",
         scenario = "SSP2")

# Données futures SSP5
precip_ssp5 <- read.csv2("data/daily_precip_regions_ssp5.csv", stringsAsFactors = FALSE) %>%
  clean_names() %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  pivot_longer(cols = -date, names_to = "regions", values_to = "precipitation") %>%
  mutate(regions = gsub("_", " ", str_to_sentence(regions)),
         period = "Futur",
         scenario = "SSP5")

# Combinaison
precip_data <- bind_rows(precip_hist, precip_ssp2, precip_ssp5)
regions <- unique(precip_data$regions)


# Define UI for application that draws a histogram
ui <- semanticPage(
  title = "Vulnérabilité Alimentaire - Burkina Faso",
  
  #### Le style HTML ####
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/semantic.min.css"),
    tags$style(HTML("
      /* Styles généraux */
      body {
        background-color: #f9f9f9;
      }
      .main.container {
        margin-top: 2em;
      }
      .tab.segment {
        padding: 2em 1em;
        border-radius: 0.28571429rem;
      }
      
      /* Styles pour la carte */
      .map-container {
        position: relative;
        width: 100%;
        margin-bottom: 5px;
      }
      
      #carte_bf {
        width: 100%;
        height: auto;
        object-fit: contain;
      }
      
      
      /* Styles pour les sections */
      .doc-section {
        margin-top: 10px;
      }
      .about-section {
        margin-top: 10px;
      }
      /* Alignement vertical */
      .about-section .ui.grid .row {
        align-items: center;
      }
      
      /* Ajustement du texte */
      .about-section p {
        margin-bottom: 0;
        line-height: 1.3;
      }
      
      /* Responsive */
      @media (max-width: 768px) {
        .about-section .two.column.row {
          display: flex;
          flex-direction: column;
        }
      .about-section .column {
        width: 100% !important;
      }
    }
      @media (max-width: 992px) {
        .ui.card .image {
          height: 250px;
        }
      }
      @media (max-width: 768px) {
        .ui.card .image {
          height: 200px;
        }
      }
      @media (max-width: 480px) {
        .ui.card .image {
          height: 150px;
        }
      }
      
      /* Amélioration de l'accordéon */
      .ui.accordion .title:not(.ui) {
        color: #2185d0;
      }
    "))
  ),
  
  
  #### Barre de navigation ####
  div(class = "ui top attached tabular menu",
      a(class = "active item", `data-tab` = "accueil", icon("home"), "Accueil")), 
  
  #### Contenu de l'onglet Accueil ####
  div(class = "ui bottom attached active tab segment", `data-tab` = "accueil",
      div(class = "ui two column stackable grid",
          
          # Colonne de gauche - Texte de présentation
          div(class = "column",
              h1(class = "ui header", 
                 div(class = "content", "Modélisation de la Vulnérabilité Alimentaire"),
                 div(class = "sub header", "Taux de Couverture des Besoins Céréalier (Burkina Faso)")
              ),
              
              div(class = "ui raised segment",
                  h3(class = "ui header", "Contexte du projet"),
                  p("Cette application présente les résultats de recherche sur la modélisation de la vulnérabilité alimentaire au Burkina Faso en utilisant:"),
                  tags$ul(
                    tags$li("Les indices climatiques ETCCDI (CDD, CWD, R95pTOT, etc.)"),
                    tags$li("Le Standardized Precipitation Index (SPI)"),
                    tags$li("Les chaînes de Markov pour les prévisions")
                  ),
                  p("L'objectif est de fournir un outil interactif pour visualiser les risques climatiques et leur impact sur la sécurité alimentaire.")
              ),
              
              div(class = "ui styled fluid accordion",
                  div(class = "active title",
                      icon("dropdown"),
                      "Objectifs spécifiques"
                  ),
                  div(class = "active content",
                      tags$ol(
                        tags$li("Analyser les tendances des extrêmes climatiques"),
                        tags$li("Évaluer la vulnérabilité des différentes régions"),
                        tags$li("Prévoir les risques alimentaires à moyen terme et long terme")
                      )
                  )
              ),
              div(class = "ui raised segment",
                  h3(class = "ui header", "Contacts"),
                  p("Email : mbacke.coly@2ie-edu.org ou smcoly1@gmail.com")
                  
              ),
              div(class = "about-section",
                  div(class = "ui message",
                      div(class = "header", "À propos"),
                      div(class = "ui grid",
                          div(class = "three column row",
                              div(class = "two wide column",
                                  style = "width: 15% !important; padding-right: 10px !important;",
                                  tags$img(src = "logo2ie.png", 
                                           class = "logo", 
                                           alt = "Institut 2iE",
                                           style = "max-height: 80px;")
                              ),
                              div(class = "twelve wide column",
                                  style = "width: 70% !important;",
                                  p(style = "margin-top: 0.5em;",
                                    "Projet développé par", tags$strong("Serigne Mbacké COLY"),
                                    "sous la direction du", tags$strong("Pr Harouna Karambiri"),
                                    "et celle du",tags$strong("Pr Abdramane Guiro"),
                                    "et de l'encadrement du", tags$strong("Dr Malicki Zorom"), "et du", tags$strong("Dr Babacar Leye"), ".")
                              ),
                              div(class = "two wide column",
                                  style = "width: 15% !important; padding-right: 10px !important;",
                                  tags$img(src = "logoUnb.png", 
                                           class = "logo", 
                                           alt = "Université Nazi Boni",
                                           style = "max-height: 80px;")
                              )
                          )
                      )
                  )
              )
          ),
          
          # Colonne de droite - Visualisations
          div(class = "column",
              # Section avec entête "Zone d'étude" et icône de localisation
              div(class = "doc-section",
                  div(class = "ui segments",
                      div(class = "ui secondary segment",
                          h4(class = "ui header", 
                             icon("map marker alternate"),  # Icône de localisation
                             div(class = "content", "Zone d'étude")
                          )
                      ),
                      div(class = "ui segment",
                          # Affichage de l'image de la carte
                          imageOutput("carte_bf", height = "auto")
                      )
                  )
              ),
              
              div(class = "doc-section",
                  div(class = "ui segments",
                      div(class = "ui secondary segment",
                          h4(class = "ui header", 
                             icon("file alternate outline"),
                             div(class = "content", "Ressources documentaires")
                          )
                      ),
                      div(class = "ui segment",
                          a(class = "ui fluid button", 
                            icon("download"), 
                            "Télécharger le guide complet",
                            href = "#", style = "margin-bottom: 5px;"),
                          a(class = "ui fluid button", 
                            icon("book"), 
                            "Publications scientifiques",
                            href = "https://www.researchgate.net/profile/Serigne-Mbacke-Coly/publications")
                      )
                  )
              )
          )
      )
  )
  
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #### Accueil ####
  
  # Image de la carte du Burkina Faso
  
  output$carte_bf <- renderImage({
    list(src = "images/Carte_des_regions_Burkina_Faso.png",
         width = "100%",
         style = "max-width: 100%; height: auto;
                  overflow: hidden; object-fit: contain",
         alt = "Carte du Burkina Faso")
  }, deleteFile = FALSE)

}

# Run the application 
shinyApp(ui = ui, server = server)
