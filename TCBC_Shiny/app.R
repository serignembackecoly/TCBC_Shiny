options(warn = -1)
# Les librairies
library(shiny)
library(raster)
library(shiny.semantic)
library(shinyjs)
library(tidyverse)
library(plotly)
library(janitor)
library(sf)
library(scales)
library(ncdf4) 
library(ggspatial)
library(leaflet)
library(scales)
library(viridis) 
library(markovchain)
library(DT)


#### Traitement des données ####

# Path to local shapefile folder
local_folder <- "data/gadm41_BFA_shp"

# If local data doesn't exist, download and unzip
if (!dir.exists(local_folder)) {
  dir.create("data", showWarnings = FALSE)
  
  url <- "https://geodata.ucdavis.edu/gadm/gadm4.1/shp/gadm41_BFA_shp.zip"
  temp <- tempfile(fileext = ".zip")
  
  download.file(url, temp)
  unzip(temp, exdir = local_folder)
}

# Read the shapefile from local
shp_path <- list.files(local_folder, pattern = "gadm41_BFA_1.shp$", full.names = TRUE)
burkina <- st_read(shp_path, quiet = TRUE)

burkina <- burkina %>% rename(region = NAME_1)

burkina$region <- burkina$region %>%
  toupper() %>%
  str_replace_all("-", " ") %>%  
  str_squish() 

burkina$region[9] <- "HAUTS BASSINS"

# Optional: simplify geometries for faster plotting
burkina <- st_simplify(burkina, dTolerance = 100)

# Données historiques
precip_hist <- read.csv2("daily_precip_regions.csv", stringsAsFactors = FALSE) %>%
  clean_names() %>%
  mutate(date = as.Date(year, format = "%Y-%m-%d")) %>%
  select(-year) %>%
  pivot_longer(cols = -date, names_to = "regions", values_to = "precipitation") %>%
  mutate(regions = gsub("_", " ", str_to_sentence(regions)),
         period = "Historique",
         scenario = "Historique")

# Données futures SSP2
precip_ssp2 <- read.csv2("daily_precip_regions_ssp2.csv", stringsAsFactors = FALSE) %>%
  clean_names() %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  pivot_longer(cols = -date, names_to = "regions", values_to = "precipitation") %>%
  mutate(regions = gsub("_", " ", str_to_sentence(regions)),
         period = "Futur",
         scenario = "SSP2")

# Données futures SSP5
precip_ssp5 <- read.csv2("daily_precip_regions_ssp5.csv", stringsAsFactors = FALSE) %>%
  clean_names() %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  pivot_longer(cols = -date, names_to = "regions", values_to = "precipitation") %>%
  mutate(regions = gsub("_", " ", str_to_sentence(regions)),
         period = "Futur",
         scenario = "SSP5")

# Combinaison
precip_data <- bind_rows(precip_hist, precip_ssp2, precip_ssp5)
regions <- unique(precip_data$regions)


# Chargement des données du tcbc
  df <- suppressMessages(read_csv2("spi_tcbc_par_regions.csv", show_col_types = FALSE))
  
  df$region <- df$region %>%
    toupper() %>%
    str_replace_all("-", " ") %>%
    str_squish()
  
  seuils_region <- df %>%
    group_by(region) %>%
    summarise(
      q1 = quantile(tcbc, probs = 1/3, na.rm = TRUE),
      q2 = quantile(tcbc, probs = 2/3, na.rm = TRUE)
    )
  
  df_cat <- df %>%
    left_join(seuils_region, by = "region") %>%
    mutate(
      categorie = case_when(
        tcbc <= q1 ~ "Déficitaire",
        tcbc <= q2 ~ "Normal",
        TRUE ~ "Excédentaire"
      )
    ) %>%
    select(region, year, categorie)
  
  df_map <- burkina %>%
    left_join(df_cat, by = "region")

#### L'interface ui ####
ui <- semanticPage(
  title = "Vulnérabilité Alimentaire - Burkina Faso",
  
  #### Le style HTML et javascript ####
  useShinyjs(),
  tags$head(
    tags$script(HTML("
      $(document).ready(function() {
        // Initialize top-level tabs
        $('.top.menu .item').tab();
        $('.ui.accordion').accordion();
        
        // When 'donnees' tab is clicked
        $('.menu .item[data-tab=\"donnees\"]').on('click', function() {
          setTimeout(function() {
            // Programmatically activate the 'tendance' tab
            $('#donnees-tabset').find('.item[data-tab=\"tendance\"]').click();
          }, 10);
        });
        // When 'indices' tab is clicked
        $('.menu .item[data-tab=\"indices\"]').on('click', function() {
          setTimeout(function() {
            // Programmatically activate the 'carte' tab
            $('#indices-tabset').find('.item[data-tab=\"carte\"]').click();
          }, 5);
        });
      // When 'modelisation' tab is clicked
        $('.menu .item[data-tab=\"modelisation\"]').on('click', function() {
          setTimeout(function() {
            // Programmatically activate the 'tcbc' tab
            $('#modelisation-tabset').find('.item[data-tab=\"tcbc\"]').click();
          }, 10);
        });
      });
    ")),
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
      a(class = "active item", `data-tab` = "accueil", icon("home"), "Accueil"),
      a(class = "item", `data-tab` = "donnees", icon("database"), "Données & Méthodologie"),
      a(class = "item", `data-tab` = "indices", icon('cloud'), "Indices climatiques"),
      a(class = "item", `data-tab` = "modelisation", icon('calculator'), "Modélisation stochastique")
  ),
  
  
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
                    tags$li("Le Taux de Couverture des Besoins Céréaliers"),
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
                        tags$li("Prévoir les risques alimentaires à court et moyen terme"),
                        tags$li("Faciliter la vulgarisation des résultats de recherche")
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
              div(class = "ui container",
                  div(class = "ui raised very padded text segment",
                      h2(class = "ui center aligned header",
                         "🗺️ Zone détude:  Régions du Burkina Faso"),
                      div(class = "ui divider"),
                      leafletOutput("map", height = "500px")
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
                            href = "guide_shiny.pdf",
                            target = "#_blank",
                            style = "margin-bottom: 5px;"),
                          a(class = "ui fluid button", 
                            icon("book"), 
                            "Publications scientifiques",
                            href = "https://www.researchgate.net/profile/Serigne-Mbacke-Coly/publications")
                      )
                  )
              )
          )
      )
  ),
  

  
  #### Contenu de l'onglet données ####
 
  # En-tête
  div(class = "ui bottom attached tab segment", `data-tab` = "donnees",
         # Contenu principal
         div(class = "ui grid",
             div(class = "four wide column",
                 div(class = "ui segment",
                     h3(class = "ui header", "Filtres"),

                     div(class = "ui form",
                         field(
                           tags$label("Période"),
                           multiple_radio(
                             "period",
                             "",
                             choices = c("Historique", "Futur"),
                             selected = "Historique",
                             position = "grouped"
                           )
                         )
                     ),

                     conditionalPanel(
                       condition = "input.period == 'Futur'",
                       div(class = "ui form",
                           field(
                             tags$label("Scénario"),
                             multiple_radio(
                               "scenario",
                               "",
                               choices = c("SSP2", "SSP5"),
                               selected = "SSP2",
                               position = "grouped"
                             )
                           )
                       )
                     ),

                     # Éléments existants
                     dateRangeInput("date_range", "Période",
                                    start = "2013-01-01",
                                    end = "2014-12-31",
                                    format = "yyyy-mm-dd",
                                    language = "fr"),

                     selectInput("region", "Région administrative",
                                 choices = regions,
                                 selected = "Sahel"),

                     br(),
                     actionButton("reset", "Réinitialiser",
                                  icon = icon("refresh"),
                                  class = "ui blue button")
                 ),

                 div(class = "ui segment",
                     div(class = "ui header", icon("info red circle"), "Informations"),
                     div(class = "ui segment",
                     p("Les données historiques proviennent de ", tags$strong("CHIRPS. "),
                       "La période couverte est du 01 janvier 1981 au 31 décembre 2014","."),
                     p("Les données futures sont issues du ", tags$strong("NEX-GDDP-CMIP6. "),
                       "La période couverte est du 01 janvier 2015 au 31 décembre 2100","."),
                     p("Deux scénarios sont utilisés dans notre étude à savoir le scénrario ",
                       tags$strong("SSP2-4.5"), "et le scénario ", tags$strong("SSP5-8.5")),
                     p("Le modèle ", tags$strong("FGOALS-g3"), "(Lijuan Li et al., 2020)",
                       " a été sélectionné, car il présentait la meilleure corrélation avec les données CHIRPS.")
                     )
                 )

              ),


             # Contenu principal
             div(class = "twelve wide column",
                 tabset(
                   id = "donnees-tabset",
                   tabs = list(
                     list(menu = "Précipitations quotidiennes",
                          content = div(class = "ui segment",
                                        plotOutput("barPlot", height = "500px"),
                                        div(class = "ui info message",
                                            p("Affichage des précipitations quotidiennes en mm.")
                                        )
                          )
                     ),
                     list(menu = "Tendance mensuelle",
                          id = "tendance",
                          content = div(class = "ui segment",
                                        plotOutput("monthly_plot", height = "500px")
                          )
                     ),
                     list(menu = "Cumul des précipitations",
                          content = div(class = "ui segment",
                                        plotOutput("cumulative", height = "500px"),
                                        div(class = "ui info message",
                                            p("Affichage du cumul des précipitation en mm.")
                                        )
                          )
                     ),
                     list(menu = "Statistiques",
                          content = div(class = "ui segment",
                                        uiOutput("stats")
                          )
                     )
                   ),

                   active = "tendance"
                 ),
                 tags$script(HTML("
                        $('div[data-tab=\"donnees\"] .menu .item').tab();
                  ")),
                 br(),
                 div(class = "ui segment",
                     h4("Télécharger les données journalières de la période selectionnée"),
                     downloadButton("download", "Exporter CSV",
                                    class = "ui green button")
                 ),

             )
         )
  ),

  ### Contenu de l'onglet Indices Climatiques ####
  
  div(class = "ui bottom attached tab segment", `data-tab` = "indices",
      # Contenu principal
      div(class = "ui grid",
          div(class = "four wide column",
              div(class = "ui segment",
                  h3(class = "ui header", "Filtres"),
                  
                  uiOutput("indices_filters"),
                  br(),
                  uiOutput("scenario_ui"),
                  br(),
                  downloadButton("dlindices", "Télécharger les données de l'indice", class = "ui green button")
              ),
              div(class = "ui segment",
                  div(class = "ui header", icon("info blue circle"), "Informations"),
                  div(class = "ui segment",
          p("Le ",tags$strong("CDD (Consecutive Dry Days) "),
            "mesure la plus longue période sans pluie significative (précipitations < 1 mm),
          utile pour suivre les épisodes de sécheresse. "),
          p("Le", tags$strong(" CWD (Consecutive Wet Days)  "),
            "quantifie les jours consécutifs avec au moins 1 mm de pluie,
          permettant d’évaluer les risques liés aux périodes humides prolongées,
          comme les inondations ou certaines maladies agricoles. "),
          p("Les indices", tags$strong("R10mm"),  "et le", tags$strong("R20mm"),
            "comptent respectivement le nombre de jours avec plus de 10 mm et 20 mm de pluie.
            Le premier sert à suivre les changements dans les régimes pluviométriques,
          utiles pour les cultures sensibles à l’érosion ou à l’engorgement. 
          Le second est éssentiel pour la planification d’infrastructures 
          hydrauliques et l’analyse du risque d’inondation")
                  )
              )
          ),
          
          
          # Contenu principal
          div(class = "twelve wide column",
              tabset(
                id = "indices-tabset",
                tabs = list(
                  list(menu = "Carte",
                       id = "carte",
                       content = div(class = "ui segment",
                                     plotlyOutput("cartePlot", height = "auto")
                       )
                  ),
                  list(menu = "Série temporelle",
                       id = "temp",
                       content = div(class = "ui segment",
                                     plotlyOutput("SerieTemporelle", height = "500px")
                       )
                  )
                ),

                active = "carte"
              )
              
          )
      )
  ),
  
  

      
    
  #### Contenu fde l'onglet chaines de Markov ####
  
  div(class = "ui bottom attached tab segment", `data-tab` = "modelisation",
      # Contenu principal
      div(class = "ui grid",
          div(class = "four wide column",
              div(class = "ui segment",
                  uiOutput("parametre_ui"),
                  br(),
              ),
              div(class = "ui segment",
                  div(class = "ui header", icon("info green circle"), "Informations"),
                  div(class = "ui segment",
                      p("Les chaînes de Markov sont des modèles stochastiques
                        où l'état futur dépend uniquement de
                        l'état présent (propriété markovienne) et non des états
                        antérieurs, formalisées par une matrice de transition P
                        contenant les probabilités pij de passer de l'état i à
                        l'état j; elles permettent de modéliser les transitions
                        entre différents états de besoins céréaliers, de prévoir 
                        des séquences temporelles basées sur ces probabilités et
                        de stimuler l'évolution de scénarios de vulnérabilité à
                        travers des calculs matriciels successifs pour obtenir
                        les distributions de probabiilté futures.")
                  )
              )
          ),
          
          
          # Contenu principal
          div(class = "twelve wide column",
              tabset(
                id = "modelisation-tabset",
                tabs = list(
                  list(menu = "Représentation spatiale du TCBC",
                       id = "tcbc",
                       content = div(class = "ui segment",
                                     leafletOutput("map_plot", height = "500px")
                       )
                  ),
                  list(menu = "Dynamique régionale du TCBC",
                       id = "dynamique_tcbc",
                       content = div(
                                    div(class = "ui two column grid",
                                        div(class = "column",
                                         div(class = "ui segment",
                                             h3("Matrice de Transition"),
                                             DTOutput("matrice_transition")
                                         )
                                        ),
                                        div(class = "column",
                                         div(class = "ui segment",
                                             h3("Résultats de Prédiction"),
                                             DTOutput("tableau_prediction")
                                         )
                                        )
                                    ),
                       
                                  div(class = "ui segment",
                                     h3("Visualisation de la Séquence d'États"),
                                     plotOutput("graphique_sequence")
                                    )
                                 )
                  )
                ),
                
                active = "tcbc"
              )
              
          )
      )
  )
)

#### Le serveur ####
server <- function(input, output, session) {
  
  #### Les fonctions ####
  # Fonction pour générer le plot
  create_precipitation_plot <- function(data, region) {
    rain_palette <- c(
      "Aucune" = "#f0f0f0",
      "Légère (0-5mm)" = "gold",
      "Modérée (5-20mm)" = "green",
      "Forte (>20mm)" = "navy"
    )
    
    ggplot(data, aes(x = date, y = precipitation, fill = rain_category)) +
      geom_col(width = 0.7) +
      scale_fill_manual(values = rain_palette, name = "Intensité des précipitations") +
      scale_x_date(
        date_labels = "%d %b",
        date_breaks = "2 weeks",
        expand = expansion(mult = 0.01)
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      labs(
        title = paste("Analyse journalière des précipitations -", region),
        subtitle = "Répartition des précipitations avec classification par intensité",
        x = "Date",
        y = "Précipitation (mm)",
        caption = paste("Période du", format(min(data$date), "%d/%m/%Y"), 
                        "au", format(max(data$date), "%d/%m/%Y"))
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "gray50", size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.title = element_text(face = "bold"),
        legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.caption = element_text(color = "gray50", margin = margin(t = 10)),
        plot.margin = margin(15, 15, 15, 15)
      ) +
      guides(fill = guide_legend(nrow = 1, title.position = "top"))
  }
  
  # Calculate CDD for each year
  calculate_cdd <- function(precip_data) {
    cdd <- 0
    max_cdd <- 0
    
    for (precip in precip_data) {
      if (precip < 1) {
        cdd <- cdd + 1
        max_cdd <- max(max_cdd, cdd)
      } else {
        cdd <- 0
      }
    }
    return(max_cdd)
  }
  
  calculate_cwd <- function(data) {
    # initialiation
    cwd <- 0
    max_cwd <- 0
    
    # Calcul
    for (precip in data) {
      if (precip >= 1) {
        cwd <- cwd + 1
        max_cwd <- max(max_cwd, cwd)
      } else {
        cwd <- 0
      }
      
    }
    
    return(max_cwd)
  }
  
  # Calculate number of heavy rain days (≥20mm)
  calculate_heavy_rain <- function(precip_data) {
    sum(precip_data >= 20, na.rm = TRUE)
  }
  
    # Calculate number of moderate rain days (≥10mm)
    calculate_mod_rain <- function(precip_data) {
      sum(precip_data >= 10, na.rm = TRUE)
    }
  
  
  #### Accueil ####
  output$map <- renderLeaflet({
    leaflet(burkina) %>%
      addProviderTiles(providers$CartoDB) %>%
      addPolygons(
        fillColor = "royalblue",
        fillOpacity = 0.6,
        color = "white",
        weight = 2,
        opacity = 1,
        smoothFactor = 0.5,
        label = ~region,
        labelOptions = labelOptions(
          style = list("font-weight" = "bold", "color" = "black"),
          textsize = "14px",
          direction = "auto"
        ),
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#ffd700",
          fillOpacity = 0.8,
          bringToFront = TRUE
        )
      ) %>%
      addControl(
        html = "<div style='text-align:center;'>
                <div style='font-size:35px; color:#000;'>⬆︎</div>
                <br/>
                <div style='font-weight:bold; font-size:22px; color:#FF0020;'>N</div>
              </div>",
      position = "topright"
      ) %>%
      addScaleBar(position = "bottomright",
                  options = scaleBarOptions(imperial = FALSE)) %>% 
      setView(lng = -1.5197, lat = 12.3714, zoom = 6)
  })
  
  
  
  #### Exploration données ####
  
  # Mise à jour dynamique des dates
  observe({
    if (is.null(input$period)) return()

    if (input$period == "Historique") {
      start_date <- "2014-01-01"
      end_date <- "2014-12-31"
    } else {
      start_date <- "2026-01-01"
      end_date <- "2026-12-31"
    }

    updateDateRangeInput(session, "date_range",
                         start = start_date,
                         end = end_date,
                         min = min(precip_hist$date),
                         max = max(precip_ssp5$date))
  })


  precipitation_data <- reactive({
    # Filtrer les données
    req(input$period, input$region, input$date_range)

    if (input$period == "Historique") {
      data <- precip_data %>%
        filter(period == "Historique")
    } else {
      req(input$scenario)
      data <- precip_data %>%
        filter(period == "Futur",
               scenario == input$scenario)
    }

    data %>%
      filter(date >= input$date_range[1],
             date <= input$date_range[2],
             regions == input$region)
  })

  output$barPlot <- renderPlot({
    req(precipitation_data())
    data <- precipitation_data() %>%
      mutate(
        rain_category = case_when(
          precipitation == 0 ~ "Aucune",
          precipitation < 5 ~ "Légère (0-5mm)",
          precipitation < 20 ~ "Modérée (5-20mm)",
          TRUE ~ "Forte (>20mm)"
        ),
        date_label = format(date, "%d %b")
      )

    create_precipitation_plot(data, input$region)
  }, height = 500, res = 96)

  # Courbe mensuelle
  output$monthly_plot <- renderPlot({
    req(precipitation_data())

    # Préparer les données mensuelles
    monthly_data <- precipitation_data() %>%
      mutate(
        month = month(date, label = TRUE, abbr = FALSE),
        year = year(date)
      ) %>%
      group_by(month, year) %>%
      summarise(
        total_precip = sum(precipitation, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      group_by(month) %>%
      summarise(
        mean_precip = mean(total_precip, na.rm = TRUE),
        sd_precip = sd(total_precip, na.rm = TRUE),
        min_precip = min(total_precip, na.rm = TRUE),
        max_precip = max(total_precip, na.rm = TRUE),
        n_years = n()
      )

    # Gestion de la période
    years_range <- range(year(precipitation_data()$date), na.rm = TRUE)
    period_text <- if(years_range[1] == years_range[2]) {
      paste("Année", years_range[1])
    } else {
      paste("Période", years_range[1], "-", years_range[2])
    }

    # Création du graphique
    ggplot(monthly_data, aes(x = month)) +
      # Bande de variabilité (écart-type)
      geom_ribbon(aes(ymin = mean_precip - sd_precip,
                      ymax = mean_precip + sd_precip,
                      group = 1,  # <-- Ajout du groupe pour geom_ribbon
                      fill = "Variabilité (±1 écart-type)"),
                  alpha = 0.3) +
      # Ligne de moyenne avec groupe explicite
      geom_line(aes(y = mean_precip, group = 1, color = "Moyenne"),
                linewidth = 1.2) +
      geom_point(aes(y = mean_precip, color = "Moyenne"), size = 3) +
      # Valeurs extrêmes
      geom_point(aes(y = max_precip, color = "Maximum"), shape = 17, size = 3) +
      geom_point(aes(y = min_precip, color = "Minimum"), shape = 15, size = 3) +

      # Échelles et légendes
      scale_color_manual(
        name = "Indicateurs",
        values = c("Moyenne" = "#2185d0",
                   "Maximum" = "#e6550d",
                   "Minimum" = "#31a354"),
        guide = guide_legend(order = 1)
      ) +
      scale_fill_manual(
        name = "",
        values = c("Variabilité (±1 écart-type)" = "#9ecae1"),
        guide = guide_legend(order = 2)
      ) +
      scale_y_continuous(
        expand = expansion(mult = c(0, 0.05)),  # Réduit l'espace au-dessus seulement
        limits = c(0, max(monthly_data$max_precip) * 1.1)  # 10% de marge haute
      ) +

      # Labels et thème
      labs(
        title = paste("Précipitations mensuelles -", input$region),
        subtitle = paste("Moyenne sur", monthly_data$n_years[1], "années"),
        x = "Mois",
        y = "Précipitation (mm)",
        caption = period_text
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "gray50"),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.spacing = unit(0.5, 'cm'),
        panel.grid.major.x = element_blank(),
        plot.margin = margin(5, 15, 0, 15),  # Top, Right, Bottom, Left (en pixels)
        axis.title.x = element_text(margin = margin(t = 10)),  # Réduit l'espace sous l'axe X
        axis.text.x = element_text(vjust = 0.5, angle = 45, hjust = 1)
      )
  }, height = 500, res = 96)

  # Graphique cumulatif
  output$cumulative <- renderPlot({
    data <- precipitation_data() %>%
      arrange(date) %>%
      mutate(cumulative = cumsum(precipitation))

    ggplot(data, aes(x = date, y = cumulative)) +
      geom_line(color = "#21ba45", size = 1) +
      geom_area(fill = "#21ba45", alpha = 0.3) +
      labs(title = paste("Cumul des précipitations -", input$region),
           x = "Date", y = "Précipitation cumulée (mm)") +
      theme_minimal()
  })

  # Statistiques
  output$stats <- renderUI({
    data <- precipitation_data()

    total <- sum(data$precipitation)
    moyenne <- mean(data$precipitation)
    max_val <- max(data$precipitation)
    jours_pluie <- sum(data$precipitation > 0)

    div(
      div(class = "ui four statistics",
          div(class = "statistic",
              div(class = "value", round(total, 1)),
              div(class = "label", "Total (mm)")
          ),
          div(class = "statistic",
              div(class = "value", round(moyenne, 1)),
              div(class = "label", "Moyenne (mm/jour)")
          ),
          div(class = "statistic",
              div(class = "value", max_val),
              div(class = "label", "Maximum (mm)")
          ),
          div(class = "statistic",
              div(class = "value", jours_pluie),
              div(class = "label", "Jours de pluie")
          )
      )
    )
  })

  # Réinitialiser les filtres
  observeEvent(input$reset, {
    updateSelectInput(session, "region", selected = "Sahel")
    updateDateRangeInput(session, "date_range",
                         start = "2012-01-01",
                         end = "2014-12-31")
  })

  # Téléchargement des données
  output$download <- downloadHandler(
    filename = function() {
      paste("precipitations-", input$region, "-", input$date_range[1],"-", input$date_range[2], ".csv", sep = "")
    },
    content = function(file) {
      write.csv(precipitation_data(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  

  #### Indices ETCCDI ####
  
  # In your server, initialize the range to avoid NULL issues
  observe({
    if (is.null(input$range)) {
      updateSliderInput(session, "range", value = c(2015, 2025))
    }
  })
  
  output$indices_filters <- renderUI({
    if (input$`indices-tabset` == "carte") {
      tagList(
        selectInput("indice", "Indice climatique:",
                    choices = c("Jours secs consécutifs (CDD)" = "cdd",
                                "Jours humides consécutifs (CWD)" = "cwd",
                                "Jours de pluie >10mm (R10mm)" = "r10mm",
                                "Jours de pluie >20mm (R20mm)" = "r20mm"),
                    selected = "cdd"),
        br(),
        selectInput("annee", "Année:",
                    choices = 1981:2100,
                    selected = 1981)
      )
    } else if (input$`indices-tabset` == "temp") {
      tagList(
        selectInput("region_indice", "Région administrative",
                    choices = unique(precip_data$regions),
                    selected = "Sahel"),
        br(),
        selectInput("indice_temp", "Indice climatique:",
                    choices = c("Précipitation annuelle" = "precip",
                                "Jours secs consécutifs (CDD)" = "cdd",
                                "Jours humides consécutifs (CWD)" = "cwd",
                                "Jours de pluie >10mm (R10mm)" = "r10mm",
                                "Jours de pluie >20mm (R20mm)" = "r20mm"),
                    selected = "precip"),
        br(),
        sliderInput("year_range", "Choisir une période:\n",
                    min = 2015,
                    max = 2100,
                    value = c(2015, 2025),
                    sep = "") %>% 
          tagAppendAttributes(
            style = "
      /* Slider track (background) */
      .irs-bar {
        background: #4CAF50;
        border: 1px solid #388E3C;
      }
      
      /* Slider handle */
      .irs-handle {
        border: 3px solid #FF5722;
        background: #FF9800;
      }
      
      /* Selected range */
      .irs-from, .irs-to, .irs-single {
        background: #2196F3;
      }
      
      /* Tick marks */
      .irs-grid-text {
        font-size: 8px;
        color: #666;
      }
    "
          ),
        p("Période sélectionnée:"),
        textOutput("range_text")
      )
    }
  })
  
  output$range_text <- renderText({
    paste(input$year_range[1], "-", input$year_range[2])
  })
  
  output$scenario_ui <- renderUI({
    # Check which tab is active and get year(s)
    if (input$`indices-tabset` == "carte") {
      # Map tab - single year selection
      req(input$annee)
      year <- as.numeric(input$annee)
      show_scenario <- year >= 2015
      if (show_scenario) {
        selectInput(
          "scenario_ind",
          "Scénario climatique:",
          choices = c("SSP2"="ssp2",
                      "SSP5"="ssp5"),
          selected = "ssp2",
          width = "100%"
        )
      }
    } else {
      # Temporal tab - range selection
      req(input$year_range)
      selectInput(
        "scenario_temp",
        "Scénario climatique:",
        choices = unique(precip_data$scenario)[-1],
        selected = "SSP2",
        width = "100%"
      )
    }
  })

  # Obtenir le chemin du fichier en fonction de l'année et du scénario
  get_file_path <- reactive({
    req(input$indice, input$annee)
    
    year <- as.numeric(input$annee)
    base_path <- paste0(input$indice, "_")
    
    if (year >= 2015) {
      req(input$scenario_ind)
      paste0(base_path, input$scenario_ind, "_", year, ".nc")
    } else {
      paste0(base_path, "historical_", year, ".nc")
    }
  })
  # 
  
  # # Traitement des données climatiques
  # Chargement et traitement des données pour la carte
  climate_data <- reactive({
    nc_file <- get_file_path()
    
    tryCatch({
      if(!file.exists(nc_file)) stop(paste("Fichier", nc_file, "introuvable"))
      
      nc_data <- brick(nc_file)
      if(nlayers(nc_data) == 0) stop("Aucune couche dans le NetCDF")
      
      r <- nc_data[[1]]
      
      if(is.na(st_crs(r))) st_crs(r) <- st_crs(4326)
      
      r_crop <- crop(r, burkina)
      if(all(is.na(values(r_crop)))) stop("Aucune donnée après découpage")
      
      r_mask <- mask(r_crop, burkina)
      
      r_df <- as.data.frame(r_mask, xy = TRUE, na.rm = TRUE)
      if(nrow(r_df) == 0) stop("Aucune donnée après conversion")
      
      colnames(r_df)[3] <- "valeur"
      r_df
    }, error = function(e) {
      showNotification(paste("Erreur données:", e$message), type = "error")
      NULL
    })
  })
  # 
  # Carte interactive
  output$cartePlot <- renderPlotly({
    req(climate_data())
    
    tryCatch({
      titles <- list(
        "cdd" = "Jours secs consécutifs (CDD)",
        "cwd" = "Jours humides consécutifs (CWD)",
        "r10mm" = "Jours de pluie >10mm (R10mm)",
        "r20mm" = "Jours de pluie >20mm (R20mm)"
      )
      
      if(all(is.na(climate_data()$valeur))) stop("Toutes les valeurs sont NA")
      
      # Ajouter le scénario dans le titre si année >= 2015
      title_suffix <- if(as.numeric(input$annee) >= 2015) {
        req(input$scenario)
        paste0(" (Scénario ", toupper(input$scenario_ind), ")")
      } else {
        " (Données historiques)"
      }

      p <- ggplot() +
        
        geom_raster(data = climate_data(), aes(x = x, y = y, fill = valeur)) +
        scale_fill_gradientn(
          colors = c("#313695", "#74add1", "#ffffbf", "#f46d43", "#a50026"),
          name = titles[[input$indice]],
          limits = range(climate_data()$valeur, na.rm = TRUE),
          na.value = "transparent"
        ) +
        labs(
          title = paste0(titles[[input$indice]], " - Burkina Faso ", input$annee, title_suffix),
          x = "Longitude",
          y = "Latitude"
        ) +
        ggthemes::theme_tufte(base_size = 13) +
        theme(
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 13),
          legend.position = "right",
          legend.title = element_text(size = 12, face = "bold"),
          legend.text = element_text(size = 10)
        ) +
        geom_sf(data = burkina, fill = NA, size = 0.1, color = "black") +
        coord_sf(expand = FALSE)
      
      ggplotly(p, height = 600) %>%
        layout(
          margin = list(l = 50, r = 50, b = 50, t = 80),
          hoverlabel = list(bgcolor = "white")
        ) %>% 
        add_annotations(
          x = 0.18,
          y = 0.98,
          xref = "paper",
          yref = "paper",
          text = "⬆\nN",
          showarrow = FALSE,
          font = list(
            size = 30,
            color = "#2c3e50",
            family = "Times New Roman"
          ),
          xanchor = "right",
          yanchor = "top",
          bgcolor = "rgba(255,255,255,0.7)",  # Semi-transparent white
          bordercolor = "rgba(255,250,250,0.7)",
          borderwidth = 2,
          borderpad = 2,
          hovertext = "Flèche Nord",
          hoverlabel = list(
            bgcolor = "white",
            font = list(color = "red", size = 12)
          )
        ) 
    }, error = function(e) {
      showNotification(paste("Erreur carte:", e$message), type = "error")
      NULL
    })
  })
  ## Série temporelle
  
  # Filter data based on user inputs
  filtered_data <- reactive({
    req(input$year_range)
    req(input$scenario_temp)
    
    data <- precip_data %>%
      filter(
        lubridate::year(date) >= input$year_range[1],
        lubridate::year(date) <= input$year_range[2],
        regions == input$region_indice,
        scenario == input$scenario_temp
      ) %>%
      mutate(year = lubridate::year(date))
    
    if (input$indice_temp == "precip") {
      data <- data %>%
        group_by(year) %>%
        summarise(value = sum(precipitation, na.rm = TRUE)) %>%
        mutate(metric = "Annual Precipitation (mm)")
    } else if (input$indice_temp == "cdd") {
      data <- data %>%
        group_by(year) %>%
        summarise(value = calculate_cdd(precipitation)) %>%
        mutate(metric = "Consecutive Dry Days (days)")
    } else if (input$indice_temp == "cwd") {
      data <- data %>%
        group_by(year) %>%
        summarise(value = calculate_cwd(precipitation)) %>%
        mutate(metric = "Consecutive Wet Days (days)")
    } else if (input$indice_temp == "r10mm") {
      data <- data %>%
        group_by(year) %>%
        summarise(value = calculate_mod_rain(precipitation)) %>%
        mutate(metric = "Moderate Precipitation Days (≥10mm)")
    } else if (input$indice_temp == "r20mm") {
      data <- data %>%
        group_by(year) %>%
        summarise(value = calculate_heavy_rain(precipitation)) %>%
        mutate(metric = "Heavy Precipitation Days (≥20mm)")
    }
    
    return(data)
  })
  

  # Create plot
  output$SerieTemporelle <- renderPlotly({
    data <- filtered_data()
    req(nrow(data) > 0)
    
    # Color mapping for different metrics
    metric_colors <- c(
      "precip" = "#2c7fb8",     # Blue
      "cdd" = "#e34a33",        # Red
      "cwd" = "#2ca25f",        # Green
      "r10mm" = "#fdae6b",   # Orange
      "r20mm" = "#756bb1"   # Purple
    )
    
    # Metric names for titles
    metric_names <- c(
      "precip" = "Précipitations Annuelles",
      "cdd" = "Jours Secs Consécutifs",
      "cwd" = "Jours Humides Consécutifs",
      "r10mm" = "Jours de Précipitations Modérées (≥10mm)",
      "r20mm" = "Jours de Fortes Précipitations (≥20mm)"
    )
    
    # Common plot elements
    p <- ggplot(data, aes(x = year, y = value)) +
      geom_line(color = metric_colors[input$indice_temp], size = 1.2) +
      geom_point(color = metric_colors[input$indice_temp], size = 2.5) +
      labs(
        x = "Année",
        y = unique(data$metric),
        title = paste(metric_names[input$indice_temp], "-", toupper(input$region_indice)),
        subtitle = paste("Scénario climatique :", input$scenario_temp)
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, face = "italic", hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = "gray85", linetype = "dotted")
      )
    
    # Add smoothing only for precipitation
    if (input$indice_temp == "precip") {
      p <- p + geom_smooth(method = "loess", se = FALSE, 
                           color = "#d95f02", linetype = "dashed", size = 1)
    }
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(
        margin = list(l = 60, r = 30, b = 60, t = 80),
        hoverlabel = list(bgcolor = "white", font = list(size = 13)),
        title = list(
          text = paste0(
            "<b>", metric_names[input$indice_temp], " - ", toupper(input$region_indice), "</b><br>",
            "<sup>Scénario climatique : ", input$scenario_temp, "</sup>"
          ),
          x = 0.5
        )
      )
  })
 
  # Téléchargement des données
  output$dlindices <- downloadHandler(
    filename = function() {
      year <- input$annee
      base_name <- paste0(input$indice, "_burkina_", year)
      
      if(as.numeric(year) >= 2015) {
        req(input$scenario)
        paste0(base_name, "_", input$scenario, ".csv")
      } else {
        paste0(base_name, "_historical.csv")
      }
    },
    content = function(file) {
      req(climate_data())
      write.csv(climate_data(), file, row.names = FALSE)
    }
  )
  
  
  
  #### Modélisation chaines de Markov ####
  etats <- c("Déficitaire", "Normal", "Excédentaire")
  
  output$parametre_ui <- renderUI({
    if(input$`modelisation-tabset` == "tcbc"){
      
      tagList(
        h3(class = "ui header", "Filtres"),
              
              # Éléments existants
        selectInput("annee_tcbc", "Année:",
                    choices = unique(df$year),
                    selected = min(df$year),
                    width = "100%"),)
      
    } else {
      
      tagList(
      h3(class ="ui header", "Paramètres de projection"),
      
      selectInput("region_selectionnee", "Sélectionner une région:",
                  choices = regions_bf,
                  selected = "BOUCLE DU MOUHOUN"),
      br(),
      selectInput("etat_initial", "État initial (2022):",
                  choices = c("Déficitaire", "Normal", "Excédentaire"),
                  selected = "Normal"),
      br(),
      sliderInput("n_annees", "Nombre d'années à prédire:",
                  min = 1, max = 10, value = 5, step = 1),
      br(),
      actionButton("predire", "Prédire", class = "ui blue button")
      )
      
    }
    
  })
  # carte tcbc
  output$map_plot <- renderLeaflet({
    req(input$annee_tcbc)
    
    data_to_plot <- df_map %>% filter(year == input$annee_tcbc)
    
    # Définir une palette manuelle
    pal <- colorFactor(
      palette = c("Déficitaire" = "red",
                  "Normal" = "blue",
                  "Excédentaire" = "yellow"),
      domain = data_to_plot$categorie
    )
    
    leaflet(data_to_plot) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%  # fond de carte clair
      addPolygons(
        fillColor = ~pal(categorie),
        weight = 1,
        opacity = 1,
        color = "black",   # bordures noires
        fillOpacity = 0.7,
        label = ~region,   # nom de la région en hover
        labelOptions = labelOptions(
          style = list("font-weight" = "bold", "color" = "black"),
          textsize = "13px"
        ),
        highlightOptions = highlightOptions(
          weight = 3,
          color = "white",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) %>%
      setView(lng = -1.5197, lat = 12.3714, zoom = 7) %>% 
      addControl(
        html = "<div style='text-align:center;'>
                <div style='font-size:35px; color:#000;'>⬆︎</div>
                <br/>
                <div style='font-weight:bold; font-size:22px; color:#FF0020;'>N</div>
              </div>",
        position = "topright"
      ) %>%
      addScaleBar(position = "bottomleft",
                  options = scaleBarOptions(imperial = FALSE)) %>% 
      addLegend(
        "bottomright",
        pal = pal,
        values = ~categorie,
        title = paste0("Catégorie (", input$annee_tcbc, ")"),
        opacity = 1
      )
  })
  
  # Serie temporelle tcbc
  # Préparation des données - création des chaînes de Markov pour toutes les régions
  regions_bf <- unique(df_cat$region)
  mc_fit_list <- lapply(regions_bf, function(reg) {
    donnees_region <- df_cat %>% 
      filter(region == reg) %>%
      arrange(year)
    markovchainFit(data = donnees_region$categorie)
  })
  names(mc_fit_list) <- regions_bf
  
  # Fonction modifiée pour inclure les probabilités et l'état 'De'
  calculer_chemin_plus_probable <- function(markov_chain, etat_initial, n_annees) {
    etat_actuel <- etat_initial
    chemin <- data.frame(
      Année = integer(),
      De = character(),
      Etat_Prédit = character(),
      Probabilité = numeric(),
      stringsAsFactors = FALSE
    )
    
    mat_trans <- markov_chain@transitionMatrix
    
    for (i in 1:n_annees) {
      annee <- 2022 + i
      probas <- mat_trans[etat_actuel, ]
      etat_suivant <- names(which.max(probas))
      prob <- max(probas)
      
      chemin <- rbind(chemin, data.frame(
        Année = annee,
        De = etat_actuel,
        Etat_Prédit = etat_suivant,
        Probabilité = prob,
        stringsAsFactors = FALSE
      ))
      
      etat_actuel <- etat_suivant
    }
    
    return(chemin)
  }
  # Mise à jour de l'état initial selon la dernière observation de la région sélectionnée
  observe({
    req(input$region_selectionnee)
    donnees_region <- df_cat %>%
      filter(region == input$region_selectionnee) %>%
      arrange(year)
    
    updateSelectInput(session, "etat_initial",
                      selected = tail(donnees_region$categorie, 1))
  })
  
  # Affichage de la matrice de transition
  output$matrice_transition <- renderDataTable({
    req(input$region_selectionnee)
    
    mat <- mc_fit_list[[input$region_selectionnee]]$estimate@transitionMatrix
    
    df <- as.data.frame(mat) %>%
      mutate(across(where(is.numeric), ~ round(.x, 3))) %>%  # Arrondi à 3 décimales
      mutate(De = rownames(mat)) %>%
      select(De, everything()) %>%
      rename_with(~gsub("X", "Vers: ", .x), -De)
    
    df
  }, options = list(
    dom = 't',
    autoWidth = TRUE,
    columnDefs = list(list(className = 'dt-center', targets = "_all")),
    ordering = FALSE
  ), rownames = FALSE)
  
 
  
  # Calcul des résultats de prédiction
  prediction <- eventReactive(input$predire, {
    req(input$region_selectionnee, input$etat_initial, input$n_annees)
    
    calculer_chemin_plus_probable(
      markov_chain = mc_fit_list[[input$region_selectionnee]]$estimate,
      etat_initial = input$etat_initial,
      n_annees = input$n_annees
    )
  })
  
  # Affichage du tableau de prédiction
   output$tableau_prediction <- renderDataTable({
    pred <- prediction()
    
    pred %>%
      mutate(
        Année = as.integer(Année),
        Probabilité = scales::percent(Probabilité, accuracy = 0.1)
      ) %>%
      select(
        Année,
        `De l'État` = De,
        `État Prédit` = Etat_Prédit,
        Probabilité
      ) %>%
      arrange(Année)
  }, options = list(
    pageLength = 10,
    autoWidth = TRUE,
    dom = 't',
    columnDefs = list(list(className = 'dt-center', targets = "_all"))
  ), rownames = FALSE)
  
  
  
  # Graphique de séquence d'états céréaliers
  output$graphique_sequence <- renderPlot({
    req(input$region_selectionnee)
    
    # Données historiques pour la région sélectionnée
    historique <- df_cat %>%
      filter(region == input$region_selectionnee) %>%
      arrange(year) %>%
      select(Année = year, Etat = categorie) %>%
      mutate(Type = "Historique")
    
    pred <- prediction() %>%
      rename(Etat = Etat_Prédit) %>%
      mutate(Type = "Prédiction")
    
    bind_rows(historique, pred) %>%
      mutate(
        Année = factor(Année, levels = unique(Année)),  # Pour garder l'ordre chronologique
        Type = factor(Type, levels = c("Historique", "Prédiction"))
      ) %>%
      ggplot(aes(x = Année, y = 1, fill = Etat)) +
      geom_tile(color = "white", alpha = 0.9, height = 0.8) +
      
      # Ligne séparatrice
      geom_vline(
        xintercept = length(unique(historique$Année)) + 0.5,
        linetype = "dashed", color = "black", size = 1
      ) +
      
      # Texte d'annotation au-dessus de la zone de prédiction
      annotate(
        "text",
        x = length(unique(historique$Année)) + (input$n_annees / 2) + 0.5,
        y = 1.5,
        label = "Prédictions",
        size = 5,
        fontface = "bold"
      ) +
      
      # Couleurs personnalisées
      scale_fill_manual(
        values = c("Déficitaire" = "red", 
                   "Normal" = "yellow", 
                   "Excédentaire" = "blue")
      ) +
      
      # Titre et axes
      labs(
        title = paste("Séquence des États Céréaliers -", input$region_selectionnee),
        x = "Année", y = NULL, fill = "État"
      ) +
      
      theme_minimal(base_size = 14) +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(face = "bold")
      )
    
  })
}

shinyApp(ui, server)