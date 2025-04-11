library(shiny)
library(shinyjs)
library(leaflet)
library(httr)
library(jsonlite)
library(bslib)

# Utilitaire pour valeur par défaut
`%||%` <- function(a, b) if (!is.null(a)) a else b

# Fonction API
get_city_info_from_api <- function(codpost, libcom = NULL, libvoie, code_insee = NULL) {
  url <- "https://api-adresse.data.gouv.fr/search/"
  query_params <- list(postcode = codpost, city = libcom, q = libvoie, limit = 1)
  if (!is.null(code_insee) && code_insee != "") {
    query_params$citycode <- code_insee
  }
  
  response <- GET(url, query = query_params)
  if (response$status_code == 200) {
    data <- fromJSON(content(response, "text"), simplifyVector = FALSE)
    if (length(data$features) > 0) {
      props <- data$features[[1]]$properties
      coords <- data$features[[1]]$geometry$coordinates
      return(list(
        longitude = coords[[1]],
        latitude = coords[[2]],
        label = props$label %||% "",
        name = props$name %||% "",
        city = props$city %||% "",
        postcode = props$postcode %||% "",
        insee = props$citycode %||% "",
        street = props$street %||% "",
        housenumber = props$housenumber %||% "",
        district = props$district %||% "",
        context = props$context %||% "",
        score = props$score %||% NA,
        type = props$type %||% ""
      ))
    }
  }
  return(NULL)
}

# UI
ui <- navbarPage(
  title = "Géolocalisation Adresse",
  theme = bs_theme(
    base_theme = "default",
    primary = "#6c9ef8",   # bleu pastel principal
    bg = "#f9f9fb",        # fond clair
    fg = "#2b2b2b",        # texte sombre
    success = "#9ae3c4",   # vert pastel
    info = "#8ecae6",      # bleu clair
    base_font = font_google("Inter")  # typographie moderne
  ),
  useShinyjs(),
  
  # Styles personnalisés façon ROMEO
  tags$style(HTML("
  .form-control, .selectize-input {
    font-family: Arial, sans-serif;
    font-size: 1em;
    color: #555;
    background-color: #fff;
    border: 1px solid #ccc;
    border-radius: 10px;
    padding: 8px;
    box-shadow: none;
    height: auto;
  }

  .form-control:focus, .selectize-input:focus {
    border-color: #aaa;
    outline: none;
    box-shadow: none;
  }

  ::placeholder, .form-control::placeholder, .selectize-input::placeholder {
    color: #aaa !important;
    font-family: Arial, sans-serif !important;
    font-size: 1em !important;
  }

  .btn-primary {
    background-color: #9ae3c4 !important;
    border-color: #9ae3c4 !important;
    color: #2b2b2b !important;
    border-radius: 10px !important;
  }

  .btn-primary:hover {
    background-color: #7ed6b0 !important;
    border-color: #7ed6b0 !important;
    color: #222 !important;
  }

  .well, .panel, .card, .leaflet-container {
    border-radius: 10px !important;
  }

  pre {
    border-radius: 10px;
    background-color: #f3f3f3;
    padding: 10px;
  }
"))
  ,
  
  # Accueil
  tabPanel("Accueil",
           fluidPage(
             titlePanel("Bienvenue dans l’application de géolocalisation"),
             fluidRow(
               column(8,
                      h3("Objectif de l’application"),
                      p("Cette application vous permet de localiser une adresse via l’API ouverte "),
                      a("adresse.data.gouv.fr", href = "https://adresse.data.gouv.fr/api", target = "_blank"),
                      p("Elle retourne la position géographique ainsi que des informations comme la commune, le code INSEE, et un score de fiabilité.")
               )
             )
           )
  ),
  
  # Onglet de géolocalisation
  tabPanel("Recherche géographique",
           fluidPage(
             titlePanel("Recherche d'adresse"),
             sidebarLayout(
               sidebarPanel(
                 h4("Saisir une adresse"),
                 textInput("codpost", "Code postal", "75001"),
                 textInput("libcom", "Ville (facultatif)", ""),
                 textInput("code_insee", "Code commune INSEE (prioritaire)", ""),
                 textInput("libvoie", "Adresse (voie)", "Rue de Rivoli"),
                 actionButton("go", "Rechercher", class = "btn btn-primary")
               ),
               mainPanel(
                 leafletOutput("map", height = "400px"),
                 tags$hr(),
                 h4("Informations renvoyées par l'API"),
                 verbatimTextOutput("info")
               )
             )
           )
  ),
  
  # Onglet À propos
  tabPanel("À propos",
           fluidPage(
             h3("Détails techniques"),
             p("Cette application utilise l’API de géolocalisation de "),
             a("adresse.data.gouv.fr", href = "https://adresse.data.gouv.fr", target = "_blank"),
             tags$ul(
               tags$li("App codée avec R et le framework Shiny."),
               tags$li("Utilisation de Leaflet pour la cartographie."),
               tags$li("Thème personnalisé et interface alignée avec la charte ROMEO.")
             ),
             hr(),
             h4("Auteur"),
             p("Développé par Romuald Weidmann (INSEE)."),
             p("Version 1.0 - Avril 2025")
           )
  )
)

# Serveur
server <- function(input, output, session) {
  coords <- reactiveVal(NULL)
  
  observeEvent(input$go, {
    req(input$codpost, input$libvoie)
    res <- get_city_info_from_api(input$codpost, input$libcom, input$libvoie, input$code_insee)
    coords(res)
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 2.2, lat = 46.6, zoom = 6)  # France
  })
  
  observe({
    coord <- coords()
    if (!is.null(coord)) {
      leafletProxy("map") %>%
        clearMarkers() %>%
        setView(lng = coord$longitude, lat = coord$latitude, zoom = 16) %>%
        addMarkers(lng = coord$longitude, lat = coord$latitude, popup = coord$label)
    }
  })
  
  output$info <- renderPrint({
    coord <- coords()
    
    if (is.null(coord)) {
      if (input$go == 0) {
        cat("ℹ️ Saisissez une adresse puis cliquez sur \"Rechercher\".")
      } else {
        cat("❌ Aucune donnée trouvée pour cette adresse.")
      }
      return()
    }
    
    cat(paste0(
      "📍 Adresse : ", coord$label, "\n",
      "🏙️ Ville : ", coord$city, "\n",
      "📮 Code postal : ", coord$postcode, "\n",
      "🆔 Code commune INSEE : ", coord$insee, "\n",
      "🛣️ Rue : ", ifelse(coord$street != "", coord$street, "Non fournie"), "\n",
      "🏠 Numéro : ", ifelse(coord$housenumber != "", coord$housenumber, "Non fourni"), "\n",
      "📌 Quartier : ", ifelse(coord$district != "", coord$district, "Non fourni"), "\n",
      "🗺️ Contexte : ", ifelse(coord$context != "", coord$context, "Non fourni"), "\n",
      "📏 Score : ", ifelse(!is.na(coord$score), paste0(round(coord$score * 100, 1), " %"), "Non fourni"), "\n",
      "🔍 Type : ", ifelse(coord$type != "", coord$type, "Non fourni")
    ))
  })
  
}

# Lancer l'application
shinyApp(ui, server)
