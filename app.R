# Définir la liste des packages nécessaires
liste_packages <- c("shiny", "shinyjs", "leaflet", "httr", "jsonlite", "bslib")

# Boucle d'installation/chargement
for (package in liste_packages) {
  if (!require(package, character.only = TRUE, quietly = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  } else {
    library(package, character.only = TRUE)
  }
}

# Utilitaire pour valeur par défaut
`%||%` <- function(a, b) if (!is.null(a)) a else b
# Fonction API

get_city_info_from_api <- function(codpost, libcom = NULL, libvoie, code_insee = NULL) {
  # url <- "https://api-adresse.data.gouv.fr/search/"
  url <- "https://data.geopf.fr/geocodage/search"
  
  # Construire la requête dynamiquement
  query_params <- list(q = libvoie, limit = 1)
  
  if (!is.null(codpost) && codpost != "") {
    query_params$postcode <- codpost
  }
  if (!is.null(libcom) && libcom != "") {
    query_params$city <- libcom
  }
  if (!is.null(code_insee) && code_insee != "") {
    query_params$citycode <- code_insee
  }
  
  print(query_params)  # utile pour voir les requêtes générées
  
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
get_city_info_from_api_multi <- function(codpost, libcom = NULL, libvoie, code_insee = NULL) {
  # url <- "https://api-adresse.data.gouv.fr/search/"
  url <- "https://data.geopf.fr/geocodage/search"
  
  champ_q <- libvoie
  if (is.null(champ_q) || champ_q == "") {
    champ_q <- libcom %||% codpost %||% code_insee %||% "France"
  }
  
  # Initialiser la requête avec q
  query_params <- list(q = champ_q, limit = 5)
  
  # Ajouter les autres paramètres SEULEMENT si libvoie est présent
  if (!is.null(libvoie) && libvoie != "") {
    if (!is.null(codpost) && codpost != "") query_params$postcode <- codpost
    if (!is.null(libcom) && libcom != "") query_params$city <- libcom
    if (!is.null(code_insee) && code_insee != "") query_params$citycode <- code_insee
  }
  
  print(query_params)
  
  response <- GET(url, query = query_params)
  if (response$status_code == 200) {
    data <- fromJSON(content(response, "text", encoding = "UTF-8"), simplifyVector = FALSE)
    return(data$features)
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
  extendShinyjs(text = "
  document.addEventListener('keydown', function(e) {
    const active = document.activeElement;
    const idsCibles = ['codpost', 'libcom', 'code_insee', 'libvoie'];

    if (idsCibles.includes(active.id)) {
      if (e.key === 'Enter') {
        setTimeout(() => {
          Shiny.setInputValue('go', Date.now());
        }, 20);
      } else if (e.key === 'Escape') {
        setTimeout(() => {
          Shiny.setInputValue('reset', Date.now());
        }, 10);
      }
    }
  });
", functions = c())
  
  ,
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
  # Onglet de géolocalisation
  tabPanel("Recherche géographique",
           fluidPage(
             titlePanel("Recherche d'adresse"),
             sidebarLayout(
               # sidebarPanel(
               # 
               #   selectInput("fond_carte", "Fond de carte",
               #               choices = c(
               #                 "Plan (OSM)" = "osm",
               #                 "Satellite (Esri)" = "satellite",
               #                 "Carto clair" = "carto_light",
               #                 "Carto sombre" = "carto_dark",
               #                 "Relief (Esri)" = "esri_topo",
               #                 "Relief (OpenTopoMap)" = "topo"
               #               ),
               #               selected = "osm")
               #   
               #   ,
               #   h4("Saisir une adresse"),
               #   textInput("codpost", "Code postal", ""),
               #   textInput("libcom", "Ville", ""),
               #   textInput("code_insee", "Code commune INSEE", ""),
               #   textInput("libvoie", "Adresse", ""),
               #   actionButton("go", "Rechercher", class = "btn btn-primary"),
               #   div(style = "margin-top: 10px;"),
               #   actionButton("reset", "Réinitialiser", class = "btn btn-primary")
               # ),
               sidebarPanel(
                 # 🔍 Boîte de saisie d'adresse
                 wellPanel(
                   h4("Saisir une adresse"),
                   textInput("codpost", "Code postal", ""),
                   textInput("libcom", "Ville", ""),
                   textInput("code_insee", "Code commune INSEE", ""),
                   textInput("libvoie", "Adresse", ""),
                   actionButton("go", "Rechercher", class = "btn btn-primary"),
                   div(style = "margin-top: 10px;"),
                   actionButton("reset", "Réinitialiser", class = "btn btn-primary")
                 ),
                 p(),p(),
                 # 🗺️ Boîte séparée pour le fond de carte
                 wellPanel(
                   h4("Choix du fond de carte"),
                   selectInput("fond_carte", "Fond de carte",
                               choices = c(
                                 "Plan (OSM)" = "osm",
                                 "Satellite (Esri)" = "satellite",
                                 "Carto clair" = "carto_light",
                                 "Carto sombre" = "carto_dark",
                                 "Relief (Esri)" = "esri_topo"
                               ),
                               selected = "osm")
                 )
               )
               ,
               mainPanel(
                 leafletOutput("map", height = "400px"),
                 tags$hr(),
                 h4("Informations renvoyées par l'API"),
                 verbatimTextOutput("info"),
                 tags$hr(),
                 uiOutput("liste_resultats")
               )
             )
           )
  ),
    # Onglet À propos fusionné avec Accueil
  tabPanel("À propos",
           fluidPage(
             h3("À propos de l’application"),
             p("Cette application permet de localiser une adresse à partir d’un code postal, d’un nom de commune ou d’un libellé de voie. Elle repose sur le service public de géocodage proposé par ",
               a("data.geopf.fr", href = "https://data.geopf.fr/geocodage/search", target = "_blank"), "."),
             
             tags$hr(),
             
             h4("Fonctionnement technique"),
             tags$ul(
               tags$li("Interface développée en R avec le framework ", strong("Shiny"), "."),
               tags$li("Utilisation de l’API REST de l’IGN : ", a("https://data.geopf.fr/geocodage/search", href = "https://data.geopf.fr/geocodage/search", target = "_blank"), "."),
               tags$li("Cartographie assurée par le package ", strong("Leaflet"), "."),
               tags$li("Aucune donnée personnelle n’est collectée ni stockée.")
             ),
             
             tags$hr(),
             
             h4("Code source & déploiement"),
             tags$ul(
               tags$li("💻 Code source de l’application : ", 
                       a("github.com/rwinsee/api_ban_edep", href = "https://github.com/rwinsee/api_ban_edep", target = "_blank")),
               tags$li("📦 Versions et releases : ", 
                       a("github.com/rwinsee/api_ban_edep/releases", href = "https://github.com/rwinsee/api_ban_edep/releases", target = "_blank")),
               tags$li("🚀 Projet de déploiement (infrastructure) : ", 
                       a("github.com/rwinsee/api_ban_edep_deploy", href = "https://github.com/rwinsee/api_ban_edep_deploy", target = "_blank")),
               tags$li("🐳 Image Docker disponible sur Docker Hub : ", 
                       a("rwinsee/app_shiny_ban", href = "https://hub.docker.com/r/rwinsee/app_shiny_ban/tags", target = "_blank"))
             ),
             
             tags$hr(),
             
             h4("Auteur"),
             p("Développé par ", strong("Romuald Weidmann"), " (INSEE)."),
             p("Version 0.0.2"),
             p(em("Dernière mise à jour : 12 mai 2025")),
             
             tags$hr(),
             
             h4("Glossaire"),
             tags$ul(
               tags$li(strong("SIG :"), " Système d’information géographique. Outils permettant d’analyser, représenter et croiser des données géographiques."),
               tags$li(strong("ESRI :"), " Entreprise spécialisée dans les SIG, éditrice du logiciel ArcGIS. Fournit de nombreux fonds de carte, notamment satellitaires."),
               tags$li(strong("IGN :"), " Institut national de l'information géographique et forestière. Fournit des données géographiques publiques via ", a("data.geopf.fr", href = "https://data.geopf.fr", target = "_blank"), "."),
               tags$li(strong("OSM :"), " OpenStreetMap. Projet collaboratif de cartographie libre, utilisé ici comme fond cartographique par défaut."),
               tags$li(strong("Fonds de carte :"), " Représentation visuelle du fond cartographique (plan, satellite, topographie, etc.) sélectionnable par l'utilisateur.")
             )
           )
  )
  
  
)


# Serveur
server <- function(input, output, session) {
  coords <- reactiveVal(NULL)
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(group = "osm") %>%
      setView(lng = 2.2, lat = 46.6, zoom = 6)
  })
  
  observe({
    coord <- coords()
    
    if (!is.null(coord) &&
        !is.null(coord$result) &&
        !is.null(coord$result$longitude) &&
        !is.null(coord$result$latitude)) {
      
      leafletProxy("map") %>%
        clearMarkers() %>%
        setView(lng = coord$result$longitude, lat = coord$result$latitude, zoom = 16) %>%
        addMarkers(lng = coord$result$longitude, lat = coord$result$latitude, popup = coord$result$label)
    } else {
      # Efface les anciens marqueurs si aucun résultat
      leafletProxy("map") %>%
        clearMarkers() %>%
        setView(lng = 2.2, lat = 46.6, zoom = 6)
    }
  })
  
  
  output$info <- renderPrint({
    coord <- coords()
    
    if (is.null(coord$result)) {
      if (input$go == 0) {
        cat("ℹ️ Saisissez une adresse puis cliquez sur \"Rechercher\".")
      } else {
        cat("❌ Aucune donnée trouvée pour cette adresse.")
      }
      return()
    }
    
    cat(paste0(
      "📍 Adresse : ", coord$result$label, "\n",
      "🏙️ Ville : ", coord$result$city, "\n",
      "📮 Code postal : ", coord$result$postcode, "\n",
      "🆔 Code commune INSEE : ", coord$result$insee, "\n",
      "🛣️ Rue : ", ifelse(coord$result$street != "", coord$result$street, "Non fournie"), "\n",
      "🏠 Numéro : ", ifelse(coord$result$housenumber != "", coord$result$housenumber, "Non fourni"), "\n",
      "📌 Quartier : ", ifelse(coord$result$district != "", coord$result$district, "Non fourni"), "\n",
      "🗺️ Contexte : ", ifelse(coord$result$context != "", coord$result$context, "Non fourni"), "\n",
      "📏 Score : ", ifelse(!is.na(coord$result$score), paste0(round(coord$result$score * 100, 1), " %"), "Non fourni"), "\n",
      "🔍 Type : ", ifelse(coord$result$type != "", coord$result$type, "Non fourni")
    ))
  })
  
  
  observeEvent(input$reset, {
    updateTextInput(session, "codpost", value = "")
    updateTextInput(session, "libcom", value = "")
    updateTextInput(session, "code_insee", value = "")
    updateTextInput(session, "libvoie", value = "")
    coords(NULL)  # Réinitialise la valeur stockée
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      setView(lng = 2.2, lat = 46.6, zoom = 6)  # Retour à la vue France
  })
  
  output$liste_resultats <- renderUI({
    res <- coords()
    feats <- res$all_results
    
    cat("----- DEBUG : début liste_resultats -----\n")
    cat("Nb total de résultats bruts:", length(feats), "\n")
    
    if (is.null(feats) || length(feats) == 0) {
      cat("⚠️ Aucun résultat retourné par l'API.\n")
      return(tags$div(class = "alert alert-warning", "❌ Aucun résultat trouvé."))
    }
    
    labels <- lapply(feats, function(f) {
      if (!is.null(f$properties$label)) {
        type <- f$properties$type %||% ""
        label <- f$properties$label
        insee <- f$properties$citycode %||% ""
        cp <- f$properties$postcode %||% ""
        score <- f$properties$score %||% NA
        score_txt <- if (!is.na(score)) paste0(" - Score : ", round(score * 100, 1), " %") else ""
        
        if (type == "municipality" && insee != "" && cp != "") {
          paste0(label, " (Depcom : ", insee, ", CP : ", cp, ")", score_txt)
        } else {
          paste0(label, score_txt)
        }
      } else {
        NULL
      }
    })
    
    labels_valides <- labels[!sapply(labels, is.null)]
    
    cat("Nb de résultats valides:", length(labels_valides), "\n")
    cat("Liste des labels valides :\n")
    print(labels_valides)
    cat("----- DEBUG : fin liste_resultats -----\n")
    
    titre <- if (length(labels_valides) == 1) {
      "✅ Un seul résultat trouvé :"
    } else {
      paste0("✅ ", length(labels_valides), " résultats trouvés :")
    }
    
    liste <- lapply(labels_valides, tags$li)
    
    wellPanel(
      tags$h5(titre),
      tags$ul(liste)
    )
  })
  observeEvent(input$go, {
    if (input$libvoie == "" && input$libcom == "" && input$codpost == "" && input$code_insee == "") {
      showNotification("Veuillez remplir au moins un champ pour lancer la recherche.", type = "error")
      return()
    }
    
    res <- get_city_info_from_api(input$codpost, input$libcom, input$libvoie, input$code_insee)
    coords(list(
      result = res,
      all_results = get_city_info_from_api_multi(input$codpost, input$libcom, input$libvoie, input$code_insee)
    ))
  })
  
  observeEvent(input$fond_carte, {
    proxy <- leafletProxy("map")
    proxy %>% clearTiles()
    
    switch(input$fond_carte,
           "osm" = proxy %>% addTiles(),
           "satellite" = proxy %>% addProviderTiles("Esri.WorldImagery"),
           "carto_light" = proxy %>% addProviderTiles("CartoDB.Positron"),
           "carto_dark" = proxy %>% addProviderTiles("CartoDB.DarkMatter"),
           "topo" = proxy %>% addProviderTiles("OpenTopoMap")
    )
  })
  
  
}

# Lancer l'application
shinyApp(ui, server)
