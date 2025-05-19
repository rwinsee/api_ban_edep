# Définir la liste des packages nécessaires
liste_packages <- c("shiny", "shinyjs", "leaflet", "httr", "jsonlite", "bslib", "readr", "later")

# Boucle d'installation/chargement
for (package in liste_packages) {
  if (!require(package, character.only = TRUE, quietly = TRUE)) {
    # install.packages(package)
    library(package, character.only = TRUE)
  } else {
    library(package, character.only = TRUE)
  }
}

url_cog <- "https://www.insee.fr/fr/statistiques/fichier/8377162/v_commune_2025.csv"
# library(readr)
cog_2025 <- read_delim(url_cog, delim = ",", show_col_types = FALSE)
str(cog_2025)
`%||%` <- function(a, b) {
  if (!is.null(a) && length(a) == 1 && isTRUE(a != "")) a else b
}


# Fonction API pour retour unitaire sous le bloc carte
get_city_info_from_api <- function(codpost, libcom = NULL, libvoie, code_insee = NULL) {
  url <- "https://data.geopf.fr/geocodage/search"
  
  # Construction de q
  champ_q <- libvoie %||% libcom %||% {
    if (!is.null(code_insee) && code_insee != "") {
      lib <- cog_2025$LIBELLE[cog_2025$COM == code_insee][1]
      if (!is.null(lib) && lib != "") lib else NULL
    } else {
      NULL
    }
  } %||% codpost %||% "France"
  
  champ_q <- trimws(champ_q)
  
  # Construction des paramètres
  query_params <- list(q = champ_q, limit = 1)
  if (!is.null(codpost) && codpost != "") query_params$postcode <- codpost
  if (!is.null(code_insee) && code_insee != "") {
    query_params$citycode <- code_insee
  } else if (!is.null(libcom) && libcom != "") {
    query_params$city <- libcom
  }
  
  print(query_params)
  
  response <- GET(url, query = query_params)
  if (response$status_code == 200) {
    contenu <- content(response, "text", encoding = "UTF-8")
    cat("✅ JSON brut (extrait):\n", substr(contenu, 1, 500), "\n\n")
    data <- fromJSON(contenu, simplifyVector = FALSE)
    
    if (length(data$features) > 0) {
      props <- data$features[[1]]$properties
      geometry <- data$features[[1]]$geometry
      coords <- geometry$coordinates
      
      if (!is.null(coords) && length(coords) == 2) {
        longitude <- as.numeric(coords[[1]])
        latitude  <- as.numeric(coords[[2]])
      } else {
        longitude <- NA
        latitude <- NA
      }
      
      
      return(list(
        longitude = longitude,
        latitude = latitude,
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
  url <- "https://data.geopf.fr/geocodage/search"
  
  champ_q <- libvoie %||% libcom %||% {
    if (!is.null(code_insee) && code_insee != "") {
      lib <- cog_2025$LIBELLE[cog_2025$COM == code_insee][1]
      if (!is.null(lib) && lib != "") lib else NULL
    } else {
      NULL
    }
  } %||% codpost %||% "France"
  
  champ_q <- trimws(champ_q)
  

  query_params <- list(q = champ_q, limit = 5)
  if (!is.null(codpost) && codpost != "") query_params$postcode <- codpost
  if (!is.null(code_insee) && code_insee != "") {
    query_params$citycode <- code_insee
  } else if (!is.null(libcom) && libcom != "") {
    query_params$city <- libcom
  }
  
  print(query_params)
  
  response <- GET(url, query = query_params)
  if (response$status_code == 200) {
    contenu <- content(response, "text", encoding = "UTF-8")
    cat("✅ JSON multi (extrait):\n", substr(contenu, 1, 500), "\n\n")
    data <- fromJSON(contenu, simplifyVector = FALSE)
    return(data$features)
  }
  return(NULL)
}

get_info_nominatim_multi <- function(adresse, limit = 5) {
  url <- "https://nominatim.openstreetmap.org/search"
  res <- httr::GET(
    url,
    query = list(q = adresse, format = "json", limit = limit, addressdetails = 1),
    user_agent("shiny-app/ban-edep")
  )
  
  if (res$status_code == 200) {
    contenu <- httr::content(res, as = "parsed", simplifyVector = FALSE)
    if (is.list(contenu) && length(contenu) > 0) {
      return(contenu)
    }
  }
  return(NULL)
}

# UI
ui <- navbarPage(
  title = "Géolocalisation Adresse",
theme = bs_theme(
  version = 5,
  base_font = font_google("Inter"),
  bg = "#2e2e2e",          # fond général anthracite
  fg = "#eaeaea",          # texte clair
  primary = "#9ae3c4",     # couleur primaire pour navbar active
  success = "#9ae3c4",
  info = "#8ecae6"
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

tags$style(HTML("
  body, .container-fluid, .navbar, .tab-content, .form-control,
  .panel, .well, .card, .leaflet-container {
    background-color: #2e2e2e !important;
    color: #eaeaea !important;
  }

  h1, h2, h3, h4, h5, h6, label, p, .control-label, .nav-link, .navbar-brand {
    color: #ffffff !important;
  }

  .nav-tabs > li > a, .navbar-nav > li > a {
    color: #cccccc !important;
  }

  .nav-tabs > li > a:hover, .navbar-nav > li > a:hover {
    color: #ffffff !important;
  }

  .nav-tabs > li.active > a, .navbar-nav > li.active > a,
  .nav-tabs > li.active > a:focus, .nav-tabs > li.active > a:hover {
    background-color: #2e2e2e !important;
    border-color: #9ae3c4 !important;
    color: #ffffff !important;
  }

  .form-control, .selectize-input {
    font-family: 'Inter', Arial, sans-serif;
    font-size: 1em;
    background-color: #3a3a3a !important;
    color: #ffffff !important;
    border: 1px solid #555 !important;
    border-radius: 10px !important;
    padding: 8px;
    height: auto;
  }

  .form-control:focus, .selectize-input:focus {
    border-color: #888;
    outline: none;
  }

  ::placeholder, .form-control::placeholder, .selectize-input::placeholder {
    color: #bbbbbb !important;
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

  pre {
    background-color: #444 !important;
    color: #ddd !important;
    border-radius: 10px;
    padding: 10px;
  }
  
  .pastel-box {
  background-color: #9ae3c4;
  border-radius: 10px;
  padding: 12px;
  margin-top: 10px;
  font-size: 1em;
  font-family: 'Inter', sans-serif;
  color: #2b2b2b;
  line-height: 1.6;
  box-shadow: 0 2px 4px rgba(0,0,0,0.1);
}
.selectize-input {
  position: relative !important;
  background-color: #2b2b2b !important;
  color: #f0f0f0 !important;
  border: 1px solid #555 !important;
  border-radius: 10px !important;
  padding: 8px !important;
}

.selectize-input::after {
  content: \"\";
  display: block;
  position: absolute;
  top: 50%;
  right: 12px;
  margin-top: -3px;
  width: 0;
  height: 0;
  border-left: 6px solid transparent;
  border-right: 6px solid transparent;
  border-top: 6px solid #9ae3c4;  
  pointer-events: none;
}

.selectize-dropdown {
  background-color: #2b2b2b !important;
  color: #ffffff !important;
  border: 1px solid #555 !important;
}


"))

  ,
  # Onglet de géolocalisation
  tabPanel("Recherche BAN - France",
           fluidPage(
             titlePanel("Recherche d'adresse"),
             sidebarLayout(
               
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
                 #  Boîte séparée pour le fond de carte
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
# Ajout de l'onglet international
tabPanel("Recherche OSM - Monde",
         fluidPage(
           titlePanel("Recherche internationale via Nominatim (OpenStreetMap)"),
           sidebarLayout(
             sidebarPanel(
               wellPanel(
                 h4("Saisir une adresse internationale"),
                 textInput("adresse_osm", "Adresse complète", ""),
                 actionButton("go_osm", "Rechercher", class = "btn btn-primary"),
                 div(style = "margin-top: 10px;"),
                 actionButton("reset_osm", "Réinitialiser", class = "btn btn-primary")
               )
             ),
             mainPanel(
               leafletOutput("map_osm", height = "400px"),
               tags$hr(),
               h4("Informations renvoyées par Nominatim"),
               verbatimTextOutput("info_osm")
             )
           )
         )
),
  # Onglet À propos 
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
  # Icône personnalisée verte pastel
  pastelIcon <- makeIcon(
    iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-green.png",
    iconWidth = 25, iconHeight = 41,
    iconAnchorX = 12, iconAnchorY = 41,
    shadowUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-shadow.png",
    shadowWidth = 41, shadowHeight = 41,
    shadowAnchorX = 12, shadowAnchorY = 41
  )
  
  observe({
    coord <- coords()
    res <- coord$result
    
    print(paste("longitude =", res$longitude, "| latitude =", res$latitude))  # 🔍 Ajout debug
    
    if (!is.null(res) &&
        is.numeric(res$longitude) &&
        is.numeric(res$latitude) &&
        !is.na(res$longitude) &&
        !is.na(res$latitude)) {
      
      leafletProxy("map") %>%
        clearMarkers() %>%
        setView(lng = res$longitude, lat = res$latitude, zoom = 16) %>%
        addMarkers(lng = res$longitude, lat = res$latitude, popup = res$label,
                   icon = pastelIcon)
    } else {
      leafletProxy("map") %>%
        clearMarkers() %>%
        setView(lng = 2.2, lat = 46.6, zoom = 6)
    }
  })
  
  
  
  # Initialiser l'affichage dès l'arrivée sur la page
  output$info <- renderPrint({
    coord <- coords()
    
    if (is.null(coord)) {
      return(invisible(cat(
        "👋 Bienvenue dans l’application client BAN de géolocalisation !\n",
        "🔎 Renseignez une adresse à gauche, puis appuyez sur Entrée ou cliquez sur 'Rechercher'.\n",
        "\n",
        "💡 Exemples de combinaisons possibles pour lancer une recherche :\n",
        "   • ✅ Rue + Ville            → ex. : 'place de la gare' + 'Vitry-le-François'\n",
        "   • ✅ Rue + Code postal      → ex. : 'avenue Victor Hugo' + '75016'\n",
        "   • ✅ Code commune INSEE     → ex. : '51649'\n",
        "   • ✅ Ville seule            → ex. : 'Toulouse'\n",
        "   • ✅ Code postal seul       → ex. : '13001'\n",
        "   • ✅ Rue seule              → ex. : 'impasse des Lilas'\n",
        "\n",
        "🛠️ Si plusieurs champs sont remplis, ils seront croisés pour affiner les résultats.\n"
      )))
    }
    
    
    res <- coord$result
    if (is.null(res)) {
      return(invisible(cat("❌ Aucune donnée trouvée pour cette adresse.")))
    }
    
    invisible(cat(paste0(
      "📍 Adresse : ", res$label, "\n",
      "🏙️ Ville : ", res$city, "\n",
      "📮 Code postal : ", res$postcode, "\n",
      "🆔 Code commune INSEE : ", res$insee, "\n",
      "🛣️ Rue : ", ifelse(res$street != "", res$street, "Non fournie"), "\n",
      "🏠 Numéro : ", ifelse(res$housenumber != "", res$housenumber, "Non fourni"), "\n",
      "📌 Quartier : ", ifelse(res$district != "", res$district, "Non fourni"), "\n",
      "🗺️ Contexte : ", ifelse(res$context != "", res$context, "Non fourni"), "\n",
      "📏 Score : ", ifelse(!is.na(res$score), paste0(round(res$score * 100, 1), " %"), "Non fourni"), "\n",
      "🔍 Type : ", ifelse(res$type != "", res$type, "Non fourni")
    )))
  })
  
  
  observeEvent(input$code_insee, {
    if (input$libcom == "") {
      lib <- cog_2025$LIBELLE[which(cog_2025$COM == input$code_insee)]
      if (length(lib) == 1 && !is.na(lib)) {
        updateTextInput(session, "libcom", value = lib)
      }
    }
  })
  observeEvent(input$go, {
    cat("➡️ Bouton GO cliqué\n")
    
    # 🧠 Remplir libcom depuis code INSEE si vide
    if (input$libcom == "" && input$code_insee != "") {
      lib <- cog_2025$LIBELLE[which(cog_2025$COM == input$code_insee)]
      if (length(lib) == 1 && !is.na(lib)) {
        cat("🔁 Remplissage auto de libcom depuis code_insee :", lib, "\n")
        updateTextInput(session, "libcom", value = lib)
      }
    }
    
    # ❌ Pas d’appel API si tout est vide
    if (input$libvoie == "" && input$libcom == "" && input$codpost == "" && input$code_insee == "") {
      showNotification("Veuillez remplir au moins un champ pour lancer la recherche.", type = "error")
      output$info <- renderPrint({ cat("ℹ️ L’API nécessite au moins une adresse...") })
      return()
    }
    
    # ✅ On appelle l’API avec code_insee, pas city
    res <- get_city_info_from_api(codpost = input$codpost, libcom = input$libcom, libvoie = input$libvoie, code_insee = input$code_insee)
    all <- get_city_info_from_api_multi(codpost = input$codpost, libcom = input$libcom, libvoie = input$libvoie, code_insee = input$code_insee)
    
    # Rattrapage si pas de résultat unique
    if (is.null(res) && length(all) > 0) {
      props <- all[[1]]$properties
      coords_simple <- all[[1]]$geometry$coordinates
      if (!is.null(coords_simple) && length(coords_simple) == 2) {
        longitude <- as.numeric(coords_simple[[1]])
        latitude  <- as.numeric(coords_simple[[2]])
      } else {
        longitude <- NA
        latitude <- NA
      }
      res <- list(
        longitude = longitude,
        latitude = latitude,
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
      )
      cat("✅ Résultat reconstruit depuis MULTI\n")
    }
    
    coords(list(result = res, all_results = all))
  })
  
  
  
  observeEvent(input$reset, {
    updateTextInput(session, "codpost", value = "")
    updateTextInput(session, "libcom", value = "")
    updateTextInput(session, "code_insee", value = "")
    updateTextInput(session, "libvoie", value = "")
    coords(NULL)
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      setView(lng = 2.2, lat = 46.6, zoom = 6)
    
    # 🔁 Forcer le reset de la boîte d'information
    output$info <- renderPrint({
      coord <- coords()
      if (is.null(coord)) {
        return(invisible(cat(
          "👋 Bienvenue dans l’application client BAN de géolocalisation !\n",
          "🔎 Renseignez une adresse à gauche, puis appuyez sur Entrée ou cliquez sur 'Rechercher'.\n",
          "\n",
          "💡 Exemples de combinaisons possibles pour lancer une recherche :\n",
          "   • ✅ Rue + Ville            → ex. : 'place de la gare' + 'Vitry-le-François'\n",
          "   • ✅ Rue + Code postal      → ex. : 'avenue Victor Hugo' + '75016'\n",
          "   • ✅ Code commune INSEE     → ex. : '51649'\n",
          "   • ✅ Ville seule            → ex. : 'Toulouse'\n",
          "   • ✅ Code postal seul       → ex. : '13001'\n",
          "   • ✅ Rue seule              → ex. : 'impasse des Lilas'\n",
          "\n",
          "🛠️ Si plusieurs champs sont remplis, ils seront croisés pour affiner les résultats.\n"
        )))
      }
      
      
      res <- coord$result
      if (is.null(res)) {
        return(invisible(cat("❌ Aucune donnée trouvée pour cette adresse.")))
      }
      
      invisible(cat(paste0(
        "📍 Adresse : ", res$label, "\n",
        "🏙️ Ville : ", res$city, "\n",
        "📮 Code postal : ", res$postcode, "\n",
        "🆔 Code commune INSEE : ", res$insee, "\n",
        "🛣️ Rue : ", ifelse(res$street != "", res$street, "Non fournie"), "\n",
        "🏠 Numéro : ", ifelse(res$housenumber != "", res$housenumber, "Non fourni"), "\n",
        "📌 Quartier : ", ifelse(res$district != "", res$district, "Non fourni"), "\n",
        "🗺️ Contexte : ", ifelse(res$context != "", res$context, "Non fourni"), "\n",
        "📏 Score : ", ifelse(!is.na(res$score), paste0(round(res$score * 100, 1), " %"), "Non fourni"), "\n",
        "🔍 Type : ", ifelse(res$type != "", res$type, "Non fourni")
      )))
    })
  })
  
  
  output$liste_resultats <- renderUI({
    res <- coords()
    if (is.null(res)) return(NULL)  # 🔹 Ajout clé
    
    feats <- res$all_results
    if (is.null(feats) || length(feats) == 0) {
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
    
    # labels_valides <- labels[!sapply(labels, is.null)]
    labels_valides <- labels[!sapply(labels, function(x) is.null(x) || x == "")]
    

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
  observeEvent(input$code_insee, {
    if (input$libcom == "" && input$code_insee != "") {
      lib <- cog_2025$LIBELLE[which(cog_2025$COM == input$code_insee)]
      if (length(lib) == 1 && !is.na(lib)) {
        updateTextInput(session, "libcom", value = lib)
      }
    }
  })
  

  
  
  ## Étape 3 : Serveur - logique dédiée
  output$map_osm <- renderLeaflet({
    leaflet() %>% addTiles() %>% setView(lng = 0, lat = 20, zoom = 2)
  })
  
  observeEvent(input$go_osm, {
    if (input$adresse_osm == "") {
      showNotification("Veuillez entrer une adresse étrangère.", type = "error")
      return()
    }
    
    resultats <- get_info_nominatim_multi(input$adresse_osm, limit = 5)
    
    if (is.null(resultats)) {
      output$info <- renderPrint({ cat("❌ Aucun résultat trouvé via OSM.") })
      return()
    }
    
    output$info_osm <- renderPrint({
      cat("✅ Résultats trouvés :", length(resultats), "\n\n")
      
      for (i in seq_along(resultats)) {
        res <- resultats[[i]]
        if (is.list(res) && !is.null(res$display_name) && !is.null(res$lat) && !is.null(res$lon)) {
          cat(paste0(
            "🔹 Résultat ", i, " :\n",
            "📍 ", res$display_name, "\n",
            "🌍 Lat : ", res$lat, " | Lon : ", res$lon, "\n\n"
          ))
        } else {
          cat(paste0("⚠️ Résultat ", i, " invalide ou incomplet.\n\n"))
        }
      }
    })
    
    # Centrer la carte sur le premier résultat
    res <- resultats[[1]]
    if (!is.null(res$lat) && !is.null(res$lon)) {
      leafletProxy("map_osm") %>%
        clearMarkers() %>%
        setView(lng = as.numeric(res$lon), lat = as.numeric(res$lat), zoom = 16) %>%
        addMarkers(lng = as.numeric(res$lon), lat = as.numeric(res$lat),
                   popup = res$display_name,
                   icon = pastelIcon)
    }
  })
  
  
  observeEvent(input$reset_osm, {
    updateTextInput(session, "osm_address", value = "")
    leafletProxy("map_osm") %>%
      clearMarkers() %>%
      setView(lng = 0, lat = 20, zoom = 2)
    output$info_osm <- renderPrint({ "👋 Saisissez une adresse complète à rechercher dans le monde entier." })
  })
  
}

# Lancer l'application
shinyApp(ui, server)  