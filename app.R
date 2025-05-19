# charger les packages nécessaires à l'application (interface shiny, carte leaflet, appels http, etc.)
liste_packages <- c("shiny", "shinyjs", "leaflet", "httr", "jsonlite", "bslib", "readr", "later")

# tester si chaque package est installé ; si oui, charger
for (package in liste_packages) {
  if (!require(package, character.only = TRUE, quietly = TRUE)) {
    # si non installé, possibilité de l’installer (ligne commentée ici)
    library(package, character.only = TRUE)
  } else {
    library(package, character.only = TRUE)
  }
}

# définir l'url vers le fichier officiel insee des communes (version 2025)
url_cog <- "https://www.insee.fr/fr/statistiques/fichier/8377162/v_commune_2025.csv"

# importer ce fichier comme tableau (dataframe) ; ignorer les types de colonnes lors du chargement
cog_2025 <- read_delim(url_cog, delim = ",", show_col_types = FALSE)

# afficher brièvement la structure de la table pour contrôle
str(cog_2025)

# définir un opérateur personnalisé : retourne a si non nul et non vide, sinon b
`%||%` <- function(a, b) {
  if (!is.null(a) && length(a) == 1 && isTRUE(a != "")) a else b
}

# fonction pour interroger l’api ban (geopf) et extraire une réponse unique
get_city_info_from_api <- function(codpost, libcom = NULL, libvoie, code_insee = NULL) {
  url <- "https://data.geopf.fr/geocodage/search"
  
  # construire le champ principal de recherche (q)
  # priorité à l’adresse, puis ville, puis code insee ou code postal
  champ_q <- libvoie %||% libcom %||% {
    if (!is.null(code_insee) && code_insee != "") {
      lib <- cog_2025$LIBELLE[cog_2025$COM == code_insee][1]
      if (!is.null(lib) && lib != "") lib else NULL
    } else {
      NULL
    }
  } %||% codpost %||% "France"
  
  # retirer les espaces superflus dans la requête
  champ_q <- trimws(champ_q)
  
  
  # construire les paramètres de la requête http envoyée à l’api
  query_params <- list(q = champ_q, limit = 1)
  
  # si un code postal est fourni, l'ajouter à la requête
  if (!is.null(codpost) && codpost != "") query_params$postcode <- codpost
  
  # si un code insee est fourni, l'ajouter aussi
  if (!is.null(code_insee) && code_insee != "") {
    query_params$citycode <- code_insee
    # sinon, si une ville est fournie, utiliser le nom de ville
  } else if (!is.null(libcom) && libcom != "") {
    query_params$city <- libcom
  }
  
  # afficher les paramètres construits (utile pour débogage)
  print(query_params)
  
  # envoyer la requête http avec les paramètres définis
  response <- GET(url, query = query_params)
  
  # si la réponse est correcte (code 200), traiter le contenu
  if (response$status_code == 200) {
    contenu <- content(response, "text", encoding = "UTF-8")
    
    # afficher les premiers caractères du json reçu (à titre informatif)
    cat("✅ JSON brut (extrait):\n", substr(contenu, 1, 500), "\n\n")
    
    # transformer le texte json en liste R sans simplification forcée
    data <- fromJSON(contenu, simplifyVector = FALSE)
    
    # si au moins une réponse est trouvée dans le champ "features"
    if (length(data$features) > 0) {
      props <- data$features[[1]]$properties        # extraire les propriétés de l'adresse
      geometry <- data$features[[1]]$geometry        # extraire les coordonnées
      coords <- geometry$coordinates                 # latitude et longitude
      
      # tester si les coordonnées sont bien présentes
      if (!is.null(coords) && length(coords) == 2) {
        longitude <- as.numeric(coords[[1]])
        latitude  <- as.numeric(coords[[2]])
      } else {
        longitude <- NA
        latitude <- NA
      }
      
      # retourner un objet contenant les informations extraites
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
  
  # si aucun résultat ou erreur, retourner null
  return(NULL)
}

# fonction pour interroger l'api ign (ban) et récupérer plusieurs résultats possibles
get_city_info_from_api_multi <- function(codpost, libcom = NULL, libvoie, code_insee = NULL) {
  url <- "https://data.geopf.fr/geocodage/search"  # url de l’api
  
  # définir la chaîne de recherche prioritaire (champ 'q') selon les infos disponibles
  champ_q <- libvoie %||% libcom %||% {
    if (!is.null(code_insee) && code_insee != "") {
      lib <- cog_2025$LIBELLE[cog_2025$COM == code_insee][1]  # chercher le libellé de la commune
      if (!is.null(lib) && lib != "") lib else NULL
    } else {
      NULL
    }
  } %||% codpost %||% "France"
  
  # nettoyer les espaces en trop
  champ_q <- trimws(champ_q)
  
  # construire les paramètres de requête pour demander jusqu’à 5 résultats
  query_params <- list(q = champ_q, limit = 5)
  
  # ajouter le code postal si renseigné
  if (!is.null(codpost) && codpost != "") query_params$postcode <- codpost
  
  # ajouter soit le code commune insee, soit le nom de ville si présent
  if (!is.null(code_insee) && code_insee != "") {
    query_params$citycode <- code_insee
  } else if (!is.null(libcom) && libcom != "") {
    query_params$city <- libcom
  }
  
  # afficher les paramètres pour vérifier la requête
  print(query_params)
  
  # envoyer la requête http
  response <- GET(url, query = query_params)
  
  # si réponse correcte (code 200), lire le contenu
  if (response$status_code == 200) {
    contenu <- content(response, "text", encoding = "UTF-8")
    cat("✅ JSON multi (extrait):\n", substr(contenu, 1, 500), "\n\n")  # afficher un aperçu
    data <- fromJSON(contenu, simplifyVector = FALSE)  # convertir le json en liste R
    return(data$features)  # retourner uniquement les objets "features"
  }
  
  # sinon retourner null
  return(NULL)
}

# fonction pour interroger nominatim (osm) et récupérer plusieurs résultats internationaux
get_info_nominatim_multi <- function(adresse, limit = 5) {
  url <- "https://nominatim.openstreetmap.org/search"  # url de l’api osm
  
  # envoyer une requête http avec l'adresse à chercher
  res <- httr::GET(
    url,
    query = list(
      q = adresse,                  # champ adresse complet
      format = "json",             # format attendu
      limit = limit,               # nombre de résultats maximum
      addressdetails = 1           # inclure les détails d'adresse
    ),
    user_agent("shiny-app/ban-edep")  # user-agent personnalisé pour nominatim
  )
  
  # si réponse correcte
  if (res$status_code == 200) {
    contenu <- httr::content(res, as = "parsed", simplifyVector = FALSE)
    
    # vérifier que la réponse est une liste non vide
    if (is.list(contenu) && length(contenu) > 0) {
      return(contenu)  # retourner la liste complète des résultats
    }
  }
  
  # en cas d’erreur ou pas de résultat
  return(NULL)
}

# UI
# ui principal structuré avec une barre de navigation
ui <- navbarPage(
  title = "Géolocalisation Adresse",  # titre affiché dans la barre de navigation
  
  # thème graphique personnalisé avec couleurs sombres et police inter
  theme = bs_theme(
    version = 5,
    base_font = font_google("Inter"),
    bg = "#2e2e2e",       # couleur de fond principale (anthracite)
    fg = "#eaeaea",       # couleur du texte (clair)
    primary = "#9ae3c4",  # couleur pour les onglets actifs et éléments principaux
    success = "#9ae3c4",  # couleur de succès harmonisée
    info = "#8ecae6"      # couleur informative pour éléments secondaires
  ),
  
  # activer shinyjs (pour interactions javascript dans l’interface)
  useShinyjs(),
  
  # injection de javascript personnalisé via extendShinyjs
  extendShinyjs(
    text = "
      document.addEventListener('keydown', function(e) {
        const active = document.activeElement;  // champ actuellement sélectionné
        const id = active.id;

        // détection de la touche entrée
        if (e.key === 'Enter') {
          if (id === 'adresse_osm') {
            setTimeout(() => {
              Shiny.setInputValue('go_osm', Date.now());  // déclenche le bouton recherche OSM
            }, 20);
          } else if (['codpost', 'libcom', 'code_insee', 'libvoie'].includes(id)) {
            setTimeout(() => {
              Shiny.setInputValue('go', Date.now());  // déclenche le bouton recherche BAN
            }, 20);
          }
        }

        // détection de la touche échappement
        if (e.key === 'Escape') {
          if (id === 'adresse_osm') {
            setTimeout(() => {
              Shiny.setInputValue('reset_osm', Date.now());  // déclenche le bouton reset OSM
            }, 10);
          } else if (['codpost', 'libcom', 'code_insee', 'libvoie'].includes(id)) {
            setTimeout(() => {
              Shiny.setInputValue('reset', Date.now());  // déclenche le bouton reset BAN
            }, 10);
          }
        }
      });
    ",
    functions = c()  # aucune fonction js déclarée côté R ici
  )
  ,

tags$style(HTML("

  /* style général des éléments de structure */
  body, .container-fluid, .navbar, .tab-content, .form-control,
  .panel, .well, .card, .leaflet-container {
    background-color: #2e2e2e !important;  /* fond anthracite */
    color: #eaeaea !important;              /* texte clair */
  }

  /* couleur blanche pour les titres, étiquettes et textes */
  h1, h2, h3, h4, h5, h6, label, p, .control-label, .nav-link, .navbar-brand {
    color: #ffffff !important;
  }

  /* couleur gris clair par défaut pour les liens d’onglet/navigation */
  .nav-tabs > li > a, .navbar-nav > li > a {
    color: #cccccc !important;
  }

  /* couleur blanche au survol des liens */
  .nav-tabs > li > a:hover, .navbar-nav > li > a:hover {
    color: #ffffff !important;
  }

  /* style actif pour les onglets sélectionnés */
  .nav-tabs > li.active > a, .navbar-nav > li.active > a,
  .nav-tabs > li.active > a:focus, .nav-tabs > li.active > a:hover {
    background-color: #2e2e2e !important;  /* même fond que global */
    border-color: #9ae3c4 !important;      /* bordure pastel */
    color: #ffffff !important;
  }

   /* style des champs de formulaire (textInput, selectInput, etc.) */
  .form-control, .selectize-input {
    font-family: 'inter', arial, sans-serif;
    font-size: 1em;
    background-color: #3a3a3a !important;  /* fond gris foncé */
    color: #ffffff !important;             /* texte blanc */
    border: 1px solid #555 !important;     /* bordure grise */
    border-radius: 10px !important;
    padding: 8px;
    height: auto;
  }

  /* style des champs en focus (bordure + suppression contour bleu natif) */
  .form-control:focus, .selectize-input:focus {
    border-color: #888;
    outline: none;
  }

  /* style des placeholders (texte indicatif dans les champs vides) */
  ::placeholder, .form-control::placeholder, .selectize-input::placeholder {
    color: #bbbbbb !important;
  }

  /* style des boutons principaux */
  .btn-primary {
    background-color: #9ae3c4 !important;  /* vert pastel */
    border-color: #9ae3c4 !important;
    color: #2b2b2b !important;             /* texte sombre */
    border-radius: 10px !important;
  }

  /* style des boutons au survol */
  .btn-primary:hover {
    background-color: #7ed6b0 !important;
    border-color: #7ed6b0 !important;
    color: #222 !important;
  }

  /* style des blocs de texte brut (output$info, etc.) */
  pre {
    background-color: #444 !important;
    color: #ddd !important;
    border-radius: 10px;
    padding: 10px;
  }

  /* style des encadrés vert pastel (résultats, messages, etc.) */
  .pastel-box {
    background-color: #9ae3c4;
    border-radius: 10px;
    padding: 12px;
    margin-top: 10px;
    font-size: 1em;
    font-family: 'inter', sans-serif;
    color: #2b2b2b;  /* texte sombre lisible sur fond clair */
    line-height: 1.6;
    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
  }

  /* style des coordonnées dans les encadrés pastel */
  .pastel-box .coordonnees {
    color: #2b2b2b !important;
    font-weight: 500;
  }

  /* style des champs selectize (sélecteurs avec autocomplétion) */
  .selectize-input {
    position: relative !important;
    background-color: #2b2b2b !important;
    color: #f0f0f0 !important;
    border: 1px solid #555 !important;
    border-radius: 10px !important;
    padding: 8px !important;
  }

  /* flèche personnalisée dans les menus déroulants selectize */
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

  /* menu déroulant selectize (fond + bordure) */
  .selectize-dropdown {
    background-color: #2b2b2b !important;
    color: #ffffff !important;
    border: 1px solid #555 !important;
  }

  /* style des liens dans les encadrés osm (résultats cliquables) */
  .osm-link {
    color: #2b2b2b !important;
    font-weight: 500;
    text-decoration: underline;
    cursor: pointer;
  }

"))
  ,
# onglet principal pour la recherche d'adresses en france via api ban (ign)
tabPanel("Recherche BAN - France",
         
         fluidPage(
           
           # titre principal en haut de la page
           titlePanel("Recherche d'adresse"),
           
           # mise en page en deux colonnes : panneau latéral + panneau principal
           sidebarLayout(
             
             # panneau latéral à gauche
             sidebarPanel(
               
               # encadré pour les champs de saisie de l'adresse
               wellPanel(
                 h4("Saisir une adresse"),  # titre de l'encadré
                 
                 textInput("codpost", "Code postal", ""),        # champ code postal
                 textInput("libcom", "Ville", ""),               # champ nom de commune
                 textInput("code_insee", "Code commune INSEE", ""),  # champ code insee
                 textInput("libvoie", "Adresse", ""),            # champ voie (rue)
                 
                 actionButton("go", "Rechercher", class = "btn btn-primary"),  # bouton de recherche
                 div(style = "margin-top: 10px;"),                            # espacement vertical
                 actionButton("reset", "Réinitialiser", class = "btn btn-primary")  # bouton de réinitialisation
               ),
               
               p(), p(),  # espaces verticaux supplémentaires
               
               # encadré séparé pour le choix du fond de carte
               wellPanel(
                 h4("Choix du fond de carte"),  # titre
                 
                 selectInput(
                   "fond_carte",                     # identifiant de l'input
                   "Fond de carte",                  # étiquette
                   choices = c(                      # liste des options disponibles
                     "Plan (OSM)" = "osm",
                     "Satellite (Esri)" = "satellite",
                     "Carto clair" = "carto_light",
                     "Carto sombre" = "carto_dark",
                     "Relief (Esri)" = "esri_topo"
                   ),
                   selected = "osm"                  # valeur par défaut
                 )
               )
             ),
             
             # panneau principal à droite
             mainPanel(
               leafletOutput("map", height = "400px"),  # carte leaflet affichée
               tags$hr(),                               # ligne de séparation
               h4("Informations renvoyées par l'API"),  # titre des résultats
               uiOutput("info"),                        # affichage de l'adresse détaillée
               tags$hr(),
               uiOutput("liste_resultats")              # affichage des suggestions alternatives
             )
           )
         )
),
# ajout de l'onglet dédié à la recherche mondiale via nominatim (openstreetmap)
tabPanel("Recherche OSM - Monde",
         
         fluidPage(
           
           # titre affiché en haut de l'interface
           titlePanel("Recherche internationale via Nominatim (OpenStreetMap)"),
           
           # disposition en deux colonnes : gauche (inputs), droite (résultats)
           sidebarLayout(
             
             # colonne de gauche contenant les champs de saisie
             sidebarPanel(
               
               # encadré principal pour la recherche
               wellPanel(
                 h4("Saisir une adresse internationale"),   # titre de la boîte
                 
                 textInput("adresse_osm", "Adresse complète", ""),  # champ d'adresse libre
                 
                 actionButton("go_osm", "Rechercher", class = "btn btn-primary"),  # bouton pour lancer la recherche
                 
                 div(style = "margin-top: 10px;"),  # espacement vertical
                 
                 actionButton("reset_osm", "Réinitialiser", class = "btn btn-primary")  # bouton de remise à zéro
               ),
               
               # encadré séparé pour le choix du fond de carte
               wellPanel(
                 h4("Choix du fond de carte"),  # titre
                 
                 selectInput("fond_carte_osm", "Fond de carte",   # liste déroulante des fonds
                             choices = c(
                               "Plan (OSM)" = "osm",
                               "Satellite (Esri)" = "satellite",
                               "Carto clair" = "carto_light",
                               "Carto sombre" = "carto_dark",
                               "Relief (Esri)" = "esri_topo"
                             ),
                             selected = "osm")  # valeur sélectionnée par défaut
               )
             ),
             
             # colonne de droite contenant la carte et les résultats
             mainPanel(
               leafletOutput("map_osm", height = "400px"),  # affichage de la carte leaflet
               tags$hr(),                                   # ligne de séparation visuelle
               h4("Informations renvoyées par Nominatim"),  # titre des résultats
               uiOutput("info_osm")                         # affichage dynamique des adresses retournées
             )
           )
         )
),

  # Onglet À propos 
tabPanel("À propos",
         fluidPage(
           h3("À propos de l’application"),
           p("Cette application permet de localiser une adresse à partir d’un code postal, d’un nom de commune ou d’un libellé de voie, en France ou à l’international."),
           
           tags$hr(),
           
           h4("📍 Géolocalisation en France - Service BAN"),
           p("La recherche nationale repose sur le ", a("service public de géocodage", href = "https://data.geopf.fr/geocodage/search", target = "_blank"), 
             " proposé par l’IGN via la Base Adresse Nationale (BAN)."),
           p("Il est possible d’utiliser un ou plusieurs champs suivants : code postal, nom de commune, code INSEE, libellé de voie."),
           p("Un score de pertinence est affiché pour indiquer la confiance de la réponse."),
           
           tags$hr(),
           
           h4("🌍 Géolocalisation internationale - OpenStreetMap"),
           p("La recherche internationale repose sur le moteur ", strong("Nominatim"), " du projet libre ", 
             a("OpenStreetMap", href = "https://nominatim.openstreetmap.org", target = "_blank"), "."),
           p("Il suffit de saisir une adresse complète, par exemple :"),
           tags$ul(
             tags$li("🇵🇱 5 Konstytucji 3 Maja, Grajewo, Pologne"),
             tags$li("🇮🇹 Piazza San Marco, Venice, Italy"),
             tags$li("🇬🇧 10 Downing Street, London, UK")
           ),
           p("Plusieurs résultats peuvent être retournés, chacun cliquable pour être localisé sur la carte."),
           
           tags$hr(),
           
           h4("⚙️ Fonctionnement technique"),
           tags$ul(
             tags$li("Interface développée en R avec le framework ", strong("Shiny"), "."),
             tags$li("Utilisation de l’API REST de l’IGN pour la France."),
             tags$li("Utilisation de l’API ", a("Nominatim (OSM)", href = "https://nominatim.openstreetmap.org", target = "_blank"), " pour l’international."),
             tags$li("Cartographie assurée par le package ", strong("Leaflet"), "."),
             tags$li("Aucune donnée personnelle n’est collectée ni stockée.")
           ),
           
           tags$hr(),
           
           h4("💾 Code source & déploiement"),
           tags$ul(
             tags$li("💻 Code source : ", 
                     a("github.com/rwinsee/api_ban_edep", href = "https://github.com/rwinsee/api_ban_edep", target = "_blank")),
             tags$li("📦 Releases : ", 
                     a("github.com/rwinsee/api_ban_edep/releases", href = "https://github.com/rwinsee/api_ban_edep/releases", target = "_blank")),
             tags$li("🚀 Déploiement : ", 
                     a("github.com/rwinsee/api_ban_edep_deploy", href = "https://github.com/rwinsee/api_ban_edep_deploy", target = "_blank")),
             tags$li("🐳 Docker Hub : ", 
                     a("rwinsee/app_shiny_ban", href = "https://hub.docker.com/r/rwinsee/app_shiny_ban/tags", target = "_blank"))
           ),
           
           tags$hr(),
           
           h4("🧑‍💻 Auteur"),
           p("Développé par ", strong("Romuald Weidmann"), " (INSEE)."),
           p("Version 0.0.2"),
           p(em("Dernière mise à jour : 19 mai 2025")),
           
           tags$hr(),
           
           h4("📚 Glossaire"),
           tags$ul(
             tags$li(strong("SIG :"), " Système d’information géographique."),
             tags$li(strong("ESRI :"), " Éditeur de cartes et de fonds de type satellite ou topographique."),
             tags$li(strong("IGN :"), " Institut national de l'information géographique et forestière (France)."),
             tags$li(strong("BAN :"), " Base Adresse Nationale, regroupant les adresses françaises."),
             tags$li(strong("OSM :"), " OpenStreetMap, projet collaboratif de cartographie libre."),
             tags$li(strong("Fonds de carte :"), " Couche d’arrière-plan cartographique (plan, satellite, etc.).")
           )
         )
)
)


# serveur principal de l'application
server <- function(input, output, session) {
  
  # variable réactive pour stocker les coordonnées et résultats de recherche
  coords <- reactiveVal(NULL)
  
  # initialiser la carte leaflet pour l’onglet france
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(group = "osm") %>%                       # ajout du fond de carte par défaut (plan)
      setView(lng = 2.2, lat = 46.6, zoom = 6)          # centrage sur la france avec niveau de zoom standard
  })
  
  # icône personnalisée verte pastel pour marquer les résultats sur la carte
  pastelIcon <- makeIcon(
    iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-green.png",
    iconWidth = 25, iconHeight = 41,
    iconAnchorX = 12, iconAnchorY = 41,
    shadowUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-shadow.png",
    shadowWidth = 41, shadowHeight = 41,
    shadowAnchorX = 12, shadowAnchorY = 41
  )
  
  # observer les modifications dans la variable coords pour actualiser la carte
  observe({
    coord <- coords()            # récupérer les coordonnées actuelles
    res <- coord$result          # extraire le résultat principal
    
    # affichage console pour vérification des coordonnées
    print(paste("longitude =", res$longitude, "| latitude =", res$latitude))
    
    # condition : présence de coordonnées valides
    if (!is.null(res) &&
        is.numeric(res$longitude) &&
        is.numeric(res$latitude) &&
        !is.na(res$longitude) &&
        !is.na(res$latitude)) {
      
      # mise à jour dynamique de la carte avec le marqueur vert pastel
      leafletProxy("map") %>%
        clearMarkers() %>%
        setView(lng = res$longitude, lat = res$latitude, zoom = 16) %>%
        addMarkers(
          lng = res$longitude,
          lat = res$latitude,
          popup = res$label,
          icon = pastelIcon
        )
      
    } else {
      # recentrage sur la france si coordonnées invalides ou absentes
      leafletProxy("map") %>%
        clearMarkers() %>%
        setView(lng = 2.2, lat = 46.6, zoom = 6)
    }
  })
}
# afficher un message d'accueil ou les résultats de la recherche à l'ouverture
output$info <- renderUI({
  coord <- coords()  # récupérer les coordonnées mémorisées
  
  # cas où aucun champ n'a encore été rempli
  if (is.null(coord)) {
    return(tags$div(class = "pastel-box",
                    tags$p("👋 bienvenue dans l’application client ban de géolocalisation"),
                    tags$p("🔎 renseigner une adresse à gauche, puis appuyer sur entrée ou cliquer sur 'rechercher'"),
                    tags$ul(
                      tags$li("✅ rue + ville → ex. : 'place de la gare' + 'vitry-le-françois'"),
                      tags$li("✅ rue + code postal → ex. : 'avenue victor hugo' + '75016'"),
                      tags$li("✅ code commune insee → ex. : '51649'"),
                      tags$li("✅ ville seule → ex. : 'toulouse'"),
                      tags$li("✅ code postal seul → ex. : '13001'"),
                      tags$li("✅ rue seule → ex. : 'impasse des lilas'")
                    ),
                    tags$p("🛠️ croiser les champs si plusieurs sont renseignés pour affiner les résultats")
    ))
  }
  
  res <- coord$result  # récupérer le résultat principal
  
  # cas où la requête n’a rien retourné
  if (is.null(res)) {
    return(tags$div(class = "pastel-box", tags$p("❌ aucune donnée trouvée pour cette adresse")))
  }
  
  # affichage du détail des informations géographiques trouvées
  tags$div(class = "pastel-box",
           tags$p(tags$strong("📍 adresse : "), res$label),
           tags$p(tags$strong("🏙️ ville : "), res$city),
           tags$p(tags$strong("📮 code postal : "), res$postcode),
           tags$p(tags$strong("🆔 code commune insee : "), res$insee),
           tags$p(tags$strong("🛣️ rue : "), ifelse(res$street != "", res$street, "non fournie")),
           tags$p(tags$strong("🏠 numéro : "), ifelse(res$housenumber != "", res$housenumber, "non fourni")),
           tags$p(tags$strong("📌 quartier : "), ifelse(res$district != "", res$district, "non fourni")),
           tags$p(tags$strong("🗺️ contexte : "), ifelse(res$context != "", res$context, "non fourni")),
           tags$p(tags$strong("📏 score : "), ifelse(!is.na(res$score), paste0(round(res$score * 100, 1), " %"), "non fourni")),
           tags$p(tags$strong("🔍 type : "), ifelse(res$type != "", res$type, "non fourni"))
  )
})

# remplir automatiquement le champ "ville" si le code insee est saisi
observeEvent(input$code_insee, {
  if (input$libcom == "") {
    lib <- cog_2025$LIBELLE[which(cog_2025$COM == input$code_insee)]
    if (length(lib) == 1 && !is.na(lib)) {
      updateTextInput(session, "libcom", value = lib)
    }
  }
})

# déclencher une recherche lorsque le bouton "go" est cliqué ou entrée pressée
observeEvent(input$go, {
  cat("➡️ bouton go cliqué\n")
  
  # auto-compléter le champ "ville" si vide et code insee fourni
  if (input$libcom == "" && input$code_insee != "") {
    lib <- cog_2025$LIBELLE[which(cog_2025$COM == input$code_insee)]
    if (length(lib) == 1 && !is.na(lib)) {
      cat("🔁 remplissage auto de libcom depuis code_insee :", lib, "\n")
      updateTextInput(session, "libcom", value = lib)
    }
  }
  
  # bloquer la recherche si tous les champs sont vides
  if (input$libvoie == "" && input$libcom == "" && input$codpost == "" && input$code_insee == "") {
    showNotification("veuillez remplir au moins un champ pour lancer la recherche.", type = "error")
    output$info <- renderPrint({ cat("ℹ️ l’api nécessite au moins une adresse...") })
    return()
  }
  
  # appeler l’api principale avec les champs disponibles
  res <- get_city_info_from_api(
    codpost = input$codpost,
    libcom = input$libcom,
    libvoie = input$libvoie,
    code_insee = input$code_insee
  )
  
  # appeler aussi l’api multi pour récupérer plusieurs résultats éventuels
  all <- get_city_info_from_api_multi(
    codpost = input$codpost,
    libcom = input$libcom,
    libvoie = input$libvoie,
    code_insee = input$code_insee
  )
  
  # si aucun résultat principal mais au moins un résultat alternatif, reconstruire un résultat
  if (is.null(res) && length(all) > 0) {
    props <- all[[1]]$properties
    coords_simple <- all[[1]]$geometry$coordinates
    
    # extraire les coordonnées si disponibles
    if (!is.null(coords_simple) && length(coords_simple) == 2) {
      longitude <- as.numeric(coords_simple[[1]])
      latitude  <- as.numeric(coords_simple[[2]])
    } else {
      longitude <- NA
      latitude <- NA
    }
    
    # construire manuellement un objet de réponse simplifié
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
    cat("✅ résultat reconstruit depuis multi\n")
  }
  
  # stocker les résultats (principal + liste multi) pour affichage et carte
  coords(list(result = res, all_results = all))
})

# observer le clic sur le bouton "réinitialiser"
observeEvent(input$reset, {
  
  # vider les champs de saisie : code postal, commune, insee, libellé de voie
  updateTextInput(session, "codpost", value = "")
  updateTextInput(session, "libcom", value = "")
  updateTextInput(session, "code_insee", value = "")
  updateTextInput(session, "libvoie", value = "")
  
  # réinitialiser la variable de coordonnées à null
  coords(NULL)
  
  # réinitialiser la carte à la vue d'ensemble par défaut
  leafletProxy("map") %>%
    clearMarkers() %>%
    setView(lng = 2.2, lat = 46.6, zoom = 6)
  
  # afficher un message d’accueil dans la boîte d'information
  output$info <- renderUI({
    coord <- coords()
    
    if (is.null(coord)) {
      return(tags$div(class = "pastel-box",
                      tags$p("👋 bienvenue dans l’application client ban de géolocalisation !"),
                      tags$p("🔎 renseigner une adresse à gauche, puis appuyer sur entrée ou cliquer sur 'rechercher'."),
                      tags$ul(
                        tags$li("✅ rue + ville → ex. : 'place de la gare' + 'vitry-le-françois'"),
                        tags$li("✅ rue + code postal → ex. : 'avenue victor hugo' + '75016'"),
                        tags$li("✅ code commune insee → ex. : '51649'"),
                        tags$li("✅ ville seule → ex. : 'toulouse'"),
                        tags$li("✅ code postal seul → ex. : '13001'"),
                        tags$li("✅ rue seule → ex. : 'impasse des lilas'")
                      ),
                      tags$p("🛠️ croiser les champs renseignés pour affiner les résultats")
      ))
    }
    
    # cas rare si coords() est revenu entre-temps
    res <- coord$result
    if (is.null(res)) {
      return(tags$div(class = "pastel-box", tags$p("❌ aucune donnée trouvée pour cette adresse.")))
    }
    
    # afficher les infos du résultat si présent
    tags$div(class = "pastel-box",
             tags$p(tags$strong("📍 adresse : "), res$label),
             tags$p(tags$strong("🏙️ ville : "), res$city),
             tags$p(tags$strong("📮 code postal : "), res$postcode),
             tags$p(tags$strong("🆔 code commune insee : "), res$insee),
             tags$p(tags$strong("🛣️ rue : "), ifelse(res$street != "", res$street, "non fournie")),
             tags$p(tags$strong("🏠 numéro : "), ifelse(res$housenumber != "", res$housenumber, "non fourni")),
             tags$p(tags$strong("📌 quartier : "), ifelse(res$district != "", res$district, "non fourni")),
             tags$p(tags$strong("🗺️ contexte : "), ifelse(res$context != "", res$context, "non fourni")),
             tags$p(tags$strong("📏 score : "), ifelse(!is.na(res$score), paste0(round(res$score * 100, 1), " %"), "non fourni")),
             tags$p(tags$strong("🔍 type : "), ifelse(res$type != "", res$type, "non fourni"))
    )
  })
  
})

# générer dynamiquement une liste des résultats alternatifs issus de l’api
output$liste_resultats <- renderUI({
  
  # récupérer les coordonnées et tous les résultats
  res <- coords()
  if (is.null(res)) return(NULL)  # aucun résultat encore, ne rien afficher
  
  feats <- res$all_results
  if (is.null(feats) || length(feats) == 0) {
    # aucun résultat alternatif trouvé
    return(tags$div(class = "alert alert-warning", "❌ aucun résultat trouvé."))
  }
  
  # construire les étiquettes de chaque résultat
  labels <- lapply(feats, function(f) {
    if (!is.null(f$properties$label)) {
      type <- f$properties$type %||% ""
      label <- f$properties$label
      insee <- f$properties$citycode %||% ""
      cp <- f$properties$postcode %||% ""
      score <- f$properties$score %||% NA
      score_txt <- if (!is.na(score)) paste0(" - score : ", round(score * 100, 1), " %") else ""
      
      # afficher le depcom et le code postal si c’est une commune
      if (type == "municipality" && insee != "" && cp != "") {
        paste0(label, " (depcom : ", insee, ", cp : ", cp, ")", score_txt)
      } else {
        paste0(label, score_txt)
      }
    } else {
      NULL
    }
  })
  
  # filtrer les libellés valides (non null, non vides)
  labels_valides <- labels[!sapply(labels, function(x) is.null(x) || x == "")]
  
  # ajuster le titre selon le nombre de résultats
  titre <- if (length(labels_valides) == 1) {
    "✅ un seul résultat trouvé :"
  } else {
    paste0("✅ ", length(labels_valides), " résultats trouvés :")
  }
  
  # afficher les résultats dans une liste
  liste <- lapply(labels_valides, tags$li)
  
  wellPanel(
    tags$h5(titre),
    tags$ul(liste)
  )
})


# changement de fond de carte selon la sélection utilisateur
observeEvent(input$fond_carte, {
  proxy <- leafletProxy("map")  # récupérer la carte existante
  proxy %>% clearTiles()        # retirer les tuiles actuelles
  
  # ajouter les nouvelles tuiles selon le fond sélectionné
  switch(input$fond_carte,
         "osm" = proxy %>% addTiles(),
         "satellite" = proxy %>% addProviderTiles("Esri.WorldImagery"),
         "carto_light" = proxy %>% addProviderTiles("CartoDB.Positron"),
         "carto_dark" = proxy %>% addProviderTiles("CartoDB.DarkMatter"),
         "topo" = proxy %>% addProviderTiles("OpenTopoMap")
  )
})


# remplir automatiquement le champ ville si uniquement le code insee est saisi
observeEvent(input$code_insee, {
  if (input$libcom == "" && input$code_insee != "") {
    lib <- cog_2025$LIBELLE[which(cog_2025$COM == input$code_insee)]
    if (length(lib) == 1 && !is.na(lib)) {
      updateTextInput(session, "libcom", value = lib)
    }
  }
})

  
  
  output$map_osm <- renderLeaflet({
    leaflet() %>% addTiles() %>% setView(lng = 0, lat = 20, zoom = 2)
  })
  output$info_osm <- renderUI({
    tags$div(class = "pastel-box",
             tags$p("👋 Bienvenue dans la recherche internationale d’adresses."),
             tags$p("🔎 Saisissez une adresse complète ou partielle à gauche (ex. : '5 Konstytucji 3 Maja, Grajewo')."),
             tags$p("🌍 Cette recherche interroge le service ", tags$strong("Nominatim"), " d’OpenStreetMap."),
             tags$p("💡 Vous pouvez également taper Entrée pour valider ou Échap pour réinitialiser.")
    )
  })
  observeEvent(input$go_osm, {
    if (input$adresse_osm == "") {
      showNotification("Veuillez entrer une adresse étrangère.", type = "error")
      return()
    }
    
    resultats <- get_info_nominatim_multi(input$adresse_osm, limit = 20)
    
    if (is.null(resultats)) {
      output$info <- renderPrint({ cat("❌ Aucun résultat trouvé via OSM.") })
      return()
    }
    
    output$info_osm <- renderUI({
      if (is.null(resultats)) {
        return(tags$div("❌ Aucun résultat trouvé."))
      }
      
      liste <- lapply(seq_along(resultats), function(i) {
        res <- resultats[[i]]
        if (!is.null(res$display_name) && !is.null(res$lat) && !is.null(res$lon)) {
          tags$li(
            tags$span(paste0("📍 Résultat ", i, " : ")),
            actionLink(inputId = paste0("osm_result_", i), label = res$display_name, class = "osm-link")
            ,
            tags$br(),
            tags$span(class = "coordonnees", paste0("🌍 Lat : ", res$lat, " | Lon : ", res$lon))
          )
        }
      })
      
      tags$div(
        class = "pastel-box",
        tags$strong(paste0("✅ Résultats trouvés : ", length(resultats))),
        tags$ul(liste)
      )
    })
    
    for (i in seq_along(resultats)) {
      local({
        idx <- i
        observeEvent(input[[paste0("osm_result_", idx)]], {
          res <- resultats[[idx]]
          if (!is.null(res$lat) && !is.null(res$lon)) {
            leafletProxy("map_osm") %>%
              clearMarkers() %>%
              setView(lng = as.numeric(res$lon), lat = as.numeric(res$lat), zoom = 16) %>%
              addMarkers(
                lng = as.numeric(res$lon),
                lat = as.numeric(res$lat),
                popup = res$display_name,
                icon = pastelIcon
              )
          }
        })
      })
    }
    
    
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
    updateTextInput(session, "adresse_osm", value = "")
    leafletProxy("map_osm") %>%
      clearMarkers() %>%
      setView(lng = 0, lat = 20, zoom = 2)
    
    output$info_osm <- renderUI({
      tags$div(class = "pastel-box",
               tags$p("🔄 Recherche réinitialisée."),
               tags$p("👋 Vous pouvez saisir une nouvelle adresse internationale."),
               tags$p("📌 Exemple : ", tags$em("10 Downing Street, London"), " ou ", tags$em("Piazza San Marco, Venice"))
      )
    })
  })
  
  
  # Ajout du déclenchement avec touche clavier (simulateur)
  observeEvent(input$reset_osm, { shinyjs::delay(50, { updateTextInput(session, "adresse_osm", value = "") }) })
  
  
  observeEvent(input$fond_carte_osm, {
    proxy <- leafletProxy("map_osm")
    proxy %>% clearTiles()
    
    switch(input$fond_carte_osm,
           "osm" = proxy %>% addTiles(),
           "satellite" = proxy %>% addProviderTiles("Esri.WorldImagery"),
           "carto_light" = proxy %>% addProviderTiles("CartoDB.Positron"),
           "carto_dark" = proxy %>% addProviderTiles("CartoDB.DarkMatter"),
           "esri_topo" = proxy %>% addProviderTiles("Esri.WorldTopoMap")
    )
  })
  
}

# Lancer l'application
shinyApp(ui, server)  