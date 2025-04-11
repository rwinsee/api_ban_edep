library(shiny)
library(httr)
library(jsonlite)
library(leaflet)

# Fonction enrichie avec code commune prioritaire
get_city_info_from_api <- function(codpost, libcom = NULL, libvoie, code_insee = NULL) {
  url <- "https://api-adresse.data.gouv.fr/search/"
  query_params <- list(
    postcode = codpost,
    city = libcom,
    q = libvoie,
    limit = 1
  )
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
    } else {
      return(NULL)
    }
  } else {
    return(NULL)
  }
}

# Utilitaire pour valeurs par défaut (comme `%>%` mais pour valeurs)
`%||%` <- function(a, b) if (!is.null(a)) a else b


# Interface
ui <- fluidPage(
  titlePanel("Géolocalisation via Adresse.data.gouv.fr"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("codpost", "Code postal", value = "57365"),
      textInput("libcom", "Ville (facultatif)", value = ""),
      textInput("code_insee", "Code commune INSEE (prioritaire si rempli)", value = ""),
      textInput("libvoie", "Adresse", value = "15 GOTTLY"),
      actionButton("go", "Localiser")
    ),
    
    mainPanel(
      leafletOutput("map", height = "500px"),
      h4("Informations trouvées :"),
      verbatimTextOutput("info")
    )
  )
)

# Serveur
server <- function(input, output, session) {
  coords <- reactiveVal(NULL)
  
  observeEvent(input$go, {
    req(input$codpost, input$libvoie)
    res <- get_city_info_from_api(
      codpost = input$codpost,
      libcom = input$libcom,
      libvoie = input$libvoie,
      code_insee = input$code_insee
    )
    coords(res)
  })
  
  output$map <- renderLeaflet({
    leaflet() %>% addTiles()
  })
  
  observe({
    coord <- coords()
    if (!is.null(coord)) {
      leafletProxy("map") %>%
        clearMarkers() %>%
        setView(lng = coord$longitude, lat = coord$latitude, zoom = 16) %>%
        addMarkers(lng = coord$longitude, lat = coord$latitude, popup = "Adresse trouvée !")
    }
  })
  
  output$info <- renderPrint({
    coord <- coords()
    if (!is.null(coord)) {
      cat(paste0(
        "📍 Adresse : ", coord$label, "\n",
        "🏙️ Ville : ", coord$city, "\n",
        "📮 Code postal : ", coord$postcode, "\n",
        "🆔 Code commune INSEE : ", coord$insee, "\n",
        "🛣️ Rue : ", ifelse(coord$street != "", coord$street, "Non fournie"), "\n",
        "🏠 Numéro : ", ifelse(coord$housenumber != "", coord$housenumber, "Non fourni"), "\n",
        "📌 Quartier : ", ifelse(coord$district != "", coord$district, "Non fourni"), "\n",
        "🗺️ Contexte : ", ifelse(coord$context != "", coord$context, "Non fourni"), "\n",
        "📏 Score de pertinence : ", ifelse(!is.na(coord$score), paste0(round(coord$score * 100, 1), " %"), "Non fourni"), "\n",
        "🔍 Type : ", ifelse(coord$type != "", coord$type, "Non fourni")
      ))
    } else {
      cat("Aucune donnée trouvée pour cette adresse.")
    }
  })
  
  
}

# Lancer l'app
# shinyApp(ui, server)
