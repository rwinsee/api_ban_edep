library(httr)
library(jsonlite)
library(leaflet)

get_city_info_from_api <- function(codpost, libcom = NULL, libvoie) {
  url <- "https://api-adresse.data.gouv.fr/search/"
  
  # Utiliser des paramètres de requête spécifiques pour le code postal, la ville et l'adresse
  query_params <- list(postcode = codpost, city = libcom, q = libvoie, limit = 1)
  
  response <- GET(url, query = query_params)
  if (response$status_code == 200) {
    data <- fromJSON(content(response, "text"), simplifyVector = FALSE)
    if (length(data$features) > 0) {
      longitude <- data$features[[1]]$geometry$coordinates[[1]]
      latitude <- data$features[[1]]$geometry$coordinates[[2]]
      return(list(longitude = longitude, latitude = latitude))
    } else {
      message("Aucune donnée trouvée pour l'adresse donnée.")
      return(list(longitude = NA, latitude = NA))
    }
  } else {
    message("Erreur de requête API")
    return(list(longitude = NA, latitude = NA))
  }
}

# Exemple d'utilisation avec seulement le code postal et l'adresse
info_adresse1 <- get_city_info_from_api("51300", libvoie = "Rue de l'Abbé Goujet")

# Exemple d'utilisation avec le code postal, la ville, et l'adresse
info_adresse2 <- get_city_info_from_api("51300", "", "Rue de labbé Gouget")

# Affichage des cartes pour les deux adresses
if (!is.na(info_adresse1$longitude) && !is.na(info_adresse1$latitude)) {
  map1 <- leaflet() %>%
    addTiles() %>%
    setView(lng = info_adresse1$longitude, lat = info_adresse1$latitude, zoom = 15) %>%
    addMarkers(lng = info_adresse1$longitude, lat = info_adresse1$latitude, popup = "Adresse 1 trouvée !")
  map1
} else {
  message("Impossible de créer la carte pour l'adresse 1 en raison de l'absence de coordonnées valides.")
}

if (!is.na(info_adresse2$longitude) && !is.na(info_adresse2$latitude)) {
  map2 <- leaflet() %>%
    addTiles() %>%
    setView(lng = info_adresse2$longitude, lat = info_adresse2$latitude, zoom = 15) %>%
    addMarkers(lng = info_adresse2$longitude, lat = info_adresse2$latitude, popup = "Adresse 2 trouvée !")
  map2
} else {
  message("Impossible de créer la carte pour l'adresse 2 en raison de l'absence de coordonnées valides.")
}
