#' @title De donde esta? Shows location of IP address
#' @description Shows geographic location of given IP-address
#' @param ip vector of IP address as character
#' @return Leaflet map
#' @export 
#' @references See citation("gisfin") 
#' @author Jani Miettinen \email{jani.k.miettinen@@gmail.com}
#' @note El functione que dijabo el tu locasion.
#' @examples \dontrun{dedondee(ip="72.14.192.0")}
#' @keywords utilities
dedondee <- function(ip="72.14.192.0"){
  # library(gisfin)
  # library(leaflet)
  ip <- ip
  point <- ip_location(ip)
  # point <- gisfin::ip_location(ip)
  point <- as.numeric(point)
  m <- leaflet::leaflet() %>%
    leaflet::addTiles() %>%  # Add default OpenStreetMap map tiles
    leaflet::addMarkers(lng=point[2], lat=point[1], popup=ip)
  return(m)
}
