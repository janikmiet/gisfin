#' @title Get locations of IP addresses
#' @description Get geographic coordinates for given IP-addresses from \url{http://www.datasciencetoolkit.org//ip2coordinates/}
#' @param ip vector of IP address as character
#' @return Data frame with parameters ip, latitude and longitude 
#' @export 
#' @references See citation("gisfin") 
#' @author Jani Miettinen \email{jani.k.miettinen@@gmail.com}
#' @note Modified from original version by Kay Cichini and Leo Laine
#' @examples \dontrun{get_ips(c("137.224.252.10", "127.124.152.15"))}
#' @keywords utilities
get_ips <- function(ip){
  stopifnot(is.character(ip))
  df <- NULL
  for (i in ip) {
    point <- ip_location(i) # depedency to gisfin::ip_location()
    lon = as.double(point[2])
    lat = as.double(point[1])
    if(is.null(df)){
      df <- data.frame(ip=i,lon=lon, lat=lat)
    }else{
      df <- rbind(df, data.frame(ip=i,lon=lon, lat=lat))
    }
  }
  return(df) 
}