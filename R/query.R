#' Get the ERF reachID of a Point
#'
#' Get the reachID corresponding with a query point-waterbody overlay.
#'
#' @param lon numeric longitude
#' @param lat numeric lattitude
#' @param lines sf polyline
#' @param poly sf polygon
#' @importFrom nhdR select_point_overlay
#' @importFrom sf st_sfc st_point st_crs st_transform st_intersects st_crs<-
#'
#' @export
#'
get_id <- function(lon, lat, lines, poly){

  pnt <- sf::st_sfc(sf::st_point(c(lon, lat)))
  st_crs(pnt) <- 4326
  erf_sub <- nhdR::select_point_overlay(pnt, lines, buffer_dist = 0.3)

  id_lake <- erf_sub[unlist(lapply(sf::st_intersects(erf_sub, poly),
                                   length)) > 0,]
  id_lake$MRB_ID
}

get_lagos_id <- function(pnt, locus){

  # try to match gnis name
  gnis_match <- locus[grep(pnt$Name, locus$gnis_name),]
  gnis_match <- st_as_sf(gnis_match, coords = c("nhd_long", "nhd_lat"),
                         crs = 4326)

  # try to match location
  pnt_buffer <- st_buffer(pnt, 0.3)
  if(nrow(gnis_match) > 0){
    st_intersection(gnis_match, pnt_buffer)$lagoslakeid
  }else{
    st_intersection(st_as_sf(locus, coords = c("nhd_long", "nhd_lat"),
                             crs = 4326), pnt_buffer)$lagoslakeid
  }
}
