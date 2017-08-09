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
#'
get_id <- function(lon, lat, lines, poly){

  pnt <- sf::st_sfc(sf::st_point(c(lon, lat)))
  st_crs(pnt) <- 4326

  poly_sub <- nhdR::select_point_overlay(pnt, poly, buffer_dist = 0.2)

  within_lines  <- lines[sapply(st_within(lines, poly),
                                 function(x){length(x) > 0}),]
  # crosses_lines <- lines[sapply(st_crosses(lines, poly),
  #                               function(x){length(x) > 0}),]
  # mapview::mapview(list(crosses_lines), zcol = list("MRB_ID"))

  intersects_lines <- lines[sapply(st_intersects(lines,
                                      st_buffer(poly, dist = 200)),
                              function(x){length(x) > 0}),]

  list(intersects = intersects_lines$MRB_ID, within = within_lines$MRB_ID)
}

#' @importFrom sf st_buffer st_as_sf st_intersection
get_lagos_id <- function(pnt, locus){

  # try to match gnis name
  gnis_match <- locus[grep(pnt$Name, locus$gnis_name),]
  gnis_match <- st_as_sf(gnis_match, coords = c("nhd_long", "nhd_lat"),
                         crs = 4326)

  # try to match location
  pnt_buffer <- st_buffer(pnt, 0.02)
  if(nrow(gnis_match) > 0){
    st_intersection(gnis_match, pnt_buffer)$lagoslakeid
  }else{
    st_intersection(st_as_sf(locus, coords = c("nhd_long", "nhd_lat"),
                             crs = 4326), pnt_buffer)$lagoslakeid
  }
}
