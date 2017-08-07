get_upstream_ids <- function(id_lake, erf){
  fnodes <- erf[erf$MRB_ID %in% id_lake, "FNODE_"]
  erf[erf$TNODE_ %in% fnodes,]$MRB_ID
}

get_downstream_ids <- function(id_lake, erf){
  tnodes <- erf[erf$MRB_ID %in% id_lake, "TNODE_"]
  erf[erf$FNODE_ %in% tnodes,]$MRB_ID
}

assign_reach_position <- function(id_lake, id_upstream, id_downstream,
                                  polygon, erf){
  erf_sub <- erf[erf$MRB_ID %in% c(id_lake, id_upstream, id_downstream),]
  erf_sub <- sf::st_transform(erf_sub, sf::st_crs(wb_sub))

  erf_sub$position <- "focal"
  erf_sub[erf_sub$MRB_ID %in% id_downstream, "position"] <- "downstream"
  erf_sub[erf_sub$MRB_ID %in% id_upstream, "position"]   <- "upstream"
  erf_sub$position <- factor(erf_sub$position,
                             levels = c("upstream", "focal", "downstream"))
  erf_sub
}


#' Return sparrow network
#'
#' @param lon longitude
#' @param lat latitude
#' @param lines linestring
#' @param dbf dbf object
#' @param polygon polygon
#'
#' @export
#'
network <- function(lon, lat, lines, dbf, polygon){
  id_lake  <- get_id(lon, lat, lines, polygon)
  up_ids   <- get_upstream_ids(id_lake, dbf)
  down_ids <- get_downstream_ids(id_lake, dbf)
  reaches  <- assign_reach_position(id_lake, up_ids, down_ids, polygon, lines)

  list(upstream_ids = up_ids, downstream_ids = down_ids, reaches = reaches)
}
