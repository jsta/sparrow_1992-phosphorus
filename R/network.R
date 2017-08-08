assign_reach_position <- function(id_lake, dbf, polygon, erf){

  dbf <- dbf[!duplicated(dbf$MRB_ID),]

  fnodes <- dbf[dbf$MRB_ID %in% id_lake$intersects, ]

  # fnodes_erf <- erf[erf$MRB_ID %in% fnodes$MRB_ID,]
  # mapview::mapview(fnodes_erf)

  fnodes_upstream   <- fnodes[!(fnodes$FNODE_ %in% fnodes$TNODE_),]
  fnodes_downstream <- fnodes[!(fnodes$TNODE_ %in% fnodes$FNODE_),]
  fnodes_focal <- fnodes[!(fnodes$MRB_ID %in% fnodes_downstream$MRB_ID) &
                           !(fnodes$MRB_ID %in% fnodes_upstream$MRB_ID), ]

  erf_sub <- erf[erf$MRB_ID %in% fnodes$MRB_ID,]
  erf_sub <- sf::st_transform(erf_sub, sf::st_crs(polygon))

  erf_sub$position <- "focal"
  erf_sub[erf_sub$MRB_ID %in%
            fnodes_downstream$MRB_ID, "position"] <- "downstream"
  erf_sub[erf_sub$MRB_ID %in%
            fnodes_upstream$MRB_ID, "position"]   <- "upstream"
  erf_sub$position <- factor(erf_sub$position,
                             levels = c("upstream", "focal", "downstream"))
  list(erf_sub = erf_sub,
       up_ids = fnodes_upstream$MRB_ID,
       down_ids = fnodes_downstream$MRB_ID)
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
  reaches  <- assign_reach_position(id_lake, dbf, polygon, lines)

  list(upstream_ids = reaches$up_ids,
       downstream_ids = reaches$down_ids,
       reaches = reaches$erf_sub)
}
