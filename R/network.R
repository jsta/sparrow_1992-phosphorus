assign_reach_position <- function(id_lake, dbf, polygon, erf){

  dbf <- dbf[!duplicated(dbf$MRB_ID),]

  fnodes <- dbf[dbf$MRB_ID %in% id_lake$intersects, ]

  # fnodes_erf <- erf[erf$MRB_ID %in% fnodes$MRB_ID,]
  # mapview::mapview(fnodes_erf)

  if(nrow(fnodes) == 1){
    # headwater if f not in t
    if(!(any(dbf$TNODE_ == fnodes$FNODE_))){
      fnodes_upstream   <- data.frame(MRB_ID = NA)
      fnodes_downstream <- fnodes
      fnodes_focal      <- data.frame(MRB_ID = NA)
    }

    # terminal if t not in any f
    if(!(any(dbf$FNODE_ == fnodes$TNODE_))){
      fnodes_upstream   <- fnodes
      fnodes_downstream <- data.frame(MRB_ID = NA)
      fnodes_focal      <- data.frame(MRB_ID = NA)
    }

  }else{
    fnodes_upstream   <- fnodes[!(fnodes$FNODE_ %in% fnodes$TNODE_),]
    fnodes_downstream <- fnodes[!(fnodes$TNODE_ %in% fnodes$FNODE_),]
    fnodes_focal      <- fnodes[
      !(fnodes$MRB_ID %in% fnodes_downstream$MRB_ID) &
        !(fnodes$MRB_ID %in% fnodes_upstream$MRB_ID), ]
  }

  erf_sub <- erf[erf$MRB_ID %in% fnodes$MRB_ID,]
  erf_sub <- sf::st_transform(erf_sub, sf::st_crs(polygon))

  erf_sub$position <- "focal"

  if(!all(is.na(fnodes_downstream$MRB_ID))){
    erf_sub[erf_sub$MRB_ID %in%
            fnodes_downstream$MRB_ID, "position"] <- "downstream"
  }

  if(!all(is.na(fnodes_upstream$MRB_ID))){
    erf_sub[erf_sub$MRB_ID %in%
            fnodes_upstream$MRB_ID, "position"]   <- "upstream"
  }
  erf_sub$position <- factor(erf_sub$position,
                             levels = c("upstream", "focal", "downstream"))
  list(erf_sub = erf_sub,
       up_ids = fnodes_upstream$E2RF1,
       focal_ids = fnodes_focal$E2RF1,
       down_ids = fnodes_downstream$E2RF1)
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
       focal_ids = reaches$focal_ids,
       downstream_ids = reaches$down_ids,
       reaches = reaches$erf_sub)
}
