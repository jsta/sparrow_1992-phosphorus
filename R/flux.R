#' Get Phosphorus Flux
#'
#' Get phosphorus flux given upstream and downstream nodes as well as
#'  incremental and total load data.
#'
#' @param id_upstream integer vector
#' @param id_downstream integer vector
#' @param p_inc data.frame
#' @param p_upstream data.frame
#'
#' @return data.frame
#' @export
#'
get_p_flux <- function(id_upstream, id_downstream, p_inc, p_upstream){

  lk_inc  <- sum(p_inc[p_inc$id %in%
                            id_upstream,]$MappedValueIncrementalLoadkgyear)
  lk_up   <- sum(p_upstream[p_upstream$id %in%
                            id_upstream,]$MappedValueTotalLoadkgyear)
  lk_down <- sum(p_upstream[p_upstream$id %in%
                            id_downstream,]$MappedValueTotalLoadkgyear)

  data.frame(loading = lk_inc + lk_up, outflow = lk_down)
}
