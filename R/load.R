#' erf_load
#' @description Load files from local file system
#' @param version_id character database version string
#' @param format character choice of rds or sqlite
#' @param fpath file.path optionally specify custom location of rds file
#' @export
#' @importFrom rappdirs user_data_dir
#' @examples \dontrun{
#' erf  <- erf_load("1")
#' }
erf_load <- function(version_id, format = "rds", fpath = NA){

  if(!is.na(fpath)){
      readRDS(fpath)
  }else{
      rds_path <- paste0(cache_path(), "erf_", version_id, ".rds")
      stop_if_not_exists(rds_path)
      readRDS(rds_path)
  }
}

