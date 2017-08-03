#' erf_ls
#'
#' @param version_id character version id
#' @param ... extra arguments passed to list.files
#'
#' @export
#'
#' @examples \dontrun{
#' erf_ls("1")
#' }
erf_ls <- function(version_id, ...){
  list.files(paste0(cache_path(), version_id),
             pattern = "\\d.e00$", ...)
}

#' p_ls
#'
#' @param version_id character version id
#' @param ... extra arguments passed to list.files
#'
#' @export
#'
#' @examples \dontrun{
#' p_ls("1")
#' }
p_ls <- function(version_id, ...){
  list.files(paste0(cache_path(), version_id),
             pattern = "\\.csv$", ...)
}

#'@name p_ingest
#'@title Ingest flat files
#'@description Ingest data from component flat files
#'@param version_id character temp database version string
#'@param folder file.path to data folder. optional.
#'@param skip numeric vector of lines to skip on file read. optional.
#'@importFrom sf st_read
#'@examples \dontrun{
#'p_ingest("1")
#'}
#'
p_ingest <- function(version_id, folder = NA, skip = NA){

  # Set-up paths ####
  p_raw <- erf_ls(version_id = version_id,
                   full.names = TRUE, include.dirs = TRUE)

  erf_decompress <- gsub(".e00", "_none.e00", erf_raw)

  # convert data ####
  lapply(seq_along(erf_raw), function(i) system(
      paste0("e00conv ", erf_raw, " ", erf_decompress, " NONE")))

  lapply(seq_along(erf_raw), function(i) system(
    paste0("ogr2ogr -f 'ESRI Shapefile' ", folder, " ", erf_decompress)))

  sf::st_read(file.path(folder, "ARC.shp"))
}
