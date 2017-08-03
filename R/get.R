#' Get Enhanced River Reach Data
#'
#' Get Enhanced River Reach data.
#'
#' @export
#' @description Retrieves external files and store in file cache.
#' @importFrom rappdirs user_data_dir
#' @importFrom utils download.file unzip
#' @param version_id version id
#' @param skip numeric vector of lines to skip on file read. optional.
#' @param dest_folder file.path optional will default to the location returned by \code{\link[rappdirs]{user_data_dir}}.
#' @param compile logical perform on-the-fly compilation to rds?
#' @examples \dontrun{
#' erf_get(version = "1")
#' }
erf_get <- function(version_id, skip = NA, dest_folder = NA, compile = TRUE){

  baseurl <- "https://water.usgs.gov/GIS/dsdl/"
  files <- c("mrb_e2rf1.zip")

  # dir.exists(cache_path())
  dir.create(cache_path(), showWarnings = FALSE)
  versioned_path <- paste0(cache_path(), version_id)
  dir.create(versioned_path, showWarnings = FALSE)

  invisible(
    lapply(files,
           function(x) get_if_not_exists(
             paste0(baseurl, x), file.path(versioned_path, x))
    ))

  if(compile){
    utils::unzip(file.path(versioned_path, files), exdir = versioned_path)
    erf_compile(version_id = version_id,
                folder = versioned_path)
  }
}

#' Get Phosphorus Loading Data
#'
#' Get phosphorus loading data.
#'
#' @param version_id version id
#'
#' @export
#'
p_get <- function(version_id){

  # dir.exists(cache_path())
  dir.create(cache_path(), showWarnings = FALSE)
  versioned_path <- paste0(cache_path(), version_id)
  dir.create(versioned_path, showWarnings = FALSE)

  files <- list.files(versioned_path, "predict_export")

  if(length(files) == 0){
    stop(paste0("You need to manually place phosphorus loading data at: ",
              file.path(cache_path(), version_id)))
  }

  browser()

  p_compile(files)
}
