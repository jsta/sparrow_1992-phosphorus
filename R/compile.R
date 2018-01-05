#'@name erf_compile
#'@title Compile Enhanced River Reach data to an R data (rds) object
#'@export
#'@description Compile erf data from compressed files
#'@param version_id character temp database version string
#'@param format "rds"
#'@param folder file.path to data folder.
#'@examples \dontrun{
#' erf_compile("1", format = "rds")
#' }
erf_compile <- function(version_id, format = "rds", folder = NA){

  res <- erf_ingest(version_id = version_id, folder = folder)

  # dir.exists(cache_path())
  dir.create(cache_path(), recursive = TRUE, showWarnings = FALSE)

  outpath <- file.path(cache_path(), paste0("erf_", version_id, ".rds"))

  saveRDS(res, outpath)
  message(paste0("data compiled to ", outpath))
}

#'@name p_compile
#'@title Compile phosphorus data to an R data (rds) object
#'@export
#'@description Compile phosphorus erf data from flat files
#'@param version_id character temp database version string
#'@param format "rds"
#'@param folder file.path to data folder.
#'@param skip numeric vector of lines to skip on file read. optional.
#'@examples \dontrun{
#' p_compile("1")
#' }
p_compile <- function(version_id, format = "rds", folder = NA, skip = NA){

  if(is.na(folder)){
    folder <- cache_path()
  }

  res <- p_ingest(version_id = version_id, folder = folder, skip = skip)

  # dir.exists(cache_path())
  dir.create(cache_path(), recursive = TRUE, showWarnings = FALSE)

  outpath <- file.path(cache_path(), paste0("p_", version_id, ".rds"))

  saveRDS(res, outpath)
  message(paste0("data compiled to ", outpath))
}
