#' @title mosaic.do
#' 
#' @description This is a hack to avoid warning about importing both mosaic::do and dplyr::do
#' 
#' 
mosaic.do <- function (...) {
  return(mosaic::do(...))
}