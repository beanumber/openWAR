
## Hack to avoid warning about importing both mosaic::do and dplyr::do

mosaic.do <- function (...) {
  return(mosaic::do(...))
}