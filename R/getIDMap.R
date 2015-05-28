#' @title getIDMap
#' 
#' @description Retrieve a mapping of player Ids from different data sources
#' 
#' @details Downloads the BP player list and cross-checks it with the Lahman database 
#' 
#' @return A data.frame with a row for each player and a column for each id
#' 
#' @export getIDMap
#' @examples
#' 
#' ids = getIDMap()
#' 

getIDMap = function() {
    bp.ids = read.csv("http://www.baseballprospectus.com/sortable/playerids/playerid_list.csv")
    names(bp.ids) = c("nameLast", "nameFirst", "bpId", "davenportId", "mlbamId", "retroId")
    ids = merge(x = Lahman::Master, y = bp.ids, by.x = "retroID", by.y = "retroId", all = TRUE)
    return(ids)
} 
