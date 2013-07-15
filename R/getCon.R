#' @title Get a RMySQL connection to the database
#' 
#' @description Get a RMySQL connection to the central database
#' 
#' @details This function creates a connection to the database that houses WAR data.
#' 
#' @export
#' @examples
#' con = getCon()
#' getWAR(con)

getCon <- function () {
  con = dbConnect(MySQL()
        , user="openwar"
        , host="macgarnagle.no-ip.org"
        , password="X9SaZz7jztb3JtVE"
        , dbname="openwar")
  return(con)
}
