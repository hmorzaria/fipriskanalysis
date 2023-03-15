
#' Read life history Google sheet
#'
#' @param sheetname
#' @param dataname
#'
#' @return rds files for every sheet
#' @export
#'
#' @examples
read_googlesheet <- function(sheetname, dataname){

 googlesheets4::gs4_deauth() #indicates no need to ask for credentials
 this.data <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1HoWV3HponuSLKvF1UyBOFXD-gbdwtiuhdui7OUnybZ4/edit#gid=2103071940", sheet = sheetname)

saveRDS(this.data, file = here::here("data",paste0(dataname,".rds")))
 #creates template describing the data
 data.description <-sinew::makeOxygen(this.data)
 cat(data.description, file=here::here("R",paste0("data-",dataname,".R")))


 }
