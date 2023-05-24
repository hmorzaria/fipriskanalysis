#' Scale variable
#'
#' @param datatable
#' @param sspscenario
#'
#' @return num.scale data with scaled total
#' @export
#'
#' @examples scaled.exp <- scale_var(exp.data.set.sc, NA)
scale_var <- function(datatable, sspscenario){

if (is.character(sspscenario)){

  num.vars <- datatable %>%
    dplyr::filter(scenario==sspscenario)

} else if(is.na(sspscenario)){

  num.vars <- datatable

}
  process.num <- num.vars %>%
    dplyr::select(dplyr::where(is.numeric)) %>%
    dplyr::mutate(tot_score = rowSums(dplyr::select_if(., is.numeric), na.rm = TRUE)) %>%
    dplyr::select(tot_score) %>%
    caret::preProcess(., method=c("range"))

  num.scale <- num.vars %>%
    dplyr::mutate(tot_score = rowSums(dplyr::select_if(., is.numeric), na.rm = TRUE)) %>%
    dplyr::select(tot_score) %>%
    stats::predict(process.num, .) %>%
    dplyr::bind_cols(num.vars, .)

   return(num.scale)
}



