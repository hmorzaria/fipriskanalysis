#topsis multicriteria
calc_topsis <- function(eachscenario, hazard.sc.data, vul.expo.dat, sp.cols){

  risk.data <- hazard.sc.data %>%
    dplyr::filter(escenario==eachscenario) %>%
    dplyr::left_join(vul.expo.dat, by=c("fip_name")) %>%
    dplyr::select(starts_with("tot_score"))

  #calculate topsis
  risk.matrix <- risk.data %>%
    as.matrix()

  topsis.w <- rep(1,ncol(risk.matrix)) #
  topsis.i <- rep("+",ncol(risk.matrix))
  topsis.res <- topsis::topsis(risk.matrix, topsis.w, topsis.i)

  topsis.res.sc <- hazard.sc.data %>%
    dplyr::filter(escenario==eachscenario) %>%
    dplyr::select(dplyr::where(is.character)) %>%
    dplyr::bind_cols(topsis.res) %>%
    dplyr::mutate(FIP = dplyr::if_else(fip_name == "Mexico Bahia de Los Angeles octopus - trap/diver-caught/hand gathered", "Pulpo",
                                       dplyr::if_else(fip_name =="Mexico Baja California red sea urchin - diver-caught","Erizo rojo",
                                                      dplyr::if_else(fip_name =="Mexico North Pacific barred sand bass - pot/trap","Verdillo", "Langosta"))))
  return(topsis.res.sc)
  #d <- matrix(rpois(12, 5), nrow = 4)
  #w <- c(1, 1, 2)
  #i <- c("+", "-", "+")
  #topsis(d, w, i)

}
