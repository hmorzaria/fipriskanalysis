#' Get lifehistory data
#' @param lifehistorydata life history characteristics
#' @param localities FIP locations
#' @param fipinventory Inventory of FIP projects from fisheryprogress.org
#'
#' @return lifehistory.trait
#' @export
#'
#' @examples
get_lifehistory <- function(lifehistorydata, localities, fipinventory, lifehistorytraits){

  fiplocal.bc <- localities %>%
    dplyr::filter(fip_id_number %in% c(9158, 12856, 12947, 8040)) %>%
    dplyr::select(fip_id_number, COM_ID, locality, municipality, state) %>%
    dplyr::mutate(fip_id_number = as.factor(fip_id_number))

  fiplocal.bc.id <- fiplocal.bc %>%
    dplyr::distinct(COM_ID)

  fipinventory.bc <- fipinventory %>%
    dplyr::filter(fip_id_number %in% c(9158, 12856, 12947, 8040)) %>%
    tidyr::unnest(cols = c(estimated_total_fip_landings_mt, estimated_total_fishery_landings_mt, landings_date)) %>%
    dplyr::mutate(species = as.factor(species))

  species.bc <- fipinventory.bc %>%
    dplyr::distinct(species) %>%
    dplyr::pull(species)

  lf.traits <- lifehistorytraits %>%
    dplyr::select(-Trait,-Definition) %>%
    dplyr::filter(quant_index == 1) %>%
    tidyr::unnest(cols = c(Low, Moderate, High, `Very high`)) %>%
    tidyr::pivot_longer(cols=Low:`Very high`,names_to = "category") %>%
    dplyr::mutate(score=dplyr::if_else(category=="Low", 1,
                                       dplyr::if_else(category=="Moderate", 2,
                                                      dplyr::if_else(category=="High", 3, 4)))) %>%
    dplyr::rename(traits = col_name) %>%
    dplyr::select(-quant_index, -ideal_directionality)

  col.traits <- lf.traits %>%
    dplyr::distinct(traits) %>%
    dplyr::pull(traits)

  lifehistory.bc <- lifehistorydata %>%
    dplyr::select(dplyr::matches(c("species","common_name",col.traits))) %>%
    dplyr::filter(species %in% species.bc) %>%
    tidyr::unnest(cols = c(fecundity_mean_eggs)) %>%
    tidyr::pivot_longer(cols=3:16,names_to = "traits") %>%
    dplyr::left_join(lf.traits, by=c("traits","value")) %>%
    dplyr::mutate(category = as.factor(category), species = as.factor(species), common_name = as.factor(common_name)) %>%
    dplyr::left_join(fipinventory.bc, by = c("species","common_name")) %>%
    dplyr::mutate(fip_id_number = as.factor(fip_id_number))

  lifehistory.trait <- lifehistory.bc %>%
    dplyr::select(species, traits, score) %>%
    tidyr::pivot_wider(id_cols = c(species), names_from = traits, values_from = score)

  return(lifehistory.trait)

}

