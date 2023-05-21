#' Calculate distance change in future distribution
#'
#' @param presentraster historical raster
#' @param speciesname scientific name
#' @param dirname common species name
#' @param futurescenarioname1 ssp126
#' @param futurescenarioname2 ssp245
#' @param futurescenarioname3 ssp585
#'
#' @return dist.change
#' @export
#'
#' @examples
calc_distchange <- function(eachfutureraster, raster.present.proj, eachspeciesname, crs.eqap, eachdirname){

    print(eachfutureraster)
    raster.future <- terra::rast(here::here("data-raw","modelos",eachdirname,paste0(eachspeciesname,"_",eachfutureraster,"_avg.asc")))
    raster::plot(raster.future)
    raster.future.proj <- terra::project(raster.future, crs.eqap)

    delta <- raster.future.proj - raster.present.proj
    terra::plot(delta, main = 'Change in Suitability')


    ## calculate biotic velocity
    #############################
    #https://github.com/adamlilith/enmSdmX/blob/master/man/examples/bioticVelocity_examples.r

    series <- c(raster.present.proj, raster.future.proj)


    names(series) <- c('present', eachfutureraster)
    terra::plot(series)

    times <- c(2022, 2050)
    quants <- c(0.10, 0.90)

    bv <- enmSdmX::bioticVelocity(
      x = series,
      times = times,
      quants = quants,
      cores = 2
    )

    dist.change <- tidyr::tibble(species = eachspeciesname, scenario=eachfutureraster,
    centroid_velocity = bv$centroidVelocity, # positive value shows Northward shift
    suitability = bv$simpleMeanDiff, # average change in suitability from one time period to next
    rmsq = bv$rmsd, # root-mean square difference from one time period to the next
    godsoeESp = 1 - bv$godsoeEsp, # similarity
    schoenerD = 1 - bv$schoenerD, # similarity
    warrenI = 1 - bv$warrenI, # similarity
    cor = 1 - bv$cor, #similarity
    rankCor = 1 - bv$rankCor) #similarity

    return(dist.change)
}




