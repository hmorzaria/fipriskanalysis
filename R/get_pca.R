#' Get PCA results
#'
#' @param eachfile
#'
#' @return this.file.max
#' @export
#'
#' @examples
get_pca <- function(eachfile){

  this.file <- readr::read_csv(eachfile)
  print(eachfile)

  if(grepl("peligro",eachfile)){

    this.category <- stringr::str_split(eachfile,pattern="/") %>%
      unlist %>%
      .[7] %>%
      gsub("pca_","", .) %>%
      gsub("_contrib.csv","", .) %>%
      gsub("_", " ", .)
    print(this.category)

  } else {

    this.category <- stringr::str_split(eachfile,pattern="/") %>%
      unlist %>%
      .[7] %>%
      gsub("pca_","", .) %>%
      gsub("_contrib.csv","", .) %>%
      gsub("_", " ", .)
    print(this.category)

  }


  this.file.max <- this.file %>%
    dplyr::arrange(Dim.1) %>%
    .[nrow(.),ncol(.)] %>%
    dplyr::mutate(categoria=this.category)

  return(this.file.max)

}
