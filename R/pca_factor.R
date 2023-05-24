#' @title Normalize function
#' @description  Normalize from 0-1
#' @details INPUT: 1) data
#' @details OUTPUT: 1) normalized data
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com


pca_factor <- function(thisdata, datasetname, namesdata){

  #eliminate column with row names, needs only data values

  print(datasetname)

  data.set <- thisdata %>%
    dplyr::select(tidyr::any_of(namesdata)) %>%
    as.data.frame

  items.scale  <- ncol(data.set)
  #obtain correlation matrix
  hozdatamatrix.impact <- stats::cor(data.set, use="complete.obs")
  #print correlation plot
  #pairs(data.set)

  # print out the correlation matrix but ask for numbers to 3 decimal places
  corrmat.impact  <- round(hozdatamatrix.impact,3)
  corrmat.impact
  # bartlett test - want a small p value here to indicate c0rrelation matrix not zeros
  psych::cortest.bartlett(data.set)

  #principal components analysis

  # Determine Number of Factors to Extract

  #This code can be use to determine the optimal number of factors, however Jacob et al (2012)
  #recommends using a single factor solution
  # Generally, if the goal is simple and parsimonious description of a correlation
  # or covariance matrix, the first k principal components
  # will do a better job than any other k-dimensional solution
  ev <- eigen(hozdatamatrix.impact) # get eigenvalues
  ap <- nFactors::parallel(subject=nrow(data.set),var=ncol(data.set),
                 rep=100,cent=.05)
  try(nS <- nFactors::nScree(ev$values, ap$eigen$qevpea)) # Non graphical Catell's score test, returns an analysis of the number of factors to retain
  #plotnScree(nS)

  #run single factor solution
  model1.impact<- psych::principal(data.set, nfactors = 1, rotate = "none",scores=TRUE) #can specify rotate="varimax"
  model1.impact
  #SS loading is the eigenvalue
  # h2is called the communality estimate. Measures the % of variance
  # in an observed variable accounted for by the retained components

  # prcomp library factoextra
  #plot variables that continue most to the PCA
  res.pca <- stats::prcomp(data.set, scale = TRUE)

  var <- factoextra::get_pca_var(res.pca)

contrib.var <- var$contrib %>%
  as.data.frame() %>%
  dplyr::mutate(var = rownames(var$contrib))

readr::write_csv(contrib.var, here::here("outputs","analysis",paste0("pca_",datasetname, "_contrib.csv")))

factoextra::fviz_pca_var(res.pca,
               col.var = "contrib", # Color by contributions to the PC
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
               repel = TRUE     # Avoid text overlapping
  )

  variable.plot <-  factoextra::fviz_cos2(res.pca, choice="var", axes = 1 )

  ggplot2::ggsave(here::here("outputs","figures",paste0("variables_pca_",datasetname,".png")), variable.plot, device="png", width = 8, height = 5)



  # can find the reproduced correlations and the communalities (the diagonals)
  psych::factor.model(model1.impact$loadings)

  model.loadings <- unclass(model1.impact$loadings) %>%
    tidyr::as_tibble(rownames="var") %>%
    dplyr::rename(loadings = PC1) %>%
    dplyr::mutate(data_name = datasetname)

  #readr::write_csv(model.loadings,here::here("outputs","analysis",paste0("model_loadings_",datasetname,".csv")))


  # the diagonals represent the uniqueness values (1- R squared):
  residuals.impact <- psych::factor.residuals(hozdatamatrix.impact, model1.impact$loadings)
  residuals.impact
  # nice to plot the residuals to check there are normally distributed
  #hist(residuals.impact)

  # to save the above values we need to add them to a dataframe
  factorscores.impact <- unlist(model1.impact$scores) %>%
    as.data.frame() %>%
    dplyr::bind_cols(. ,tidyr::tibble(COM_ID = thisdata$COM_ID, NOM_LOC = thisdata$NOM_LOC))

  readr::write_csv(factorscores.impact, here::here("outputs","analysis",paste0("factor_scores_",datasetname,".csv")))

  #obtain largest eigenvalue
  larg.eigen  <- max(model1.impact$values)
  #Armor's Theta tests for internal consistency of a factor scale
  Theta  <- (items.scale/(items.scale-1)) * (1-(1/larg.eigen))

  #shows the summary of the loadings table

  p <- print(model1.impact)

  model.summary <-  round(p$Vaccounted,2)  %>%
    tidyr::as_tibble(rownames = "Var") %>%
    dplyr::bind_rows(tidyr::tibble(Var = c("Eigenvalue","Theta"),PC1=c(larg.eigen, Theta))) %>%
    dplyr::filter(Var!="SS loadings")

  readr::write_csv(model.summary,here::here("outputs","analysis",paste0("model_summary_",datasetname,".csv")))

  return(model.loadings)

}

