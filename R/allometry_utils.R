#' @name dbh2h
#' @title dbh2h
#' @author Félicien Meunier
#' @export
#' @description Returns allometric h
#' @param href href (ED2)
#' @param b1Ht b1Ht (ED2)
#' @param b2Ht b2Ht (ED2)
#' @param dbh dbh

dbh2h <- function(href,b1Ht,b2Ht,dbh){
  return(href*(1 -exp(-b1Ht*(dbh**b2Ht))))
}

#' @name dbh2ca
#' @title dbh2ca
#' @author Félicien Meunier
#' @export
#' @description Returns allometric Ca
#' @param b1Ca b1Ca (ED2)
#' @param b2Ca b2Ca (ED2)
#' @param dbh dbh

dbh2ca <- function(b1Ca,b2Ca,dbh){
  return(b1Ca * (dbh^b2Ca))
}


#' @name dbh2bd
#' @title dbh2bd
#' @author Félicien Meunier
#' @export
#' @description Returns allometric dead Biomass
#' @param b1Bs_small b1Bs_small (ED2)
#' @param b2Bs_small b2Bs_small (ED2)
#' @param b1Bs_large b1Bs_large (ED2)
#' @param b2Bs_large b2Bs_large (ED2)
#' @param dbh_crit dbh_crit (ED2)
#' @param dbh dbh


dbh2bd <- function(b1Bs_small,b2Bs_small,b1Bs_large,b2Bs_large,dbh_crit,dbh){

  C2B <- 2

  dbh2bd <- rep(0,length(dbh))
  dbh2bd[dbh2bd <= dbh_crit] <- b1Bs_small / C2B * ((dbh[dbh2bd <= dbh_crit])^b2Bs_small)
  dbh2bd[dbh2bd > dbh_crit]  <- b1Bs_large / C2B * ((dbh[dbh2bd > dbh_crit])^b2Bs_large )

  return(dbh2bd)
}



