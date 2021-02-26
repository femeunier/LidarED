#' @name dbh2h
#' @title dbh2h
#' @author Félicien Meunier
#' @export
#' @description Returns allometric h
#' @param href href (ED2)
#' @param b1Ht b1Ht (ED2)
#' @param b2Ht b2Ht (ED2)
#' @param dbh dbh
#' @param is.tropical is.tropical
#'
dbh2h <- function(href,b1Ht,b2Ht,dbh,is.tropical = TRUE){

  if (is.tropical){
    h = (href*(1 -exp(-b1Ht*(dbh**b2Ht))))
  } else {
    h = href + b1Ht * (1 - exp(b2Ht * dbh))
  }

  return(h)
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


#' @name dbh2agb_exp
#' @title dbh2agb_exp
#' @author Félicien Meunier
#' @export
#' @description Returns exp allometric dead Biomass
#' @param a a
#' @param b b
#' @param dbh dbh


dbh2agb_exp <- function(a,b,dbh){

  AGB <- exp(a+b*log(dbh*3.141593))

  return(AGB)
}




#' @name dbh2bl
#' @title dbh2bl
#' @author Félicien Meunier
#' @export
#' @description Returns allometric dead Biomass
#' @param b1Bs_small b1Bl_small (ED2)
#' @param b2Bs_small b2Bl_small (ED2)
#' @param b1Bs_large b1Bl_large (ED2)
#' @param b2Bs_large b2Bl_large (ED2)
#' @param dbh_crit dbh_crit (ED2)
#' @param dbh dbh


dbh2bl <- function(b1Bl_small,b2Bl_small,b1Bl_large,b2Bl_large,dbh_crit,dbh){

  C2B <- 2

  dbh2bl <- rep(0,length(dbh))
  dbh2bl[dbh2bl <= dbh_crit] <- b1Bl_small / C2B  * ((dbh[dbh2bl <= dbh_crit])^b2Bl_small)
  dbh2bl[dbh2bl > dbh_crit]  <- b1Bl_large / C2B  * ((dbh[dbh2bl > dbh_crit])^b2Bl_large )

  return(dbh2bl)
}





