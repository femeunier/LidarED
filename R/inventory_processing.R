#' @name patchnumber_from_position
#' @title extremum
#' @author Félicien Meunier
#' @export
#' @description Returns a meaningless (but unique) patch number from position x and y
#' @param x x position
#' @param y y position
#' @param patch_X patch size in X
#' @param patch_Y patch size in Y
#'
patchnumber_from_position <- function(x,y,patch_X,patch_Y){

  extr_x <- extremum(x)
  extr_y <- extremum(y)
  N_X <- ceiling((extr_x[2]-extr_x[1])/patch_X)
  N_Y <- ceiling((extr_y[2]-extr_y[1])/patch_Y)
  N <- N_X*N_Y

  compt <- 1
  patch <- rep(0,length(x))
  for (ix in seq(N_X)){
    for (iy in seq(N_Y)){
      pos <- which(x >= (extr_x[1] + (ix-1)*patch_X) &
                   x < (extr_x[1] + (ix)*patch_X) &
                   y >= (extr_y[1] + (iy-1)*patch_Y) &
                   y < (extr_y[1] + (iy)*patch_Y))
      patch[pos] <- compt
      compt <- compt + 1
    }
  }

  return(patch)
}
