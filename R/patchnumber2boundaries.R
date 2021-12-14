patchnumber2boundaries <- function(patchnum=100,xmin=0,xmax= 1000,ymin=0,ymax=500,patch_X=20,patch_Y=20){


  N_X <- ceiling((xmax - xmin)/patch_X)
  N_Y <- ceiling((ymax-ymin)/patch_Y)
  X <- (patchnum - 1)%/%N_Y
  Y <- (patchnum - 1) %% N_Y

  return(list(xmin = X*patch_X,
              xmax = (X + 1)*patch_X,
              ymin = Y*patch_Y,
              ymax = (Y + 1)*patch_Y))
}
