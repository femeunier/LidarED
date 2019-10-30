#' @name extremum
#' @title extremum
#' @author Félicien Meunier
#' @export
#' @description Returns min and max
#' @param vector input vector


extremum <- function(vector){
  return(c(min(vector,na.rm = TRUE),max(vector,na.rm = TRUE)))
}
