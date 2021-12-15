sample.try <- function(list_trait){

  list_trait.single <- list_trait
  for (iparam in seq(1,length(list_trait))){
    list_trait.single[[iparam]] <- sample(size = 1,list_trait[[iparam]])
  }

  return(list_trait.single)
}
