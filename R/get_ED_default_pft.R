get_ED_default_pft <- function(xml, var, pft_target = NULL){

  history_xml <- XML::xmlParse(xml)
  history_list <- XML::xmlToList(history_xml)

  if (!is.null(pft_target)){
    pft <- 0
    while (pft != pft_target){
      pft <- history_list %>% .[["pft"]] %>% .[["num"]]  %>% str_replace("/n", "") %>% str_trim() %>% as.numeric()
      if (pft != pft_target){
        history_list$pft <- NULL
      }
    }
  }

  out <- history_list %>% .[["pft"]] %>% .[[var]] %>% str_replace("/n", "") %>% str_trim() %>% as.numeric()


  return(out)

}
