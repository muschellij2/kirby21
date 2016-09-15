#' @title Get IDs with Data in Package
#'
#' @description Return the IDs for the people scanned available for reading
#' 
#' @export
#' @importFrom utils data
get_ids = function(){
  # kirby21_demog = NULL
  # rm(list = "kirby21_demog")
  # 
  id_visit_1 = list.dirs(system.file("visit_1", package = "kirby21"), 
            recursive = FALSE, 
            full.names = FALSE)
  id_visit_2 = list.dirs(system.file("visit_1", package = "kirby21"), 
                         recursive = FALSE, 
                         full.names = FALSE)  
  ids = as.numeric(c(id_visit_1, id_visit_2))
  ids = unique(ids)
  # data("kirby21_demog", envir = environment())
  # ids = c(113, 505)
  return(ids)
}