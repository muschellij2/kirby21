#' @name get_image_filenames
#' @title Get Image Filenames
#'
#' @description Return the filenames for the images
#' @param ids ID to return
#' @param modalities vector of image modalities within
#' \code{c("FLAIR", "MPRAGE", "T2w", "fMRI", "DTI")} to return
#' @param visits Vector of scan indices to return (1 or 2 or both)
#' @return Data.frame of filenames
#' 
#' @examples
#' get_image_filenames()
#' @export
get_image_filenames = function(ids = get_ids(), 
                               modalities = c("FLAIR", "MPRAGE", "T2w", 
                                              "fMRI", "DTI"), 
                               visits = c(1,2)){
  modalities = unique(modalities)
  visits = as.numeric(visits)
  visits = sprintf("%02.0f", visits)
  v_ids = c(outer(ids, visits, paste, sep = "-"))
  fnames = c(outer(v_ids, modalities, paste, sep = "-"))
  fnames = paste0(fnames, ".nii.gz")
  df = data.frame(fname = fnames, stringsAsFactors = FALSE)
  ss = strsplit(df$fname, "-")
  df$id = sapply(ss, `[`, 1)
  df$visit = as.numeric(sapply(ss, `[`, 2))
  df$fname = file.path(paste0("visit_", df$visit), df$id, df$fname)
  df$id = NULL
  filenames = system.file( df$fname, package = "kirby21")
  return(filenames)
}

#' @title Get Image Filenames in a matrix
#'
#' @description Return the filenames for the images
#' @param ... arguments passed to \code{\link{get_image_filenames}}
#' @param long if \code{TRUE}, each row is a subject, visit, modality pair
#' @importFrom tidyr spread
#' @export
get_image_filenames_df = function(...,
                                  long = FALSE){
  modality = fname = NULL
  
  filenames = get_image_filenames(...)
  df = data.frame(fname = filenames, stringsAsFactors = FALSE)
  ff = basename(df$fname)
  ff = gsub("[.]nii[.]gz$", "", ff)
  ss = strsplit( ff, "-")
  x = do.call("rbind", ss)
  colnames(x) = c("Subject_ID", "visit", "modality")
  x = data.frame(x, stringsAsFactors = FALSE)
  x$Subject_ID = as.numeric(x$Subject_ID)
  x$visit = as.numeric(x$visit)
  df = data.frame(fname = filenames, stringsAsFactors = FALSE)
  df = cbind(df, x)
  if (!long) {
    df = spread(df, key = modality, value = fname)
  }

  return(df)
}

#' @rdname get_image_filenames_df
#' @export
get_image_filenames_matrix = function(...,
                                      long = FALSE){
  df = as.matrix(get_image_filenames_df(...,
                                        long = long))
  return(df)
}

#' @rdname get_image_filenames_df
#' @export
get_image_filenames_list = function(...){
  
  df = get_image_filenames_df(..., long = TRUE)
  df$Subject_ID = df$visit = NULL
  ss = as.list(df)
  return(ss)
}


#' @rdname get_image_filenames_df
#' @export
get_image_filenames_list_by_visit = function(...){

  df = get_image_filenames_df(..., long = TRUE)
  ss = split(df, df$visit)
  ss = lapply(ss, function(x){
    split(x, x$Subject_ID)
  })
  return(ss)
}

#' @rdname get_image_filenames_df
#' @export
get_image_filenames_list_by_subject = function(...){
  
  df = get_image_filenames_df(..., long = TRUE)
  ss = split(df, df$Subject_ID)
  ss = lapply(ss, function(x){
    split(x, x$visit)
  })
  return(ss)
}