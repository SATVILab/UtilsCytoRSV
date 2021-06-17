#' @rdname chnl_lab
#'
#' @title Get channel- or marker-labelling named vectors
#'
#' @description From a cytometry object (e.g. flowFrame or flowSet),
#' get a named vector that converts between channel names and marker names.
#' If the marker name is not available, then it is replaced with the channel name.
#'
#' @param data object of class flowFrame, flowSet. Channel and corresponding
#' marker names are drawn from here.
#'
#' @return A named character vector.
#'
#' @examples
#' data('GvHD', package = 'flowCore')
#' chnl_lab(GvHD)
#' marker_lab(GvHD)
#' fr <- GvHD[[1]]
#' chnl_lab(fr)
chnl_lab <- function(data){

  fr <- switch(class(data)[1],
               'flowFrame' = data,
               'flowSet' = data[[1]],
               stop('class of data not recognised'))

  adf <- flowCore::parameters(fr)@data
  lab_vec <- setNames(adf$desc, adf$name)
  for(i in seq_along(lab_vec)){
    if(is.na(lab_vec[i])){
      lab_vec[i] <- names(lab_vec)[i]
    }
  }

  lab_vec

}

#' @rdname chnl_lab
marker_lab <- function(data){
  chnl_lab_vec <- chnl_lab(data)
  setNames(names(chnl_lab_vec), chnl_lab_vec)
}
