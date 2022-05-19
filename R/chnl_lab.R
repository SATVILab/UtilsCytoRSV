#' @rdname chnl_lab
#'
#' @title Get markers and channels
#'
#' @description From a cytometry object (e.g. flowFrame or flowSet),
#' either get a character vector of markers
#' or channels (get_chnl and get_marker),
#' or get a named vector that converts
#' between channel names and marker names (e.g. chnl_to_marker).
#'
#' @param data object of class flowFrame, flowSet. Channel and corresponding
#' marker names are drawn from here.
#'
#' @details
#' Note that chnl_lab is equivalent to chnl_to_marker,
#' and marker_lab is equivalent to marker_to_chnl.
#'
#' @return A named character vector.
#'
#' @export
#'
#' @aliases marker_lab, chnl_to_marker, marker_to_chnl, get_marker, get_chnl
#'
#' @examples
#' data("GvHD", package = "flowCore")
#' chnl_lab(GvHD)
#' marker_lab(GvHD)
#' fr <- GvHD[[1]]
#' chnl_lab(fr)
chnl_lab <- function(data) {
  adf <- switch(class(data)[1],
    "flowFrame" = flowCore::parameters(data)@data,
    "flowSet" =  flowCore::parameters(data[[1]])@data,
    "cytoframe" = flowCore::parameters(data)@data,
    "cytoset" = flowCore::parameters(data[[1]])@data,
    stop("class of data not recognised")
  )

  lab_vec <- setNames(adf$desc, adf$name)
  for (i in seq_along(lab_vec)) {
    if (is.na(lab_vec[i])) {
      lab_vec[i] <- names(lab_vec)[i]
    }
  }

  lab_vec
}

#' @rdname chnl_lab
#' @export
marker_lab <- function(data) {
  chnl_lab_vec <- chnl_lab(data)
  setNames(names(chnl_lab_vec), chnl_lab_vec)
}

#' @rdname chnl_lab
#' @export
chnl_to_marker <- chnl_lab

#' @rdname chnl_lab
#' @export
marker_to_chnl <- marker_lab

#' @export
get_chnl <- function(data) {
  names(chnl_lab(data))
}
#' @export
get_marker <- function(data) {
  names(marker_lab(data))
}
