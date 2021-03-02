#' Function to change absorbance in licor files
#'
#' @param x Data frame with licor 6400 data from text file.
#' @param red.abs Numeric. Red absorbance.
#' @param blue.abs Numeric. Blue absorbance.
#' @param adark Numeric. Dark Assimilation.  Must be negative.
#'
#' @export


change.abs <- function(x, red.abs, blue.abs, adark = NULL){

  x$RedAbs <- red.abs
  x$BlueAbs <- blue.abs
  if(!is.null(adark)){
    x$Adark <- adark
  }

  x$LeafAbs <- (x$X.Blue * x$BlueAbs + (100 - x$X.Blue) * x$RedAbs) / 100
  x$ETR <- x$PhiPS2 * x$PS2.1 * x$LeafAbs * x$ParIn.Fs
  x$PhiCO2 <- (x$Photo - x$Adark) / (x$PARi * x$LeafAbs)

}
