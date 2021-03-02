#' Assign Constants for 6800.  Used by read6800()
#'
#' Function to create varibles for each of the constants found in the header
#' @param x x
#' @param y y


assign.const <- function(x, y){
  if(x != "" & y != "") out <- assign(x, type.convert(y, as.is=T), envir = parent.frame(3))
}
