#' QA/QC Licor data
#'
#' @param x Data frame. Input data
#' @param out.dir Output directory for QC data and info
#' @param gsw Character.  Name of column containing conductance values.
#' @param gsw.cutoff Numeric. Cutoff for low conductance.
#' @param Ci Character.  Name of column containing Ci values.
#' @param Ci.cutoff Numeric list. Cutoff range of values in form (low, high) for nonsensical Ci.  Default is c(0, 2000)
#' @param Tleaf Character.  Name of column containing Tleaf values. Optional.
#' @param Tleaf.cutoff Numeric. Cutoff for indiviual Tleaf variation from mean.
#' @param QC Character. Initial QC column name. Optional
#'
#' @export

licor.qc <- function(x, out.dir=NULL, gsw, gsw.cutoff=0, Ci, Ci.cutoff=c(0,2000), Tleaf, Tleaf.cutoff=NULL, QC=NULL){

  # Create extra QC columns
  x[ c("initial", "gsw.check", "ci.check", "tleaf.check")] <- 0
  # Create data.frame for data that fails checks to be added to
  failed <- data.frame(matrix(ncol=ncol(x), nrow=0))
  names(failed) <- names(x)

  # Remove all values not meeting initial QC
  if( !is.null(QC) ){
    x$initial <- x[,QC]
    failed <- rbind(failed, x[ x[,QC] == 1, ])
    x <- x[ !x[,QC] == 1, ]
  }

  # Mark all values not meeting gsw and Ci QC parameters
  if( !is.null(gsw.cutoff) ) x$gsw.check <- ifelse( x[,gsw] > gsw.cutoff, 0, 1)
  if( !is.null(Ci.cutoff) ) x$ci.check <- ifelse( x[,Ci] > Ci.cutoff[1] & x[,Ci] < Ci.cutoff[2], 0, 1)

  # Mark all values not meeting temp QC parameters (optional)
  if( !is.null(Tleaf.cutoff) ){
      Tleaf.mean <- mean(x[,Tleaf], na.rm = T)
      x$tleaf.check <- ifelse(x[,Tleaf] < Tleaf.mean + Tleaf.cutoff & x[,Tleaf] > Tleaf.mean - Tleaf.cutoff, 0, 1)
  }

  return(x)

}

##EOF
