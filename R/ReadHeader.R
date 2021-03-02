#' Function to read LiCor 6400 header
#'
#' @param licor.file LiCor file.
#' @param start.line Numeric.  How many lines to start of data.  Usually set to more than expected.  The more lines, the slower the code.  Defaults to 30.
#' @param n.rows Numberic. How many rows to read.  This will be calculated if start.line is provided.
#' @export


read.header <- function(licor.file, start.line = 30, n.rows = NULL){

  if( is.null(n.rows) ){
  # Read first ~20 lines to find start of data of licor file
  licor.skip.df <- read.delim(licor.file, nrows=start.line, header=F, sep="\n", stringsAsFactors=F)
  # Find line where data starts using string "Obs[tab]"
  n.rows <- grep("Obs\t",licor.skip.df[,1])-1
  }

  # Read licor header, this will be placed into output file
  licor.header <- read.delim(licor.file, nrows=n.rows, header=F, sep="\n")

  return(licor.header)

}


###EOF
