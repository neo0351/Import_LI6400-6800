#' Function to read LiCor 6400 text files
#'
#' @param licor.file Path to LiCor file.
#' @param start.line Numeric.  How many lines to start of data.  Usually set to more than expected.  The more lines, the slower the code.  Defaults to 30.
#' @param time Logical.  Convert time column from char to POSIXct.  Defaults to True.
#' @param leaf.area Numeric.  Leaf area inside chamber.  Only required if it needs to be changed.
#' @param file.out Logical.  Output a file with new leaf area instead of dataframe.  Output file will look like a 6400 file with remarks removed.  Default is False.
#' @export



read6400 <- function(licor.file, start.line = 30, time = T, leaf.area = NULL, file.out = F){
  options(stringsAsFactors = FALSE)
  # Read first ~20 lines to find start of data of licor file
  licor.skip.df <- read.delim(licor.file, nrows=start.line, header=F, sep="\n")
  # Find line where data starts using string "Obs[tab]"
  skip.row <- grep("Obs\t",licor.skip.df[,1])-1
  # Read full licor file skipping header lines
  licor.df <- read.delim(licor.file, skip=skip.row, as.is = T)
  # Delete lines with NA
  licor.df <- na.omit(licor.df)
  # Delete lines where Photo column is blank or has "Photo".  This deletes remarks which will
  # cause problems in TDL script
  licor.df <- licor.df[ licor.df$Photo!="" & licor.df$Photo!="Photo", ]
  # Change class of columns to character
  licor.df <- do.call(data.frame, lapply(licor.df, as.character))
  # Convert column classes as appropriate
  licor.df <- do.call(data.frame, lapply(licor.df, type.convert, as.is=T))

  if(time & !file.out){
    # Read date from header
    licor.date <- read.delim(licor.file, nrows=1, skip = 1, header=F, sep="\n", as.is=T)
    # Change abbreviation for Thursday
    licor.date <- gsub("Thr", "Thu", licor.date)
    # Convert date into class Date
    licor.date <- as.Date(licor.date, format = "%a %b %e %Y")
    # Add date to time and convert to POSIXct.  Time in R must have a date with it.
    licor.df$HHMMSS <- as.POSIXct(paste(licor.date, licor.df$HHMMSS))
  }

  if(!is.null(leaf.area)){
   licor.df <- change.6400la(licor.df, leaf.area, rm.extra = T)
  }

  if(file.out){
    if(file.exists(paste0(licor.file,"-Rla"))){
      warning(paste(licor.file, "-Rla exists,", licor.file, "-Rla not written"))
    } else {
      licor.header <- read.header(licor.file, start.line)
      message(paste("Written to", paste0(licor.file,"-Rla")))
      write.table(licor.header, file=paste0(licor.file,"-Rla"), sep="\t",row.names=F,col.names=F)
      suppressWarnings(write.table(licor.df, file=paste0(licor.file,"-Rla"), sep="\t", row.names=F, append=T))
    }
  } else {
    return(licor.df)
  }
}

###EOF
