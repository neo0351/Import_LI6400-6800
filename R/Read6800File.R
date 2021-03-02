#' Read Licor 6800 files
#'
#' Function reads file into R, creates varibles from header constants, changes special characters to plain text, and outputs a dataframe.
#' @param file.name File name of licor file.  If file not in working directory, include path to file.
#' @param constants Logical.  If true, create varibles for each of the constants found in the header.  If false, varibles in header will be deleted.
#' @param Out.data Logical. If true, outputs data from file.  If false, outputs constants header only.
#' @keywords Licor 6800
#' @export


read6800 <- function(file.name, constants = TRUE, Out.data=TRUE){
  rows <- 70
  # Reads first line to determine if file is tab or comma delimited
  if(grepl(",", readLines(file.name, 1))){
    # Determine how many rows to skip
    skip.row <- grep("obs", read.csv(file.name, nrows=rows, header=F, as.is=T)[,1])+1
    # Read file skipping header
    licor.df <- read.csv(file.name, header=F, as.is=T, skip=skip.row)
    # Create a list of the header names
    header.txt <- unname(unlist(head(read.csv(file.name, skip=skip.row-2, as.is=T, header=F),1)))
    # Create constants header data.frame to be reinserted
    const.df <- read.csv(file.name, header=F, as.is=T, nrows=skip.row)
    # Read in Licor 6800 constants from header
    temp <- read.csv(file.name, header=F, as.is=T, nrow=skip.row-3)
    # Change colons to periods in consants names
    temp[,1] <- gsub(":", ".", temp[,1])
  } else {
    # Same as above but for tab delimited files
    skip.row <- grep("obs\t", read.delim(file.name, nrows=rows, header=F, sep="\n")[,1])+1
    licor.df <- read.delim(file.name, header=F, as.is=T, skip=skip.row)
    header.txt <- unname(unlist(head(read.delim(file.name, skip=skip.row-2, as.is=T, header=F),1)))
    const.df <- read.delim(file.name, header=F, as.is=T, nrows=skip.row)
    temp <- read.delim(file.name, header=F, as.is=T, nrow=skip.row-3)
    temp[,1] <- gsub(":", ".", temp[,1])
  }
  # Change from greek letter Delta to "Delta_".  R does not like special characters
  # in names of columns
  header.txt <- gsub("_Pcham", "Delta_Pcham", header.txt)
  header.txt <- gsub("_CO2", "Delta_CO2", header.txt)
  header.txt <- gsub("_H2O", "Delta_H2O", header.txt)
  header.txt <- gsub("P3__F", "P3Delta_F", header.txt)
  header.txt <- gsub("\u0394", "Delta_", header.txt)
  header.txt <- gsub("\u00CE", "Delta_", header.txt)
  header.txt <- gsub("\u0025", "percent", header.txt)
  # If any columns have a name of NA, change it to "delete".  Sometimes there is a
  # blank column created.
  header.txt[is.na(header.txt)] <- "delete"
  # Change column names from default R names to their proper names
  names(licor.df) <- header.txt
  # Delete blank columns
  licor.df <- licor.df[ ,!names(licor.df) %in% "delete"]
  # Create varibles for each of the constants found in the header
  if(constants) mapply(assign.const, temp[,1], temp[,2])
  if(Out.data){
    return(licor.df)
  }
  else{ return(const.df)
  }
}
