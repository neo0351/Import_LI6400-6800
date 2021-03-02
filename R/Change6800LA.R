#' Change leaf area for Licor 6800 files.  Read data into R using read6800() for best results.
#'
#' This function allows you to change leaf area of Licor 6800 csv files.
#' @param x Data frame with licor 6800 data.
#' @param leaf.area New leaf area.
#' @param Out.file logical. Output a file instead of a dataframe.
#' @keywords Licor 6800
#' @export


change.6800la <- function(x, leaf.area, Out.file=FALSE){
  # Following lines change each column that is effected by leaf area.  Equations
  # from Licor 6800 .xlsx file
  x$S <- leaf.area
  x$E <- x$Flow * (x$H2O_s - x$H2O_r) / (100 * leaf.area * (1000 - x$H2O_s))
  x$A <- x$Flow * (x$CO2_r - x$CO2_s * (1000 - x$H2O_r) / (1000 - x$H2O_s)) / (100 * leaf.area)
  x$gbw <- x$blfa_3 + x$blfa_2 * leaf.area + x$blfa_1 * leaf.area^2
  x$TleafEB <- (x$Tair + (x$Rabs + 2 * 0.95 * 5.67e-8 * (((x$Tair + LTConst.deltaTw) + 273)^4 - (x$Tair + 273)^4) - 44100 * x$E) / (1.84 * 29.3 * x$gbw + 8 * 0.95 * 5.67e-8 * (x$Tair + 273)^3))
  x$TleafCnd <- LTConst.fT1 * x$Tleaf + LTConst.fT2 * x$Tleaf2 + LTConst.fTeb * x$TleafEB
  x$SVPleaf <- 0.61365 * exp(17.502 * x$TleafCnd / (240.97 + x$TleafCnd))
  x$VPDleaf <- (x$SVPleaf - x$H2O_s * (x$Pa + x$Delta_Pcham) / 1000)
  x$gtw <- x$E * (1000 - (1000 * 0.61365 * exp(17.502 * x$TleafCnd / (240.97 + x$TleafCnd)) / (x$Pa + x$Delta_Pcham) + x$H2O_s) / 2) / ( 1000 * 0.61365 * exp(17.502 * x$TleafCnd / (240.97 + x$TleafCnd)) / (x$Pa + x$Delta_Pcham) - x$H2O_s)
  x$gsw <- 2 / ((1 / x$gtw - 1 / x$gbw) + sqrt((1 / x$gtw - 1 / x$gbw)^2 + 4 * x$K / ((x$K + 1)^2 * (2 / (x$gtw * x$gbw) - (1/x$gbw)^2))))
  x$gtc <- 1 / ((x$K + 1) / (x$gsw / 1.6) + 1 / (x$gbw / 1.37)) + x$K / ((x$K + 1) / (x$gsw / 1.6) + x$K / (x$gbw / 1.37))
  x$Ci <- ((x$gtc - x$E / 2) * x$CO2_s - x$A) / (x$gtc + x$E / 2)
  x$Pci <- x$Ci * (x$Pa + x$Delta_Pcham) / 1000
  # Last two columns are only in files with fluorescence
  if("Afs" %in% names(x)) x$Afs <- x$A
  if("PhiCO2" %in% names(x)) x$PhiCO2 <- (x$Afs - x$Adark) / x$Qabs_fs
  if(Out.file){
    if(dir.exists(paste(path, "LeafAreaCorr", sep="/")) == F){
      dir.create(paste(path, "LeafAreaCorr", sep="/"))
    }
    write.table(Read6800File(paste(path, file.name, sep="/"), constants=F, Out.data=F), paste(path, "LeafAreaCorr", file.name, sep="/"), row.names=F, col.names=F, sep=",")
    write.table(x, paste(path, "LeafAreaCorr", file.name, sep="/"), append=T, row.names=F, col.names=F, sep=",")
    } else {
      return(x)
    }
}
