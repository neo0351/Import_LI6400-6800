#' Function to change leaf area of LiCor 6400 text files.  Read data into R using read6400() for best results.
#'
#' @param x Data frame with licor 6400 data from text files.
#' @param leaf.area Numeric. New leaf area in cm^2.
#' @param rm.extra Logical. Remove extra columns that were created.  Defaults to True.
#' @export


change.6400la <- function(x, leaf.area, rm.extra=T){

  # Replace leaf area from licor file with the true leaf area
  x$Area <- leaf.area

  ## Recalculate everything that leaf area effects
  x$fda <- (x$Flow*0.000001)/(x$Area*0.0001)
  x$Photo <- (x$CO2R-x$CO2S*(1000-x$H2OR)/(1000-x$H2OS))*x$fda
  x$Trans <- (x$H2OS-x$H2OR)/(1000-x$H2OS)*x$fda
  x$Tair.K <- (x$Tleaf+273.15)
  x$Twall.k <- (x$Tair+273.15)
  x$R.W.per.m2 <- (x$PARi*x$f_parin+x$PARo*x$f_parout)*x$alphaK
  x$BLC_1 <- x$Area*x$BLCslope+x$BLCoffst
  x$BLCond <- x$BLC_1*(x$StmRat+1)*(x$StmRat+1)/((x$StmRat+1)^2+1)
  x$Tl.Ta <- ((x$R.W.per.m2+0.00000010773*(x$Twall.k^4-x$Tair.K^4))-x$Trans*44100)/(x$BLC_1*51.4+0.00000043092*x$Tair.K^3)
  x$SVTleaf <- 0.61365*exp(17.502*x$CTleaf/(240.97+x$CTleaf))
  x$h2o.i <- (x$SVTleaf*1000)/x$Press
  x$h2odiff <- x$h2o.i-x$H2OS
  x$CTair <- ifelse(x$EBal.==0,(x$Tair+x$Tleaf)/2,x$Tleaf)
  x$SVTair <- 0.61365*exp(17.502*x$CTair/(240.97+x$CTair))
  x$CndTotal <- ifelse(x$h2odiff != 0, (1000-(x$h2o.i+x$H2OS)/2)/x$h2odiff*x$Trans, 0)
  x$vp.kPa <- x$H2OS*x$Press/1000
  x$VpdA <- x$SVTair-x$vp.kPa
  x$Cond <- ifelse(x$CndTotal != 0, 1/(1/x$CndTotal-1/x$BLCond), 0)
  x$CndCO2 <- 1/(1.6/x$Cond+1.37/x$BLCond)
  x$Ci <- ((x$CndCO2-x$Trans/2)*x$CO2S-x$Photo)/(x$CndCO2-x$Trans/2)
  x$Trmmol <- x$Trans*1000
  x$Ci.per.Ca <- x$Ci/x$CO2S

  if("PhiCO2" %in% names(x)){
    x$PhiCO2 <- (x$Photo - x$Adark) / (x$PARi * x$LeafAbs)
  }

  if(rm.extra){
    # Remove extra columns that were used for calculations
    x <- x[ , -c((ncol(x)-15):ncol(x))]
  }

  return(x)
}


