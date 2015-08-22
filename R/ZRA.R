#' @name ZRA
#'
#' @title Dynamic Plots for Time Series Forecasting
#'
#' @param data (ts) Time series data.
#' @param VZ (numeric) Forecast period.
#' @param SN (numeric) significance levels.
#' @param ... further arguments passed to the forecast-function.
#'
#' @return An Object of class "ZRA".
#'
#' @examples zra <- ZRA(fdeaths)
#' plot(zra, zero = TRUE)
#'
#' @seealso forecast, dygraphs
#'
#' @import forecast
#' @import dygraphs
#' @importFrom stats end frequency ts is.ts
#'
#' @export

ZRA <- function(data,VZ = 10, SN = c(0.80,0.95), ...) {

  Vorhersagezeitraum <- VZ
  Signifikanznivau <- SN
  startwert <- end(data)[1]+1
  frequenz  <- frequency(data)
  daten <- data


  if (is.ts(daten)== TRUE) {

    if (is.matrix(daten)==TRUE) {
      result <- NULL
      stop("Momentan kann nur eine Zeitreihe gleichzeit analysiert werden.")
    }
    else {

      prognose <- forecast(daten, h = Vorhersagezeitraum, level=Signifikanznivau, ...)

      result <- list()
      result$reihe <- data
      result$SN <- Signifikanznivau
      result$VZ <- Vorhersagezeitraum

      if (length(Signifikanznivau)==1) {

        up1 <- ts(prognose$upper[,1],start=startwert,frequency = frequenz)
        low1 <- ts(prognose$lower[,1],start=startwert,frequency = frequenz)
        fit1 <- (up1 + low1)/2

        result$up1 <- up1
        result$low1 <- low1
        result$fit1 <- fit1
        result$piv1 <- cbind(fit1,up1,low1)

      }

      if (length(Signifikanznivau)==2) {

        up1 <- ts(prognose$upper[,1],start=startwert,frequency = frequenz)
        low1 <- ts(prognose$lower[,1],start=startwert,frequency = frequenz)
        fit1 <- (up1 + low1)/2

        up2 <- ts(prognose$upper[,2],start=startwert,frequency = frequenz)
        low2 <- ts(prognose$lower[,2],start=startwert,frequency = frequenz)
        fit2 <- (up2 + low2)/2

        result$up1 <- up1
        result$low1 <- low1
        result$fit1 <- fit1
        result$piv1 <- cbind(fit1,up1,low1)

        result$up2 <- up2
        result$low2 <- low2
        result$fit2 <- fit2
        result$piv2 <- cbind(fit2,up2,low2)

      }
    }
  }

  else {
    result <- NULL
    stop("Daten entsprechend zwingend einem Zeitreihen(ts)-Objekt.")
  }

  class(result) <- "ZRA"

  return(result)

}



#' @name print.ZRA
#'
#' @title Printig a ZRA-Class Obejct
#'
#' @description This print-Method will print out the original Time Series and the predicted Data.
#'
#' @param x (ZRA) An ZRA-Object.
#' @param ... further arguments passed to the print method.
#'
#' @method print ZRA
#'
#' @usage \method{print}{ZRA}(x, ...)
#'
#' @export

print.ZRA <- function(x, ...){

  sn <- as.character(x$SN)

  if (length(x$SN)==1) {
    colnames(x$piv1) <- c(paste("Fit",sn,sep = "-"),paste("Up",sn,sep = "-"),paste("Low",sn,sep = "-"))
    cat("Zeitreihe: \n")
    print(x$reihe)
    cat("\n Prognose: \n")
    print(round(x$piv1,3))

  }

  if (length(x$SN)==2) {

    tabelle <- cbind(x$piv1,x$piv2)

    colnames(tabelle) <- c(paste("Fit",sn[1],sep = "-"),paste("Up",sn[1],sep = "-"),paste("Low",sn[1],sep = "-"),paste("Fit",sn[2],sep = "-"),paste("Up",sn[2],sep = "-"),paste("Low",sn[2],sep = "-"))
    cat("Zeitreihe: \n")
    print(x$reihe)
    cat("\n Prognose: \n")
    print(round(tabelle,3))

  }

  if (length(x$SN)!=1 & length(x$SN)!=2 ) {
    stop("Dargestellt werden nur maximal 2 Signivikanzniveaus gleichzeitig.")

  }

}

#' @name plot.ZRA
#'
#' @title Ploting a ZRA-Class Obejct
#'
#' @description This plot-Method will plot the original Time Series and the predicted Data, using dygraphs.
#'
#' @param x (ZRA) An ZRA-Object.
#' @param zero (boolean) If zero=TRUE, they will be the 0 included in the Graph.
#' @param ... further arguments passed to the plot method.
#'
#' @method plot ZRA
#'
#' @usage \method{plot}{ZRA}(x,zero, ...)
#'
#' @export

plot.ZRA <- function(x, zero =TRUE, ...) {

  if (length(x$SN)==1 || length(x$SN)==2) {

    result <- ZRAplot(x, ...)
    return(result)

  }

  if (length(x$SN)!=1 & length(x$SN)!=2 ) {
    stop("Dargestellt werden nur maximal 2 Signivikanzniveaus gleichzeitig." )

  }

}

ZRAplot <- function(x,zero=TRUE, ...) {

  sn <- as.character(x$SN)
  h <- as.character(x$VZ)

  if (length(x$SN)==1) {

    plotreihe1 <- cbind(x$reihe, x$piv1, x$up1, x$low1)

    plot1 <- dygraph(plotreihe1)  %>%
      dySeries("x$reihe",label = "Zeitreihe",color="blue",fillGraph = TRUE) %>%
      dyLimit(limit=0) %>%

      dySeries("x$up1",label = "Obere Intervallgrenze",color="chocolate",strokePattern = "dotted") %>%
      dySeries("x$low1",label = "Untere Intervallgrenze",color="chocolate",strokePattern = "dotted") %>%

      dySeries(c("x$piv1.up1","x$piv1.fit1","x$piv1.low1"),label="Point Estimator", color="red")  %>%

      dyOptions(includeZero = TRUE,fillAlpha =0.1 )  %>%
      dyRangeSelector()

    print(paste("Signifikanzniveau:",sn))
    print(paste("Prognosezeitraum:",h,"Perioden"))
    return(plot1)

  }

  if (length(x$SN)==2) {

    plotreihe2 <- cbind(x$reihe, x$piv1, x$up1, x$low1, x$piv2, x$up2, x$low2)

    plot2 <- dygraph(plotreihe2)  %>%
      dySeries("x$reihe",label = "Zeitreihe",color="blue",fillGraph = TRUE) %>%
      dyLimit(limit=0) %>%

      dySeries("x$up1",label = paste("Up",sn[1]),color="chocolate",strokePattern = "dotted") %>%
      dySeries("x$low1",label = paste("Low",sn[1]),color="chocolate",strokePattern = "dotted") %>%

      dySeries("x$up2",label = paste("Up",sn[2]),color="darkseagreen",strokePattern = "dotted") %>%
      dySeries("x$low2",label = paste("Low",sn[2]),color="darkseagreen",strokePattern = "dotted") %>%

      dySeries(c("x$piv1.up1","x$piv1.fit1","x$piv1.low1"),label=paste("Point Estimator",sn[1]), color="red")  %>%
      dySeries(c("x$piv2.up2","x$piv2.fit2","x$piv2.low2"),label=paste("Point Estimator",sn[2]), color="green")  %>%

      dyOptions(includeZero = zero,fillAlpha =0.1 )  %>%
      dyRangeSelector()

    print(paste("Signifikanzniveau:",sn[1],"(red),",sn[2],"(green)"))
    print(paste("Prognosezeitraum:",h,"Perioden"))
    return(plot2)

  }

}
