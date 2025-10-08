
#' Subset JAGS summary object
#'
#' This function returns a subset of information contained in the JAGS 
#' results object.
#' 
#' 
#' This function is called by the functions \code{\link{trend}} and \code{\link{converge_fn}}.
#' 
#' @param x An object of class jagsUI as a result of a call to the wrapper
#' function \code{\link{trend}}.
#' @param parameter A character string naming the parameter(s) to extract 
#' from the JAGS summary. The character string can be a partial match, as it 
#' will extract all parameters containing this character string.
#' Defaults to "alpha".
#' @param doPlot A logical indicating whether the rhat values 
#' should be plotted.
#' 
#' @return jags.summary returns a matrix with one row for each model parameter and
#' columns describing the following variables 
#' mean    median parameter value
#' 2.5%    2.5th percentile parameter value
#' 50%     50th percentile parameter value
#' 97.5%   97.5th percentile parameter value
#' 
#' @export
#' 
#' @examples
#' y <- jags.summary(RNDU.model.results, "alpha", doPlot=TRUE)

jags.summary <- function(x, parameter = "alpha", doPlot = TRUE) {
  end <- nchar(parameter)
  names.vec <- substring(dimnames(x$summary)[[1]], 1, end)
  row.id <- grep(paste("^",parameter, sep=""), dimnames(x$summary)[[1]]) #must start with pattern
  rhat_summ <- x$summary[row.id,]
  if (doPlot == TRUE) {
    plot(seq(1:dim(rhat_summ)[1]), rhat_summ[,"Rhat"], xlab = "Index", 
         ylab = "Rhat", main = paste0("Rhat of ", parameter))
  }
  if(length(row.id)==1){
    temp <- t(as.data.frame(x$summary[row.id,]))
  } else {
    temp <- x$summary[row.id,]
  }
  row.names(temp ) <- dimnames(x$summary)[[1]][row.id]
    return(temp)
}

#' Random values for zero-inflation
#'
#' This function creates random values for zero inflation.  
#' It is called by the function \code{\link{inits_fn}}. 
#' 
#' 
#' This function replaces zero or missing (NA) abundance observations with 
#' random numbers to be used as starting values when zero inflation is assumed  
#' present.  It is called by the function \code{\link{inits_fn}}.
#' 
#' @param i deprecated. Defaults to 1.
#' @param x is a numeric vector.  Usually it identifies
#' the TIP variable in the list object used as input to the JAGS model.
#'
#' @return Returns a numeric vector.
#' @export 
#' 
#' @examples
#' pi.gen(i=1,x=RNDU.jags.data$TIP)

pi.gen <- function(i, x) {
  zeroNA <- ifelse(is.na(x), -10, x)
  out <- sapply(1:length(zeroNA), 
                function(z){ifelse(zeroNA[z]>=0, 1, rbinom(1, 1, 0.5))})
  return(out)
}

