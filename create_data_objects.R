
#' Select data for a single species
#'
#' This function extracts data for an individual species from the full dataset.
#'
#' It is essential that the dataframe 'x' contain the following variable names:
#' \itemize{ \item species  (Species name, character variable) \item year
#' (Survey year, numeric variable) \item xplot    (Plot name, numeric variable)
#' \item strat    (Strata name, numeric variable) \item TIP      (Total
#' Indicated Pairs, numeric variable) }
#'
#' @param x A dataframe object. This dataframe should contain a response
#'   variable (TIP per plot), and columns for species ID, plot ID, year, strata,
#'   and whether the plot was flown that year (flown).
#' @param sp A character string specifying a single four-letter AOU species
#'   code. Defaults to "ABDU".
#' @param year A numeric vector listing the years to include in the model.
#'   Defaults to seq(1990, 2018).
#' @param plots A character vector of plots to include in the model. Defaults to
#'   all plots present in the dataset.
#' @param strata A numeric vector of the strata to include in the model.
#'   Defaults to all strata present in the dataset.
#' @param speciesname The column name with AOU species ID.
#' @param plotname The column name with the plot ID (Plot ID should be formatted
#'   as an integer).
#' @param strataname The column name with the strata ID (strata should be
#'   formatted as an integer).
#' @param yearname The column name with the year (year should be formatted as an
#'   integer).
#' @param responsename The column name with the response variable, likely TIP
#'   (TIP should be formatted as an integer, and be summarized at the plot level
#'   for each species).
#'
#' @return select_sp returns a dataframe.
#'
#' @export
#'
#' @examples
#' RNDU.df <- select_sp(quebec.df, "ABDU", year = seq(1990, 2018), strata=seq(1, 4),
#'     plots = seq(76101, 76424))


select_sp <- function(x = quebec.df, sp = "ABDU", year = seq(1990, 2018),
                      plots=NA, strata=NA, 
                      speciesname ="species", 
                      plotname="xplot", 
                      strataname = "strat",
                      yearname="year", 
                      responsename="TIP")  {
  if(responsename!="TIP"){
    names(x)[names(x) == responsename] <- "TIP"
  }
  
  if(strataname != "strat"){
    if("strat" %in% names(x)) names(x)[names(x) == "strat"] <- "strat1"
    names(x)[names(x) == strataname] <- "strat"
  }
  if(plotname != "xplot"){
    names(x)[names(x) == plotname] <- "xplot"
  }
  if(yearname != "years"){
    names(x)[names(x) == yearname] <- "years"
  }
  if(yearname != "species"){
    names(x)[names(x) == yearname] <- "species"
  }
  
  if(is.na(plots[1])){
    plots <- unique(x[,plotname])
  }
  if(is.na(strata[1])){
    strata <- unique(x[,strataname])
  }
  
  if(sum(c("species", "years", "xplot", "strat", "TIP") %in% names(x))==5){
    
    singleSp.df <- subset(x, species==sp & years %in% year &
                            xplot %in% plots & strat %in% strata)
    names(singleSp.df)[names(singleSp.df) == "years"] <- "year"
    
    return(singleSp.df)
  } else {
    warning(paste("Dataset x is missing variables:", 
                  c("TIP", "species","years", "xplot", "strat")[c("TIP","species", "years","xplot", "strat") %in% names(x)], ".", 
                  "Set names of variables.",
                  sep=" "))
  }
  
}
#' Package the species data for JAGS
#'
#' This function packages the data used in the JAGS model.
#'
#'
#' @param x a dataframe containing the TIP observations for a single species. This
#'  dataframe is produced by the function \code{\link{select_sp}}.
#'
#'
#' @return Returns a named list.
#'
#' @export
#'
#' @examples
#' RNDU.jags.data <- dataForJAGS(RNDU.df)
#'

dataForJAGS <- function(x = x)   {
  # Create the necessary index variables
  x$YearID <- x$year - min(x$year) +1
  # Standardise 'year' to range between 0 and 1
  x$YRindex <- (x$year - min(x$year))/(length(unique(x$year)) - 1)
  # create index variables for plot
  x$PLOTindex <- as.numeric(as.factor(x$xplot))
  # create index variables for strata.
  x$STRATindex <- as.numeric(as.factor(x$strat))
  # Bundle the data into the format required to run the JAGS model(s)
  jags.data <-append(as.list(x[, c("xplot", "strat", "year", "TIP",
                                   "PLOTindex", "STRATindex", "YearID", "YRindex")]),
                     list(n=length(x$TIP), Nyears = length(unique(x$year)),
                          n.plot =length(unique(x$PLOTindex)),
                          groups = unique(x$strat),
                          n.strata = length(unique(x$STRATindex))))
  return(jags.data)
}


#' Set initial values for parameters
#'
#' This function sets the initial values for the parameters of the JAGS model.
#' 
#' 
#' @param x A named list of the data objects required by the model.  This list is 
#' produced by the function \code{\link{dataForJAGS}}.  
#' @param i This parameter is deprecated. Default value is 1.    
#' 
#' @return Returns a list of starting values for the JAGS model.
#' 
#' @export
#' 
#' @import plyr
#' 
#' @examples
#' inits <- inits_fn(RNDU.jags.data)
#' 


inits_fn <- function(x = jags.data, i = 1){
  list(mu.int = rnorm(x$n.strata,0,2),
       sigma.int = runif(x$n.strata, 0.1, 4),
       epsilon_raw.int = rnorm(x$n.plot,0,1),
       mu.beta = rnorm(x$n.strata,0,2),
       sigma.beta = runif(x$n.strata, 0.1, 4),
       epsilon_raw.beta = rnorm(x$n.plot,0,1),
       psi = runif(1, plyr::round_any(mean(x$TIP == 0, na.rm = T), 0.05, floor),
                   plyr::round_any(mean(x$TIP == 0, na.rm = T), 0.05, ceiling)),
       # pi is random only if NA or zero. must be set to 1 if >0 
       pi = pi.gen(i = i, x = x$TIP),  
       theta.tip = runif(1, 10, 100), # start close to a poisson distribution
       sigma.year = runif(x$n.strata,0.1,4),
       epsilon_raw.year = rnorm(x$Nyears,0,1)
  )} 
#' Specify parameters to monitor
#'
#' This function specifies the parameters to monitor in the JAGS model.
#' 
#' 
#' @param model A character string naming the error distribution to be used in 
#' the model. Supported distributions are Poisson = "poisson", negative 
#' binomial = "NB", zero-inflated Poisson = "ZIP", and zero-inflated negative 
#' binomial = "ZINB". 
#' 
#' @return Returns a character vector of the names of the 
#' parameters in the model to monitor.
#' 
#' @export
#' 
#' @examples
#' params <- params_fn("NB")
#' 


params_fn <- function(model = "poisson", jags.data)  {
  if(!model %in% c("poisson", "NB", "ZIP", "ZINB"))  {
    print("Incorrect model selected")
    return()
  }
  parameters <- c("alpha", "mu.int", "sigma.int", "epsilon_raw.int", # intercepts
                  "mu.beta",	# slopes
                  "sigma.year", 
                  "TIP", "TIPnew",				# observed and predicted TIP values
                  "lambda", "mu.lambda", "mu.TIP",
                  "ld_seen", "ld_seennew", "fit_seen", "fit_seennew")	# likelihood
  # overdispersion
  if(model %in% c("NB", "ZINB")) parameters <- c(parameters, "theta.TIP", "rho.TIP") 
  # zero inflation
  if(model %in% c("ZIP", "ZINB")) parameters <- c(parameters, "psi", "pi")
  
  if(jags.data$n.strata>1){parameters <- c(parameters, "sigma.beta", "beta", "epsilon_raw.beta")}
  
  return(parameters)
}

