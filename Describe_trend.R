
#' Call JAGS and run model
#'
#' This function is a user interface for running JAGS analyses for 
#' the EWS survey data.  The function sends the required data to JAGS, then
#' consolidates and summarizes the MCMC output in an object of class 'jagsUI'.
#' 
#' 
#' @param jags.data A named list containing the data for JAGS, as produced by the 
#' function \code{\link{dataForJAGS}}.  
#' @param model A character string naming the error distribution to be 
#' used in the model. Supported distributions are "poisson", "NB", "ZIP", 
#' and "ZINB".
#' @param wdrive The path to the directory containing the model written 
#' in JAGS code.
#' @param nc The number of Markov chains to run.  The default is 3, which is 
#' the recommended minimum value.
#' @param n.adapt  The number of iterations to run in the JAGS adaptive 
#' phase. The default is 1000, which is the recommended minimum value.
#' @param n.burn The number of iterations at the beginning of the chain 
#' to discard (i.e., the burn-in). Does not include the adaptive phase 
#' iterations. The default is 5000. 
#' @param n.iter The total number of iterations per chain (including 
#' burn-in).  The default is 15000.
#' @param thin The thinning rate. Must be a positive integer. The
#' default is 10.
#' 
#' 
#' @return Returns an object of class jagsUI.  Notable elements in 
#' the output include:
#' 
#' \tabular{ll}{
#' sims.list \tab A list of values sampled from the posterior distributions
#'  of each monitored parameter.\cr
#' mean      \tab Mean value of each monitored parameter.\cr
#' sd        \tab Standard deviation value of each monitored parameter.\cr
#' q2.5      \tab 2.5th percentile value of each monitored parameter.\cr
#' q25       \tab 25th quartile value of each monitored parameter.\cr
#' q50       \tab 50th percentile value of each monitored parameter.\cr
#' q75       \tab 75th percentile value of each monitored parameter.\cr
#' q97.5     \tab 97.5th percentile value of each monitored parameter.\cr
#' overlap0  \tab Does the 95% critical interval for a parameter overlap 
#' zero? Logical.\cr
#' f         \tab The proportion of the posterior with the same sign as 
#' the mean.\cr
#' Rhat      \tab Gelman-Rubin convergence diagnostic; at convergence Rhat=1.\cr
#' n.eff     \tab Effective sample size controlling for autocorrelation of 
#' the Markov chain.\cr
#' pD        \tab Estimated number of parameters.\cr
#' DIC       \tab DIC is an estimate of expected predictive error.\cr
#' summary   \tab A summary of various statistics calculated based on model 
#' output, in matrix form.\cr
#' samples   \tab The original output object from the rjags package, as 
#' class mcmc.list.\cr
#' modfile   \tab Path to file containing the model written in BUGS code.\cr
#' model     \tab The rjags model object; this will contain multiple elements 
#' if parallel=TRUE.\cr
#' parameters \tab Character vector of the parameters to monitor.\cr
#' mcmc.info \tab Description of the mcmc characteristics of the model.\cr
#' run.date  \tab Date the model was created.\cr
#' random.seed \tab Random seed.\cr
#' parallel  \tab Were the mcmc chains run in parallel on multiple CPU 
#' cores? Logical.\cr
#' bugs.format \tab Print JAGS output in classic R2WinBUGS format? Logical.\cr
#' calc.DIC  \tab Should DIC be reported? Logical.
#' }
#' 
#' @export
#' 
#' @import jagsUI
#' 
#' @examples
#' \dontrun{
#' # This function takes a long time to run!
#' model.results <- trend(RNDU.jags.data, model = "NB", wdrive="C:/temp")
#' }

trend <- function(jags.data=jags.data, model = "poisson", 
                  wdrive = system.file("jags_files", package="EWStrend"), 
                  nc=4, n.adapt=1000, n.burn=5000, n.iter=15000, thin=10)  {
  if(!model %in% c("poisson", "NB", "ZIP", "ZINB"))  {
    print("Incorrect model selected")
    return()
  }
  
    model.file <- file.path(wdrive, paste0(model,"_strata_TIP.jags"))
    if(jags.data$n.strata == 1) model.file <- file.path(wdrive, paste0(model,"_TIP.jags"))
 
  model.results <- jags(data=jags.data, inits=lapply(1:nc, function(x){inits_fn(jags.data, 1)}), 
                        parameters.to.save=params_fn(model, jags.data),
                        model.file=model.file, 
                        n.chains=nc, n.adapt=n.adapt, n.iter=n.iter, n.burnin=n.burn, n.thin=thin,parallel=TRUE,
                        modules=c('glm'), DIC=TRUE, store.data=FALSE,
                        codaOnly=FALSE,seed=floor(runif(1,1,10000)), bugs.format=FALSE, verbose=TRUE)
  return(model.results)
}

#' Assess model convergence
#'
#' This function examines whether parameter estimates
#' have converged to a stable solution.
#' 
#' 
#' @param model.results An object of class jagsUI as a result of a call to
#' the wrapper function \code{\link{trend}}. 
#' 
#' 
#' @return Plots traceplots, as well returning a named list of rhat values 
#' for each parameter
#' 
#' @export
#' 
#' @import jagsUI
#' 
#' @examples
#' rhat_results <- converge_fn(RNDU.model.results)

converge_fn <- function(model.results = model.results)   {
  
  par(mfrow = c(2,1))
  rhat_results <- list()
            traceplot(model.results, 
            parameters = c("mu.int", "sigma.int",  "mu.beta"))
  if ("sigma.beta" %in% names(model.results$sims.list)) 
      traceplot(model.results, parameters = "sigma.beta")  # sigma per strata
            
  if ("theta.TIP" %in% names(model.results$sims.list)) 
     traceplot(model.results, parameters = "theta.TIP")  # overdispersion
  if ("psi" %in% names(model.results$sims.list)) 
     traceplot(model.results, parameters = "psi")  # zero inflation
  rhat_results[["epsilon_raw.int"]] <- jags.summary(model.results, 
                                        "epsilon_raw.int", doPlot = F)
  rhat_results[["epsilon_raw.beta"]] <- jags.summary(model.results, 
                                        "epsilon_raw.beta", doPlot = F)
  
  if ("rho.TIP" %in% names(model.results$sims.list)) 
    rhat_results[["rho.TIP"]] <- jags.summary(model.results, "rho.TIP", doPlot = F)
  if ("pi" %in% names(model.results$sims.list)) 
    rhat_results[["pi"]] <- jags.summary(model.results, "pi", doPlot = F)
 
  return(rhat_results)
}


#' Extract model coefficients
#'
#' This function extracts the subset of model coefficients from the JAGS model
#' object that are useful for describing trend in TIP over time.
#'
#' @param jags.data A named list containing the data for JAGS, as produced by
#'   the function \code{\link{dataForJAGS}}.
#' @param model.results An object of class jagsUI as a result of a call to
#'   \code{\link{trend}}.
#'
#'
#' @return Returns a dataframe with coefficients and critical intervals for each
#'   parameter.  alpha and beta parameters refer to the intercept and slope
#'   respectively. mu.beta and sigma.beta refer to global means and standard
#'   deviations for the slope, while beta.1, beta.2 etc are slope estimates for
#'   each specified strata. Note that the 'beta' parameters are transformed from
#'   trend per time period to trend per year.
#'
#' @export
#'
#' @examples
#' coef_results <- model_coef(RNDU.jags.data, RNDU.model.results)

model_coef <- function(jags.data=jags.data, model.results=model.results)  {
  coef.list <- list()
  coef.list[["mu.int"]] <- jags.summary(model.results, "mu.int", doPlot = F)
  coef.list[["mu.beta"]] <- jags.summary(model.results, "mu.beta", doPlot = F)
  coef.list[["sigma.year"]] <- jags.summary(model.results, "sigma.year", doPlot = F)
  
  coef.results <- rbind(coef.list$mu.int, coef.list$mu.beta, coef.list$sigma.year)

  if("theta.TIP" %in% names(model.results$sims.list)) {
    coef.list[["theta.TIP"]] <- jags.summary(model.results, "theta.TIP", doPlot = F)
    coef.results <- rbind(coef.results,  coef.list$theta.TIP)

  }
  
  if("psi" %in% names(model.results$sims.list)) {
    coef.list[["psi"]] <- jags.summary(model.results, "psi", doPlot = F)
    coef.results <- rbind(coef.results, coef.list$psi)
  }
 
  coef.results <- coef.results[, colnames(coef.results) %in% c("mean","2.5%","97.5%")]
  
  # Transform the beta[i] from trend/(time period) to trend/year
  row.id <- rownames(coef.results)[startsWith(rownames(coef.results),"mu.beta")]
  coef.results[row.id, ] <- coef.results[row.id, ]/(jags.data$Nyears - 1)
  return(coef.results)
}


#' Assess model performance
#'
#' The loo method for matrices compute PSIS-LOO CV, an 
#' efficient approximate leave-one-out (LOO) cross-validation for Bayesian models 
#' using Pareto smoothed importance sampling (PSIS).
#' 
#' These results can be used to compare competing models for a single species.  
#'  
#' 
#' 
#' @param model.results An object of class jagsUI as a result of a call to \code{\link{trend}} 
#' 
#'  
#' @return Returns a named list with class c("psis_loo", "loo") and components:
#' \describe{
#' \item{estimates}{A matrix with two columns (Estimate, SE) and four rows (elpd_loo, 
#'           mcse_elpd_loo, p_loo, looic). This contains point estimates and 
#'           standard errors of the expected log pointwise predictive density
#'           (elpd_loo), the Monte Carlo standard error of elpd_loo 
#'           (mcse_elpd_loo), the effective number of parameters (p_loo) 
#'           and the LOO information criterion looic (which is just 
#'           -2 * elpd_loo, i.e., converted to deviance scale).}
#' \item{pointwise}{A matrix with four columns (and number of rows equal to the 
#'           number of observations) containing the pointwise contributions of 
#'           each of the above measures (elpd_loo, mcse_elpd_loo, p_loo, looic).}
#' \item{diagnostics}{A named list containing two vectors:}
#' \item{ }{pareto_k: Estimates of the shape parameter k of the generalized 
#'                    Pareto fit to the importance ratios for each leave-one-out 
#'                    distribution. See the pareto-k-diagnostic page for details.}
#' \item{ }{n_eff: PSIS effective sample size estimates.}
#' }
#' 
#' @export
#' 
#' @import loo
#' 
#' @examples
#' mod_loo <- loo_fn(RNDU.model.results)


loo_fn <- function(model.results = model.results)  {
  mod_loo <- loo.matrix(model.results$sims.list$ld_seen)
  return(mod_loo)
}

#'Plot model predictions 
#'
#'This function creates plots of predicted TIP across the strata. 
#'
#'@param jags.data A named list containing the data for JAGS, as produced by the
#'  function \code{\link{dataForJAGS}}.
#'@param model.results An object of class jagsUI as a result of a call to
#'  \code{\link{trend}}.
#'@param plot.type Option for how summary statistics are calculated (either
#'  "quantile" for the 50% quantile (i.e. mode), or "mean" for mean with a se).
#'  "quantile" option will include a trendline
#'@param response Option to change the plotted response. "total" for total TIP
#'  predicted across the strata. "density" for predicted TIP per plot.
#'@param return.df Option to return a dataframe of results instead of a plot.
#'
#'@return
#'@export
#'@import ggplot2
#' @importFrom dplyr full_join
#' @importFrom plyr ddply
#'
#' @examples
plot_trends <- function(jags.data, model.results, plot.type="mean", response="total", return.df=F){
  if(jags.data$n.strata>1){
    trend.df <- 
      data.frame(Year=rep(1:jags.data$Nyears, each=jags.data$n.strata),
                 Strata = rep(1:jags.data$n.strata, jags.data$Nyears),
                 pred_TIP_q50=c(model.results$mean$mu.TIP), 
                 pred_TIP_LCL=c(model.results$q2.5$mu.TIP), 
                 pred_TIP_UCL=c(model.results$q97.5$mu.TIP), 
                 # pred_TIP_mean= c(apply(model.results$sims.list$mu.TIP, c(2,3), mean)),
                 # pred_TIP_se = c(apply(model.results$sims.list$mu.TIP, c(2,3), function(x) sd(x)/sqrt(dim(model.results$sims.list$mu.TIP)[1]))),
                 TotalTIP_q50 = unlist(lapply(1:jags.data$n.strata, 
                                              function(x){lapply(1:jags.data$Nyears, 
                                                                 function(y){
                                                                   quantile(rowSums(model.results$sims.list$TIPnew[,jags.data$STRATindex==x & jags.data$YearID==y]), 0.5)}
                                              )})), 
                 TotalTIP_UCL = unlist(lapply(1:jags.data$n.strata, 
                                              function(x){lapply(1:jags.data$Nyears, 
                                                                 function(y){
                                                                   quantile(rowSums(model.results$sims.list$TIPnew[,jags.data$STRATindex==x & jags.data$YearID==y]), 0.025)}
                                              )})), 
                 TotalTIP_LCL = unlist(lapply(1:jags.data$n.strata, 
                                              function(x){lapply(1:jags.data$Nyears, 
                                                                 function(y){
                                                                   quantile(rowSums(model.results$sims.list$TIPnew[,jags.data$STRATindex==x & jags.data$YearID==y]), 0.975)}
                                              )})), 
                 TotalTIP_mean = unlist(lapply(1:jags.data$n.strata, 
                                               function(x){lapply(1:jags.data$Nyears, 
                                                                  function(y){
                                                                    mean(rowSums(model.results$sims.list$TIPnew[,jags.data$STRATindex==x & jags.data$YearID==y]), 0.025)}
                                               )})), 
                 TotalTIP_se = unlist(lapply(1:jags.data$n.strata, 
                                             function(x){lapply(1:jags.data$Nyears, 
                                                                function(y){
                                                                  sd(rowSums(model.results$sims.list$TIPnew[,jags.data$STRATindex==x & jags.data$YearID==y]))/sqrt(jags.data$Nyears)}
                                             )}))
                 
      )
  } else {
    trend.df <- 
      data.frame(Year=rep(1:jags.data$Nyears, each=jags.data$n.strata),
                 Strata = rep(1:jags.data$n.strata, jags.data$Nyears),
                 pred_TIP_q50=c(model.results$mean$mu.TIP), 
                 pred_TIP_LCL=c(model.results$q2.5$mu.TIP), 
                 pred_TIP_UCL=c(model.results$q97.5$mu.TIP), 
                 # pred_TIP_mean= c(apply(model.results$sims.list$mu.TIP, 2, mean)), 
                 # pred_TIP_se = 
                   # c(apply(model.results$sims.list$mu.TIP, 2, 
                   #         function(x) sd(x)/sqrt(dim(model.results$sims.list$mu.TIP)[1]))),
                 TotalTIP_q50 = unlist(lapply(1:jags.data$Nyears, 
                                              function(y){
                                                quantile(rowSums(model.results$sims.list$TIPnew[,jags.data$YearID==y]), 0.5)}
                 )), 
                 TotalTIP_UCL = unlist(lapply(1:jags.data$Nyears, 
                                              function(y){
                                                quantile(rowSums(model.results$sims.list$TIPnew[,jags.data$YearID==y]), 0.025)}
                 )), 
                 TotalTIP_LCL = unlist(lapply(1:jags.data$Nyears, 
                                              function(y){
                                                quantile(rowSums(model.results$sims.list$TIPnew[,jags.data$YearID==y]), 0.975)}
                 )), 
                 TotalTIP_mean = unlist(lapply(1:jags.data$Nyears, 
                                               function(y){
                                                 mean(rowSums(model.results$sims.list$TIPnew[,jags.data$YearID==y]), 0.025)}
                 )), 
                 TotalTIP_se = unlist(lapply(1:jags.data$Nyears, 
                                             function(y){
                                               sd(rowSums(model.results$sims.list$TIPnew[,jags.data$YearID==y]))/sqrt(jags.data$Nyears)}
                 ))
                 
      )
  }
  
    
  TIPdata <- data.frame(jags.data[c("TIP","PLOTindex","year","YearID","YRindex","strat")],
                        TIPpred = model.results$mean$TIPnew)
  dataForGroups <- ddply(TIPdata, c("YearID","strat"), summarise,
                               #Raw calculations by quantile
                               RawTIP_q5 = quantile(TIP, c(0.5), na.rm=T),
                               RawTIP_LCL=quantile(TIP, 0.025, na.rm=T),
                               RawTIP_UCL=quantile(TIP, c( 0.975), na.rm=T),
                               #Raw calculations by mean
                               RawTIP_mean = mean(TIP, na.rm = T),
                               RawTIP_se = sd(TIP, na.rm = T)/sqrt(length(na.omit(TIP))),


                               #Model estimates by quantile
                               TIP_q50= quantile(TIPpred, c(0.5)),
                               TIP_LCL=quantile(TIPpred, 0.025),
                               TIP_UCL=quantile(TIPpred, c( 0.975)),
                               #Model estimates by mean
                               TIP_mean = mean(TIPpred),
                               TIP_se = sd(TIPpred, na.rm=T)/sqrt(length(na.omit(TIPpred)))


  )
  names(dataForGroups)[1:2] <- c("Year", "Strata")
  
  results.df<- full_join(dataForGroups, trend.df)
  results.df$Year <- results.df$Year + min(jags.data$year)-1
  
  if(plot.type=="quantile"){
    if(response=="density"){
      p <- ggplot(data=results.df, aes(x=Year, linetype=factor(Strata)))+
        geom_line(aes(y=TIP_q50))+
        geom_line(aes(y=pred_TIP_q50), color="red")+
        labs(x="Year", y="Mode density TIP per plot", linetype="Strata")+
        theme_classic()
    } 
    if(response=="total"){
      p <- ggplot(data=results.df, aes(x=Year, linetype=factor(Strata)))+
        geom_line(aes(y=TotalTIP_q50))+
        labs(x="Year", y="Mode sum TIP per strata", linetype="Strata")+
        theme_classic()
    }
    
  } 
  if(plot.type=="mean"){
    #Plot means
    if(response=="density"){
      p <- ggplot(data=results.df, aes(x=Year, linetype=factor(Strata)))+
          geom_ribbon(aes(ymin=TIP_mean-TIP_se,
                        ymax=TIP_mean+TIP_se, 
                        fill=factor(Strata)), alpha=0.5)+
        geom_line(aes(y=TIP_mean))+
        #geom_line(aes(y=pred_TIP_mean), color="red")+
        labs(x="Year", y="Mean density TIP per plot", linetype="Strata", fill="Strata")+
        theme_classic()
    }
    
    if(response=="total"){
      p <- ggplot(data=results.df, aes(x=Year, linetype=factor(Strata)))+
        geom_line(aes(y=TotalTIP_mean))+
        geom_ribbon(aes(ymin=TotalTIP_mean-TotalTIP_se,
                        ymax=TotalTIP_mean+TotalTIP_se, 
                        fill=factor(Strata)), alpha=0.5)+
        labs(x="Year", y="Sum mean TIP per strata", fill="Strata", linetype="Strata", fill="Strata")+
        theme_classic()
    }
    
  }
  
  
  if(return.df==F){
    return(p)
  } else {
    return(results.df)
  }
  
}




#' Plot model predictions
#'
#' This function creates a figure showing TIP (+/- standard errors)
#' as a function of year. Two trajectories are displayed.   
#' The first trajectory describes the mean observed TIP each year, and the second trajectory 
#' describes the mean predicted TIP each year.  The predicted TIP are composed of
#' observed TIP values for surveyed plot/year combinations, and model predictions
#' for the unsurveyed plot/year combinations. 
#' 
#' @param jags.data A named list containing the data for JAGS, as produced by the 
#' function \code{\link{dataForJAGS}}. 
#' @param model.results An object of class jagsUI as a result of a call to \code{\link{trend}}.
#' @param writePlotData A logical indicating whether the data used to create the plot 
#' should be saved as a dataframe object.
#' @param y.axis.label Units for Y axis of plot
#' @return Returns a figure of mean TIP each year, and optionally,  
#' a dataframe containing the data required to create this figure.
#' @import plyr
#' @examples
#' mean_plt(jags.data = RNDU.jags.data, RNDU.model.results, writePlotData = FALSE)

mean_plt <- function(jags.data = jags.data, model.results = model.results, 
                      writePlotData = FALSE, y.axis.label="TIP per plot")  {

  TIPdata <- data.frame(jags.data[c("TIP","PLOTindex","year","YearID")],
                        TIPpred = model.results$mean$TIPnew)
  dataForPlot <- ddply(TIPdata, "year", summarise,
                       TIPmean = mean(TIP, na.rm = T), 
                       TIPse = (sd(TIP, na.rm = T)/sqrt(length(na.omit(TIP)))),
                       TIPpred_mean = mean(TIPpred),
                       TIPpred_se = (sd(TIPpred, na.rm=T)/sqrt(length(na.omit(TIPpred)))))
  par(mfrow = c(1,1))
  with(dataForPlot, {
    plot(1, 1, xlim = c(min(year) - 1, 2025), axes = F, ylab = "",
         xlab = "", ylim <- c(0, max(TIPpred_mean) * 1.3 + 2),  
         type = "n")
    axis(1, col = "grey40", col.axis = "grey20", cex = 0.7, 
         at = seq(min(year)-1, 2025, 5), 
         labels = seq(min(year) - 1, 2025, 5), lwd = 0, lwd.ticks = 1)
    axis(2, col = "grey40", col.axis = "grey20", cex = 0.7, las = 2, lwd = 0, 
         lwd.ticks = 1)
    box(col = "grey60")
    lines(year, TIPmean, lty = 1)
    arrows(year, TIPmean - TIPse, year, TIPmean + TIPse, length = 0.005, 
           angle = 90, code = 3,col = "black")  # Add standard error bars
    lines(year,TIPpred_mean, lty = 1, col = "red")
    arrows(year, TIPpred_mean - TIPpred_se, year, TIPpred_mean + TIPpred_se, 
           length = 0.005, angle = 90, code = 3,col = "red")  
    grid(NULL, NULL)
    mtext(y.axis.label, side = 2, line = 2)
    mtext("Year", side = 1, line = 2)
    legend("topright", c("Raw data", "JAGS-estimated data"), lty = 1,
           col = c("black", "red"))
  })
 
  if(writePlotData == TRUE) return(dataForPlot)
}


#' Plot model predictions
#'
#' This function creates a figure showing TIP (+/- standard errors)
#' as a function of year. Two trajectories are displayed.
#' The first trajectory describes the mean predicted TIP each year, where the predicted TIP consist of
#' observed TIP values for surveyed plot/year combinations and model predictions
#' for the unsurveyed plot/year combinations.  The second trajectory shows the linear trend plus the 95%
#' critical intervals.
#'
#' @param jags.data A named list containing the data for JAGS, as produced by the
#' function \code{\link{dataForJAGS}}.
#' @param model.results An object of class jagsUI as a result of a call to \code{\link{trend}}.
#' @param writePlotData A logical indicating whether the data used to create the plot
#' should be saved as a dataframe object.
#'
#'
#' @return Returns a figure of mean TIP each year and trend in TIP, and optionally,
#' a dataframe containing the data required to create this figure.
#'
#'
#' @import plyr
#'
#' @examples
#' \dontrun{
#' global_plot(jags.data = RNDU.jags.data, RNDU.model.results, writePlotData = FALSE)
#' }

global_plot <- function(jags.data = jags.data, model.results = model.results,
                        writePlotData = FALSE)  {
  
  TIPdata <- data.frame(jags.data[c("TIP","PLOTindex","year","YearID","YRindex")],
                        TIPpred = model.results$mean$TIPnew)
  dataForPlot <- ddply(TIPdata, "year", summarise,
                       TIPmean = mean(TIP, na.rm = T),
                       TIPse = (sd(TIP, na.rm = T)/sqrt(length(na.omit(TIP)))),
                       TIPpred_mean = mean(TIPpred),
                       TIPpred_se = (sd(TIPpred, na.rm=T)/sqrt(length(na.omit(TIPpred)))))
  par(mfrow = c(1,1))
  with(dataForPlot, {
    plot(1, 1, xlim = c(min(year) - 1, max(year) + 2), axes = F, ylab = "",
         xlab = "", ylim <- c(0, max(TIPpred_mean) * 1.3 + 2),
         type = "n")
    axis(1, col = "grey40", col.axis = "grey20", cex = 0.7,
         lwd = 0, lwd.ticks = 1)
    axis(2, col = "grey40", col.axis = "grey20", cex = 0.7, las = 2, lwd = 0,
         lwd.ticks = 1)
    box(col = "grey60")
    # To plot raw data
    #    lines(year, TIPmean, lty = 1,col="green")
    #   arrows(year, TIPmean - TIPse, year, TIPmean + TIPse, length = 0.005,
    #         angle = 90, code = 3,col = "green")  # Add standard error bars
    # To plot raw data corrected for unsurveyed years
    lines(year,TIPpred_mean, lty = 1, col = "black")
    arrows(year, TIPpred_mean - TIPpred_se, year, TIPpred_mean + TIPpred_se,
           length = 0.005, angle = 90, code = 3,col = "black")
    # To plot trend line
    output <- model.results$sims.list
    lambda <- t(output$lambda)
    mean.pred <- sapply(1:ncol(lambda), function(k){
      test.mat <- matrix(-999, nrow=max(jags.data$PLOTindex), ncol=max(jags.data$YearID))
      for(i in 1:nrow(lambda)){
        test.mat[jags.data$PLOTindex[i],jags.data$YearID[i]] <- lambda[i,k]
      }
      out <- apply(test.mat,2,mean)
      return(out)
    })
    
    pred_trend.df <- data.frame(year=seq(min(dataForPlot$year),max(dataForPlot$year)),
                                mean=apply(mean.pred,1,mean),
                                lowci=apply(mean.pred,1,quantile,prob=0.025),
                                upci=apply(mean.pred,1,quantile,prob=0.975))
    with(pred_trend.df,{
      lines(year, mean, lty = 1, lwd=2, col="red")
      lines(year, lowci, lty = 2, lwd=1, col="red")
      lines(year, upci, lty = 2, lwd=1, col="red")
    })
    grid(NULL, NULL)
    mtext("Total Indicated Pairs per 25 sqkm", side = 2, line = 2)
    mtext("Year", side = 1, line = 2)
    legend("topright", c("JAGS-estimated data", "trend"), lty = 1,
           col = c("black", "red"))
  })
  
  if(writePlotData == TRUE) return(dataForPlot)
}

#' Plot model predictions for each stratum
#'
#' This function creates a figure showing TIP (+/- standard errors)
#' as a function of year for each individual stratum. Two trajectories are displayed.
#' The first trajectory describes the mean predicted TIP each year, where the predicted TIP consist of
#' observed TIP values for surveyed plot/year combinations and model predictions
#' for the unsurveyed plot/year combinations.  The second trajectory shows the linear trend plus the 95%
#' critical intervals.
#'
#' @param jags.data A named list containing the data for JAGS, as produced by the
#' function \code{\link{dataForJAGS}}.
#' @param model.results An object of class jagsUI as a result of a call to \code{\link{trend}}.
#' @param writePlotData A logical indicating whether the data used to create the plot
#' should be saved as a dataframe object.
#'
#'
#' @return Returns a figure of mean TIP each year and trend in TIP, and optionally,
#' a dataframe containing the data required to create this figure.
#'
#'
#' @import plyr
#'
#' @examples
#' \dontrun{
#' group_plot(jags.data = RNDU.jags.data, RNDU.model.results, writePlotData = FALSE)
#' }


strat_plot <- function(jags.data = jags.data, model.results = model.results,
                       writePlotData = FALSE)  {
  
  TIPdata <- data.frame(jags.data[c("TIP","PLOTindex","year","YearID","YRindex","strat")],
                        TIPpred = model.results$mean$TIPnew)
  dataForGroups <- ddply(TIPdata, c("year","strat"), summarise,
                         TIPmean = mean(TIP, na.rm = T),
                         TIPse = (sd(TIP, na.rm = T)/sqrt(length(na.omit(TIP)))),
                         TIPpred_mean = mean(TIPpred),
                         TIPpred_se = (sd(TIPpred, na.rm=T)/sqrt(length(na.omit(TIPpred)))))
  plotGroup <- ddply(TIPdata,"PLOTindex", summarise,Group=unique(strat))
  
  names(dataForGroups)[names(dataForGroups)=="strat"] <- "Group"
  dataForGroups <- dataForGroups[order(dataForGroups$Group),]
  
  # To calculate data for trend line
  output <- model.results$sims.list
  lambda <- t(output$lambda)
  stratnames <- ddply(data.frame(PLOTindex=jags.data$PLOTindex, STRATindex=jags.data$STRATindex),
                      "PLOTindex",summarise, group=unique(STRATindex))
  mean.pred <- sapply(1:ncol(lambda), function(k){
    test.mat <- matrix(-999, nrow=max(jags.data$PLOTindex), ncol=max(jags.data$YearID))
    for(i in 1:nrow(lambda)){
      test.mat[jags.data$PLOTindex[i],jags.data$YearID[i]] <- lambda[i,k]
    }
    
    out <- sapply(1:4, function(m) {
      out.int <- apply(test.mat[stratnames$group == m,],2,mean)
    })
    return(out)
  })
  pred_trend.df <- data.frame(year=dataForGroups$year,
                              group = dataForGroups$Group,
                              mean=apply(mean.pred,1,mean),
                              lowci=apply(mean.pred,1,quantile,prob=0.025),
                              upci=apply(mean.pred,1,quantile,prob=0.975))
  # Plot results for each strata/group
  par(mfrow = c(1,1))
  for(i in unique(dataForGroups$Group)) {
    with(subset(dataForGroups,Group == i), {
      plot(1, 1, xlim = c(min(year) - 1, max(year)+2), axes = F, ylab = "",
           xlab = "", ylim <- c(0, max(TIPpred_mean) * 1.3 + 2),
           type = "n")
      axis(1, col = "grey40", col.axis = "grey20", cex = 0.7,
           lwd = 0, lwd.ticks = 1)
      axis(2, col = "grey40", col.axis = "grey20", cex = 0.7, las = 2, lwd = 0,
           lwd.ticks = 1)
      box(col = "grey60")
      # To plot raw data
      #    lines(year, TIPmean, lty = 1,col="green")
      #   arrows(year, TIPmean - TIPse, year, TIPmean + TIPse, length = 0.005,
      #         angle = 90, code = 3,col = "green")  # Add standard error bars
      # To plot raw data corrected for unsurveyed years
      
      lines(year,TIPpred_mean, lty = 1, col = "black")
      arrows(year[Group == i], TIPpred_mean[Group == i] - TIPpred_se[Group == i], year[Group == i],
             TIPpred_mean[Group == i] + TIPpred_se[Group == i],
             length = 0.005, angle = 90, code = 3,col = "black")
      # To plot trend line
      
      with(subset(pred_trend.df,group == i),{
        lines(year, mean, lty = 1, lwd=2, col="red")
        lines(year, lowci, lty = 2, lwd=1, col="red")
        lines(year, upci, lty = 2, lwd=1, col="red")
      })
      grid(NULL, NULL)
      mtext("Total Indicated Pairs per 25 sqkm", side = 2, line = 2)
      mtext("Year", side = 1, line = 2)
      legend("topright", title=paste0("Group = ",i), c("JAGS-estimated data", "trend"), lty = 1,
             col = c("black", "red"))
    })
  }
  
  if(writePlotData == TRUE) return(dataForGroups)
}

