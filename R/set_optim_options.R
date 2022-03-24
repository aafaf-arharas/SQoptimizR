#' Set options for the estimation method
#'
#' @param optim_method Estimation method (dreamzs or simplex)
#' @param path_results Path where to store results and graphs
#' @param ranseed Seed for random numbers
#' @param iterations Total number of iterations
#' @param nCR Parameter determining the number of cross-over proposals. If nCR = 1 all parameters are updated jointly.
#' @param updateInterval Determining the intervall for the pCR (crossover probabilities)
#' @param gamma Kurtosis parameter Bayesian Inference Scheme.
#' @param Eps Ergodicity term
#' @param e Ergodicity term
#' @param pCRupdate Update of crossover probabilities
#' @param burnin Number of iterations treated as burn-in
#' @param thin Thin thinning parameter. Determines the interval in which values are recorded.
#' @param adaptation Number or percentage of samples that are used for the adaptation in DREAM
#' @param DEpairs Number of pairs used to generate proposal
#' @param ZupdateFrequency Frequency to update Z matrix
#' @param pSnooker Probability of snooker update
#' @param Z Starting matrix for Z
#' @param startValue Number of markov chains
#' @param consoleUpdates Intervall in which the sampling progress is printed to the console
#' @param message Logical determines whether the sampler's progress should be printed
#' @param nb_rep Number of minimization repetitions
#' @param xtol_rel Stop on small optimization step
#' @param maxeval Stop on this many function evaluations
#'
#' @return List of options for the estimation method
#'

set_optim_options <- function(optim_method = "dreamzs", path_results=getwd(),ranseed=NULL,iterations=NULL,nCR=NULL,updateInterval=NULL,gamma=NULL,Eps=NULL,e=NULL,pCRupdate=NULL,burnin=NULL,thin=NULL,adaptation=NULL,DEpairs=NULL,ZupdateFrequency=NULL,pSnooker=NULL,Z=NULL, startValue=NULL, consoleUpdates=NULL,message=NULL, nb_rep=5,xtol_rel=1e-4,maxeval=500) {

  optim_options                   <-  list()
  optim_options$path_results      <-  path_results
  optim_options$ranseed           <-  ranseed

  if (optim_method =="dreamzs") {
    optim_options$iterations       <-  iterations
    optim_options$nCR              <-  nCR
    optim_options$updateInterval   <-  updateInterval
    optim_options$gamma            <-  gamma
    optim_options$Eps              <-  Eps
    optim_options$e                <-  e
    optim_options$pCRupdate        <-  pCRupdate
    optim_options$burnin           <-  burnin
    optim_options$thin             <-  thin
    optim_options$adaptation       <-  adaptation
    optim_options$DEpairs          <-  DEpairs
    optim_options$ZupdateFrequency <-  ZupdateFrequency
    optim_options$pSnooker         <-  pSnooker
    optim_options$Z                <-  Z
    optim_options$startValue       <-  startValue
    optim_options$consoleUpdates   <-  consoleUpdates
    optim_options$message          <-  message

  } else if(optim_method == "simplex") {

    optim_options$nb_rep           <-  nb_rep
    optim_options$xtol_rel         <-  xtol_rel
    optim_options$maxeval          <-  maxeval

  }else {
    stop("Invalid estimation method ")
  }

  return(optim_options)
}

