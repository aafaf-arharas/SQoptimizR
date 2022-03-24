# Exemple how to use SQwrapper

## Load libraries
library(SQoptimizR)
library(tools)
library(CroptimizR)


RUNS = c("RUN_GAID", "RUN_PLADE")


#for(run in RUNS){

  # optim_options
  optim_options  <-  list()
  optim_options$path_results    <- "C:/Users/arharasa/Documents/Stage_Aafaf/Breedwheat/5-Optimization/package_SQoptimizR/Dreamzs" # path where to store results graphs

  optim_options$iterations      <-  1000                      # Total number of iterations
  optim_options$startValue      <-  3                        # Number of markov chains #22
  optim_options$ranseed         <-  1234                     # seed for random numbers

  optim_options$nb_rep          <-  7                         # Number of repetitions of the minimization (each time starting with different initial values for the estimated parameters)
  optim_options$maxeval         <-  500                      # Maximum number of evaluations of the minimized criteria
  optim_options$xtol_rel        <-  1e-03                     # Tolerance criterion between two iterations(threshold for the relative difference ofparameter values between the 2 previousiterations)


  #run optimisation
  results <- SQ_optim(crit_function  = likelihood_log_ciidn,
                      optim_method   = "BayesianTools.dreamzs",
                      obs_daily_path = "C:/Users/arharasa/Documents/Stage_Aafaf/Breedwheat/6-Observations/BW elite panel for model calibration (T1.3).sqday",
                      obs_sum_path   = "C:/Users/arharasa/Documents/Stage_Aafaf/Breedwheat/6-Observations/BW elite panel for model calibration (T1.3).sqsum",
                      dailyVar       = c("GAID"),
                      sumVar         = NULL,
                      obs_sim_path   = "C:/Users/arharasa/Documents/Stage_Aafaf/Breedwheat/6-Observations/obs_simulations.csv",
                      project_path   = "C:/Users/arharasa/Documents/Stage_Aafaf/Breedwheat/1-Project",
                      output_path    = "C:/Users/arharasa/Documents/Stage_Aafaf/Breedwheat/3-Output/",
                      console_path   = "C:/Users/arharasa/Documents/Stage_Aafaf/Breedwheat/8-Console/SiriusQuality-Console.exe",
                      param_info     = param_info  <- list(lb=c(AreaPL=15, NLL=4, AreaSL=2),
                                                       ub=c(AreaPL=40, NLL=7, AreaSL=10),
                                                       init_values = data.frame(AreaPL = c(35.7), NLL = c(6.5), AreaSL=c(2.5))),
                      optim_options  = optim_options,
                      run2simulate   = "RUN_GAID",
                      cores_nb       = 8,
                      time_display   = T,
                      saves_daily_output = "true",
                      print_console  = "true")


#}






