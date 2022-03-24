#' Main function for parameter estimation
#'
#'
#' @param crit_function Criterium to optimise depends on the chosen method :
#'  Simplex : "crit_log_cwss", "crit_log_cwss_corr", "crit_ols"
#'  Dreamzs : "likelihood_log_ciidn" , "likelihood_log_ciidn_corr"
#' @param optim_method Estimation method (Dreamzs, Simplex)
#' @param obs_daily_path Path to daily observations
#' @param obs_sum_path Path to summary observations
#' @param dailyVar Vector of variables names to estimate (variables from daily file)
#' @param sumVar  Vector of the variables names to estimate (variables from summary file)
#' @param obs_sim_path Path to the data frame which matches between the names of the simulated and observed variables
#' @param project_path Path to the directory of input files
#' @param output_path Path where to save results
#' @param console_path Path to the console
#' @param param_info Information on the parameters to estimate.
#' #' Either
#' a list containing:
#'    - (named) vectors of upper and lower bounds (`ub` and `lb`) (-Inf and Inf can be used),
#'    - `init_values`, A data.frame containing initial
#' values to test for the parameters (optional, if not provided, or if less values
#' than number of repetitions of the minimization are provided, the, or part
#' of the, initial values will be randomly generated using LHS sampling within
#' parameter bounds).
#'
#' or
#' a named list containing for each parameter the list of situations per group
#' (`sit_list`), the vector of upper and lower bounds (one value per group)
#' (`ub` and `lb`) and the list of initial values per group
#' `init_values` (data.frame, one column per group, optional).
#' (see [here](https://sticsrpacks.github.io/CroptimizR/articles/Parameter_estimation_Specific_and_Varietal.html)
#' for an example)
#'
#' @param optim_options List of options of the parameter estimation method, containing:
#'   - `path_results` The path where to store the results (optional, default=getwd())
#'   - specific options depending on the method used. Click on the links to see examples with the [simplex](https://sticsrpacks.github.io/CroptimizR/articles/Parameter_estimation_simple_case.html)
#' and [DreamZS](https://sticsrpacks.github.io/CroptimizR/articles/Parameter_estimation_DREAM.html) methods.
#'
#'
#' @param run2simulate Run name to simulate
#' @param cores_nb Number of cores to simulate on
#' @param time_display Flag (TRUE/FALSE) for display of calculation time
#' @param saves_daily_output Boolean to save daily output (if set to "true")
#' @param print_console Boolean to print information on the simulation progression on the console (if set to "false")
#'
#' @return prints, graphs and a list containing the results of the parameter estimation,
#' All results are saved in the folder `optim_options$path_results`.
#' @importFrom CroptimizR estim_param
#' @export
#'
#'
SQ_optim <- function(crit_function,optim_method, obs_daily_path, obs_sum_path, dailyVar, sumVar, obs_sim_path, project_path,output_path,console_path,param_info,optim_options,run2simulate, cores_nb,time_display,saves_daily_output, print_console) {



  obs_list           <- load_obs(obs_daily_path=obs_daily_path,
                                 obs_sum_path=obs_sum_path,
                                 project_path=project_path,
                                 dailyVar=dailyVar,
                                 sumVar=sumVar)

  param_list         <- set_param_list(param_info=param_info,project_path=project_path)


  console_data       <- set_console_data(run2simulate=run2simulate,
                                          project_path=project_path)


  #set Var_names and Var_simule
  Var_names         <- c("Date",dailyVar, sumVar)
  Var_simule <- c("DATE")
  obsim_names            <- read.csv(obs_sim_path, header=T,
                                     na.strings = "-999", sep = ",")
  for(var in c(dailyVar,sumVar)){
    k= which(obsim_names$NameObs == var)
    Var_simule <- c(Var_simule, obsim_names$NameSQ[[k]])
  }

  # names(obs_list) and sit2simulate must be the same
  situationsSQ <- console_data$sit2simulate
  situationsobs <- names(obs_list)
  for(sit in situationsobs ){
    if(! is.element(sit, situationsSQ )){
      obs_list[sit] =NULL
    }

  }



  model_options       <- set_model_options(cores_nb=cores_nb,
                                           time_display=time_display,
                                           project_path=project_path,
                                           output_path=output_path,
                                           console_path=console_path,
                                           saves_daily_output=saves_daily_output,
                                           print_console=print_console,
                                           Var_simule=Var_simule,
                                           Var_names=Var_names,
                                           Param_list=param_list,
                                           console_data=console_data)


  if(model_options$Var_names != names(obs_list[[1]]) ) {
    message("Names of simulated variables must be identical to the names of observed variables ")
  }



  ## Run the optimization

  optim_results=estim_param(obs_list=obs_list,
                            crit_function = crit_function,
                            model_function=SQ_wrapper,
                            model_options=model_options,
                            optim_options=optim_options,
                            optim_method=optim_method,
                            param_info=param_info)

  print(paste("Results of the optimization were saved in",
              optim_options$path_results," folder."))


  return(optim_results)



}
