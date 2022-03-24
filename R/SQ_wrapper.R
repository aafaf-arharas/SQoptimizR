#' SiruisQuality wrapper for CroptimizR
#'
#'
#' @description This function runs SiruisQuality on a set of situations (i.e. environments)
#' using the values of the parameters defined in the param_values argument. It returns
#' the values of the simulated outputs.
#'
#' @param param_values (optional) a named vector that contains the value(s) and name(s)
#' of the parameters to force for each situation to simulate. If not provided (or if is
#' NULL), the simulations will be performed using default values of the parameters
#' (e.g. as read in the model input files).
#' @param sit_names Vector of situations names for which results must be returned
#' @param sit_var_dates_mask mask to filter situations
#' @param model_options options for the model wrapper
#' @param ... other options
#'
#' @return A list containing:
#' `sim_list`: a `named list` (names = situations names) of data.frames (or tibbles) of
#` simulated output values (one element of the list per simulated situation)
#'  `error`: an error code indicating if at least one simulation ended with an error.
#'
#' @import doParallel
#' @import  parallel
#' @importFrom dplyr filter
#' @importFrom tibble tibble
#' @importFrom stats setNames
#' @import foreach
#'
SQ_wrapper <- function( param_values = NULL, sit_names=NULL, sit_var_dates_mask = NULL, model_options, ...) {

  time_display      <- model_options$time_display

  if (time_display)   start_time <- Sys.time()
  cores_nb          <- model_options$cores_nb
  cl                <- makeCluster(cores_nb)
  registerDoParallel(cl)

  console_data      <- model_options$console_data
  output_path       <- model_options$output_path

  sit2simulate   <- console_data$sit2simulate
  N = length(sit2simulate)
  res               <- list(sim_list = setNames(vector("list",length(sit2simulate)),
                                                nm = sit2simulate), error=FALSE)

  sim_batch <- foreach(Ind_situation = 1:N,
                       .export = c('run_SQ',
                                   'sim_read')) %dopar% {


                                     print(Ind_situation)

                                     # Select param_values depending on the situation to simulate
                                     param_values_sit <- tibble(!!!param_values)  # convert param_values in a tibble if needed
                                     if (!is.null(param_values)) {
                                       if ("situation" %in% names(param_values_sit)) {
                                         param_values_sit <- param_values_sit %>% filter(situation==sit2simulate[i]) %>% select(-situation)
                                       }
                                     }
                                     if (is.null((param_values_sit)) | nrow(param_values_sit)==0) {
                                       param_values_sit <- tibble(NA)
                                     }



                                     situation    = sit2simulate[Ind_situation]
                                     sim_output_path = paste0(output_path, situation)


                                     run_SQ(param_values_sit, model_options, sim_output_path, situation)

                                     sim_tmp = sim_read(sim_output_path, model_options)




                                     return(list(sim_tmp))}




  N = length(sim_batch)

  for (n in 1:N){
    name = names(sim_batch[[n]][[1]])
    res$sim_list[[name]] = sim_batch[[n]][[1]][[1]]
  }

  print("res")
  print(res)

  # Stopping the cluster
  stopCluster(cl)

  if (time_display) {
    duration <- Sys.time() - start_time
    print(duration)
  }

  if (length(res$sim_list)==0) {
    warning("SiriusQuality simulations failed for all situations!")
    res$sim_list <- NULL
  } else {
    # Add the attribute cropr_simulation for using CroPlotR package
    attr(res$sim_list, "class")= "cropr_simulation"
  }



  return(res)
}
