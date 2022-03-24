#' Set options for the model wrapper
#'
#' @param cores_nb Number of cores to simulate on
#' @param time_display  Flag (TRUE/FALSE) for display of calculation time
#' @param project_path Path to the directory of input files of SQ
#' @param output_path Path where to save results
#' @param console_path Path to the console
#' @param saves_daily_output Boolean to save daily output (if set to "true")
#' @param print_console Boolean to print information on the simulation progression on the console (if set to "false")
#' @param Var_simule Names of variables to simulate used in the output files of SQ
#' @param Var_names  Names of variables to simulate used in the observations files
#' @param Param_list A data frame containing the names of parameters to estimate and their types
#' @param console_data Set of lists needed to build the command line
#'
#'
#'
#' @return List containing options needed to run the model
#'
#'
set_model_options <- function(cores_nb,time_display,project_path, output_path, console_path,saves_daily_output, print_console, Var_simule, Var_names, Param_list, console_data) {

  model_options                    <- {}
  model_options$cores_nb           <- cores_nb
  model_options$time_display       <- time_display
  model_options$projects_path      <- Sys.glob(paste0(project_path,"/*.sqpro"))
  model_options$output_path        <- output_path
  model_options$console_path       <- console_path
  model_options$print_console      <- print_console
  model_options$saves_daily_output <- saves_daily_output
  model_options$Var_simule         <- Var_simule
  model_options$Var_names          <- Var_names
  model_options$Param_list         <- Param_list
  model_options$console_data       <- console_data

  return(model_options)

}


