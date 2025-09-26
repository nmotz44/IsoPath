#' Test the sensitiity of Rpath trophic levels to changes in diets.
#' @description
#' This function utilizes the diets resampled in diet_resample and passes each iteration to Rpath and runs the model. If the model is balanced, the trophic level of all species is exracted and saved as a list for examination/visualization.
#'
#'
#' @param resample_data_list the list of each resampled diet produced by diet_resample
#' @param REco_params an Rpath parameter file. This needs to be produced prior to using this function. In most cases, this should be the same parameters used in the base model with the mean diets. For help producing this, see the Rpath documentation.
#' @param metric either "trophic level" or "biomass" to specify if you want to investigate trophic level or biomass changes with the changing diets. Default is "trophic level"
#' @param model_name name of the Rpath model if needed. Default is "Rpath Model"
#'
#' @return a list of the trophic levels for each species included in the Rpath model. This list can then be analyzed by the user, or the diet_visulization function can be used for quick histograms and boxplots.
#' @export
#'
#' @examples diet_sensitivity_test(diet_resample, REco_params, "Model Name")
diet_sensitivity_test = function(resample.data.list, # list of all diets that were resampled
                                 REco.params, # REco parameters
                                 metric = "trophic level",
                                 model.name = "Rpath Model" # name of model if necessary
)
{
  ## CHECK PARAMETERS ##
  # make sure resample_data_list is a list from the previous step
  if(!is.list(resample_data_list))
    stop("resample_data_list must be the output from diet_resample")
  # make sure REco_params is a list that is a Rpath item
  if(!is.list(REco_params))
    stop("REco_params must be a Rpath parameter object")
  # make sure metric is either "trophic level" or "biomass"
  if(metric != "trophic level" & metric != "biomass")
    stop("Invalid metric type. Metric must be either 'trophic level' or 'biomass'")
  # make sure model_name is a vector
  if(!is.character(model_name))
    stop("model_name must be a character")

  ## FUNCTION ##
  # set up initial progress bar
  pb = txtProgressBar(min = 0, max = length(resample_data_list[[1]]), style = 3)

  # Set the list of trophic levels orbiomass to save (all included in the model)
  save_species = REco_params$model$Group

  # Initialize a list to store trophic levels or biomass for each species
  metric_list <- lapply(save_species, function(species)
    numeric(length(resample_data_list[[1]])))
  names(metric_list) <- save_species

  # Loop over each iteration
  for (i in 1:length(resample_data_list[[1]])) {
    # Update diet inputs for each species if they are present in resample_data_list
    for (species in names(resample_data_list)) {
      if (species %in% save_species) {
        REco_params$diet[, species] = c(resample_data_list[[species]][,i])
      }
    }

    # Run ecopath model
    REco = rpath(REco_params, eco.name = model_name)

    # Check if the model is balanced
    if (max(REco$EE) <= 1) {
      # Pull out trophic levels for each species in the model
      for (species in save_species) {
        if (metric == "trophic level" & species %in% names(REco$TL)) {
          metric_list[[species]][i] = REco$TL[species]
        }
        else if (metric == "biomass" & species %in% names(REco$Biomass)) {
          metric_list[[species]][i] <- REco$Biomass[species]
        }
      }
    }
    else {
      # Set trophic level or biomass values to NA if the model is not balanced
      for (species in save_species) {
        metric_list[[species]][i] = NA
      }
    }
    # update progress bar
    setTxtProgressBar(pb, i)
  }
  # close progress bar
  close(pb)
  return(metric_list)
}
