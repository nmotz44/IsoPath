##### MASTER CODE TO RUN ALL THE ITERATIONS OF DIETS THROUGH RPATH #####
# this code will take the outputs from diet_resample and put them through rpath to save
# for future analysis/graphing
# the inputs will be:
  # a list of the resampled diets
  # the REco parameters

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
diet_sensitivity_test = function(resample_data_list, # list of all diets that were resampled
                                 REco_params, # REco parameters
                                 metric = "trophic level",
                                 model_name = "Rpath Model" # name of model if necessary
)
{
  ## CHECK PARAMETERS ##
  # make sure resample_data_list is a list from the previous step
  if(!is.list(resample_data_list))
    stop("resample_data_list must be the output from diet_resample")
  # make sure REco_params is a list that is a Rpath item
  if(!is.list(REco_params))
    stop("REco_params must be a Rpath parameter object")
  # make sure model_name is a vector
  if(!is.character(model_name))
    stop("model_name must be a character")

  ## FUNCTION ##
  # set up initial progress bar
  pb = txtProgressBar(min = 0, max = length(resample_data_list[[1]]), style = 3)

  if (metric == "trophic level"){
    # Set the list of trophic levels to save (all included in the model)
    TL_save_species = REco_params$model$Group

    # Initialize a list to store trophic levels for each species
    trophic_levels_list = lapply(TL_save_species, function(species)
      numeric(length(resample_data_list[[1]])))
    names(trophic_levels_list) = TL_save_species

    # Loop over each iteration
    for (i in 1:length(resample_data_list[[1]])) {
      # Update diet inputs for each species if they are present in resample_data_list
      for (species in names(resample_data_list)) {
        if (species %in% TL_save_species) {
          REco_params$diet[, species] = c(resample_data_list[[species]][,i])
        }
      }

      # Run ecopath model
      REco = rpath(REco_params, eco.name = model_name)

      # Check if the model is balanced
      if (max(REco$EE) <= 1) {
        # Pull out trophic levels for each species in the model
        for (species in TL_save_species) {
          if (species %in% names(REco$TL)) {
            trophic_levels_list[[species]][i] = REco$TL[species]
          }
        }
      } else {
        # Set trophic levels to NA if the model is not balanced
        for (species in TL_save_species) {
          trophic_levels_list[[species]][i] = NA
        }
      }
      # update progress bar
      setTxtProgressBar(pb, i)
    }
    # close progress bar
    close(pb)
    return(trophic_levels_list)
  } else if (metric == "biomass"){
      # Set the list of trophic levels to save (all included in the model)
      bio_save_species = REco_params$model$Group

      # Initialize a list to store biomass for each species
      biomass_list = lapply(bio_save_species, function(species)
        numeric(length(resample_data_list[[1]])))
      names(biomass_list) = bio_save_species

      # Loop over each iteration
      for (i in 1:length(resample_data_list[[1]])) {
        # Update diet inputs for each species if they are present in resample_data_list
        for (species in names(resample_data_list)) {
          if (species %in% bio_save_species) {
            REco_params$diet[, species] = c(resample_data_list[[species]][,i])
          }
        }

        # Run ecopath model
        REco = rpath(REco_params, eco.name = model_name)

        # Check if the model is balanced
        if (max(REco$EE) <= 1) {
          # Pull out biomass for each species in the model
          for (species in bio_save_species) {
            if (species %in% names(REco$Biomass)) {
              biomass_list[[species]][i] = REco$Biomass[species]
            }
          }
        } else {
          # Set trophic levels to NA if the model is not balanced
          for (species in bio_save_species) {
            biomass_list[[species]][i] = NA
          }
        }
        # update progress bar
        setTxtProgressBar(pb, i)
      }
      # close progress bar
      close(pb)
      return(biomass_list)
  } else {
    stop("Invalid metric type. Metric must be either 'trophic level' or 'biomass'")
  }

}

## THIS IS TESTED AND WORKS!! ##
