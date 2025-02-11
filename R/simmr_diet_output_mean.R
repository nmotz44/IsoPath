#' Pull the average diets from simmr to run through Rpath
#'
#' @description
#' This function pulls the means from the simmr diet output and formats them to be run through Rpath. This is intended to create a streamlined process to format diets from simmr to Rpath and run an initial model to check for balance.
#'
#'
#' @param simmr_data the output from running a simmr model. For more infomration on this, please see the simmr documentation.
#' @param rpath_groups a vector of all the groups included in the rpath model. This is crucial because it will assign a percentage of 0 to every species that is in the Rpath model, but not in the simmr model so the data is formatted in a way to be passed to Rpath.
#'
#' @return a list with the mean diet contribution for each species within the Rpath model.
#' @export
#'
#' @examples simmr_diet_output_mean(simmr_output, rpath_groups)
simmr_diet_output_mean = function(simmr_data, # simmr output file
                                  rpath_groups # vector of all the groups for rpath
){
  ## CHECK INPUTS ##
  # simmr_data must be a simmr object
  if(!is.list(simmr_data))
    stop("simmr_data must be a simmr output, please use the output from 'simmr_mcmc' function from simmr package")

  # second check to make sure its a simmr object not just a list
  tryCatch({
    if (!("p" %in% names(simmr_data$output[[1]]$BUGSoutput$sims.list))) {
      stop("simmr_data must be a simmr object, please use the output from 'simmr_mcmc' function from simmr package")
    }
    # If it exists, you can proceed with further processing
  }, error = function(e) {
    # Handle the error and print the error message
    print(e$message)
  })

  # rpath_groups must be a vector of all the groups in rpath
  if(!is.vector(rpath_groups))
    stop("rpath_groups must be a vector of all the groups included in rpath model")
  ## PULL DATA FROM SIMMR OUTPUT ##
  simmr.diets = simmr_data$output[[1]]$BUGSoutput$sims.list$p # extract data from simmr
  colnames(simmr.diets) = simmr_data$input$source_names # makes the column names match simmr

  # convert this to a dataframe for easier manipulation
  df = reshape2::melt(simmr.diets)
  colnames(df) = c("Num", "Source", "Proportion")

  ## TAKE MEAN OF SIMMR OUTPUT AND SAVE IT FOR RPATH ##
  averages = df %>%
    group_by(Source) %>%
    summarise(
      meanss = mean(Proportion)
    )

  ## MAKE OUTPUT DATAFRAME
  # name blank dataframe with all species as a column
  export_data = data.frame(row.names = rpath_groups, means = rep(0, length(rpath_groups)))

  # fill in mean columns with means for each species
  for(species in rpath_groups) {
    if (species %in% averages$Source) {
      export_data[species, "means"] = averages %>%
        filter(Source == species) %>%
        pull(meanss)
    }
  }

  ## SAVE OUTPUT ##
  return(export_data)
}
