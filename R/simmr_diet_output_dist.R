##### MASTER CODE TO PULL DIETS FROM SIMMR OUTPUTS #####
# this code will have the function to pull data from a simmr output into a format for rpath
# it will also create a vector with the 25-75% confidence intervals to test for sensitivity
#   in a following function
# the inputs will be:
#   simmr object
#   rpath group vector


##### FUNCTION ATTEMPT 1 #####
#' Extract a range of diet possibilities from simmr within predefined confidence intervals
#'
#' @description
#' Create a list of a diet range for each species from each prey included in simmr. This can be used in a following function to create random samples within this range, or for other analysis by the user.
#'
#'
#' @param simmr_data the output from running a simmr model. For more infomration on this, please see the simmr documentation.
#' @param rpath_groups a vector of all the groups included in the rpath model. This is crucial because it will assign a percentage of 0 to every species that is in the Rpath model, but not in the simmr model so the data is formatted in a way to be passed to Rpath.
#' @param CI.low a number between 0 and 1 specifying the low % range to sample from.
#' @param CI.high a number between 0 and 1 specifying the high % range to sample from.
#'
#' @return a list of the diet ranges within the specified range
#' @export
#'
#' @examples simmr_diet_output_dist(simmr_ouput, rpath_groups, CI.low = 0.1, CI.high = 0.9)
simmr_diet_output_dist = function(simmr_data, # simmr output file
                                  rpath_groups, # vector of all the groups for rpath
                                  CI.low = 0.025, # lower limit of confidence intervals
                                  CI.high = 0.975 # upper limit of confidence intervals
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

  # CI.low must be a positive number between 0 and 1 less than CI.high
  if (CI.low <= 0 || CI.low >= 1 || CI.low > CI.high)
    stop("CI.low must be a number between 0 and 1, and less than CI.high")

  # CI.high must be a positive number between 0 and 1 greater than CI.low
  if (CI.high <= 0 || CI.high >= 1 || CI.low > CI.high)
    stop("CI.high must be a number between 0 and 1, and larger than CI.high")

  ## PULL DATA FROM SIMMR OUTPUT ##
  simmr.diets = simmr_data$output[[1]]$BUGSoutput$sims.list$p # extract data from simmr
  colnames(simmr.diets) = simmr_data$input$source_names # makes the column names match simmr

  # convert this to a dataframe for easier manipulation
  df = reshape2::melt(simmr.diets)
  colnames(df) = c("Num", "Source", "Proportion")

  ## TAKE MEAN OF SIMMR OUTPUT AND SAVE IT FOR RPATH ##
  dist = df %>%
    group_by(Source) %>%
    summarise(
      lows = quantile(Proportion, CI.low),
      highs = quantile(Proportion, CI.high)
    )

  ## MAKE OUTPUT DATA FRAME ##
  all = data.frame(row.names = rpath_groups, low = rep(0, length(rpath_groups)),
                   high = rep(0, length(rpath_groups)))

  # fill the columns with the high and low CI intervals
  for (species in rpath_groups) {
    # fill in lows
    if (species %in% dist$Source) {
      all[species, "low"] = dist %>%
        filter(Source == species) %>%
        pull(lows)
    }
    # fill in highs
    if (species %in% dist$Source) {
      all[species, "high"] = dist %>%
        filter(Source == species) %>%
        pull(highs)
    }
  }
  ## SAVE OUTPUT ##
  return(all)
}

### THIS IS TESTED AND WORKS!! ###








