#' Resample within the diet distribution from simmr_output_dist.
#' @description
#' The distributions are resampled in a way so that the diet proportions always sum to 1 so this output can be passed directly to Rpath.
#'
#' @param diet_CI a data frame with the confidence intervals of the consumer diets produced with simmr. The output from simmr_output_dist is intended to be used for this.
#' @param num_iterations the number of random samples to take within the diet distribution. Default is 1000, but any positive integer can be used.
#'
#' @return a dataframe with a row for each species in the consumer diet, and num_iterations columns with a possible diet proportion for this prey.
#' @export
#'
#' @examples diet_resample(simmr_output_dist, num_iterations = 10000)
diet_resample = function(diet_CI, # confidence interval data frame from simmr/mixSIAR_output_dist
                         num_iterations = 1000) # number of iterations to run, default 1000

{
  ## CHECK INPUTS ##
  # check diet_CI
  if (!is.data.frame(diet_CI))
    stop("diet_CI must be a dataframe produced by simmr_diet_output_dist")
  # check num_iterations
  if (!is.numeric(num_iterations))
    stop("num_iterations must be an number")

  ##### SET UP #####
  ## convert the previous output to a list and set up a blank dataframe ##
  # make empty list
  listspp = list()

  # fill in species names and CI from the diet_CI
  for(species in rownames(diet_CI)){
    low = diet_CI[species, "low"]
    high = diet_CI[species, "high"]
    listspp[[species]] = c(low, high)
  }

  # make empty dataframe with species as the columns
  # extract row names
  row_names = rownames(diet_CI)

  # create the blank dataframe and rename rows
  final_df = data.frame(matrix(NA, nrow = length(row_names), ncol = num_iterations))
  rownames(final_df) = row_names

  ##### GENERATE NUMBERS AND MAKE SURE THEY SUM TO 1 #####
  generate_numbers = function(ranges, # list of the high and low values
                              target_sum = 1.0){ # target to sum numbers to (default is 1)
    n = length(ranges)
    nums = numeric(n)

    # generate random numbers within the ranges
    for (i in 1:n){
      nums[i] = runif(1, ranges[[i]][1], ranges[[i]][2])
    }

    # adjust the numbers to make sure they sum to exactly target_sum (1)
    current_sum = sum(nums)
    while (abs(current_sum - target_sum) > 1e-6) {
      adjustment = (target_sum - current_sum) / n
      for (i in 1:n){
        proposed = nums[i] + adjustment
        # if the numbers are within the range, keep them
        if (proposed >= ranges[[i]][1] && proposed <= ranges[[i]][2]) {
          nums[i] = proposed
        # if the numbers are not within the range, resample
        } else {
          nums[i] = runif(1, ranges[[i]][1], ranges[[i]][2])
        }
      }
      current_sum = sum(nums)
    }
    return(nums) # return the numbers to be passed onto the next part
  }
  # generate a diet for the specified number of iterations
  for (z in 1:num_iterations) {
    # generate a possible diet
    diet = generate_numbers(listspp)
    # put the diet in the final dataframe
    final_df[,z] = diet
  }
  return(final_df)
}
