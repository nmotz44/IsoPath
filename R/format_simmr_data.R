#' Format stable isotope data to run through simmr
#' @description
#' A function used to format a dataframe of raw stable isotope data for simmr runs. This removes the need to make a separate excel or .csv for each consumer and instead a consumer and their prey can be specified from a single dataframe. In this function, all prey will have the same discrimination factor, if prey have different discrimination factors please see the simmr package and format in their reccomended way. This function creates a simmr object that will be functional with the rest of simmr's functions.
#'
#'
#' @param data a dataframe with the stable isotope data. At minimum, this must include a column for each isotope (d13C and d15N), as well as a column with species names. Other columns are allowed but will not be utilized for this function.
#' @param species.column a character vector of the name of the column with species information
#' @param d13C.column a character vector of the name of the column with carbon data
#' @param d15N.column a character vector of the name of the column with nitrogen data
#' @param d34S.column a character vector of the name of the column with sulfur data (default is NA)
#' @param consumer.names a vector (or string) of the names of each consumer to create a simmr object
#' @param prey.names a vector (or string) of the names of each prey to include in each consumer's simmr
#' @param disc.factors.c a number representing the carbon discrimination factor (default is 0.4)
#' @param disc.factors.n a number representing the nitrogen discrimination factor (default is 3.4)
#' @param disc.factors.s a number representing the sulfur distrimination factor (default is NA)
#' @param disc.factors.c.sd a number representing standard deviation for the carbon discrimination factor (default is 1)
#' @param disc.factors.n.sd a number representing standard deviation for the nitrogen discrimination factor (default is 1.3)
#' @param disc.factors.s.sd a number representing standard deviation for the sulfur discrimination factor (default is NA)
#' @param data.type a chacter of the type of data. Either "raw" if raw data is used, or "means" if the prey data are already averaged, default is raw
#' @param d13C.column.sd a character vector of the name of the column with carbon standard deviation data, only needed if data.type is "means", default is NA
#' @param d15N.column.sd a character vector of the name of the column with nitrogen standard deviation data, only needed if data.type is "means", default is NA
#' @param d34S.column.sd a character vector of the name of the column with sulfur standard deviation data, only needed if data.type is "means", default is NA
#' @param group.column a character vector of the name of the column with predator grouping information for simmr. The default is NA, so only specify if there is a grouping column
#'
#' @import tidyverse
#' @import simmr
#' @import Rpath
#' @return a simmr object that can be utilized in any function within the simmr package
#' @export
#'
#'
#' @examples format_simmr_data(data, "species", "d13C", "d15N", c("species 1", "species 2"), c("species 3", "species 4", "species 5"))
format_simmr_data = function(data, # data frame of raw SIA data
                             species.column, # name of column with species ID in data
                             d13C.column, # name of column with d13C data in data
                             d15N.column, # name of column with d15N data in data
                             d34S.column = NA, # name of column with d34S data in data, NA by default
                             consumer.names, # vector of consumer names
                             prey.names, # vector of prey names
                             disc.factors.c = 0.4, # d13C discrimination factor
                             disc.factors.n = 3.4, # d15N discrimination factor
                             disc.factors.s = NA, # d34S discrimination factor, NA by default
                             disc.factors.c.sd = 1.3, # sd of d13C discrimination factor
                             disc.factors.n.sd = 1.0, # sd of d15N discrimination factor
                             disc.factors.s.sd = NA, # sd of d34S discrimination factor, NA by default
                             data.type = "raw", # either "raw", or "means" to specify if we are using raw data or pre meaned and SD data
                             d13C.column.sd = NA, # specify the column name with the d13C sd data, default is NA
                             d15N.column.sd = NA, # specify the column name with the d15N sd data, default is NA
                             d34S.column.sd = NA, # specify the column name with the d34S sd data, default is NA
                             group.column = NA # specify a grouping column if needed, default is NA since there is usually not one
){
  ## CHECK INPUTS ##
  # make sure data is in the right format
  if(!is.data.frame(data))
    stop("data must be a data frame or a tibble")
  # make sure consumer names and prey names are character vectors
  if(!is.character(consumer.names) || !is.character(prey.names)) {
    stop("consumer.names and prey.names must be character vectors")
  }
  # make sure that the isotope columns are numeric
  if (!is.numeric(data[[d13C.column]]) || !is.numeric(data[[d15N.column]])) {
    stop("isotope data must be numeric")
  }
  # make sure discrimination factors and sds are numeric
  if (!is.numeric(disc.factors.c) || !is.numeric(disc.factors.n) || !is.numeric(disc.factors.c.sd) || !is.numeric(disc.factors.n.sd)){
    stop("discrimination factors (disc.factors) and standard deviations (dis.factors.sd) must be numeric")
  }

  # check if any species named in consumer.names are not in data
  # find missing species
  missing_pred = consumer.names[!consumer.names %in% data[[species.column]]]
  # print any missing
  if (length(missing_pred) > 0){
    stop(paste("error, not all consumer.names are found in data, missing species are:",
                paste(missing_pred, collapse = ",")))
  }
  # check if any species named in prey.names are not in data
  # find missing species
  missing_prey = prey.names[!prey.names %in% data[[species.column]]]
  # print missing if any
  if (length(missing_prey) != 0) {
    print(paste("Warning: Not all prey.names are found in data. Missing species are:",
                paste(missing_prey, collapse = ", ")))
  }
  
  # 2 isotope mixing model
  if (is.na(d34S.column)){
    ## PREP DATA ##
    if(data.type == "raw"){
      # tibble of consumer data with species, d13C and d15N columns
      if(is.na(group.column)){
        consumer_data = data %>%
          filter(!!sym(species.column) %in% consumer.names) %>%
          select(!!sym(species.column), all_of(d13C.column), all_of(d15N.column))
      } else {
        consumer_data = data %>%
          filter(!!sym(species.column) %in% consumer.names) %>%
          select(!!sym(species.column), all_of(d13C.column), all_of(d15N.column), all_of(group.column))
      } # tested and works
      
      
      # tibble of prey data with d13C and d15N mean + SD
      prey_data = data %>%
        filter(!!sym(species.column) %in% prey.names) %>%
        group_by(!!sym(species.column)) %>%
        filter(n() >= 2) %>% # make sure sample size is at least 2
        reframe(
          d13C.mean = mean(!!sym(d13C.column), na.rm = TRUE),
          d15N.mean = mean(!!sym(d15N.column), na.rm = TRUE),
          d13C.sd = sd(!!sym(d13C.column), na.rm = TRUE),
          d15N.sd = sd(!!sym(d15N.column), na.rm = TRUE)
        )  # now this works as well!!
      
      # tibble of prey names with discrimiation factors
      disc_factors = data %>%
        filter(!!sym(species.column) %in% prey.names) %>%
        group_by(!!sym(species.column)) %>%
        filter(n() >= 2) %>% # make sure sample size is at least 2
        reframe(
          d13C.mean = disc.factors.c,
          d15N.mean = disc.factors.n,
          d13C.sd = disc.factors.c.sd,
          d15N.sd = disc.factors.n.sd
        ) # this works!!
    } else if(data.type == "means"){
      # tibble of consumer data with species, d13C and d15N columns
      # consumer data always needs to be raw data
      if(is.na(group.column)){
        consumer_data = data %>%
          filter(!!sym(species.column) %in% consumer.names) %>%
          select(!!sym(species.column), all_of(d13C.column), all_of(d15N.column))
      } else {
        consumer_data = data %>%
          filter(!!sym(species.column) %in% consumer.names) %>%
          select(!!sym(species.column), all_of(d13C.column), all_of(d15N.column), all_of(group.column))
      }
      
      # prey stuff - should just be able to specify the columns now named in the functions
      prey_data = data %>%
        filter(!!sym(species.column) %in% prey.names) %>%
        group_by(!!sym(species.column)) %>%
        reframe(
          d13C.mean = (!!sym(d13C.column)),
          d15N.mean = (!!sym(d15N.column)),
          d13C.sd = (!!sym(d13C.column.sd)),
          d15N.sd = (!!sym(d15N.column.sd))
        )
      
      disc_factors = data %>%
        filter(!!sym(species.column) %in% prey.names) %>%
        group_by(!!sym(species.column)) %>%
        reframe(
          d13C.mean = disc.factors.c,
          d15N.mean = disc.factors.n,
          d13C.sd = disc.factors.c.sd,
          d15N.sd = disc.factors.n.sd
        )
    } else {
      stop("data.type must be either 'raw' or 'means'")
    }
    
    ## PASS DATA TO SIMMR ##
    if(is.na(group.column)){
      simmr = simmr_load(mixtures=as.matrix(consumer_data[,2:3]),
                         source_names=unlist(prey_data[,1]),
                         source_means=as.matrix(prey_data[,2:3]),
                         source_sds=as.matrix(prey_data[,4:5]),
                         correction_means=as.matrix(disc_factors[,2:3]),
                         correction_sds=as.matrix(disc_factors[,4:5]),
                         group=unlist(consumer_data[[species.column]]))
    } else {
      simmr = simmr_load(mixtures=as.matrix(consumer_data[,2:3]),
                         source_names=unlist(prey_data[,1]),
                         source_means=as.matrix(prey_data[,2:3]),
                         source_sds=as.matrix(prey_data[,4:5]),
                         correction_means=as.matrix(disc_factors[,2:3]),
                         correction_sds=as.matrix(disc_factors[,4:5]),
                         group=unlist(consumer_data[[group.column]]))
    } 
  } # 3 source mixing model
  else if (!is.na(d34S.column)) {
    ## PREP DATA ##
    if(data.type == "raw"){
      # tibble of consumer data with species, d13C and d15N columns
      if(is.na(group.column)){
        consumer_data = data %>%
          filter(!!sym(species.column) %in% consumer.names) %>%
          select(!!sym(species.column), all_of(d13C.column), all_of(d15N.column), all_of(d34S.column))
      } else {
        consumer_data = data %>%
          filter(!!sym(species.column) %in% consumer.names) %>%
          select(!!sym(species.column), all_of(d13C.column), all_of(d15N.column), all_of(d34S.column), all_of(group.column))
      } # tested and works
      
      
      # tibble of prey data with d13C and d15N mean + SD
      prey_data = data %>%
        filter(!!sym(species.column) %in% prey.names) %>%
        group_by(!!sym(species.column)) %>%
        filter(n() >= 2) %>% # make sure sample size is at least 2
        reframe(
          d13C.mean = mean(!!sym(d13C.column), na.rm = TRUE),
          d15N.mean = mean(!!sym(d15N.column), na.rm = TRUE),
          d34S.mean = mean(!!sym(d34S.column), na.rm = TRUE),
          d13C.sd = sd(!!sym(d13C.column), na.rm = TRUE),
          d15N.sd = sd(!!sym(d15N.column), na.rm = TRUE),
          d34S.sd = sd(!!sym(d34S.column), na.rm = TRUE),
        )  # now this works as well!!
      
      # tibble of prey names with discrimiation factors
      disc_factors = data %>%
        filter(!!sym(species.column) %in% prey.names) %>%
        group_by(!!sym(species.column)) %>%
        filter(n() >= 2) %>% # make sure sample size is at least 2
        reframe(
          d13C.mean = disc.factors.c,
          d15N.mean = disc.factors.n,
          d34S.mean = disc.factors.s,
          d13C.sd = disc.factors.c.sd,
          d15N.sd = disc.factors.n.sd,
          d34S.sd = disc.factors.s.sd
        ) # this works!!
    } else if(data.type == "means"){
      # tibble of consumer data with species, d13C and d15N columns
      # consumer data always needs to be raw data
      if(is.na(group.column)){
        consumer_data = data %>%
          filter(!!sym(species.column) %in% consumer.names) %>%
          select(!!sym(species.column), all_of(d13C.column), all_of(d15N.column), all_of(d34S.column))
      } else {
        consumer_data = data %>%
          filter(!!sym(species.column) %in% consumer.names) %>%
          select(!!sym(species.column), all_of(d13C.column), all_of(d15N.column), all_of(d34S.column), all_of(group.column))
      }
      
      # prey stuff - should just be able to specify the columns now named in the functions
      prey_data = data %>%
        filter(!!sym(species.column) %in% prey.names) %>%
        group_by(!!sym(species.column)) %>%
        reframe(
          d13C.mean = (!!sym(d13C.column)),
          d15N.mean = (!!sym(d15N.column)),
          d34S.mean = (!!sym(d34S.column)),
          d13C.sd = (!!sym(d13C.column.sd)),
          d15N.sd = (!!sym(d15N.column.sd)),
          d34S.sd = (!!sym(d34S.column.sd))
        )
      
      disc_factors = data %>%
        filter(!!sym(species.column) %in% prey.names) %>%
        group_by(!!sym(species.column)) %>%
        reframe(
          d13C.mean = disc.factors.c,
          d15N.mean = disc.factors.n,
          d34S.mean = disc.factors.s,
          d13C.sd = disc.factors.c.sd,
          d15N.sd = disc.factors.n.sd,
          d34S.column.sd = disc.factors.c.sd
        )
    } else {
      stop("data.type must be either 'raw' or 'means'")
    }
    
    ## PASS DATA TO SIMMR ##
    if(is.na(group.column)){
      simmr = simmr_load(mixtures=as.matrix(consumer_data[,2:4]),
                         source_names=unlist(prey_data[,1]),
                         source_means=as.matrix(prey_data[,2:4]),
                         source_sds=as.matrix(prey_data[,5:7]),
                         correction_means=as.matrix(disc_factors[,2:4]),
                         correction_sds=as.matrix(disc_factors[,5:7]),
                         group=unlist(consumer_data[[species.column]]))
    } else {
      simmr = simmr_load(mixtures=as.matrix(consumer_data[,2:4]),
                         source_names=unlist(prey_data[,1]),
                         source_means=as.matrix(prey_data[,2:4]),
                         source_sds=as.matrix(prey_data[,5:7]),
                         correction_means=as.matrix(disc_factors[,2:4]),
                         correction_sds=as.matrix(disc_factors[,5:7]),
                         group=unlist(consumer_data[[group.column]]))
    } 
  }
  return(simmr)
} 



