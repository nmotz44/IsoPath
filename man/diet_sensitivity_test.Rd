% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diet_sensitivity_test.R
\name{diet_sensitivity_test}
\alias{diet_sensitivity_test}
\title{Test the sensitiity of Rpath trophic levels to changes in diets.}
\usage{
diet_sensitivity_test(
  resample_data_list,
  REco_params,
  metric = "trophic level",
  model_name = "Rpath Model"
)
}
\arguments{
\item{resample_data_list}{the list of each resampled diet produced by diet_resample}

\item{REco_params}{an Rpath parameter file. This needs to be produced prior to using this function. In most cases, this should be the same parameters used in the base model with the mean diets. For help producing this, see the Rpath documentation.}

\item{metric}{either "trophic level" or "biomass" to specify if you want to investigate trophic level or biomass changes with the changing diets. Default is "trophic level"}

\item{model_name}{name of the Rpath model if needed. Default is "Rpath Model"}
}
\value{
a list of the trophic levels for each species included in the Rpath model. This list can then be analyzed by the user, or the diet_visulization function can be used for quick histograms and boxplots.
}
\description{
This function utilizes the diets resampled in diet_resample and passes each iteration to Rpath and runs the model. If the model is balanced, the trophic level of all species is exracted and saved as a list for examination/visualization.
}
\examples{
diet_sensitivity_test(diet_resample, REco_params, "Model Name")
}
