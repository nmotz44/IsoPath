#' Quickly visualize trophic level changes from changes in diet
#'
#' @description
#' Using the output from diet_sensitivity_test, create histograms and boxplots of the variabliltiy in trophic levels resulting from variability in diets. This function is not intended for in-depth analysis or manuscript ready plots, but meerly for quick visualization to examine if the diet ranges significantly alter the model.
#'
#'
#' @param diet_sensitivity_test_output list of the trophic levels from the Rpath sensitivity runs. The function is designed to use the output from diet_sensitivity_test
#' @param plot_type type of plot to make, either "histogram", "boxplot", or "violin. Default is histogram where a seperate graph is made for each species. In boxplot, all species are plotted on the same graph.
#' @param species_list a vector of all species the user wants plotted. Default is all, but a subset can be specified. For larger models where only a few species are of interest, a subset is recommend.
#' @param metric either "trophic level" or "biomass" to specify if you want to investigate trophic level or biomass changes with the changing diets. Default is "trophic level". This should match the metric used in diet_sensitivity_test, but it onlu changes the axis name in these graphs.
#'
#' @import vioplot
#' @return plot(s) showing the variability of trophic levels related to different diet matricies.
#' @export
#'
#' @examples diet_visualization(diet_sensitivity_test_output)
#' diet_visualization(diet_sensitivity_test_output, plot_type = "histogram")
#' diet_visualization(diet_sensitivity_test_output, plot_type = "boxplot")
#' diet_visualization(diet_sensitivity_test_output, species_list = c("species 1", "species 2")
diet_visualization = function(diet.sensitivity.test.output, # output from diet_sensitivity_test
                               plot.type = "histogram",      # type of plot to make, default is histogram
                               species.list = names(diet.sensitivity.test.output), # species to plot, default is all species in the model
                              metric = "trophic level" # metric being plotted for y-axis name
){
  ## CHECK INPUTS ##
  # make sure resample_data_list is a list from the diet_resample function
  if(!is.list(diet.sensitivity.test.output))
    stop("diet.sensitivity.test.output must be the output from diet_resample")

  # make sure the species list is a vector
  if(!is.character(species.list))
    stop("species_list must be a vector of character names that matches names in resample_data_list")

  # make sure all species in species_list are in diet_sensitivity_test_output
  # find missing spp
  missing_spp = species.list[!species.list %in% names(diet.sensitivity.test.output)]
  # check if any are missing
  if (length(missing_spp) > 0)
    stop(paste("the following species in species_list are not found in diet_sensitivity_test_output:",
               paste(missing_spp, collapse = ", ")))

  ## FUNCTION ##
  # if the plot type is histogram make histogram
  if (plot.type == "histogram") {
    for (i in 1:length(species.list)) {
      species_name = species.list[i]
      # define x-axis label (cannot be done inside the hist function)
      xlab_text = if (metric == "trophic level"){
        "Trophic Level"
      } else if (metric == "biomass"){
        "Biomass"
      }else{
        print("Invalid metric type, x-axis will not be labeled. Metric must be either 'trophic level'
                   or 'biomass'")
        ""
      }
      hist(diet.sensitivity.test.output[[species_name]],
           main = species_name,
           xlab = xlab_text,
           ylab = "Frequency")
    }
  } else if (plot.type == "boxplot") {
    # select only the species in species_list
    boxplot_species = diet.sensitivity.test.output[species.list]
    # define title
    title = if (metric == "trophic level"){
      "Selected Species TL Distribution"
    } else if (metric == "biomass"){
      "Selected Species Biomass Distribution"
    } else{
      print("Invalid metric type, title will not be labeled. Metric must be either 'trophic level'
                               or 'biomass'")
      ""
    }
    # define y-axis label
    ylab_text = if (metric == "trophic level"){
      "Trophic Level"
    } else if (metric == "biomass"){
      "Biomass"
    }else{
      print("Invalid metric type, x-axis will not be labeled. Metric must be either 'trophic level'
                   or 'biomass'")
      ""
    }
    # make boxplot
    boxplot(boxplot_species,
            main = title,
            xlab = "Species",
            ylab = ylab_text)
  }
    else if (plot_type == "violin") {
      # select the species in species_list
      violinplot_species = diet.sensitivity.test.output[species.list]
      # define title outside of vioplot function
      title = if (metric == "trophic level"){
                "Selected Species TL Distribution"
              } else if (metric == "biomass"){
                "Selected Species Biomass Distribution"
              } else{
                print("Invalid metric type, title will not be labeled. Metric must be either 'trophic level'
                               or 'biomass'")
                ""
              }
      # make violin plot
      vioplot::vioplot(violinplot_species,
              main = title,
              xlab = "",
              ylab = "")
      if (metric == "trophic level"){
        # add y axis label
        mtext("Trophic Level", side = 2, line = 3)
      } else if (metric == "biomass"){
        # add y axis label
        mtext("Biomass", side = 2, line = 3)
      } else{
        print("Invalid metric type, y-axis will not be labeled. Metric must be either 'trophic level' or 'biomass'")
      }
      # add x axis label
      mtext("Species", side = 1, line = 3)
    }
   else {
    print("Error, invalid plot_type, must be 'histogram' or 'boxplot'")
  }
}
