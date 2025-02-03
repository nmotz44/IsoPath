## error testing
# trying to get the errors from format_simmr_data to pop up
library(RpathSIA2)
library(simmr)
# load in data
data("AFS_SIA_Data")

# this should load normally
simmr_object = format_simmr_data(data = AFS_SIA_Data, species.column = "Functional_Group", d13C.column = "d13C",
                                 d15N.column = "d15N", consumer.names = "AnchovyJuv", prey.names =  c("Phytoplankton", "Zooplankton", "Detritus",
                                                                                                      "BenthicAlg", "Ulva","BenthicInverts", "MudSnail"))

# this should give error and not make an object
# error should be: error, not all consumer.names are found in data, missing species are: AnchovyJuv2
simmr_object2 = format_simmr_data(data = AFS_SIA_Data, species.column = "Functional_Group", d13C.column = "d13C",
                                 d15N.column = "d15N", consumer.names = "AnchovyJuv2", prey.names =  c("Phytoplankton", "Zooplankton", "Detritus",
                                                                                                      "BenthicAlg", "Ulva","BenthicInverts", "MudSnail"))

# this should give error and not make object
# error should be: isotope data must be numeric
simmr_object3 = format_simmr_data(data = AFS_SIA_Data, species.column = "Functional_Group", d13C.column = "d13Cs",
                                  d15N.column = "d15N", consumer.names = "AnchovyJuv", prey.names =  c("Phytoplankton", "Zooplankton", "Detritus",
                                                                                                        "BenthicAlg", "Ulva","BenthicInverts", "MudSnail"))

# this should give warning but make object
# warning should be: [1] "Warning: Not all prey.names are found in data. Missing species are: MudSnails"
simmr_object3 = format_simmr_data(data = AFS_SIA_Data, species.column = "Functional_Group", d13C.column = "d13C",
                                  d15N.column = "d15N", consumer.names = "AnchovyJuv", prey.names =  c("Phytoplankton", "Zooplankton", "Detritus",
                                                                                                       "BenthicAlg", "Ulva","BenthicInverts", "MudSnails"))
