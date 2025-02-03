##### AFS Model V2 #####
# THIS CODE WILL RUN A MODEL ON THE WEEK 2 OYSTER REEF DATA FOR AFS
# THIS WILL BE BROAD AND HAVE MOST SPECIES AS SPERATE GROUPS
# THE ONLY GROUPINGS ARE FOR FUNDULUS, BENTHIC INVERTS, AND JUVENILE FISHES WITH ANCHOVIES
# THIS VERSION REVISES DIETS TO TAKE OUT UNLIKELY PREY FROM SPOT (REMOVING OTHER FISH)
# ALSO AUDITS OTHER DIETS COMPARED TO VASSLIDES MODEL
  # TOOK OUT FISH FROM THE SPOT DIET AND ADDED IN ZOOPLANKTON
# PHYTOPLANKTON ADDED TO BENTHIC INVERT DIET

##### SECTION 1: LOAD PACKAGES AND FUNCTIONS #####
# clear workspace
rm(list = ls())

# load required packages
library(simmr)
library(Rpath)
library(RpathSIA2)


##### SECTION 2: SETWD AND LOAD DATA #####
# set wd
setwd('~/Documents/Oyster Reef Model/Data/Afs Model')

# load data
iso_data = read.csv("AFS SIA Data2.csv")

##### SECTION 3: SETUP AND RUN SIMMR FOR ALL SPECIES THAT NEEDS SIMMR #####
# list of species that needs simmr
predator_list = c("AnchovyJuv", "BenthicInverts", "BlueCrab", "Fundulus", "MudSnail", "Oyster", "Silverside",
                  "Spot")
# list of diets for each predator
prey_list = list(
  c("Phytoplankton", "Zooplankton", "Detritus", "BenthicAlg", "Ulva","Benthic Inverts", "MudSnail"), # diet for AnchovyJuv
  c("Phytoplankton","Zooplankton", "BenthicAlg", "Ulva","Detritus"), # diet for BenthicInverts
  c("BenthicAlg", "Ulva","Detritus", "BenthicInverts", "MudSnail", "AnchovyJuv", "Silverside", "Fundulus"), # diet for BlueCrab
  c("Zooplankton","Detritus", "BenthicAlg", "Ulva","BenthicInverts", "MudSnail", "Silverside", "AnchovyJuv"), #diet for fundulus
  c("BenthicAlg", "Ulva", "Detritus"), # diet for mudsnail
  c("Phytoplankton", "Detritus"), # diet for oyster
  c("Zooplankton", "BenthicAlg", "Ulva", "Detritus", "BenthicInverts", "MudSnail"), # diet for Silverside
  c("Zooplankton","Detritus", "BenthicInverts", "MudSnail") # diet for spot
)

# pass diets and species to simmr and run simmr
for (i in 1:length(predator_list)){
  print("=====================================================================")
  print(predator_list[i])
  # create simmr object
  simmr_object = format_simmr_data(iso_data, "Functional_Group", "d13C", "d15N", predator_list[i], prey_list[[i]])
  # run simmr
  simmr.out = simmr_mcmc(simmr_object, mcmc_control = list(iter = 10000, burn = 1000, thin = 10, n.chain = 4))
  # check diagnostics
  summary(simmr.out, type = "diagnostics")
  # if prey_list has BenthicAlg then combine sources here below
  if (grepl("BenthicAlg", prey_list[i])){
    # combine sources
    simmr.out.post =
      combine_sources(simmr.out,
                      to_combine = c("Ulva", "BenthicAlg"),
                      new_source_name = "BenthicAlgea")
    # check diagnostics
    summary(simmr.out.post, type = "diagnostics")
    # save output
    assign(paste0("simmr_", predator_list[i]), simmr.out.post)
  } else{
    # save simmr output with predator name
    assign(paste0("simmr_", predator_list[i]), simmr.out)
  }
}

##### SECTION 3: SET UP BASIC RPATH #####
# create vector of group names
groups = c('Phytoplankton', 'BenthicAlgea', 'Oyster', 'Zooplankton','MudSnail', 'BenthicInverts', 'AnchovyJuv',
           'Silverside', 'Fundulus', 'Spot','BlueCrab', 'Detritus', 'Fishing')

# create vector of group types
# 0 - living, 1 - primary producer, 2 - detritus, 3 - fishing fleet
types = c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 3)

# create rpath parameter file
REco.params = create.rpath.params(group = groups, type = types)

# biomass and parameter values for model
# biomass
biomass = c(NA, # phytoplankton
            NA, # benthicalgea
            8.1157, # oyster
            NA, # zooplankton
            NA, # mud snail
            NA, # benthic inverts
            2.04247261, # anchovy and juveniles
            6.24915461, # silversides
            9.99594211, # fundulus
            0.97389422, # spot
            6.03273367, # blue crab
            NA, # detritus
            NA) # fishing

# P/B
pb = c(160, # phytoplankton
       80, #benthic algea
       0.63, # oysters
       25, # zooplankton
       2, # mudsnail
       2, # benthic inverts
       3.12, # anchovy and juveniles changed back from 3.1
       1.1, # silversides
       1.2, # fundulus
       0.9, # spot
       1.21, # blue crab
       NA, # detritus
       NA) # fishing

# Q/B
qb = c(NA, # phytoplankton
       NA, # benthic algea
       2, # oysters
       83.33, # zooplankton
       10, # mudsnail
       10, # benthic inverts
       9.7, # anchovy and juveniles
       4, # silversides
       3.65, # fundulus
       6.2, # spot
       4, # blue crab
       NA, # detritus
       NA) # fishing

# assign values to rpath parameter
REco.params$model[, Biomass := biomass]
REco.params$model[, PB := pb]
REco.params$model[, QB := qb]

# EE for groups without biomass
REco.params$model[Group == 'Phytoplankton', EE := 0.95]
REco.params$model[Group == 'Zooplankton', EE := 0.95]
REco.params$model[Group == 'BenthicAlgea', EE := 0.899]
REco.params$model[Group == 'Oyster', EE := 0]
REco.params$model[Group == 'MudSnail', EE := 0.9]
REco.params$model[Group == 'BenthicInverts', EE := 0.9]

#Biomass accumulation and unassimilated consumption
REco.params$model[, BioAcc  := c(rep(0, 12), rep(NA, 1))]
REco.params$model[, Unassim := c(rep(0, 2), rep(0.2, 10), rep(NA, 1))]

#Detrital Fate
REco.params$model[, Detritus := c(rep(1, 11), rep(0, 2))]

# fisheries landings and discards (assuming 0 here)
# create landings and discards object
land = c(rep(0, 12), rep(NA, 1))
disc = c(rep(0, 12), rep(NA, 1))

# assign them to the rpath model
REco.params$model[, Fishing := land]
REco.params$model[, Fishing.disc := disc]

##### SECTION 4: PULL OUT MEAN DIETS FROM SIMMR OBJECTS #####
# make list of simmr objects
simmr_list = list(simmr_AnchovyJuv, simmr_BenthicInverts, simmr_BlueCrab, simmr_Fundulus, simmr_MudSnail,
                  simmr_Oyster, simmr_Silverside, simmr_Spot)

# pull out means from each of the simmr objects
for (i in 1:length(simmr_list)){
  # function to pull out means
  simmr.diet = simmr_diet_output_mean(simmr_list[[i]], groups)
  # save simmr output with predator name
  assign(paste0("diet_", predator_list[i]), simmr.diet)
}

################################################################################
##### SECTION 5: PASS DIETS TO RPATH AND RUN RPATH #####
# input diets for each species
REco.params$diet[, AnchovyJuv := c(diet_AnchovyJuv)]
REco.params$diet[, Fundulus := c(diet_Fundulus)]
REco.params$diet[, BlueCrab := c(diet_BlueCrab)]
REco.params$diet[, MudSnail := c(diet_MudSnail)]
REco.params$diet[, BenthicInverts := c(diet_BenthicInverts)]
REco.params$diet[, Oyster := c(diet_Oyster)]
REco.params$diet[, Silverside := c(diet_Silverside)]
REco.params$diet[, Spot := c(diet_Spot)]
REco.params$diet[, Zooplankton := c(1, rep(0, 11), NA)]

# run Rpath
# base run to check balance
REco = rpath(REco.params, eco.name = "Oyster Reef_SIA")
REco

# make webplot
webplot(REco, labels = T)

webplot(REco, fleets = T, highlight = 'BlueCrab', labels = T)

################################################################################
###### SECTION 6: RESAMPLE DIETS AND TEST FOR SENSITIVITY
# obtain diet distribution
for (i in 1:length(simmr_list)) {
  # make list of the distribution
  simmr.dist = simmr_diet_output_dist(simmr_list[[i]], groups)
  # resample within the distribution
  resample.diet = diet_resample(simmr.dist)
  # save the output
  assign(paste0("resample_", predator_list[i]), resample.diet)
}

# make list of resampled diets
resample_data_list <- list(
  AnchovyJuv = resample_AnchovyJuv,
  BenthicInverts = resample_BenthicInverts,
  BlueCrab = resample_BlueCrab,
  Fundulus = resample_Fundulus,
  MudSnail = resample_MudSnail,
  Silverside = resample_Silverside,
  Oyster = resample_Oyster,
  Spot = resample_Spot
)

# pass diets to rpath and examine sensitivity
resampled.TL = diet_sensitivity_test(resample_data_list, REco.params)

# make histograms
diet_visualization(resampled.TL, plot_type = "histogram",
                   species_list = c("MudSnail", "BenthicInverts", "AnchovyJuv",
                                    "Silverside", "Fundulus", "Spot", "BlueCrab"))
# make boxplot
diet_visualization(resampled.TL, plot_type = "boxplot",
                   species_list = c("MudSnail", "BenthicInverts", "AnchovyJuv",
                                    "Silverside", "Fundulus", "Spot", "BlueCrab"))
# make violin plot
diet_visualization(resampled.TL, plot_type = "violin",
                   species_list = c("MudSnail", "BenthicInverts", "AnchovyJuv",
                                    "Silverside", "Fundulus", "Spot", "BlueCrab"))

## exploring the ecopath output

