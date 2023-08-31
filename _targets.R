# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c("tibble", "tidyverse"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Load the R scripts with your custom functions:
source("Code/0-packages.R")
source("Code/a-processing_functions.R")
source("Code/b-analysis_functions.R")
source("code/fticrrr/b-functions_analysis.R")

# source("other_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
list(
  
  # sample metadata
  #tar_target(sample_key_data, "Data/Sample_key_AntecedentTemp.csv", format = "file"),
  #tar_target(sample_key, read.csv(sample_key_data)),
  
  
  # data files
  #tar_target()

  tar_target(fticr_meta_load,"data/processed/fticr_meta_combined.csv", format="file"),
  tar_target(fticr_meta, read.csv(fticr_meta_load)),
  tar_target(fticr_data_longform_load,"data/processed/fticr_data_longform_combined.csv", format="file"),
  tar_target(fticr_data_longform, read.csv(fticr_data_longform_load)),
  tar_target(fticr_data_trt_load,"data/processed/fticr_data_trt_combined.csv", format="file"),
  tar_target(fticr_data_trt, read.csv(fticr_data_trt_load)),
  
  tar_target(fticr_hcoc, process_fticr_hcoc(fticr_data_trt,fticr_meta)),
  tar_target(relabund_cores, process_fticr_relabund_cores(fticr_data_longform,fticr_meta)),
  tar_target(fticr_hcoc_polar, process_fticr_hcoc_polar(fticr_hcoc)),
  tar_target(fticr_hcoc_nonpolar, process_fticr_hcoc_nonpolar(fticr_hcoc)),
  tar_target(relabund_cores_polar, process_fticr_relabund_cores_polar(relabund_cores)),
  tar_target(relabund_cores_nonpolar, process_fticr_relabund_cores_nonpolar(relabund_cores)),
  tar_target(pca_polar, process_pca_polar(relabund_cores)),
  tar_target(pca_nonpolar, process_pca_nonpolar(relabund_cores)),
  tar_target(pca_hydric, process_pca_hydric(relabund_cores)),
  
  
  
  #Soil Temperature
  tar_target(KotzTemp_data_read,"Data/Temp/kotz.daily.air.temp.1900.2022.csv", format="file"),
  tar_target(KotzTemp_data, read.csv(KotzTemp_data_read)),
  tar_target(Kotz_proccessed, process_KotzTemp(KotzTemp_data)),
  tar_target(SoilTempHydric_data_read,"Data/Temp/Hydric Met Station.csv", format="file"),
  tar_target(SoilTempHydric_data, read.csv(SoilTempHydric_data_read)),
  tar_target(SoilTempMesic_data_read,"Data/Temp/Mesic Met Station.csv", format="file"),
  tar_target(SoilTempMesic_data, read.csv(SoilTempMesic_data_read)),
  tar_target(SoilTempXeric_data_read,"Data/Temp/Xeric Met Station.csv", format="file"),
  tar_target(SoilTempXeric_data, read.csv(SoilTempXeric_data_read)),
  
  tar_target(Ibutton_data_read,"Data/Temp/ibuttons.csv", format="file"),
  tar_target(Ibutton_data, read.csv(Ibutton_data_read)),
  
  #Merging and predicting soil temperatures based on air temps
  tar_target(Kotz_proccessed_HMX, process_SoilTemp(SoilTempHydric_data,SoilTempMesic_data,SoilTempXeric_data,Kotz_proccessed)),
  
  tar_target(Soil_Temp, plot_soilTemp(SoilTempHydric_data,SoilTempMesic_data,SoilTempXeric_data,Kotz_proccessed)),
  
  #tar_target(Ibutton_Temp, plot_Ibutton(Ibutton_data)),
  #branch extension measurments
  tar_target(AggieBranch_data_read,"Data/tree/snowfence.branch.primary.growth.csv", format="file"),
  tar_target(AggieBranch_data, read.csv(AggieBranch_data_read)),
  tar_target(AggieNNeedle_data_read,"Data/tree/Needle_N.csv", format="file"),
  tar_target(AggieNNeedle_data, read.csv(AggieNNeedle_data_read)),
  
  
  
  
  
  
  tar_target(PoreWater_data_read,"Data/PoreWater.csv", format="file"),
  tar_target(PoreWater_data, read.csv(PoreWater_data_read)),
  tar_target(PoreWater_processed, process_PoreWater(PoreWater_data)),
  tar_target(PoreWater_processed_Seasonal, process_PoreWater_Seasonal(PoreWater_data)),
  tar_target(PoreWater_processed_all, process_PoreWater_all(PoreWater_data)),
  
  tar_target(PoreWater_data_read_Lysim,"Data/Lysim.csv", format="file"),
  tar_target(PoreWater_data_Lysim, read.csv(PoreWater_data_read_Lysim)),
  tar_target(PoreWater_processed_Lysim, process_Lysim_Destruct(PoreWater_data_Lysim)),
  
  tar_target(Extract_data_read,"Data/Extractions.csv", format="file"),
  tar_target(Extract_data, read.csv(Extract_data_read)),
  tar_target(Extract_processed, process_Extract(Extract_data)),
  tar_target(Extract_processed_Seasonal, process_Extract_Seasonal(Extract_data)),
  tar_target(Extract_processed_all, process_Extract_all(Extract_data)),
  tar_target(Extract_processed_H2O, process_Extract_H2O(Extract_data)),
  tar_target(Extract_processed_Seasonal_H2O, process_Extract_Seasonal_H2O(Extract_data)),
  tar_target(Extract_processed_all_H2O, process_Extract_all_H2O(Extract_data)),
  tar_target(Extract_processed_lysim, process_Extract_lysim(Extract_data)),
  
  tar_target(Inaccessable, plot_inaccess(Extract_processed_Seasonal,PoreWater_processed_Seasonal)),
  tar_target(Inaccessable_p, plot_inaccess_p(Extract_processed_Seasonal,PoreWater_processed_Seasonal)),
  
  tar_target(Resin_data_read,"Data/Resin.csv", format="file"),
  tar_target(Resin_data, read.csv(Resin_data_read)),
  tar_target(Resin_processed, process_resin(Resin_data)),
  
  
  # analysis - graphs
  tar_target(gg_aggie_Extract_Snowfence_Control, plot_Extract_Snowfence(Extract_processed)),
  tar_target(gg_aggie_Extract_Snowfence_Control_H2O, plot_Extract_Snowfence_H2O(Extract_processed_H2O)),
  tar_target(gg_aggie_PoreWater_Snowfence_Control, plot_PoreWater_Snowfence(PoreWater_processed)),
  tar_target(gg_aggie_PoreWater_Lysim, plot_PoreWater_Lysim(PoreWater_processed_Lysim)),
  tar_target(gg_aggie_PoreWater_Seasonal, plot_PoreWater_Seasonal(PoreWater_processed_Seasonal)),
  tar_target(gg_Extract_Seasonal, plot_Extract_Seasonal(Extract_processed_Seasonal)),
  tar_target(gg_Extract_Seasonal_H2O, plot_Extract_Seasonal_H2O(Extract_processed_Seasonal_H2O)),

  tar_target(gg_aggie_PoreWater_all, plot_PoreWater_all(PoreWater_processed_all)),
  tar_target(gg_Extract_all, plot_Extract_all(Extract_processed_all)),
  tar_target(gg_Extract_all_H2O, plot_Extract_all_H2O(Extract_processed_all_H2O)),
  tar_target(gg_Extract_lysim, plot_Extract_lysim(Extract_processed_lysim)),
  tar_target(gg_resin, plot_resin(Resin_processed)),
  tar_target(gg_resin_Snowfence, plot_resin_Snowfence(Resin_processed)),
  tar_target(gg_resin_Ancillary, plot_resin_Ancillary(Resin_processed)),
  
  tar_target(gg_Extract_Fert, plot_Extract_Fert(Extract_data)),
  tar_target(gg_Porewater_Fert, plot_PoreWater_Fert(PoreWater_data)),
  tar_target(gg_Extract_H2O_Fert, plot_Extract_Fert_H2O(Extract_data)),
  tar_target(gg_Resin_Fert, plot_resin_Fert(Resin_processed)),
  
  
  tar_target(gg_fticr_Domains, plot_fticr_Domains(fticr_meta)),
  tar_target(gg_polarVnonpolar_vk, plot_polarVnonPolar_vk(fticr_hcoc)),
  tar_target(gg_polarVnonpolar_pca, plot_polarVnonPolar_pca(relabund_cores)),
  tar_target(Permanova_table_polar, plot_permanova_polar(relabund_cores_polar)),
  tar_target(Permanova_table_nonpolar, plot_permanova_nonpolar(relabund_cores_nonpolar)),
  tar_target(gg_pca_by_site, plot_pca_by_site(pca_polar)),
  tar_target(gg_pca_by_site_nonpolar, plot_pca_by_site_nonpolar(pca_nonpolar)),
  tar_target(gg_pca_polar, plot_pca_polar(pca_polar)),
  tar_target(gg_pca_nonpolar, plot_pca_nonpolar(pca_nonpolar)),
  tar_target(gg_pca_hydric, plot_pca_hydric(pca_hydric)),
  tar_target(gg_vk_polar, plot_vk_polar(fticr_hcoc_polar)),
  tar_target(gg_vk_nonpolar, plot_vk_polar(fticr_hcoc_nonpolar)),
  tar_target(gg_unique, plot_unique(fticr_hcoc,fticr_meta)),
  tar_target(gg_unique2, plot_unique2(fticr_hcoc,fticr_meta)),
  tar_target(gg_seasonal, plot_seasonal_Mesic_Hydric_polar(fticr_hcoc_polar)),
  tar_target(gg_seasonal_nonpolar, plot_seasonal_Mesic_Hydric_polar(fticr_hcoc_nonpolar)),
  
  # combined data
  tar_target(pca_polar_nonpolar, plot_pca_PNP(gg_pca_by_site_nonpolar,gg_pca_by_site)),
  
  # report  
  tar_render(report, path = "reports/Aggie_report.Rmd"),
  tar_render(report2, path = "reports/Aggie_Short_report.Rmd"),
  tar_render(report3, path = "reports/Aggie_Seasonal.Rmd"),
  tar_render(report4, path = "reports/Treeline_Supplement.Rmd")
  
)
