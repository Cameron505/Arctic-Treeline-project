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


# source("other_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
list(
  
  # sample metadata
  #tar_target(sample_key_data, "Data/Sample_key_AntecedentTemp.csv", format = "file"),
  #tar_target(sample_key, read.csv(sample_key_data)),
  
  
  # data files
  #tar_target()
  tar_target(PoreWater_data_read,"Data/PoreWater.csv", format="file"),
  tar_target(PoreWater_data, read.csv(PoreWater_data_read)),
  tar_target(PoreWater_processed, process_PoreWater(PoreWater_data)),
  tar_target(PoreWater_processed_Seasonal, process_PoreWater_Seasonal(PoreWater_data)),
  
  tar_target(PoreWater_data_read_Lysim,"Data/Lysim.csv", format="file"),
  tar_target(PoreWater_data_Lysim, read.csv(PoreWater_data_read_Lysim)),
  tar_target(PoreWater_processed_Lysim, process_Lysim_Destruct(PoreWater_data_Lysim)),
  
  tar_target(Extract_data_read,"Data/Extractions.csv", format="file"),
  tar_target(Extract_data, read.csv(Extract_data_read)),
  tar_target(Extract_processed, process_Extract(Extract_data)),
  tar_target(Extract_processed_Seasonal, process_Extract_Seasonal(Extract_data)),
  tar_target(Extract_processed_H2O, process_Extract_H2O(Extract_data)),
  tar_target(Extract_processed_Seasonal_H2O, process_Extract_Seasonal_H2O(Extract_data)),
  
  #tar_target(Resin_data_read,"Data/Resin.csv", format="file"),
  #tar_target(Resin_data, read.csv(Resin_data_read)),
  #tar_target(Resin_processed, process_respiration(Resin_data)),
  
  
  # analysis - graphs
  tar_target(gg_aggie_Extract_Snowfence_Control, plot_Extract_Snowfence(Extract_processed)),
  tar_target(gg_aggie_Extract_Snowfence_Control_H2O, plot_Extract_Snowfence_H2O(Extract_processed_H2O)),
  tar_target(gg_aggie_PoreWater_Snowfence_Control, plot_PoreWater_Snowfence(PoreWater_processed)),
  tar_target(gg_aggie_PoreWater_Lysim, plot_PoreWater_Lysim(PoreWater_processed_Lysim)),
  tar_target(gg_aggie_PoreWater_Seasonal, plot_PoreWater_Seasonal(PoreWater_processed_Seasonal)),
  tar_target(gg_Extract_Seasonal, plot_Extract_Seasonal(Extract_processed_Seasonal)),
  tar_target(gg_Extract_Seasonal_H2O, plot_Extract_Seasonal(Extract_processed_Seasonal)),

  
  # combined data
 
  
  # report  
  tar_render(report, path = "reports/Aggie_report.Rmd")
  
)
