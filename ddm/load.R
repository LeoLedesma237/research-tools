# Get the directory containing this load.R file

# Set default path if ddm_dir has not already been defined
if (!exists("ddm_dir")) {
  ddm_dir <- here("ddm")
}

# Source in the code
source(file.path(ddm_dir, "R", "data_design.R"))
source(file.path(ddm_dir, "R", "get_descriptives.R"))
source(file.path(ddm_dir, "R", "plot_defectiveDensity.R"))
source(file.path(ddm_dir, "R", "plot_defectiveDensityComparison.R"))
source(file.path(ddm_dir, "R", "plot_defectiveDensityGNG.R"))
source(file.path(ddm_dir, "R", "plot_defectiveDensityComparisonGNG.R"))