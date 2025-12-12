# Makefile for Data Cleaning Scripts
# This builds all cleaned datasets needed for analysis

.PHONY: all shapefiles afrobarometer dependent-vars population panel-data clean help

# R command (adjust if needed for your environment)
RSCRIPT = Rscript

# Output directories
SHAPEFILE_DIR = ../../00_rawdata/shapefiles
AFRO_PROCESSED = ../../00_rawdata/ab_raw/processed
NL_DIR = ../../00_rawdata/nightlights/topcodefix
POP_DIR = ../../00_rawdata/population
PANEL_DIR = ../../01_panel_data

# Target files
SHAPEFILES = $(SHAPEFILE_DIR)/gadm_admin1.shp $(SHAPEFILE_DIR)/gadm_admin2.shp
AFRO_MERGED = $(AFRO_PROCESSED)/afrobarometer_w3_w6_geomerged_new.csv
AFRO_PANEL = $(AFRO_PROCESSED)/admin1_afro_panel.csv $(AFRO_PROCESSED)/admin2_afro_panel.csv
DEP_VARS = ../../00_rawdata/processed_dep_vars_admin1.csv ../../00_rawdata/processed_dep_vars_admin2.csv
NL_DATA = $(NL_DIR)/processed_topcodefix_nl_admin1.csv $(NL_DIR)/processed_topcodefix_nl_admin2.csv
POPULATION = $(POP_DIR)/admin1_population.csv $(POP_DIR)/admin2_population.csv
PANEL_DATA = $(PANEL_DIR)/panel_aid_admin1_fin.csv $(PANEL_DIR)/panel_aid_admin2_fin.csv


# Main target - build all cleaned datasets
all: shapefiles afrobarometer dependent-vars population panel-data
	@echo "All data cleaning complete!"

# Build shapefiles from GADM data
shapefiles: $(SHAPEFILES)

$(SHAPEFILES): 00_build_full_shp.r
	@echo "Building shapefiles..."
	cd ../.. && $(RSCRIPT) 02_scripts/01_cleaning/00_build_full_shp.r

# Process Afrobarometer data
afrobarometer: $(AFRO_PANEL)

$(AFRO_PANEL): 01_build_afro_full.r $(SHAPEFILES)
	@echo "Processing Afrobarometer data..."
	cd ../.. && $(RSCRIPT) 02_scripts/01_cleaning/01_build_afro_full.r

# Build dependent variables (nightlights, U5M)
dependent-vars: $(DEP_VARS)

$(DEP_VARS): 02_build_dependent_vars.r $(SHAPEFILES) ../utils/spatial_processing_utils.r
	@echo "Building dependent variables..."
	cd ../.. && $(RSCRIPT) 02_scripts/01_cleaning/02_build_dependent_vars.r

# Build population data
population: $(POPULATION)

$(POPULATION): 03_build_pop.r $(SHAPEFILES)
	@echo "Building population data..."
	cd ../.. && $(RSCRIPT) 02_scripts/01_cleaning/03_build_pop.r

# Build final panel datasets
panel-data: $(PANEL_DATA) $(PANEL_DATA_HEALTH)

$(PANEL_DATA): 04_create_iv.r $(DEP_VARS) $(POPULATION)
	@echo "Building panel data (main)..."
	cd ../.. && $(RSCRIPT) 02_scripts/01_cleaning/04_create_iv.r

# Clean all generated files
clean:
	@echo "Cleaning generated data files..."
	rm -f $(SHAPEFILES)
	rm -f $(SHAPEFILE_DIR)/gadm_admin1.*
	rm -f $(SHAPEFILE_DIR)/gadm_admin2.*
	rm -f $(AFRO_PROCESSED)/*.csv
	rm -f $(NL_DIR)/*.csv
	rm -f $(DEP_VARS)
	rm -f $(POPULATION)
	rm -f $(PANEL_DATA) $(PANEL_DATA_HEALTH)

# Display help
help:
	@echo "Data Cleaning Pipeline - Available Targets:"
	@echo ""
	@echo "  make all            - Run entire cleaning pipeline"
	@echo "  make shapefiles     - Build GADM shapefiles"
	@echo "  make afrobarometer  - Process Afrobarometer survey data"
	@echo "  make dependent-vars - Build dependent variables"
	@echo "  make population     - Build population data"
	@echo "  make panel-data     - Build final panel datasets"
	@echo "  make clean          - Remove all generated files"
	@echo "  make help           - Display this help message"
	@echo ""
