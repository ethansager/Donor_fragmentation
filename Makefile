# Main Makefile for Donor Fragmentation Project
# This orchestrates the entire data processing and analysis pipeline

.PHONY: all build clean clean-data clean-output help cleaning cleaning-5year analysis analysis-5year tables tables-5year fresh-analysis

# Default target
all: cleaning analysis

# Alias for all
build: all

# Run data cleaning pipeline
cleaning:
	@echo "Running data cleaning pipeline..."
	$(MAKE) -C 02_scripts/01_cleaning

# Run analysis pipeline
analysis: cleaning
	@echo "Running analysis pipeline..."
	$(MAKE) -C 02_scripts/02_analysis

# Build 5-year bucketed panels only
cleaning-5year:
	@echo "Running 5-year data build..."
	$(MAKE) -C 02_scripts/01_cleaning panel-data-5year

# Run 5-year control-function analysis
analysis-5year: cleaning-5year
	@echo "Running 5-year analysis pipeline..."
	$(MAKE) -C 02_scripts/02_analysis tables-5year

# Run fresh from-scratch analysis (branch: fresh-analysis)
fresh-analysis: cleaning-5year
	@echo "Running fresh analysis pipeline..."
	$(MAKE) -C 02_scripts/02_analysis fresh-analysis-5year

# Generate all tables (shortcut for analysis target)
tables: analysis

# Generate 5-year tables only
tables-5year: analysis-5year

# Clean all generated data and outputs
clean: clean-data clean-output

# Clean processed data files
clean-data:
	@echo "Cleaning processed data files..."
	rm -f 00_rawdata/shapefiles/gadm_admin1.*
	rm -f 00_rawdata/shapefiles/gadm_admin2.*
	rm -f 00_rawdata/ab_raw/processed/*.csv
	rm -f 00_rawdata/nightlights/topcodefix/processed_topcodefix_nl_admin*.csv
	rm -f 00_rawdata/nightlights/processed_nl_admin*.csv
	rm -f 00_rawdata/processed_dep_vars_admin*.csv
	rm -f 00_rawdata/population/admin*_population.csv
	rm -f 01_panel_data/panel_aid_admin*.csv

# Clean output files (tables, figures, etc.)
clean-output:
	@echo "Cleaning output files..."
	rm -f *.tex
	rm -f *.txt

# Display help information
help:
	@echo "Donor Fragmentation Project - Available Make Targets:"
	@echo ""
	@echo "  make all/build    - Run entire pipeline (cleaning + analysis)"
	@echo "  make cleaning     - Run data cleaning scripts only"
	@echo "  make analysis     - Run analysis scripts (depends on cleaning)"
	@echo "  make tables       - Generate regression tables"
	@echo "  make fresh-analysis - Run from-scratch analysis (branch: fresh-analysis)
  make clean        - Remove all generated data and outputs"
	@echo "  make clean-data   - Remove processed data files only"
	@echo "  make clean-output - Remove output files (tables) only"
	@echo "  make help         - Display this help message"
	@echo ""
