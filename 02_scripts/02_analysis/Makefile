# Makefile for Analysis Scripts
# This runs regressions and generates output tables

.PHONY: all regressions tables figures clean help

# R command
RSCRIPT = Rscript

# Input data
PANEL_DATA_MAIN = ../../01_panel_data/panel_aid_admin1_new.csv ../../01_panel_data/panel_aid_admin2_new.csv
PANEL_DATA_HEALTH = ../../01_panel_data/panel_aid_admin1_health_fin.csv ../../01_panel_data/panel_aid_admin2_health_fin.csv

# Output files
TABLES_MAIN = ../../03_outputs/tabs/table1_ols.tex ../../03_outputs/tabs/table1_fe.tex ../../03_outputs/tabs/table2_cfe.tex ../../03_outputs/tabs/table2_cfe_u5m.tex
TABLES_HEALTH = ../../03_outputs/tabs/table1_ols_health.tex ../../03_outputs/tabs/table1_fe_health.tex ../../03_outputs/tabs/table2_cfe_health.tex ../../03_outputs/tabs/table2_cfe_u5m_health.tex
SUMMARY_TABLES = ../../03_outputs/tabs/summary_table_admin1.tex ../../03_outputs/tabs/summary_table_admin2.tex
MODEL_OUTPUT = ../../03_outputs/tabs/ols_models.txt ../../03_outputs/tabs/fe_models_health.txt ../../03_outputs/tabs/fe_models.tex ../../03_outputs/tabs/ols_models_health.txt
FIGURES = # Add figure files if they are generated

# Main target - run all analysis
all: regressions tables
	@echo "All analysis complete!"

# Run regressions
regressions: $(PANEL_DATA_MAIN)
	@echo "Running regression analysis..."
	cd ../.. && $(RSCRIPT) 02_scripts/02_analysis/05_run_regressions.R

# Generate tables from regression results
tables: regressions $(PANEL_DATA_HEALTH)
	@echo "Generating regression tables..."
	cd ../.. && $(RSCRIPT) 02_scripts/02_analysis/06_dreher_tables.R

# Generate figures
figures: $(PANEL_DATA_MAIN)
	@echo "Generating figures..."
	cd ../.. && $(RSCRIPT) "02_scripts/02_analysis/Make figure 1 and 2.r"

# Clean generated outputs
clean:
	@echo "Cleaning analysis outputs..."
	rm -f $(TABLES_MAIN) $(TABLES_HEALTH)
	rm -f $(SUMMARY_TABLES)
	rm -f $(MODEL_OUTPUT)
	rm -f ../../*.tex ../../*.txt

# Display help
help:
	@echo "Analysis Pipeline - Available Targets:"
	@echo ""
	@echo "  make all         - Run regressions and generate tables"
	@echo "  make regressions - Run regression analysis"
	@echo "  make tables      - Generate regression output tables"
	@echo "  make figures     - Generate figures 1 and 2"
	@echo "  make clean       - Remove all generated outputs"
	@echo "  make help        - Display this help message"
	@echo ""
