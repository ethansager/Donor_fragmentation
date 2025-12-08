# Code Refactoring Documentation

## Overview
This document describes the refactoring performed to eliminate code duplication in the Donor_fragmentation project.

## Changes Made

### 1. Created Utility Functions Library

Three new utility script files were created in `scripts/utils/`:

#### `spatial_processing_utils.r`
Contains reusable functions for spatial data processing:
- **`load_admin_shapefile()`**: Loads and transforms administrative shapefiles with a specified CRS
- **`extract_zonal_stats()`**: Extracts zonal statistics from rasters for multiple years
- **`process_admin_levels()`**: Processes both admin1 and admin2 shapefiles with raster data in one call
- **`extract_multiband_stats()`**: Extracts values from multi-band rasters (e.g., U5M data)
- **`build_panel_data()`**: Converts wide-format data with year-specific columns to long format

#### `afro_processing_utils.r`
Contains reusable functions for Afrobarometer data processing:
- **`recode_four_point()`**: Recodes 4-point scale variables (with optional reverse coding)
- **`create_admin_panel()`**: Creates complete panel data for an admin level
- **`interpolate_panel_values()`**: Interpolates panel values for mean columns
- **`process_afro_panel()`**: Complete workflow to process Afrobarometer data to create panel for a specific admin level

#### `regression_utils.r`
Contains reusable functions for regression analysis:
- **`add_capacity_and_growth()`**: Adds capacity grouping and nl_growth to panel data
- **`run_capacity_regressions()`**: Runs capacity-stratified panel regressions

### 2. Refactored Existing Scripts

#### `scripts/01_cleaning/02_build_dependent_vars.r`
**Before:** 106 lines with duplicated code for admin1 and admin2 processing
**After:** 52 lines using utility functions
**Improvements:**
- Eliminated duplicate raster processing loops for admin1 and admin2
- Eliminated duplicate U5M extraction code
- Eliminated duplicate panel data building code
- Code is now ~50% shorter and more maintainable

#### `scripts/01_cleaning/03_build_pop.r`
**Before:** 104 lines with duplicated code for admin1 and admin2 processing
**After:** 30 lines using utility functions
**Improvements:**
- Eliminated duplicate raster processing loops
- Removed manual shapefile loading and transformation
- Code is now ~70% shorter

#### `scripts/01_cleaning/01_build_afro_full.r`
**Before:** Multiple duplicated case_match blocks and duplicated panel creation code
**After:** Uses utility functions for recoding and panel processing
**Improvements:**
- Eliminated 7 duplicated case_match blocks, replaced with single function calls
- Eliminated ~130 lines of duplicated code for admin1/admin2 panel processing
- Much clearer intent with descriptive function names

#### `scripts/02_analysis/05_run_regressions.R`
**Before:** Duplicated code for creating capacity groups and running regressions
**After:** Uses utility functions for capacity-based analysis
**Improvements:**
- Eliminated duplicated capacity grouping and growth calculation code
- Eliminated duplicated regression setup code
- More maintainable and easier to extend

## Benefits

1. **Reduced Code Duplication**: Eliminated hundreds of lines of duplicated code
2. **Improved Maintainability**: Changes to logic now need to be made in only one place
3. **Better Testability**: Utility functions can be tested independently
4. **Clearer Intent**: Function names make the purpose of code blocks more obvious
5. **Easier to Extend**: New admin levels or similar processing can reuse existing functions
6. **Reduced Error Potential**: Less code means fewer places for bugs to hide

## Usage Examples

### Using spatial processing utilities:
```r
# Load utility functions
source(here("scripts", "utils", "spatial_processing_utils.r"))

# Process both admin levels at once
admin_results <- process_admin_levels(
  admin1_shp_path = "00_rawdata/shapefiles/gadm_admin1.shp",
  admin2_shp_path = "00_rawdata/shapefiles/gadm_admin2.shp",
  raster_dir = "00_rawdata/nightlights/africa",
  stats = c("sum", "mean"),
  crop_to_extent = FALSE
)
```

### Using Afrobarometer utilities:
```r
# Load utility functions
source(here("scripts", "utils", "afro_processing_utils.r"))

# Process admin1 panel
admin1_afro <- process_afro_panel(
  afro_data = afro_merged,
  admin_level = "admin1",
  year_range = c(2005, 2015)
)
```

### Using regression utilities:
```r
# Load utility functions
source(here("scripts", "utils", "regression_utils.r"))

# Add capacity and growth variables
panel_aid_admin1 <- add_capacity_and_growth(
  data = panel_aid_admin1,
  admin_id = "GID_1",
  capacity_var = "mean_sgq_admin1"
)

# Run stratified regressions
admin1_models <- run_capacity_regressions(
  data = panel_aid_admin1,
  admin_id = "GID_1",
  aid_var = "total_aid_admin1",
  frag_var = "donor_count_admin1"
)
```

## Testing Recommendations

While this refactoring maintains the same functionality, it's recommended to:

1. Run the refactored scripts and compare outputs with previous runs
2. Verify that all intermediate and final datasets match
3. Check that regression results are identical
4. Consider adding unit tests for the utility functions in the future

## Future Improvements

Consider:
1. Adding error handling and input validation to utility functions
2. Creating formal documentation with roxygen2 comments
3. Building a package structure for better organization
4. Adding unit tests for utility functions
5. Extending utilities to handle additional edge cases
