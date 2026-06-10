if (!requireNamespace("targets", quietly = TRUE)) {
    stop(
        "Package 'targets' is required for this pipeline. ",
        "Install it with install.packages('targets')."
    )
}

library(targets)

tar_option_set(
    packages = c("here", "yaml", "readr", "dplyr", "purrr", "fixest", "tibble")
)

source(here::here("02_scripts", "03_rebuild", "rebuild_utils.R"))

list(
    tar_target(
        config_path,
        here::here("02_scripts", "03_rebuild", "config.yml"),
        format = "file"
    ),
    tar_target(config, load_config(config_path)),
    tar_target(result, run_pipeline(config_path = config_path))
)
