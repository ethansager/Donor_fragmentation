`%||%` <- function(x, y) {
    if (is.null(x)) y else x
}

load_config <- function(path) {
    cfg <- yaml::read_yaml(path)
    defaults <- list(
        tag = "_5year_1995_2015",
        sample_year_min = 2005L,
        sample_year_max = 2015L,
        capacity_quantile = 0.75,
        output_dir = "03_output/rebuild"
    )
    utils::modifyList(defaults, cfg)
}

norm_nl <- function(d) {
    if (!"mean_nl" %in% names(d) && "mean" %in% names(d)) {
        d <- dplyr::rename(d, mean_nl = mean)
    }
    d
}

build_vars <- function(data, uid, aid_var, frag_var, pop_var) {
    data |>
        dplyr::arrange(.data[[uid]], year) |>
        dplyr::group_by(.data[[uid]]) |>
        dplyr::mutate(
            lag_mean_nl = dplyr::lag(mean_nl, 1L),
            lag2_mean_nl = dplyr::lag(mean_nl, 2L),
            lag_frag = dplyr::lag(.data[[frag_var]], 1L),
            lag_log_pop = dplyr::lag(.data[[pop_var]], 1L),
            lag_total_aid = log(dplyr::lag(.data[[aid_var]], 1L) + 0.01),
            total_aid_raw = dplyr::lag(.data[[aid_var]], 1L) / 1e6,
            log_lag_nl = log(dplyr::lag(mean_nl, 1L) + 0.01),
            nl_growth = dplyr::case_when(
                is.na(lag_mean_nl) ~ NA_real_,
                TRUE ~ ((log(mean_nl + 0.01) - log(lag_mean_nl + 0.01)) / 5) *
                    100
            ),
            pre_nl_growth = dplyr::case_when(
                is.na(lag2_mean_nl) ~ NA_real_,
                TRUE ~ ((log(lag_mean_nl + 0.01) - log(lag2_mean_nl + 0.01)) /
                    5) *
                    100
            )
        ) |>
        dplyr::ungroup()
}

to_long_diff <- function(d, uid, year_min, year_max) {
    d |>
        dplyr::filter(year >= year_min, year <= year_max) |>
        dplyr::group_by(.data[[uid]]) |>
        dplyr::filter(year == max(year, na.rm = TRUE)) |>
        dplyr::ungroup()
}

split_capacity <- function(d, capacity_var, quantile_cut = 0.75) {
    cut <- stats::quantile(d[[capacity_var]], quantile_cut, na.rm = TRUE)
    list(
        high = dplyr::filter(d, .data[[capacity_var]] > cut),
        low = dplyr::filter(d, .data[[capacity_var]] <= cut),
        cut = cut
    )
}

est_ols <- function(d, y) {
    fixest::feols(
        stats::as.formula(
            paste0(y, " ~ lag_frag + lag_total_aid + lag_log_pop + log_lag_nl")
        ),
        cluster = ~GID_0,
        data = dplyr::filter(d, is.finite(.data[[y]]), is.finite(log_lag_nl))
    )
}

est_fe <- function(d, y, fe = "GID_0") {
    fixest::feols(
        stats::as.formula(
            paste0(
                y,
                " ~ lag_frag + lag_total_aid + lag_log_pop + log_lag_nl | ",
                fe
            )
        ),
        cluster = ~GID_0,
        data = dplyr::filter(d, is.finite(.data[[y]]), is.finite(log_lag_nl))
    )
}

est_2sls <- function(d, y, fe = "GID_0") {
    fixest::feols(
        stats::as.formula(
            paste0(
                y,
                " ~ lag_frag + lag_log_pop + log_lag_nl | ",
                fe,
                " | lag_total_aid ~ IV_lag"
            )
        ),
        cluster = ~GID_0,
        data = dplyr::filter(
            d,
            is.finite(.data[[y]]),
            is.finite(lag_frag),
            is.finite(log_lag_nl),
            is.finite(lag_total_aid),
            is.finite(IV_lag)
        )
    )
}

est_cfa <- function(d, y, fe = "GID_0") {
    s1_data <- dplyr::filter(
        d,
        is.finite(total_aid_raw),
        is.finite(IV_lag),
        is.finite(lag_log_pop),
        is.finite(log_lag_nl)
    )
    if (nrow(s1_data) < 30) {
        return(NULL)
    }

    s1 <- fixest::feols(
        stats::as.formula(
            paste0(
                "total_aid_raw ~ IV_lag + lag_log_pop + lag_frag + log_lag_nl | ",
                fe
            )
        ),
        cluster = ~GID_0,
        data = s1_data
    )

    s2_data <- dplyr::mutate(s1_data, cfa_resid = stats::residuals(s1)) |>
        dplyr::filter(
            is.finite(.data[[y]]),
            is.finite(lag_frag),
            is.finite(lag_total_aid),
            is.finite(cfa_resid)
        )

    fixest::feols(
        stats::as.formula(
            paste0(
                y,
                " ~ lag_frag + lag_total_aid + lag_log_pop + log_lag_nl + cfa_resid | ",
                fe
            )
        ),
        cluster = ~GID_0,
        data = s2_data
    )
}

safe_fitstat <- function(model, stat_name) {
    if (is.null(model)) {
        return(NA_real_)
    }
    value <- tryCatch(
        fixest::fitstat(model, stat_name),
        error = function(e) NA_real_
    )
    suppressWarnings(as.numeric(unlist(value)[1]))
}

extract_term <- function(
    model,
    term,
    model_id,
    sample,
    design,
    outcome,
    scale_iqr = NA_real_
) {
    if (is.null(model)) {
        return(tibble::tibble())
    }

    ct <- as.data.frame(fixest::coeftable(model))
    if (!term %in% rownames(ct)) {
        return(tibble::tibble())
    }

    row <- ct[term, , drop = FALSE]
    estimate <- as.numeric(row[1, 1])
    std_error <- as.numeric(row[1, 2])
    ci90_lo <- estimate - (1.645 * std_error)
    ci90_hi <- estimate + (1.645 * std_error)
    scaled_effect <- if (is.finite(scale_iqr)) estimate * scale_iqr else NA_real_

    tibble::tibble(
        model_id = model_id,
        sample = sample,
        design = design,
        outcome = outcome,
        term = term,
        estimate = estimate,
        std_error = std_error,
        statistic = as.numeric(row[1, 3]),
        p_value = as.numeric(row[1, 4]),
        ci90_lo = ci90_lo,
        ci90_hi = ci90_hi,
        effect_iqr = scaled_effect,
        n_obs = as.numeric(stats::nobs(model))
    )
}

panel_audit <- function(panel, unit_id, frag_var, label) {
    n_obs <- nrow(panel)
    n_units <- dplyr::n_distinct(panel[[unit_id]])
    n_countries <- dplyr::n_distinct(panel$GID_0)
    year_min <- min(panel$year, na.rm = TRUE)
    year_max <- max(panel$year, na.rm = TRUE)
    frag_missing_share <- mean(!is.finite(panel[[frag_var]]))
    nl_growth_missing_share <- mean(!is.finite(panel$nl_growth))
    iv_lag_missing_share <- mean(!is.finite(panel$IV_lag))

    tibble::tibble(
        panel = label,
        n_obs = n_obs,
        n_units = n_units,
        n_countries = n_countries,
        year_min = year_min,
        year_max = year_max,
        frag_missing_share = frag_missing_share,
        nl_growth_missing_share = nl_growth_missing_share,
        iv_lag_missing_share = iv_lag_missing_share
    )
}

missingness_audit <- function(panel, vars, label) {
    purrr::map_dfr(vars, function(v) {
        missing_share <- if (v %in% names(panel)) {
            mean(!is.finite(panel[[v]]))
        } else {
            NA_real_
        }
        tibble::tibble(
            panel = label,
            variable = v,
            missing_share = missing_share
        )
    })
}

estimate_confirmatory <- function(panel_a1, panel_a2, cfg) {
    iqr_frag_a1 <- stats::IQR(panel_a1$lag_frag, na.rm = TRUE)

    cap_a1 <- split_capacity(
        panel_a1,
        cfg$admin1$capacity_var,
        cfg$capacity_quantile
    )
    cap_a2 <- split_capacity(
        panel_a2,
        cfg$admin2$capacity_var,
        cfg$capacity_quantile
    )

    models <- list(
        main_ols = est_ols(panel_a1, "nl_growth"),
        main_country_fe = est_fe(panel_a1, "nl_growth"),
        main_2sls = est_2sls(panel_a1, "nl_growth"),
        cfa_high_cap = est_cfa(cap_a1$high, "nl_growth"),
        cfa_low_cap = est_cfa(cap_a1$low, "nl_growth"),
        admin2_high_fe = est_fe(cap_a2$high, "nl_growth"),
        admin2_low_fe = est_fe(cap_a2$low, "nl_growth"),
        u5m_ols = est_ols(panel_a1, "u5m"),
        u5m_country_fe = est_fe(panel_a1, "u5m"),
        u5m_cfa = est_cfa(panel_a1, "u5m")
    )

    s1_data <- dplyr::filter(
        panel_a1,
        is.finite(total_aid_raw),
        is.finite(IV_lag),
        is.finite(lag_frag),
        is.finite(lag_log_pop),
        is.finite(log_lag_nl)
    )
    first_stage <- fixest::feols(
        total_aid_raw ~ IV_lag + lag_frag + lag_log_pop + log_lag_nl | GID_0,
        cluster = ~GID_0,
        data = s1_data
    )
    reduced_form <- fixest::feols(
        nl_growth ~ IV_lag + lag_frag + lag_log_pop + log_lag_nl | GID_0,
        cluster = ~GID_0,
        data = dplyr::filter(s1_data, is.finite(nl_growth))
    )
    placebo <- fixest::feols(
        pre_nl_growth ~ IV_lag + lag_frag + lag_log_pop + log_lag_nl | GID_0,
        cluster = ~GID_0,
        data = dplyr::filter(s1_data, is.finite(pre_nl_growth))
    )

    coefficients <- dplyr::bind_rows(
        extract_term(
            models$main_ols,
            "lag_frag",
            "main_ols",
            "Admin1",
            "OLS",
            "nl_growth",
            iqr_frag_a1
        ),
        extract_term(
            models$main_country_fe,
            "lag_frag",
            "main_country_fe",
            "Admin1",
            "Country FE",
            "nl_growth",
            iqr_frag_a1
        ),
        extract_term(
            models$main_2sls,
            "lag_frag",
            "main_2sls",
            "Admin1",
            "2SLS",
            "nl_growth",
            iqr_frag_a1
        ),
        extract_term(
            models$cfa_high_cap,
            "lag_frag",
            "cfa_high_cap",
            "Admin1 High Cap",
            "CFA",
            "nl_growth",
            iqr_frag_a1
        ),
        extract_term(
            models$cfa_low_cap,
            "lag_frag",
            "cfa_low_cap",
            "Admin1 Low Cap",
            "CFA",
            "nl_growth",
            iqr_frag_a1
        ),
        extract_term(
            models$admin2_high_fe,
            "lag_frag",
            "admin2_high_fe",
            "Admin2 High Cap",
            "Country FE",
            "nl_growth",
            iqr_frag_a1
        ),
        extract_term(
            models$admin2_low_fe,
            "lag_frag",
            "admin2_low_fe",
            "Admin2 Low Cap",
            "Country FE",
            "nl_growth",
            iqr_frag_a1
        ),
        extract_term(
            models$u5m_ols,
            "lag_frag",
            "u5m_ols",
            "Admin1",
            "OLS",
            "u5m",
            iqr_frag_a1
        ),
        extract_term(
            models$u5m_country_fe,
            "lag_frag",
            "u5m_country_fe",
            "Admin1",
            "Country FE",
            "u5m",
            iqr_frag_a1
        ),
        extract_term(
            models$u5m_cfa,
            "lag_frag",
            "u5m_cfa",
            "Admin1",
            "CFA",
            "u5m",
            iqr_frag_a1
        )
    ) |>
        dplyr::mutate(family = "confirmatory")

    diagnostics <- tibble::tibble(
        metric = c(
            "iv_fstat_2sls_table1",
            "first_stage_f_ols_tableA1_col1",
            "first_stage_n",
            "reduced_form_n",
            "placebo_n"
        ),
        value = c(
            safe_fitstat(models$main_2sls, "ivf"),
            safe_fitstat(first_stage, "f"),
            stats::nobs(first_stage),
            stats::nobs(reduced_form),
            stats::nobs(placebo)
        )
    )

    sample_audit <- dplyr::bind_rows(
        panel_audit(panel_a1, cfg$admin1$unit_id, "lag_frag", "admin1_long_diff"),
        panel_audit(panel_a2, cfg$admin2$unit_id, "lag_frag", "admin2_long_diff")
    )

    missing_audit <- dplyr::bind_rows(
        missingness_audit(
            panel_a1,
            c("lag_frag", "lag_total_aid", "total_aid_raw", "IV_lag", "nl_growth", "u5m"),
            "admin1_long_diff"
        ),
        missingness_audit(
            panel_a2,
            c("lag_frag", "lag_total_aid", "total_aid_raw", "IV_lag", "nl_growth"),
            "admin2_long_diff"
        )
    )

    list(
        models = c(models, list(first_stage = first_stage,
                                reduced_form = reduced_form,
                                placebo = placebo)),
        coefficients = coefficients,
        diagnostics = diagnostics,
        sample_audit = sample_audit,
        missing_audit = missing_audit
    )
}

estimate_sector_exploratory <- function(cfg) {
    sector_tbl <- purrr::imap_dfr(cfg$sector_files, function(file_name, sector) {
        path <- here::here("01_panel_data", file_name)
        if (!file.exists(path)) {
            return(tibble::tibble())
        }

        raw <- readr::read_csv(path, show_col_types = FALSE) |> norm_nl()
        panel <- build_vars(
            raw,
            cfg$admin1$unit_id,
            cfg$admin1$aid_var,
            cfg$admin1$frag_var,
            cfg$admin1$pop_var
        ) |>
            to_long_diff(
                cfg$admin1$unit_id,
                cfg$sample_year_min,
                cfg$sample_year_max
            )

        model <- est_fe(panel, "nl_growth")
        iqr_frag <- stats::IQR(panel$lag_frag, na.rm = TRUE)
        extract_term(
            model,
            "lag_frag",
            paste0("sector_", sector),
            "Admin1",
            "Country FE",
            "nl_growth",
            iqr_frag
        ) |>
            dplyr::mutate(sector = sector)
    })

    if (nrow(sector_tbl) == 0) {
        return(sector_tbl)
    }

    sector_tbl |>
        dplyr::mutate(
            p_value_bh = stats::p.adjust(p_value, method = "BH"),
            family = "exploratory_sector"
        )
}

write_outputs <- function(
    output_dir,
    confirmatory,
    diagnostics,
    sector,
    models,
    cfg,
    sample_audit,
    missing_audit
) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

    coef_tbl <- dplyr::bind_rows(confirmatory, sector)
    readr::write_csv(coef_tbl, file.path(output_dir, "fragmentation_coefficients.csv"))
    readr::write_csv(diagnostics, file.path(output_dir, "diagnostics.csv"))
    readr::write_csv(sample_audit, file.path(output_dir, "sample_audit.csv"))
    readr::write_csv(missing_audit, file.path(output_dir, "missingness_audit.csv"))
    saveRDS(models, file.path(output_dir, "models.rds"))
    yaml::write_yaml(cfg, file.path(output_dir, "run_config.yml"))

    manifest <- c(
        paste0("timestamp_utc: ", format(Sys.time(), tz = "UTC", usetz = TRUE)),
        paste0("n_confirmatory_rows: ", nrow(confirmatory)),
        paste0("n_sector_rows: ", nrow(sector)),
        paste0("output_dir: ", normalizePath(output_dir, mustWork = FALSE))
    )
    writeLines(manifest, file.path(output_dir, "run_manifest.txt"))
}

run_pipeline <- function(config_path = here::here("02_scripts", "03_rebuild", "config.yml")) {
    cfg <- load_config(config_path)

    raw_a1 <- readr::read_csv(
        here::here("01_panel_data", cfg$admin1$file),
        show_col_types = FALSE
    ) |> norm_nl()
    raw_a2 <- readr::read_csv(
        here::here("01_panel_data", cfg$admin2$file),
        show_col_types = FALSE
    ) |> norm_nl()

    panel_a1 <- build_vars(
        raw_a1,
        cfg$admin1$unit_id,
        cfg$admin1$aid_var,
        cfg$admin1$frag_var,
        cfg$admin1$pop_var
    ) |>
        to_long_diff(
            cfg$admin1$unit_id,
            cfg$sample_year_min,
            cfg$sample_year_max
        )
    panel_a2 <- build_vars(
        raw_a2,
        cfg$admin2$unit_id,
        cfg$admin2$aid_var,
        cfg$admin2$frag_var,
        cfg$admin2$pop_var
    ) |>
        to_long_diff(
            cfg$admin2$unit_id,
            cfg$sample_year_min,
            cfg$sample_year_max
        )

    confirmatory <- estimate_confirmatory(panel_a1, panel_a2, cfg)
    sector <- estimate_sector_exploratory(cfg)

    out_dir <- here::here(cfg$output_dir)
    write_outputs(
        output_dir = out_dir,
        confirmatory = confirmatory$coefficients,
        diagnostics = confirmatory$diagnostics,
        sector = sector,
        models = confirmatory$models,
        cfg = cfg,
        sample_audit = confirmatory$sample_audit,
        missing_audit = confirmatory$missing_audit
    )

    list(
        output_dir = out_dir,
        coefficients = confirmatory$coefficients,
        diagnostics = confirmatory$diagnostics,
        sector = sector
    )
}
