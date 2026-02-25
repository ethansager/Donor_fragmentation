# GODAD Processing Utility Functions
# Functions for creating and processing GODAD panel data

load_sector_mapping <- function(
    mapping_path = here("02_scripts", "01_cleaning", "Sector_mapping_GODAD.csv")
) {
    read_csv(mapping_path, show_col_types = FALSE) %>%
        janitor::clean_names() %>%
        mutate(code = as.numeric(code))
}

build_aid_type_codes <- function(sector_mapping) {
    sector_mapping %>%
        filter(!is.na(study_group), !is.na(code)) %>%
        distinct(study_group, code) %>%
        group_by(study_group) %>%
        summarise(codes = list(sort(unique(code))), .groups = "drop") %>%
        {
            setNames(.$codes, .$study_group)
        }
}

apply_sector_mapping <- function(
    dat,
    mapping_path = here("02_scripts", "01_cleaning", "Sector_mapping_GODAD.csv")
) {
    sector_mapping <- load_sector_mapping(mapping_path)
    dat <- dat %>%
        left_join(
            sector_mapping %>% select(code, study_group),
            by = c("sector_main" = "code")
        )
    aid_type_codes <- build_aid_type_codes(sector_mapping)
    list(
        dat = dat,
        sector_mapping = sector_mapping,
        aid_type_codes = aid_type_codes
    )
}

build_panels_for_type <- function(
    dat_dt,
    type_name,
    file_suffix,
    period_metadata_tbl = NULL
) {
    # Create HHI for admin2 level
    hhi_results_admin2 <- dat_dt[,
        .(
            total_disb_admin2 = sum(abs(disb_loc_evensplit), na.rm = TRUE),
            total_early_projects = sum(early_impact == 1, na.rm = TRUE),
            total_late_projects = sum(early_impact == 0, na.rm = TRUE)
        ),
        by = .(GID_0, GID_2, paymentyear, donor)
    ]

    hhi_results_admin2 <- hhi_results_admin2[,
        {
            total_aid <- sum(total_disb_admin2, na.rm = TRUE)
            list(
                total_early_admin2 = sum(total_early_projects),
                total_late_admin2 = sum(total_late_projects),
                total_proj_admin2 = sum(
                    total_early_projects + total_late_projects
                ),
                total_aid_admin2 = total_aid,
                donor_count_admin2 = uniqueN(donor),
                hhi_admin2 = sum(
                    (total_disb_admin2 / total_aid)^2,
                    na.rm = TRUE
                ),
                frag_index_admin2 = 1 -
                    sum((total_disb_admin2 / total_aid)^2, na.rm = TRUE),
                frag_1_admin2 = 1 -
                    max(total_disb_admin2 / total_aid, na.rm = TRUE),
                frag_3_admin2 = 1 -
                    sum(
                        head(
                            sort(
                                total_disb_admin2 / total_aid,
                                decreasing = TRUE
                            ),
                            3
                        ),
                        na.rm = TRUE
                    ),
                frag_below10_admin2 = sum(
                    (total_disb_admin2 / total_aid) < 0.10,
                    na.rm = TRUE
                )
            )
        },
        by = .(GID_0, GID_2, paymentyear)
    ]

    # Create HHI for admin1 level
    hhi_results_admin1 <- dat_dt[,
        .(
            total_disb_admin1 = sum(abs(disb_loc_evensplit), na.rm = TRUE),
            total_early_projects = sum(early_impact == 1, na.rm = TRUE),
            total_late_projects = sum(early_impact == 0, na.rm = TRUE)
        ),
        by = .(GID_0, GID_1, paymentyear, donor)
    ]

    hhi_results_admin1 <- hhi_results_admin1[,
        {
            total_aid <- sum(total_disb_admin1, na.rm = TRUE)
            list(
                total_early_admin1 = sum(total_early_projects),
                total_late_admin1 = sum(total_late_projects),
                total_proj_admin1 = sum(
                    total_early_projects + total_late_projects
                ),
                total_aid_admin1 = total_aid,
                donor_count_admin1 = uniqueN(donor),
                hhi_admin1 = sum(
                    (total_disb_admin1 / total_aid)^2,
                    na.rm = TRUE
                ),
                frag_index_admin1 = 1 -
                    sum((total_disb_admin1 / total_aid)^2, na.rm = TRUE),
                frag_1_admin1 = 1 -
                    max(total_disb_admin1 / total_aid, na.rm = TRUE),
                frag_3_admin1 = 1 -
                    sum(
                        head(
                            sort(
                                total_disb_admin1 / total_aid,
                                decreasing = TRUE
                            ),
                            3
                        ),
                        na.rm = TRUE
                    ),
                frag_below10_admin1 = sum(
                    (total_disb_admin1 / total_aid) < 0.10,
                    na.rm = TRUE
                )
            )
        },
        by = .(GID_0, GID_1, paymentyear)
    ]

    # Descriptives region count by admin level (all-data only)
    if (type_name == "all") {
        n_distinct(hhi_results_admin1$GID_1)
        n_distinct(hhi_results_admin2$GID_2)
    }

    dat_no_geom <- copy(dat_dt)
    if ("geometry" %in% names(dat_no_geom)) {
        dat_no_geom[, geometry := NULL]
    }
    dat_no_geom <- as_tibble(dat_no_geom)

    hhi_results_admin1 <- as_tibble(hhi_results_admin1)
    hhi_results_admin2 <- as_tibble(hhi_results_admin2)

    # Step 2: Merge the HHI results back into the main dataset
    panel_aid_admin1 <- dat_no_geom %>%
        filter(!is.na(GID_1)) %>%
        tidylog::inner_join(
            hhi_results_admin1,
            by = c("GID_0", "GID_1", "paymentyear")
        ) %>%
        select(
            GID_0,
            GID_1,
            paymentyear,
            ends_with("_admin1"),
            -contains("total_disb")
        ) %>%
        distinct()

    if (type_name == "all") {
        check <- panel_aid_admin1 %>%
            group_by(GID_0, GID_1, paymentyear) %>%
            summarise(count = n()) %>%
            filter(count > 1)
    }

    panel_aid_admin2 <- dat_no_geom %>%
        filter(!is.na(GID_1)) %>%
        tidylog::left_join(
            hhi_results_admin2,
            by = c("GID_0", "GID_2", "paymentyear")
        ) %>%
        select(
            GID_0,
            GID_1,
            GID_2,
            paymentyear,
            ends_with("_admin2"),
            -contains("total_disb")
        ) %>%
        distinct()

    if (type_name == "all") {
        check <- panel_aid_admin2 %>%
            group_by(GID_0, GID_1, GID_2, paymentyear) %>%
            summarise(count = n()) %>%
            filter(count > 1)

        n_distinct(panel_aid_admin1$GID_1)
        n_distinct(panel_aid_admin2$GID_2)
    }

    # population grid GWP

    panel_aid_admin1 <- panel_aid_admin1 %>%
        left_join(admin1_pop, by = c("GID_1", "paymentyear" = "year")) %>%
        rename(ln_pop_admin1 = ln_pop)

    panel_aid_admin2 <- panel_aid_admin2 %>%
        left_join(admin2_pop, by = c("GID_2", "paymentyear" = "year")) %>%
        rename(ln_pop_admin2 = ln_pop)

    ###### Read in Afro data and create panel to match ########
    panel_aid_admin1 <- panel_aid_admin1 %>%
        dplyr::inner_join(
            admin1_afro,
            by = c("paymentyear" = "year", "GID_0", "GID_1")
        ) %>%
        rename(year = paymentyear)

    panel_aid_admin2 <- panel_aid_admin2 %>%
        dplyr::inner_join(
            admin2_afro,
            by = c("paymentyear" = "year", "GID_0", "GID_2")
        ) %>%
        rename(year = paymentyear)

    ### Read in and compute night lights
    panel_aid_admin1 <- panel_aid_admin1 %>%
        fill(GID_0, .direction = "up") %>%
        left_join(admin1_dep_vars, by = c("year", "GID_0", "GID_1"))

    panel_aid_admin2 <- panel_aid_admin2 %>%
        fill(GID_0, GID_1, .direction = "up") %>%
        left_join(admin2_dep_vars, by = c("year", "GID_0", "GID_1", "GID_2"))

    ### okay  WGI governace effectiveness as a control
    panel_aid_admin1 <- panel_aid_admin1 %>%
        left_join(wgi, by = c("year", "GID_0"))

    panel_aid_admin2 <- panel_aid_admin2 %>%
        left_join(wgi, by = c("year", "GID_0"))

    # Select final data
    panel_aid_admin1_fin <- panel_aid_admin1 %>%
        select(
            GID_0,
            GID_1,
            year,
            starts_with("frag_"),
            starts_with("mean_"),
            starts_with("sgq_status"),
            mean_nl = mean,
            sum_nl = sum,
            u5m,
            # capital_region,
            total_early_admin1,
            total_late_admin1,
            total_proj_admin1,
            total_aid_admin1,
            donor_count_admin1,
            hhi_admin1,
            ln_pop_admin1,
            afro_count,
            wave,
            # distance_to_capital, capital_region, #spei_admin1,
            # nearest_city_dist, urban_share = Urban_share,
            # ge_pct
        )

    panel_aid_admin2_fin <- panel_aid_admin2 %>%
        select(
            GID_0,
            GID_1,
            GID_2,
            year,
            starts_with("frag_"),
            starts_with("mean_"),
            starts_with("sgq_status"),
            mean_nl = mean,
            sum_nl = sum,
            u5m,
            # capital_region,
            total_early_admin2,
            total_late_admin2,
            total_proj_admin2,
            total_aid_admin2,
            donor_count_admin2,
            hhi_admin2,
            ln_pop_admin2,
            afro_count,
            wave,
            # distance_to_capital, capital_region, #spei_admin2,
            # nearest_city_dist, urban_share = Urban_share, ge_pct
        )

    if (!is.null(period_metadata_tbl)) {
        panel_aid_admin1_fin <- panel_aid_admin1_fin %>%
            left_join(period_metadata_tbl, by = "year")
        panel_aid_admin2_fin <- panel_aid_admin2_fin %>%
            left_join(period_metadata_tbl, by = "year")
    }

    # Save outputs for this type
    write_csv(
        panel_aid_admin1_fin,
        paste0("01_panel_data/panel_aid_admin1", file_suffix, ".csv")
    )
    write_csv(
        panel_aid_admin2_fin,
        paste0("01_panel_data/panel_aid_admin2", file_suffix, ".csv")
    )

    invisible(
        list(
            panel_aid_admin1_fin = panel_aid_admin1_fin,
            panel_aid_admin2_fin = panel_aid_admin2_fin
        )
    )
}
