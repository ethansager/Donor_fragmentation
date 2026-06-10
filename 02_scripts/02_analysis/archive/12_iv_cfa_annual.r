# =============================================================================
# IV / Control-Function on the ANNUAL panel, replicating the Ahmed (2016) /
# Dreher & Langlotz (2020) shift-share instrument used in Bomprezzi, Dreher,
# Fuchs et al. (2025, "Wedded to Prosperity", Appendix F).
#
#   IV_it = sum_j  pbar_ji * FRAC_jt      (share fixed, shift = annual govt frac)
#
# The shift FRAC_jt is annual, so the instrument has WITHIN-region time variation
# and is compatible with region + year (and country-year) fixed effects. This is
# why it works here but died in the 5-year-bucketed build (which averaged out the
# annual shift). The annual IV already lives in panel_aid_admin{1,2}_fin.csv.
#
# Specs per admin level:
#   (1) First stage strength (KP / F) under region + year FE
#   (2) 2SLS, linear log(lights+0.01)  [Dreher replication form]
#   (3) Poisson control function (PPML + first-stage residual)  [fixes log(y+c)]
#   (4) Aid x Fragmentation: does fragmentation reduce aid effectiveness?
# =============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, here, readr, fixest)

C <- 0.01

prep <- function(admin) {
  uid <- paste0("GID_", admin)
  frag_v <- paste0("frag_index_admin", admin)
  aid_v  <- paste0("total_aid_admin", admin)
  pop_v  <- paste0("ln_pop_admin", admin)
  read_csv(here("01_panel_data", paste0("panel_aid_admin", admin, "_fin.csv")),
           show_col_types = FALSE) |>
    mutate(
      reg     = .data[[uid]],
      cy      = paste(GID_0, year, sep = "_"),
      lnl     = log(mean_nl + C),
      ln_aid  = log(.data[[aid_v]] + 1),        # Dreher: log(aid + 1)
      frag    = .data[[frag_v]],
      log_pop = .data[[pop_v]]
    ) |>
    filter(is.finite(lnl), is.finite(ln_aid), is.finite(frag),
           is.finite(log_pop), is.finite(IV))
}

run_admin <- function(admin) {
  d <- prep(admin)
  cat(sprintf("\n############## ADMIN%d  (n=%d, regions=%d, countries=%d) ##############\n",
              admin, nrow(d), dplyr::n_distinct(d$reg), dplyr::n_distinct(d$GID_0)))

  # (1)+(2) 2SLS, region + year FE
  iv_ry <- feols(lnl ~ frag + log_pop | reg + year | ln_aid ~ IV,
                 cluster = ~GID_0, data = d)
  # 2SLS with the more conservative country-year FE (Dreher Table F.1 form)
  iv_cy <- feols(lnl ~ frag + log_pop | reg + cy | ln_aid ~ IV,
                 cluster = ~GID_0, data = d)

  cat("\n--- First-stage strength (IV on aid) ---\n")
  print(fitstat(iv_ry, c("ivf", "ivwald")))
  print(fitstat(iv_cy, c("ivf", "ivwald")))

  cat("\n--- 2SLS, log(lights+0.01) ---\n")
  etable(iv_ry, iv_cy, dict = c(fit_ln_aid = "Aid (IV)", frag = "Fragmentation",
                                log_pop = "Log Pop"),
         headers = c("region+year FE", "region+country-year FE"),
         fitstat = ~ n + ivf)

  # (3) Poisson control function: PPML with first-stage residual
  fs <- feols(ln_aid ~ IV + frag + log_pop | reg + year, cluster = ~GID_0, data = d)
  d$cf_resid <- resid(fs)
  pcf <- fepois(mean_nl ~ ln_aid + frag + log_pop + cf_resid | reg + year,
                cluster = ~GID_0, data = d)
  cat("\n--- Poisson control function (handles zeros; coef = semi-elasticity) ---\n")
  print(coeftable(pcf)[c("ln_aid", "frag"), , drop = FALSE])

  # (4) Aid x Fragmentation (CFA): is aid less effective where donors fragmented?
  axf <- fepois(mean_nl ~ ln_aid * frag + log_pop + cf_resid | reg + year,
                cluster = ~GID_0, data = d)
  cat("\n--- Aid x Fragmentation (Poisson CF) ---\n")
  print(coeftable(axf)[c("ln_aid", "frag", "ln_aid:frag"), , drop = FALSE])

  invisible(list(iv_ry = iv_ry, iv_cy = iv_cy, pcf = pcf, axf = axf))
}

r1 <- run_admin(1)
r2 <- run_admin(2)
cat("\nDONE.\n")
