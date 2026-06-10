# =============================================================================
# HEAVY robustness suite — meant for an HPC high-memory node (request ~128 GB).
# These are the checks that OOM'd on the 5.8 GB workstation:
#   (1) native-resolution (0.1°) cell-specific LINEAR TRENDS de-trended triple
#   (2) Sun & Abraham (2021) heterogeneity-robust event study + ATT at 0.1°
#   (3) Callaway & Sant'Anna (2021) group-time ATT (if `did` installed)
#   (4) wild-cluster bootstrap of the triple (linear asinh-NL analog; 33 clusters)
# Writes everything to 03_output/hpc/. Each block is wrapped so one failure
# doesn't kill the rest.
# =============================================================================
# package bootstrap: install missing packages into a personal library (module R is
# usually read-only). Set R_LIBS_USER in the SLURM script or it defaults to ~/R.
.libloc <- Sys.getenv("R_LIBS_USER", unset = file.path("~", "R", "frag-lib"))
.libloc <- path.expand(.libloc); dir.create(.libloc, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(.libloc, .libPaths()))
for (p in c("data.table", "fixest", "here", "fwildclusterboot", "did")) {
  if (!requireNamespace(p, quietly = TRUE))
    install.packages(p, lib = .libloc, repos = "https://cloud.r-project.org")
}
suppressMessages({
  library(data.table); library(fixest); library(here)
})
setDTthreads(0)                                   # use all allocated cores
fixest::setFixest_nthreads(parallel::detectCores())
outdir <- here("03_output", "hpc"); dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
log <- function(...) cat(format(Sys.time(), "%H:%M:%S"), "-", ..., "\n")

# ---- load + features (native 0.1° spine) ----
log("loading spine")
d <- fread(here("01_panel_data", "wb_grid_spine.csv"))
s1 <- fread(here("01_panel_data", "panel_aid_admin1_fin.csv"), select = c("GID_1","mean_sgq_admin1"))
s1 <- s1[is.finite(mean_sgq_admin1), .(sgq1 = mean(mean_sgq_admin1)), by = GID_1]
d <- merge(d, s1, by = "GID_1", all.x = TRUE)
d[, cy := paste(GID_0, year, sep = "_")]
d[, frag_pre := fifelse(is.finite(frag_pre), frag_pre, 0)]
d[, mature := as.integer(is.finite(event_time) & event_time >= 3)]
d[, early  := as.integer(is.finite(event_time) & event_time >= 0 & event_time < 3)]
d[, cohort := fifelse(is.finite(wb_first_year), wb_first_year, 9999L)]
dd <- d[is.finite(sgq1)]

res <- list()

# (1) native cell-specific linear trends ---------------------------------------
try({
  log("(1) de-trended triple, cell-specific linear trends (cell[year])")
  m <- fepois(NL ~ mature*frag_pre*scale(sgq1) + early | cell[year] + cy,
              cluster = ~GID_0, data = dd)
  res$detrend <- coeftable(m)
  log("    triple =", round(coef(m)["mature:frag_pre:scale(sgq1)"], 4))
  saveRDS(m, file.path(outdir, "detrend_triple.rds"))
})

# (2) Sun-Abraham at native resolution -----------------------------------------
try({
  log("(2) Sun-Abraham event study (native 0.1°)")
  m <- fepois(NL ~ sunab(cohort, year, ref.p = -1) | cell + cy, cluster = ~GID_1, data = d)
  res$sunab_es  <- coeftable(m)
  res$sunab_att <- summary(m, agg = "att")$coeftable
  log("    SA ATT =", round(res$sunab_att[1,1], 4))
  saveRDS(m, file.path(outdir, "sunab.rds"))
})

# (3) Callaway-Sant'Anna (optional) --------------------------------------------
try({
  if (requireNamespace("did", quietly = TRUE)) {
    log("(3) Callaway-Sant'Anna att_gt")
    dcs <- copy(d); dcs[cohort == 9999L, cohort := 0L]
    cs <- did::att_gt(yname = "NL", tname = "year", idname = "cell", gname = "cohort",
                      control_group = "notyettreated", clustervars = "GID_0",
                      data = as.data.frame(dcs), allow_unbalanced_panel = TRUE)
    res$cs <- did::aggte(cs, type = "dynamic")
    saveRDS(res$cs, file.path(outdir, "callaway_santanna.rds"))
    log("    CSA overall ATT =", round(res$cs$overall.att, 4))
  } else log("(3) skipped: install.packages('did')")
})

# (4) wild-cluster bootstrap of the triple (linear asinh-NL analog) ------------
try({
  if (requireNamespace("fwildclusterboot", quietly = TRUE)) {
    log("(4) wild-cluster bootstrap (asinh-NL linear analog, 33 clusters)")
    dd[, reg := factor(cell)][, cyf := factor(cy)][, G0 := factor(GID_0)]
    dd[, asNL := asinh(NL)]
    lm0 <- feols(asNL ~ mature + mature:frag_pre + mature:scale(sgq1) +
                   mature:frag_pre:scale(sgq1) + early | reg + cyf, data = dd)
    bt <- fwildclusterboot::boottest(lm0, clustid = ~G0,
                                     param = "mature:frag_pre:scale(sgq1)", B = 9999)
    res$wcb <- list(coef = coef(lm0)["mature:frag_pre:scale(sgq1)"],
                    p = bt$p_val, ci = bt$conf_int)
    log("    WCB p =", round(bt$p_val, 4), " CI =", paste(round(bt$conf_int, 3), collapse = ", "))
  } else log("(4) skipped: install.packages('fwildclusterboot')")
})

saveRDS(res, file.path(outdir, "hpc_robustness_summary.rds"))
log("DONE -> 03_output/hpc/")
