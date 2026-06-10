# =============================================================================
# (A) Is the fragmentation null PRECISE or just imprecise?  -> wild-cluster
#     bootstrap (33 countries) + minimum detectable effect (MDE).
# (B) Anything in U5M (health outcome)?
# (C) Governance quality (mean_sgq) as the MODERATOR -> frag x sgq + capacity split.
#
# Annual files + live shift-share IV (un-bucketed). Linear control-function CFA
# (OLS) throughout so the wild bootstrap applies. FE/cluster as factors (req. by
# fwildclusterboot).
# =============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, here, readr, fixest, fwildclusterboot)
set.seed(1234); dqrng::dqset.seed(1234)
C <- 0.01; B <- 4999

prep <- function(admin) {
  read_csv(here("01_panel_data", paste0("panel_aid_admin", admin, "_fin.csv")),
           show_col_types = FALSE) |>
    mutate(
      reg = factor(.data[[paste0("GID_", admin)]]),
      GID_0 = factor(GID_0),
      lnl = log(mean_nl + C),
      ln_aid = log(.data[[paste0("total_aid_admin", admin)]] + 1),
      frag = .data[[paste0("frag_index_admin", admin)]],
      sgq  = .data[[paste0("mean_sgq_admin", admin)]],
      log_pop = .data[[paste0("ln_pop_admin", admin)]]
    ) |>
    filter(is.finite(ln_aid), is.finite(frag), is.finite(log_pop), is.finite(IV))
}

# OLS control-function model on outcome y; rhs supplied
cfa <- function(d, y, rhs) {
  d <- d |> filter(is.finite(.data[[y]]))
  fs <- feols(ln_aid ~ IV + frag + log_pop | reg + year, cluster = ~GID_0, data = d)
  d$cf_resid <- resid(fs)
  list(m = feols(as.formula(paste0(y, " ~ ", rhs, " + log_pop + cf_resid | reg + year")),
                 cluster = ~GID_0, data = d), d = d)
}

line <- function(label, m, param) {
  bt <- tryCatch(boottest(m, clustid = ~GID_0, param = param, B = B),
                 error = function(e) NULL)
  wp <- if (is.null(bt)) "NA" else sprintf("%.3f", bt$p_val)
  ci <- if (is.null(bt)) "NA" else sprintf("[% .3f,% .3f]", bt$conf_int[1], bt$conf_int[2])
  cat(sprintf("%-30s coef=% .4f  asy.p=%.3f  WCBp=%s  95%%CI=%s  MDE80=%.3f\n",
              label, unname(coef(m)[param]), pvalue(m)[param], wp, ci, 2.8*se(m)[param]))
}

a1 <- prep(1); a2 <- prep(2)
cap2 <- quantile(a2$sgq, 0.75, na.rm = TRUE); cap1 <- quantile(a1$sgq, 0.75, na.rm = TRUE)

cat("\n========== (A) NIGHTLIGHTS: precision of the fragmentation null ==========\n")
nl2 <- cfa(a2, "lnl", "ln_aid + frag")$m
nlx2<- cfa(a2, "lnl", "ln_aid * frag")$m
line("ADM2 lnl frag",       nl2,  "frag")
line("ADM2 lnl aid",        nl2,  "ln_aid")
line("ADM2 lnl aid:frag",   nlx2, "ln_aid:frag")
nl1 <- cfa(a1, "lnl", "ln_aid + frag")$m
line("ADM1 lnl frag",       nl1,  "frag")
cat(sprintf("[outcome SD: ADM2 lnl = %.2f, ADM1 lnl = %.2f]\n", sd(a2$lnl), sd(a1$lnl)))

cat("\n========== (B) UNDER-5 MORTALITY (u5m, ~prob death; lower=better) ==========\n")
u1 <- cfa(a1, "u5m", "ln_aid + frag")$m
u2 <- cfa(a2, "u5m", "ln_aid + frag")$m
line("ADM1 u5m frag",       u1, "frag")
line("ADM1 u5m aid",        u1, "ln_aid")
line("ADM2 u5m frag",       u2, "frag")
line("ADM2 u5m aid",        u2, "ln_aid")
cat(sprintf("[outcome SD: ADM1 u5m = %.4f (mean %.3f), ADM2 u5m = %.4f]\n",
            sd(a1$u5m,na.rm=T), mean(a1$u5m,na.rm=T), sd(a2$u5m,na.rm=T)))

cat("\n========== (C) GOVERNANCE QUALITY as MODERATOR (mean_sgq) ==========\n")
cat("Interaction frag:sgq -> does state capacity change the fragmentation effect?\n\n")
# NL, interaction with continuous governance
nlg <- cfa(a2, "lnl", "frag * sgq + ln_aid")$m
line("ADM2 lnl frag",        nlg, "frag")
line("ADM2 lnl frag:sgq",    nlg, "frag:sgq")
# u5m, interaction
ug <- cfa(a1, "u5m", "frag * sgq + ln_aid")$m
line("ADM1 u5m frag",        ug, "frag")
line("ADM1 u5m frag:sgq",    ug, "frag:sgq")

cat("\n-- Capacity split (High = top 25% of mean_sgq) --\n")
splitrun <- function(d, y, lvl, cut) {
  hi <- cfa(filter(d, sgq >  cut), y, "ln_aid + frag")$m
  lo <- cfa(filter(d, sgq <= cut), y, "ln_aid + frag")$m
  line(paste0(lvl," ",y," HIGH frag"), hi, "frag")
  line(paste0(lvl," ",y," LOW  frag"), lo, "frag")
}
splitrun(a2, "lnl", "ADM2", cap2)
splitrun(a1, "u5m", "ADM1", cap1)

cat("\n========== (D) Expansion / moderator coverage ==========\n")
cat(sprintf("Panel countries: ADM1=%d, ADM2=%d ; raw GODAD has 185.\n",
            nlevels(a1$GID_0), nlevels(a2$GID_0)))
cat(sprintf("Share of rows with non-missing governance (sgq): ADM1=%.0f%%, ADM2=%.0f%%\n",
            100*mean(is.finite(a1$sgq)), 100*mean(is.finite(a2$sgq))))
cat("NOTE: mean_sgq is from Afrobarometer = SSA-only. Expanding beyond Africa\n",
    "adds clusters/power but DROPS the governance moderator.\n")
cat("DONE.\n")
