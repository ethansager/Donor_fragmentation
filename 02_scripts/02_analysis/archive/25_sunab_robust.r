# Heterogeneity-robust staggered estimator: Sun & Abraham (2021) via fixest sunab(),
# inside PPML (outcome 70% zeros). Fixes the forbidden-comparisons bias that made the
# TWFE `mature` dummy negative. Then moderation: robust post-ATT by capacity x fragmentation.
# Regan bltcfix spine, 2000-2020.
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, fixest, here)

d0 <- fread(here("01_panel_data","wb_grid_spine.csv"))
# collapse 0.1deg -> 0.25deg (~28km, WB catchment scale) to make sunab tractable
d0[, `:=`(gx = floor(lon/0.25), gy = floor(lat/0.25))]
d0[, cell25 := .GRP, by=.(gx,gy)]
d <- d0[, .(NL = mean(NL, na.rm=TRUE),
            wb_active = max(wb_active),
            wb_disb = sum(wb_disb, na.rm=TRUE),
            wb_first_year = as.double(suppressWarnings(min(wb_first_year, na.rm=TRUE))),
            frag_pre = mean(frag_pre, na.rm=TRUE),
            GID_0 = GID_0[1], GID_1 = GID_1[1]),
        by=.(cell25, year)]
d[!is.finite(wb_first_year), wb_first_year := NA_integer_]
d[, cell := cell25]
s1 <- fread(here("01_panel_data","panel_aid_admin1_fin.csv"), select=c("GID_1","mean_sgq_admin1"))
s1 <- s1[is.finite(mean_sgq_admin1), .(sgq1=mean(mean_sgq_admin1)), by=GID_1]
d <- merge(d, s1, by="GID_1", all.x=TRUE)
d[, cy := paste(GID_0, year, sep="_")]
cat("0.25deg cells:", uniqueN(d$cell), " rows:", nrow(d), "\n")
d[, frag_pre := fifelse(is.finite(frag_pre), frag_pre, 0)]
d[, cohort := fifelse(is.finite(wb_first_year), wb_first_year, 9999L)]   # 9999 = never-treated
d <- d[cohort==9999 | cohort>=2000]                                       # drop pre-2000 cohorts (no clean ref)

# ---- (A) robust event study + overall ATT (Sun-Abraham, PPML) ----
m_sa <- fepois(NL ~ sunab(cohort, year, ref.p = -1)
               | cell + cy, cluster = ~GID_1, data = d)
cat("=== Sun-Abraham event study (robust), PPML ===\n")
print(round(coeftable(m_sa)[,1:2], 4))
cat("\n=== overall post-treatment ATT (Sun-Abraham aggregate) ===\n")
print(summary(m_sa, agg = "att")$coeftable)

# ---- (B) capacity x fragmentation moderation: robust post-ATT per subgroup ----
d[, cap := fifelse(sgq1 > median(sgq1, na.rm=TRUE), "HiCap", "LoCap")]
d[, frg := fifelse(frag_pre > median(frag_pre[frag_pre>0], na.rm=TRUE), "HiFrag", "LoFrag")]
subatt <- function(sub, lab){
  m <- fepois(NL ~ sunab(cohort, year, ref.p=-1)
              | cell + cy, cluster=~GID_1, data = sub)
  a <- summary(m, agg="att")$coeftable
  cat(sprintf("%-18s ATT=% .4f  SE=%.4f  p=%.4f  (n=%d cells)\n",
              lab, a[1,1], a[1,2], a[1,4], uniqueN(sub$cell)))
}
cat("\n=== robust post-ATT of WB aid, by capacity x fragmentation ===\n")
dd <- d[is.finite(sgq1)]
subatt(dd[cap=="HiCap" & frg=="LoFrag"], "HiCap x LoFrag")
subatt(dd[cap=="HiCap" & frg=="HiFrag"], "HiCap x HiFrag")
subatt(dd[cap=="LoCap" & frg=="LoFrag"], "LoCap x LoFrag")
subatt(dd[cap=="LoCap" & frg=="HiFrag"], "LoCap x HiFrag")   # <- predicted weakest
cat("\nHypothesis: fragmentation cuts the WB ATT most in LoCap x HiFrag.\n")
saveRDS(m_sa, here("03_output","wb_sunab.rds"))
cat("\nDONE\n")
