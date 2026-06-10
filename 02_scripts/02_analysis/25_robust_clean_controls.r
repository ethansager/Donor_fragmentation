# Memory-light heterogeneity-robust check (5.8GB box; sunab OOMs).
# Event study with NEVER-TREATED cells as the clean control group -> removes the
# "already-treated-as-control" forbidden comparisons that made the TWFE `mature`
# dummy negative. PPML (70% zeros), cell + country-year FE, Regan bltcfix 2000-2020.
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, fixest, here)

d <- fread(here("01_panel_data","wb_grid_spine.csv"))
s1 <- fread(here("01_panel_data","panel_aid_admin1_fin.csv"), select=c("GID_1","mean_sgq_admin1"))
s1 <- s1[is.finite(mean_sgq_admin1), .(sgq1=mean(mean_sgq_admin1)), by=GID_1]
d <- merge(d, s1, by="GID_1", all.x=TRUE)
d[, cy := paste(GID_0, year, sep="_")]
d[, frag_pre := fifelse(is.finite(frag_pre), frag_pre, 0)]

# event time; never-treated stay NA -> serve as pure controls (all dummies 0)
d[, et := event_time]
d[is.finite(et) & et < -8, et := -8L][is.finite(et) & et > 10, et := 10L]
d[, ever := as.integer(is.finite(wb_first_year))]
# clean post indicator for ATT (treated post vs never-treated)
d[, postT := as.integer(is.finite(event_time) & event_time>=0)]
d[, matureT := as.integer(is.finite(event_time) & event_time>=3)]

# (A) clean event study (never-treated controls)
m_es <- fepois(NL ~ i(et, ref=-1) | cell + cy, cluster=~GID_1, data=d)
cat("=== Event study, never-treated controls (PPML, Regan, 2000-2020) ===\n")
print(round(coeftable(m_es)[,1:2],4))

# (B) clean ATT: treated cells (post) vs never-treated
m_att <- fepois(NL ~ matureT | cell + cy, cluster=~GID_0, data=d)
cat("\n=== clean ATT (WB mature vs never-treated), cluster country ===\n")
print(coeftable(m_att)["matureT",])

# (C) moderation: clean ATT by capacity x fragmentation subgroup
d[, cap := fifelse(sgq1 > median(sgq1, na.rm=TRUE), "Hi", "Lo")]
d[, frg := fifelse(frag_pre > median(frag_pre[frag_pre>0], na.rm=TRUE), "Hi", "Lo")]
cat("\n=== clean mature ATT by capacity x fragmentation ===\n")
dd <- d[is.finite(sgq1)]
for(cc in c("Hi","Lo")) for(ff in c("Lo","Hi")){
  sub <- dd[cap==cc & frg==ff]
  m <- fepois(NL ~ matureT | cell + cy, cluster=~GID_0, data=sub)
  a <- coeftable(m)["matureT",]
  cat(sprintf("  cap=%s frag=%s :  ATT=% .4f  p=%.4f  (cells=%d)\n", cc, ff, a[1], a[4], uniqueN(sub$cell)))
}

# (D) triple interaction, clean controls (continuous capacity)
m_tri <- fepois(NL ~ matureT + matureT:frag_pre + matureT:scale(sgq1) + matureT:frag_pre:scale(sgq1)
                | cell + cy, cluster=~GID_0, data=dd)
cat("\n=== triple (clean controls): matureT x frag x capacity ===\n")
print(round(coeftable(m_tri)[grep("matureT",rownames(coeftable(m_tri))),],4))
saveRDS(list(es=m_es, att=m_att, tri=m_tri), here("03_output","wb_robust_clean.rds"))
cat("\nDONE\n")
