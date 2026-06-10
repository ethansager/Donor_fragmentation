# =============================================================================
# Design v3 headline: does donor fragmentation undermine WB aid effectiveness,
# and does state capacity buffer it?
#   - Single-sensor DMSP window 2005-2013 (the harmonized DMSP-VIIRS splice is
#     NOT comparable within-cell; see 22_wb_event_study.r sensor diagnostic).
#   - PPML (70% zero nightlights), cell + country-year FE, cluster GID_1.
#   - Dynamic treatment (effect matures by t+3); binary `post` dilutes it.
# Input: 01_panel_data/wb_grid_spine.csv  (built by 06_build_wb_grid_spine.r)
# =============================================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, fixest, here)

d <- fread(here("01_panel_data","wb_grid_spine.csv"))

# district governance (Afrobarometer) moderator
sg <- fread(here("01_panel_data","panel_aid_admin2_fin.csv"), select=c("GID_2","mean_sgq_admin2"))
sg <- sg[is.finite(mean_sgq_admin2), .(sgq=mean(mean_sgq_admin2)), by=GID_2]
d <- merge(d, sg, by="GID_2", all.x=TRUE)

# clean single-sensor window + features
d <- d[year>=2005 & year<=2013]
d[, cy := paste(GID_0, year, sep="_")]
d[, frag_pre := fifelse(is.finite(frag_pre), frag_pre, 0)]   # no pre non-WB aid => frag 0
d[, et := event_time]
d[, early  := as.integer(is.finite(et) & et>=0 & et<3)]      # 0-2 yrs since WB arrival
d[, mature := as.integer(is.finite(et) & et>=3)]             # 3+ yrs (effect matured)
d[, ln_wbdisb := log(fifelse(wb_disb>0, wb_disb, 0)+1)]      # intensity
d[, frag_t := cut(frag_pre, c(-1,1e-4,0.3,1.1), labels=c("none","low","high"))]
d[, hi_gov := as.integer(sgq > median(sgq, na.rm=TRUE))]

# (0) event study (pre-trend check) -- windowed
d[, etb := et]; d[is.finite(etb) & etb< -6, etb:=-6L][is.finite(etb) & etb>7, etb:=7L]
m_es  <- fepois(NL ~ i(etb, ref=-1) | cell + cy, cluster=~GID_1, data=d[is.finite(et)])

# (1) dynamic WB effect + fragmentation moderation
m_dyn <- fepois(NL ~ early + mature | cell + cy, cluster=~GID_1, data=d)
m_lr  <- fepois(NL ~ early + mature + mature:frag_pre | cell + cy, cluster=~GID_1, data=d)
m_trc <- fepois(NL ~ early + mature:frag_t | cell + cy, cluster=~GID_1, data=d)
m_int <- fepois(NL ~ ln_wbdisb + ln_wbdisb:frag_pre | cell + cy, cluster=~GID_1, data=d)

# (2) governance triple interaction (time-invariant mains absorbed by cell FE)
m_tri <- fepois(NL ~ mature*frag_pre*hi_gov + early | cell + cy, cluster=~GID_1, data=d[is.finite(sgq)])

etable(m_dyn, m_lr, m_trc, m_int, m_tri,
       keep = c("early","mature","ln_wbdisb","frag"),
       dict = c(early="WB 0-2y", mature="WB 3y+", ln_wbdisb="WB disb (log)",
                frag_pre="Frag", hi_gov="High capacity",
                "mature:frag_pre"="WB3y+ x Frag", "ln_wbdisb:frag_pre"="WBdisb x Frag",
                "mature:frag_tlow"="WB3y+ (low frag)", "mature:frag_thigh"="WB3y+ (high frag)",
                "mature:frag_pre:hi_gov"="WB3y+ x Frag x Capacity"),
       fitstat=~n, tex=TRUE, replace=TRUE, style.tex=style.tex("aer"),
       title="Donor fragmentation and the local return to World Bank aid (DMSP 2005-2013, PPML).",
       file=here("03_output","tabs","wb_fragmentation_moderation.tex"))

saveRDS(list(es=m_es,dyn=m_dyn,lr=m_lr,trc=m_trc,int=m_int,tri=m_tri),
        here("03_output","wb_dynamic_models.rds"))
cat("event-study (DMSP) coefs:\n"); print(round(coeftable(m_es)[,1:2],4))
cat("\nmoderation table -> 03_output/tabs/wb_fragmentation_moderation.tex\nDONE\n")
