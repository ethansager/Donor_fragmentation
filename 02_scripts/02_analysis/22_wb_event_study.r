# First look: does WB aid raise local nightlights, and does fragmentation blunt it?
# PPML (70% zeros), cell + country-year FE, cluster by district (GID_1).
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, fixest, here)

d <- fread(here("01_panel_data","wb_grid_spine.csv"))
d[, cy := paste(GID_0, year, sep="_")]
d[, frag_pre := fifelse(is.finite(frag_pre), frag_pre, 0)]   # cells w/ no pre non-WB aid = 0 frag
d[, hi_frag := as.integer(frag_pre > median(frag_pre[frag_pre>0], na.rm=TRUE))]

# (1) pooled DiD: effect of an active WB project on luminosity
m_did <- fepois(NL ~ wb_active | cell + cy, cluster = ~GID_1, data = d)

# (2) moderation: does the WB effect shrink as district fragmentation rises?
m_mod <- fepois(NL ~ wb_active + wb_active:frag_pre | cell + cy, cluster = ~GID_1, data = d)

# (3) event study (pre-trends + dynamics), windowed
d[, et := event_time]
d[is.finite(et) & et < -8, et := -8L][is.finite(et) & et > 10, et := 10L]
d[is.na(et), et := -1L]   # never-treated pinned at reference
m_es <- fepois(NL ~ i(et, ref = -1) | cell + cy, cluster = ~GID_1,
               data = d[is.finite(event_time) | wb_first_year %in% NA])

cat("\n===== (1) WB project effect on nightlights (PPML DiD) =====\n")
print(coeftable(m_did))
cat("\n===== (2) WB x district fragmentation (does frag blunt WB?) =====\n")
print(coeftable(m_mod))
cat("\n===== (3) event-study coefficients (ref = -1) =====\n")
print(round(coeftable(m_es)[, 1:2], 4))
saveRDS(list(did=m_did, mod=m_mod, es=m_es), here("03_output","wb_eventstudy_models.rds"))
cat("\nDONE\n")
