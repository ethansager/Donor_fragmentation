# Firm up the governance moderation:
#  - Primary capacity moderator = ADMIN1 mean_sgq (90.6% cell coverage vs 62% at
#    admin2; corr 0.84) -> fixes power + urban-selection from sparse admin2 sampling.
#  - Triangulate with a FULLY-COVERED geographic capacity proxy: distance to the
#    country's economic core (brightest baseline cell), within-country.
#  - Main fragmentation result: re-cluster at country (33) + report.
# DMSP 2005-2013, PPML, cell + country-year FE.
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, fixest, here)

d <- fread(here("01_panel_data","wb_grid_spine.csv"))

# admin1 governance (better covered) + admin2 (finest, robustness)
s1 <- fread(here("01_panel_data","panel_aid_admin1_fin.csv"), select=c("GID_1","mean_sgq_admin1"))
s1 <- s1[is.finite(mean_sgq_admin1), .(sgq1=mean(mean_sgq_admin1)), by=GID_1]
s2 <- fread(here("01_panel_data","panel_aid_admin2_fin.csv"), select=c("GID_2","mean_sgq_admin2"))
s2 <- s2[is.finite(mean_sgq_admin2), .(sgq2=mean(mean_sgq_admin2)), by=GID_2]
d <- merge(d, s1, by="GID_1", all.x=TRUE); d <- merge(d, s2, by="GID_2", all.x=TRUE)

# geographic capacity proxy: distance to country's economic core (brightest 2005 cell)
haversine <- function(lon1,lat1,lon2,lat2){
  r<-6371; p<-pi/180; a<-sin((lat2-lat1)*p/2)^2 + cos(lat1*p)*cos(lat2*p)*sin((lon2-lon1)*p/2)^2
  2*r*asin(pmin(1,sqrt(a)))
}
base05 <- d[year==2005]
core <- base05[, .SD[which.max(NL)], by=GID_0][, .(GID_0, core_lon=lon, core_lat=lat)]
cells <- unique(d[, .(cell,GID_0,lon,lat)], by="cell")
cells <- merge(cells, core, by="GID_0")
cells[, dist_core := haversine(lon,lat,core_lon,core_lat)]
cells[, near_core := as.integer(dist_core < median(dist_core)), by=GID_0]   # within-country split
d <- merge(d, cells[,.(cell,dist_core,near_core)], by="cell", all.x=TRUE)

# DMSP window + features
d <- d[year>=2005 & year<=2013]
d[, cy := paste(GID_0, year, sep="_")]
d[, frag_pre := fifelse(is.finite(frag_pre), frag_pre, 0)]
d[, mature := as.integer(is.finite(event_time) & event_time>=3)]
d[, early  := as.integer(is.finite(event_time) & event_time>=0 & event_time<3)]
d[, hi_gov1 := as.integer(sgq1 > median(sgq1, na.rm=TRUE))]
d[, hi_gov2 := as.integer(sgq2 > median(sgq2, na.rm=TRUE))]

# ---- main fragmentation result, clustered at COUNTRY ----
m_main_c <- fepois(NL ~ mature + mature:frag_pre | cell + cy, cluster=~GID_0, data=d)
cat("=== MAIN: WB(mature) x fragmentation, clustered at COUNTRY (33) ===\n")
print(coeftable(m_main_c))

# ---- governance triple: admin1 (primary), admin2, geographic ----
tri <- function(dat, gov, lab){
  m <- fepois(NL ~ mature*frag_pre*get(gov) + early | cell + cy, cluster=~GID_0, data=dat[is.finite(get(gov))])
  rn <- rownames(coeftable(m)); keep <- grep("mature", rn)
  ct <- coeftable(m)[keep,,drop=FALSE]
  cat(sprintf("\n=== %s  (n cells w/ moderator: %.0f%%) ===\n", lab, 100*mean(is.finite(dat[[gov]]))))
  print(round(ct,4))
}
tri(d, "hi_gov1",  "TRIPLE: admin1 governance (PRIMARY, 90.6% cov)")
tri(d, "hi_gov2",  "TRIPLE: admin2 governance (robustness, 62% cov)")
tri(d, "near_core","TRIPLE: distance-to-core capacity proxy (100% cov)")

# continuous admin1 version
mc <- fepois(NL ~ mature*frag_pre*scale(sgq1) + early | cell + cy, cluster=~GID_0, data=d[is.finite(sgq1)])
cat("\n=== TRIPLE continuous (admin1 sgq, standardized) ===\n")
print(round(coeftable(mc)[grep("mature",rownames(coeftable(mc))),,drop=FALSE],4))
cat("\nDONE\n")
