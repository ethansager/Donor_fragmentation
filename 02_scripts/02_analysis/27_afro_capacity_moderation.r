# Capacity moderation of the fragmentation -> citizen-outcome effects.
# Capacity = NON-Afrobarometer proxy (distance to country economic core / state reach),
# to avoid circularity with the trust/governance outcomes (sgq is built from same survey).
# Orientation CONFIRMED from .sav: trust_rec/listen_rec higher=better; corruption_rec
# higher=cleaner; ea_svc/fac_index = counts. Local-government items.
# Hypothesis (mirrors nightlights): fragmentation erodes trust MORE where state reach is low.
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, fixest, here)

ab <- fread(here("00_rawdata","ab_raw","processed","afrobarometer_w3_w6_geomerged_new.csv"))
ssa <- unique(fread(here("01_panel_data","panel_aid_admin2_fin.csv"), select="GID_0")$GID_0)
ab <- ab[GID_0 %in% ssa & is.finite(latitude) & is.finite(longitude)]

# predetermined district fragmentation (non-WB 1995-2004) + local WB presence
g <- fread(here("00_rawdata","GODAD_projectlevel.csv"),
  select=c("donor","gid_0","gid_2","latitude","longitude","startyear","closingyear",
           "paymentyear","precision_code","disb","disb_loc_evensplit"))
g[, disb_use := fifelse(is.finite(disb_loc_evensplit)&disb_loc_evensplit!=0, disb_loc_evensplit, disb)]
nb <- g[gid_0%in%ssa & donor!="World Bank" & gid_2!="" & donor!="" & is.finite(paymentyear) & paymentyear%between%c(1995,2004)]
fr <- nb[, .(d=sum(disb_use,na.rm=TRUE)), by=.(gid_2,donor)][, .(frag_pre=1-sum((d/sum(d))^2)), by=gid_2]
setnames(fr,"gid_2","GID_2"); ab <- merge(ab, fr, by="GID_2", all.x=TRUE)
ab[, frag_pre := fifelse(is.finite(frag_pre), frag_pre, 0)]
wb <- g[gid_0%in%ssa & donor=="World Bank" & precision_code%in%1:2 & is.finite(latitude)&is.finite(startyear)]
wb[, cyr := fifelse(is.finite(closingyear)&closingyear>=startyear,closingyear,startyear+4L)][, ck := paste(floor(latitude/0.1),floor(longitude/0.1))][, rid:=.I]
wba <- wb[, .(year=startyear:pmin(cyr,2016L)), by=.(rid,ck)][, .(wb=1L), by=.(ck,year)]
ab[, ck := paste(floor(latitude/0.1),floor(longitude/0.1))]
ab <- merge(ab, wba, by=c("ck","year"), all.x=TRUE)[is.na(wb), wb:=0L]

# capacity = distance to country economic core (brightest 2005 cell), standardized within country
sp <- fread(here("01_panel_data","wb_grid_spine.csv"), select=c("GID_0","lon","lat","NL","year"))
core <- sp[year==2005][, .SD[which.max(NL)], by=GID_0][, .(GID_0, clon=lon, clat=lat)]
ab <- merge(ab, core, by="GID_0", all.x=TRUE)
hav <- function(a,b,c,d){r<-6371;p<-pi/180;x<-sin((c-a)*p/2)^2+cos(a*p)*cos(c*p)*sin((d-b)*p/2)^2;2*r*asin(pmin(1,sqrt(x)))}
ab[, dist_core := hav(latitude,longitude,clat,clon)]
ab[, reach := scale(-dist_core)[,1], by=GID_0]    # higher = closer to core = more state reach
ab[, GID_1:=factor(GID_1)][, GID_0:=factor(GID_0)][, wave:=factor(wave)]

run <- function(y){
  dd <- ab[is.finite(get(y)) & is.finite(reach)]
  m <- feols(as.formula(paste0(y," ~ frag_pre*reach + wb | GID_0 + wave")), cluster=~GID_0, data=dd)
  ct <- coeftable(m)
  data.table(outcome=y, n=nobs(m),
             frag=round(ct["frag_pre",1],3), frag_p=round(ct["frag_pre",4],3),
             frag_x_reach=round(ct["frag_pre:reach",1],3), int_p=round(ct["frag_pre:reach",4],3))
}
cat("=== frag x state-reach (distance-to-core) moderation ===\n")
cat("frag = effect at mean reach; frag_x_reach = how effect changes as reach rises (>0 => high-capacity buffers)\n\n")
res <- rbindlist(lapply(c("trust_rec","listen_rec","corruption_rec","contact_rec",
                          "ea_svc_index","ea_fac_index","maintian_road_rec"), run))
print(res)
fwrite(res, here("03_output","afro_capacity_moderation.csv"))
cat("\nDONE\n")
