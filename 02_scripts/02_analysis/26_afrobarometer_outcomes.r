# Does donor fragmentation erode citizens' relationship with the state?
# Afrobarometer microdata (rounds 3-6, 2005-2015, 161k georeferenced respondents)
# as OUTCOMES (govt performance, trust, corruption, responsiveness, services),
# vs predetermined district donor fragmentation + local WB aid presence.
# Region (GID_1) + wave FE, cluster country.
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, fixest, here)

ab <- fread(here("00_rawdata","ab_raw","processed","afrobarometer_w3_w6_geomerged_new.csv"))
ssa <- unique(fread(here("01_panel_data","panel_aid_admin2_fin.csv"), select="GID_0")$GID_0)
ab <- ab[GID_0 %in% ssa & is.finite(latitude) & is.finite(longitude)]
cat("respondents (SSA, geo):", nrow(ab), " countries:", uniqueN(ab$GID_0), "\n")

# ---- predetermined district fragmentation (non-WB, 1995-2004) ----
g <- fread(here("00_rawdata","GODAD_projectlevel.csv"),
           select=c("donor","gid_0","gid_2","latitude","longitude","startyear","closingyear",
                    "paymentyear","precision_code","disb","disb_loc_evensplit"))
g[, disb_use := fifelse(is.finite(disb_loc_evensplit)&disb_loc_evensplit!=0, disb_loc_evensplit, disb)]
nb <- g[gid_0 %in% ssa & donor!="World Bank" & gid_2!="" & donor!="" &
        is.finite(paymentyear) & paymentyear>=1995 & paymentyear<=2004]
fr <- nb[, .(d=sum(disb_use,na.rm=TRUE)), by=.(gid_2,donor)][
        , .(frag_pre = 1-sum((d/sum(d))^2), n_other_pre=uniqueN(donor)), by=gid_2]
setnames(fr,"gid_2","GID_2")
ab <- merge(ab, fr, by="GID_2", all.x=TRUE)
ab[, frag_pre := fifelse(is.finite(frag_pre), frag_pre, 0)]

# ---- local WB aid presence: WB precise project active within respondent's 0.1deg cell at survey yr ----
wb <- g[gid_0 %in% ssa & donor=="World Bank" & precision_code %in% 1:2 &
        is.finite(latitude)&is.finite(longitude) & is.finite(startyear)]
wb[, cy := fifelse(is.finite(closingyear)&closingyear>=startyear, closingyear, startyear+4L)]
wb[, ck := paste(floor(latitude/0.1), floor(longitude/0.1))]
wb[, rid := .I]
wb_act <- wb[, .(year=startyear:pmin(cy,2016L)), by=.(rid, ck)][, .(wb=1L), by=.(ck,year)]
ab[, ck := paste(floor(latitude/0.1), floor(longitude/0.1))]
ab <- merge(ab, wb_act, by.x=c("ck","year"), by.y=c("ck","year"), all.x=TRUE)
ab[is.na(wb), wb := 0L]
cat("respondents with WB project in-cell at survey year:", round(100*mean(ab$wb),1), "%\n")
cat("frag_pre non-missing share:", round(100*mean(ab$frag_pre>0),1), "%\n\n")

# ---- outcomes (recoded). Run region + wave FE, cluster country ----
outs <- c("preformance_rec","trust_rec","corruption_rec","listen_rec","contact_rec",
          "maintian_road_rec","maintian_market_rec","ea_svc_index","ea_fac_index","sub_gov_qual")
ab[, GID_1 := factor(GID_1)][, GID_0 := factor(GID_0)][, wave := factor(wave)]

res <- rbindlist(lapply(outs, function(y){
  if(!y %in% names(ab)) return(NULL)
  dd <- ab[is.finite(get(y))]
  m <- feols(as.formula(paste0(y," ~ frag_pre + wb | GID_1 + wave")), cluster=~GID_0, data=dd)
  ct <- coeftable(m)
  data.table(outcome=y, n=nobs(m),
             frag_b=ct["frag_pre",1], frag_se=ct["frag_pre",2], frag_p=ct["frag_pre",4],
             wb_b=ct["wb",1], wb_p=ct["wb",4])
}))
cat("=== Fragmentation (predetermined) & local WB aid on citizen outcomes ===\n")
print(res[, .(outcome, n, frag_b=round(frag_b,3), frag_p=round(frag_p,3),
              wb_b=round(wb_b,3), wb_p=round(wb_p,3))])
fwrite(res, here("03_output","afrobarometer_outcomes.csv"))
cat("\nDONE\n")
