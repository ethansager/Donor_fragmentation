# Use the EXTERNAL pre-colonial centralization measure (Murdock v33 jurisdictional
# hierarchy, 0-4) as a NON-circular capacity moderator:
#   (A) legitimacy channel — does fragmentation erode trust MORE where historical
#       state capacity is low?  (sgq couldn't test this: circular w/ trust)
#   (B) material channel — does the WB x frag x capacity triple survive with an
#       external capacity measure instead of Afrobarometer sgq?
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, fixest, here)

jh <- fread(here("01_panel_data","murdock_jh_by_gid2.csv"))   # GID_2, jh (0-4)

# ---- shared: predetermined district fragmentation ----
g <- fread(here("00_rawdata","GODAD_projectlevel.csv"),
  select=c("donor","gid_0","gid_2","paymentyear","disb","disb_loc_evensplit"))
g[, du := fifelse(is.finite(disb_loc_evensplit)&disb_loc_evensplit!=0,disb_loc_evensplit,disb)]
ssa <- unique(fread(here("01_panel_data","panel_aid_admin2_fin.csv"), select="GID_0")$GID_0)
nb <- g[gid_0%in%ssa & donor!="World Bank" & gid_2!="" & donor!="" & paymentyear%between%c(1995,2004)]
fr <- nb[, .(d=sum(du,na.rm=TRUE)), by=.(gid_2,donor)][, .(frag_pre=1-sum((d/sum(d))^2)), by=gid_2]
setnames(fr,"gid_2","GID_2")

# ============ (A) Afrobarometer legitimacy x pre-colonial centralization ============
ab <- fread(here("00_rawdata","ab_raw","processed","afrobarometer_w3_w6_geomerged_new.csv"))
ab <- ab[GID_0 %in% ssa]
ab <- merge(ab, fr, by="GID_2", all.x=TRUE); ab[, frag_pre:=fifelse(is.finite(frag_pre),frag_pre,0)]
ab <- merge(ab, jh, by="GID_2", all.x=TRUE)
ab[, jhz := scale(jh)[,1]]
ab[, GID_0:=factor(GID_0)][, wave:=factor(wave)]
cat("Afro respondents with jh:", round(100*mean(is.finite(ab$jh)),1), "%\n\n")
cat("=== (A) frag x pre-colonial centralization (jh), country+wave FE ===\n")
cat("    int>0 => higher historical capacity BUFFERS the fragmentation effect\n")
for(y in c("trust_rec","listen_rec","corruption_rec","ea_svc_index","ea_fac_index")){
  d <- ab[is.finite(get(y)) & is.finite(jhz)]
  m <- feols(as.formula(paste0(y," ~ frag_pre*jhz | GID_0 + wave")), cluster=~GID_0, data=d)
  ct <- coeftable(m)
  cat(sprintf("  %-15s frag=% .3f(p=%.3f)  frag x jh=% .3f(p=%.3f)\n",
      y, ct["frag_pre",1], ct["frag_pre",4], ct["frag_pre:jhz",1], ct["frag_pre:jhz",4]))
}

# ============ (B) WB nightlights triple with external capacity (jh) ============
cat("\n=== (B) WB x frag x pre-colonial centralization (nightlights PPML) ===\n")
sp <- fread(here("01_panel_data","wb_grid_spine.csv"))
sp <- merge(sp, jh, by="GID_2", all.x=TRUE)
sp[, cy:=paste(GID_0,year,sep="_")][, frag_pre:=fifelse(is.finite(frag_pre),frag_pre,0)]
sp[, mature:=as.integer(is.finite(event_time)&event_time>=3)]
sp[, early:=as.integer(is.finite(event_time)&event_time>=0&event_time<3)]
spd <- sp[is.finite(jh)]
m <- fepois(NL ~ mature*frag_pre*scale(jh) + early | cell + cy, cluster=~GID_0, data=spd)
print(round(coeftable(m)[grep("mature",rownames(coeftable(m))),],4))
cat("\nDONE\n")
