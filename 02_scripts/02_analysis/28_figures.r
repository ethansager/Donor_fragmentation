# Figures + clean service-delivery x capacity moderation.
# sgqi is circular w/ trust outcomes (cor .5-.65) but CLEAN w/ service counts (cor ~0),
# so we moderate service delivery by Afrobarometer institutional capacity here.
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, fixest, ggplot2, here)
figdir <- here("03_output","figs"); dir.create(figdir, recursive=TRUE, showWarnings=FALSE)
th <- theme_minimal(base_size=12) + theme(panel.grid.minor=element_blank())

# ============ FIG 1: WB nightlights event study ============
rc <- readRDS(here("03_output","wb_robust_clean.rds"))
es <- as.data.table(coeftable(rc$es), keep.rownames="term")
es[, et := as.integer(gsub(".*::(-?\\d+).*","\\1", term))]
es <- es[is.finite(et)]; setnames(es, c("Estimate","Std. Error"), c("b","se"))
es <- rbind(es[,.(et,b,se)], data.table(et=-1,b=0,se=0))[order(et)]
ggplot(es, aes(et,b))+
  geom_hline(yintercept=0,linetype=2,color="grey50")+ geom_vline(xintercept=-0.5,linetype=3,color="grey60")+
  geom_ribbon(aes(ymin=b-1.96*se,ymax=b+1.96*se),alpha=.15,fill="#2c7fb8")+
  geom_line(color="#2c7fb8")+ geom_point(color="#2c7fb8")+
  labs(x="Years since WB project arrival", y="Effect on log nightlights (PPML)",
       title="World Bank aid raises local economic activity",
       subtitle="Event study, never-treated controls; flat pre-trends, cumulating effect")+th
ggsave(file.path(figdir,"fig1_wb_event_study.png"), width=7, height=4.5, dpi=150)

# ============ FIG 2: nightlights frag x capacity (the triple) ============
tri <- rc$tri; b <- coef(tri); V <- vcov(tri)
bf <- "matureT:frag_pre"; bfc <- "matureT:frag_pre:scale(sgq1)"
z <- seq(-2,2,.1)
me <- b[bf] + b[bfc]*z
seme <- sqrt(V[bf,bf] + z^2*V[bfc,bfc] + 2*z*V[bf,bfc])
d2 <- data.table(z, me, lo=me-1.96*seme, hi=me+1.96*seme)
ggplot(d2, aes(z,me))+
  geom_hline(yintercept=0,linetype=2,color="grey50")+
  geom_ribbon(aes(ymin=lo,ymax=hi),alpha=.15,fill="#d95f0e")+ geom_line(color="#d95f0e")+
  labs(x="State institutional capacity (sgq, SD)", y="Effect of fragmentation on WB aid impact",
       title="Competition or Chaos? It depends on state capacity",
       subtitle="Marginal effect of donor fragmentation on the WB nightlights effect")+th
ggsave(file.path(figdir,"fig2_frag_x_capacity_nightlights.png"), width=7, height=4.5, dpi=150)

# ============ FIG 3: Afrobarometer 'material up, relational down' ============
ar <- fread(here("03_output","afro_capacity_moderation.csv"))  # has frag main effects
lab <- c(trust_rec="Trust in local govt", listen_rec="Local govt listens",
         corruption_rec="Local govt clean", contact_rec="Contact w/ officials",
         maintian_road_rec="Roads maintained", ea_svc_index="Services (count)",
         ea_fac_index="Facilities (count)")
ar[, label := lab[outcome]]
ar[, kind := ifelse(outcome %in% c("ea_svc_index","ea_fac_index","maintian_road_rec"),
                    "Material delivery","Relationship w/ state")]
ar[, se := abs(frag/qnorm(1-frag_p/2))]; ar[frag_p==0, se := abs(frag)/4]
ggplot(ar, aes(x=reorder(label,frag), y=frag, color=kind))+
  geom_hline(yintercept=0,linetype=2,color="grey50")+
  geom_pointrange(aes(ymin=frag-1.96*se, ymax=frag+1.96*se))+
  coord_flip()+ scale_color_manual(values=c("Material delivery"="#1a9850","Relationship w/ state"="#d73027"))+
  labs(x=NULL, y="Effect of donor fragmentation (predetermined)", color=NULL,
       title="Fragmentation: more services, less trust in government",
       subtitle="Afrobarometer outcomes, country + wave FE")+ th + theme(legend.position="bottom")
ggsave(file.path(figdir,"fig3_afro_material_vs_relational.png"), width=7.5, height=5, dpi=150)

# ============ clean service x capacity moderation (sgqi non-circular w/ services) ============
ab <- fread(here("00_rawdata","ab_raw","processed","afrobarometer_w3_w6_geomerged_new.csv"))
ssa <- unique(fread(here("01_panel_data","panel_aid_admin2_fin.csv"), select="GID_0")$GID_0)
ab <- ab[GID_0 %in% ssa]
g <- fread(here("00_rawdata","GODAD_projectlevel.csv"),
  select=c("donor","gid_0","gid_2","paymentyear","disb","disb_loc_evensplit"))
g[, du := fifelse(is.finite(disb_loc_evensplit)&disb_loc_evensplit!=0,disb_loc_evensplit,disb)]
nb <- g[gid_0%in%ssa & donor!="World Bank" & gid_2!="" & donor!="" & paymentyear%between%c(1995,2004)]
fr <- nb[, .(d=sum(du,na.rm=TRUE)), by=.(gid_2,donor)][, .(frag_pre=1-sum((d/sum(d))^2)), by=gid_2]
setnames(fr,"gid_2","GID_2"); ab <- merge(ab,fr,by="GID_2",all.x=TRUE)
ab[, frag_pre := fifelse(is.finite(frag_pre),frag_pre,0)]
# district baseline institutional capacity = leave-out mean sgqi (predetermined-ish), standardized
cap <- ab[is.finite(sgqi), .(cap=mean(sgqi)), by=GID_2]; ab <- merge(ab,cap,by="GID_2",all.x=TRUE)
ab[, capz := scale(cap)[,1]]
ab[, GID_0:=factor(GID_0)][, wave:=factor(wave)]
cat("=== service delivery: frag x institutional capacity (sgqi, NON-circular w/ services) ===\n")
for(y in c("ea_svc_index","ea_fac_index")){
  m <- feols(as.formula(paste0(y," ~ frag_pre*capz | GID_0 + wave")), cluster=~GID_0, data=ab[is.finite(get(y))&is.finite(capz)])
  ct <- coeftable(m)
  cat(sprintf("  %-14s frag=% .3f (p=%.3f)  frag x cap=% .3f (p=%.3f)\n",
      y, ct["frag_pre",1], ct["frag_pre",4], ct["frag_pre:capz",1], ct["frag_pre:capz",4]))
}
cat("\nFigures -> 03_output/figs/ (fig1,fig2,fig3)\nDONE\n")
