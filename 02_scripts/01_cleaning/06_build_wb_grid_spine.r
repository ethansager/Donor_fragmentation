# =============================================================================
# Design v3 data spine: 0.1deg SSA grid x year (2000-2020) with
#   - NL_ct  : nightlights, Tanner Regan world_nightlights bloom+topcode-fixed,
#              CONTINUOUS DMSP-scale series (africa/DMSP*_bltcfix.tif). NOT the
#              raw Li et al. Harmonized_DN_NTL product (that has a 2.3x calDMSP->
#              simVIIRS jump at 2014 that contaminates within-cell event studies).
#   - admin  : GID_0/GID_1/GID_2 per cell (rasterized from gadm_admin2)
#   - wb_active_ct, wb_disb_ct, wb_first_year : precise WB project treatment
#   - frag_d : pre-period (1995-2004) non-WB district fragmentation (moderator)
# Output: 01_panel_data/wb_grid_spine.csv  (+ cell lookup)
# =============================================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(terra, sf, data.table, tidyverse, here)
terraOptions(progress = 0)

RES <- 0.1; FACT <- 12L            # 0.008333 * 12 = 0.1 deg
WB_RADIUS_CELLS <- 0L              # 0 = same cell (10km); robustness later

ssa <- unique(fread(here("01_panel_data","panel_aid_admin2_fin.csv"), select="GID_0")$GID_0)
message("SSA countries: ", length(ssa))

# ---- admin polygons -> integer-coded raster aligned to a 0.1deg template ----
shp <- vect(here("00_rawdata","shapefiles","gadm_admin2.shp"))
shp <- shp[shp$GID_0 %in% ssa, ]
e <- ext(shp)
tmpl <- rast(ext(floor(e[1]), ceiling(e[2]), floor(e[3]), ceiling(e[4])),
             resolution = RES, crs = "EPSG:4326")
shp$g2code <- as.integer(factor(shp$GID_2))
lut <- as.data.table(as.data.frame(shp[, c("g2code","GID_0","GID_1","GID_2")]))
setkey(lut, g2code); lut <- unique(lut, by = "g2code")
r_g2 <- rasterize(shp, tmpl, field = "g2code")
message("template cells: ", ncell(tmpl), " ; admin-assigned: ", global(!is.na(r_g2), "sum", na.rm=TRUE)[[1]])

# ---- nightlights: Regan bloom+topcode-fixed continuous series ----
YEARS <- 2000:2020
nlf <- file.path(here("00_rawdata","nightlights","africa"), paste0("DMSP", YEARS, "_bltcfix.tif"))
keep <- file.exists(nlf); nlf <- nlf[keep]
yrs <- as.integer(gsub(".*DMSP(\\d{4}).*","\\1", basename(nlf)))
nl_list <- lapply(seq_along(nlf), function(i){
  r <- rast(nlf[i]); r <- crop(r, ext(tmpl))
  ra <- aggregate(r, fact = FACT, fun = "mean", na.rm = TRUE)
  ra <- resample(ra, tmpl, method = "average")
  names(ra) <- paste0("nl_", yrs[i]); ra
})
nl <- rast(nl_list)
message("NL years: ", paste(sort(yrs), collapse=","))

# ---- assemble cell base table ----
base <- data.table(cell = 1:ncell(tmpl),
                   lon = xyFromCell(tmpl, 1:ncell(tmpl))[,1],
                   lat = xyFromCell(tmpl, 1:ncell(tmpl))[,2],
                   g2code = as.integer(values(r_g2)[,1]))
nlvals <- as.data.table(values(nl)); setnames(nlvals, names(nl))
base <- cbind(base, nlvals)
base <- base[!is.na(g2code)]                       # keep SSA land cells only
base <- merge(base, lut, by = "g2code", all.x = TRUE)
message("SSA grid cells (admin-assigned): ", nrow(base))

# ---- long panel cell x year ----
panel <- melt(base, id.vars = c("cell","lon","lat","GID_0","GID_1","GID_2"),
              measure.vars = patterns("^nl_"), variable.name = "yvar", value.name = "NL")
panel[, year := as.integer(gsub("nl_","", yvar))][, yvar := NULL]

# ---- WB precise treatment -> assign to cell, build active years ----
g <- fread(here("00_rawdata","GODAD_projectlevel.csv"),
           select=c("project_id","donor","gid_0","gid_2","latitude","longitude",
                    "startyear","closingyear","precision_code","disb","disb_loc_evensplit"))
g[, disb_use := fifelse(is.finite(disb_loc_evensplit)&disb_loc_evensplit!=0, disb_loc_evensplit, disb)]
wb <- g[gid_0 %in% ssa & donor=="World Bank" & precision_code %in% 1:2 &
        is.finite(latitude)&is.finite(longitude)]
wb[, cell := cellFromXY(tmpl, cbind(longitude, latitude))]
wb[, sy := fifelse(is.finite(startyear), startyear, NA_integer_)]
wb[, cy := fifelse(is.finite(closingyear) & closingyear>=startyear, closingyear, startyear + 4L)]  # default 5yr
wb <- wb[is.finite(sy) & !is.na(cell)]
# expand to active cell-years
wb_active <- wb[, .(year = sy:pmin(cy, 2020L), disb = disb_use[1]/.N), by = .(project_id, cell, sy)]
wb_cellyr <- wb_active[year>=2000 & year<=2020,
                       .(wb_active = 1L, wb_disb = sum(disb, na.rm=TRUE), n_wb = .N),
                       by = .(cell, year)]
wb_first <- wb[, .(wb_first_year = min(sy, na.rm=TRUE)), by = cell]

panel <- merge(panel, wb_cellyr, by = c("cell","year"), all.x = TRUE)
panel <- merge(panel, wb_first, by = "cell", all.x = TRUE)
panel[is.na(wb_active), `:=`(wb_active = 0L, wb_disb = 0, n_wb = 0L)]
panel[, event_time := fifelse(is.finite(wb_first_year), year - wb_first_year, NA_integer_)]

# ---- pre-period (1995-2004) non-WB district fragmentation moderator ----
nb <- g[gid_0 %in% ssa & donor!="World Bank" & gid_2!="" & donor!=""]
# need paymentyear for pre-period window; reread it
py <- fread(here("00_rawdata","GODAD_projectlevel.csv"), select=c("project_id","paymentyear"))
nb <- merge(nb, py, by="project_id", all.x=TRUE)
pre <- nb[is.finite(paymentyear) & paymentyear>=1995 & paymentyear<=2004]
fr <- pre[, .(d=sum(disb_use,na.rm=TRUE)), by=.(gid_2,donor)][
         , .(frag_pre = 1 - sum((d/sum(d))^2), n_other_pre = uniqueN(donor)), by=gid_2]
setnames(fr, "gid_2", "GID_2")
panel <- merge(panel, fr, by = "GID_2", all.x = TRUE)

# ---- write ----
out <- here("01_panel_data","wb_grid_spine.csv")
fwrite(panel, out)
message("WROTE ", out, " : ", nrow(panel), " cell-years, ",
        uniqueN(panel$cell), " cells, treated cells=", uniqueN(panel[wb_active==1]$cell))
message("frag_pre non-missing: ", round(100*mean(is.finite(panel$frag_pre)),1), "%")
