# External pre-colonial state-capacity measure (NON-circular w/ Afrobarometer):
# Murdock ethnic-boundary polygons x Ethnographic Atlas v33 "jurisdictional
# hierarchy beyond the local community" (Gennaioli-Rainer; Michalopoulos-
# Papaioannou). Recoded 0-4 (levels of pre-colonial political centralization).
#
# Sources (downloaded 2026-06 from Nathan Nunn, UBC):
#   00_rawdata/murdock/shapefile/Murdock_Map_2020.shp
#   00_rawdata/murdock/atlas/Ethnographic_Atlas_final.dta   (v33, v107=society name)
#   00_rawdata/murdock/Murdock_EA_Concordance.xlsx          (map name <-> EA name)
#
# Output: 01_panel_data/murdock_jh.gpkg (polygons + jh) and
#         01_panel_data/murdock_jh_by_gid2.csv (district -> centralization)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(sf, haven, readxl, data.table, dplyr, here)
md <- here("00_rawdata","murdock")

# ---- v33 jurisdictional hierarchy by Ethnographic Atlas society ----
clean_name <- function(x) toupper(trimws(gsub("\\s+", " ", gsub("\\.", "", as.character(x)))))
atlas <- read_dta(file.path(md,"atlas","Ethnographic_Atlas_final.dta")) |>
  transmute(ea_name = clean_name(v107),         # v107 is padded e.g. "HERERO. ."
            v33 = as.numeric(v33)) |>
  filter(v33 > 0) |>                       # 0 = missing
  mutate(jh = v33 - 1) |>                   # -> 0..4 levels beyond local
  group_by(ea_name) |> summarise(jh = mean(jh, na.rm = TRUE), .groups = "drop")

# ---- concordance: Murdock-map name -> EA name ----
cc <- read_excel(file.path(md,"Murdock_EA_Concordance.xlsx"), sheet = 1) |>
  transmute(map_name = toupper(trimws(`MURDOCK MAP ETHNICITY NAME`)),
            ea_name  = toupper(trimws(`ETHNOGRAPHIC ATLAS ETHNICITY NAME`))) |>
  filter(!is.na(map_name), !is.na(ea_name)) |>
  distinct(map_name, .keep_all = TRUE)

# ---- polygons + jh ----
poly <- st_read(file.path(md,"shapefile","Murdock_Map_2020.shp"), quiet = TRUE) |>
  st_make_valid() |>
  mutate(map_name = toupper(trimws(NAME))) |>
  left_join(cc, by = "map_name") |>
  left_join(atlas, by = "ea_name")
# direct-name fallback (map name == EA name) for unmatched
poly <- poly |>
  left_join(atlas, by = c("map_name" = "ea_name"), suffix = c("", "_direct")) |>
  mutate(jh = coalesce(jh, jh_direct)) |> select(-jh_direct)

cat(sprintf("polygons: %d | with jh: %d (%.0f%%)\n",
            nrow(poly), sum(is.finite(poly$jh)), 100*mean(is.finite(poly$jh))))
cat("jh distribution (0=stateless ... 4=large state):\n"); print(table(round(poly$jh)))

st_write(poly["jh"], here("01_panel_data","murdock_jh.gpkg"), delete_dsn = TRUE, quiet = TRUE)

# ---- assign each GADM admin2 district to its (point-on-surface) ethnic polygon ----
adm <- st_read(here("00_rawdata","shapefiles","gadm_admin2.shp"), quiet = TRUE) |>
  st_make_valid() |> st_transform(st_crs(poly))
pts <- st_point_on_surface(adm)
idx <- st_nearest_feature(pts, poly)          # nearest polygon (handles centroids just off boundaries)
out <- data.table(GID_2 = adm$GID_2, jh = poly$jh[idx])
out <- out[is.finite(jh)]
fwrite(out, here("01_panel_data","murdock_jh_by_gid2.csv"))
cat(sprintf("\ndistricts assigned jh: %d | mean=%.2f sd=%.2f\n", nrow(out), mean(out$jh), sd(out$jh)))
cat("wrote 01_panel_data/murdock_jh_by_gid2.csv\nDONE\n")
