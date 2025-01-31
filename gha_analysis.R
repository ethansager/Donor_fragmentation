# Set up and packages  ----------------------------------------------------
setwd("C:/Users/eman7/Dropbox/GitHub/ra_work/Donor_fragmentation/")

if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(
  tidyverse,
  here,
  readr,
  janitor,
  sjmisc,
  plm,
  scales,
  psych,
  mapview,
  ### GEO Packages
  sf,
  spdep,
  spatstat,
  geodata,
  terra
)


dat <- read_rds("C:/Users/eman7/Dropbox/GitHub/ra_work/Donor_fragmentation/ssa_hhi_computed.rds")

sjmisc::frq(dat$LOCATION.LEVEL.1)

dat %>%
  group_by(name_0)%>%
  summarise(mean.hhi = mean(1-hhi, na.rm = TRUE),
            donors = n_distinct(donor),
            projects = n())%>%
  arrange(projects)%>%
  print(n=100)


gha <- dat %>% 
  filter(name_0 == "Ghana" & paymentyear %in% c(2013,2104))

afro_gha <- readxl::read_xlsx("C:/Users/eman7/Downloads/GHA_r6.csv.xlsx")

afro_gha$latitude
 
# Convert to sf object
afro_points <- st_as_sf(
    afro_gha,
    coords = c("longitude", "latitude"), # Specify longitude and latitude columns
    crs = 4326                # Set CRS (WGS84)
  )

gha_shp <- geodata::gadm(country = "Ghana", level = 2, path = "C:/Users/eman7/OneDrive/Desktop/shapefiles/")

gha_shp <- sf::st_as_sf(gha_shp)

mapview(gha_shp)+
  mapview(afro_points)


afro_points <- st_join(afro_points, gha_shp)

aid_points <- st_as_sf(
  gha,
  coords = c("longitude", "latitude"), # Specify longitude and latitude columns
  crs = 4326                # Set CRS (WGS84)
)

gha <- st_join(aid_points, gha_shp)



gha <- gha %>%
  filter(!is.na(disb) & disb != 0) %>%
  group_by(GID_2, donor) %>%
  mutate(
    total_disb = sum(disb_loc_evensplit, na.rm = TRUE)
  ) %>%
  group_by(GID_2) %>%
  mutate(
    total_aid = sum(total_disb),
    donor_count = n_distinct(donor),
    projects = n(),
    percentage_donor = total_disb / total_aid,
    hhi = sum((percentage_donor)^2),
    frag_index = 1 - hhi
  ) %>%
  ungroup()
# EA measures top three index
# ea_svc_a : ea_sva_c (electricty, piped water, sewage)
# ea_fac_a : ea_fac_c ()



# Self report items
# Q53D Corruption local gov 
# Q52E Trust local gov 
# Q59B Local government councilors listen
# Q67A Local gov road matain
# Q67B Local gov market matain
# Q68C Local gov approval 

# Value Labels: 0=None, 1=Some of them, 2=Most of them, 3=All of them, 9=Don’t know, 98=Refused to
# answer, -1=Missing


afro_points <-afro_points %>%
  mutate(
  #self report
  #corruption should be reverse coded 
  q53d_rec = case_match(
    q53d, 
    0~4,
    1~3,
    2~2,
    3~1,
    TRUE ~ NA
  ),
  q52e_rec = case_match(
    q52e, 
    0~1,
    1~2,
    2~3,
    3~4,
    TRUE ~ NA
  ),
  q59b_rec = case_match(
    q59b, 
    0~1,
    1~2,
    2~3,
    3~4,
    TRUE ~ NA
  ),
  #4 point scales
  q67a_rec = case_match(
    q67a, 
    1~1,
    2~2,
    3~3,
    4~4,
    TRUE ~ NA
  ),
  q67b_rec = case_match(
    q67b, 
    1~1,
    2~2,
    3~3,
    4~4,
    TRUE ~ NA
  ),
  q68c_rec =case_match(
    q67b, 
    1~1,
    2~2,
    3~3,
    4~4,
    TRUE ~ NA
  ))

#construct index 
afro_points <- afro_points %>%
  rowwise() %>%
  mutate(
    sub_gov_qual = sum(c_across(ends_with("rec")), na.rm = TRUE) / 
      sum(!is.na(c_across(ends_with("rec")))),
    non_miss = sum(!is.na(c_across(ends_with("rec"))))
  ) %>%
  ungroup()%>%
  mutate(
    sgqi = scales::rescale(sub_gov_qual, to = c(0, 100))
  )



rec_columns <- grep("rec$", names(look), value = TRUE)

# Pass the subset to alpha
look<-afro_points%>%
  select(GID_1, GID_2, ends_with("rec"), sub_gov_qual, non_miss, sgqi)%>%
  sf::st_drop_geometry()

psych::alpha(look[, rec_columns])

corrplot::corrplot(look, type = "lower", method = "number")

reg_means <-afro_points%>%
  group_by(GID_2)%>%
  filter(!is.na(GID_2))%>%
  sf::st_drop_geometry()%>%
  summarise(mean.sgqi = mean(sgqi, na.rm = TRUE)
            )

admin1_means <-afro_points%>%
  group_by(GID_1)%>%
  filter(!is.na(GID_1))%>%
  sf::st_drop_geometry()%>%
  summarise(mean.sgqi = mean(sgqi, na.rm = TRUE)
  )

gha_reg_means <- gha_shp %>%
  tidylog::left_join(reg_means, by = "GID_2")


gha_reg1_means <- gha_shp %>%
  tidylog::left_join(admin1_means, by = "GID_1")

ggplot(data = gha_reg_means) +
  geom_sf(aes(fill = mean.sgqi)) +
  scale_fill_viridis_c(option = "viridis", na.value = "grey50") +
  theme_minimal() +
  labs(fill = "Mean Value", title = "Subgoverment Quailty Index by Region")

ggplot(data = gha_reg1_means) +
  geom_sf(aes(fill = mean.sgqi)) +
  scale_fill_viridis_c(option = "viridis", na.value = "grey50") +
  theme_minimal() +
  labs(fill = "Mean Value", title = "Subgoverment Quailty Index by Admin1")



gha_reg_aid <- gha %>%
  group_by(GID_2)%>%
  select(GID_2, total_aid:hhi, -percentage_donor, -total_disb, frag_index)%>%
  sf::st_drop_geometry()%>%
  distinct()

reg_aid <- gha_shp%>%
  tidylog::left_join(gha_reg_aid, by = "GID_2")


ggplot(data = reg_aid) +
  geom_sf(aes(fill = log(total_aid)))+
  scale_fill_viridis_c(option = "viridis", na.value = "grey50") +
  theme_minimal() +
  labs(fill = "ODA in Dollars", title = "Total ODA by Region")

ggplot(data = reg_aid) +
  geom_sf(aes(fill = frag_index))+
  scale_fill_viridis_c(option = "viridis", na.value = "grey50") +
  theme_minimal() +
  labs(fill = "ODA in Dollars", title = "Total ODA by Region")

total_dat <- gha_reg_means %>%
  tidylog::left_join(gha_reg_aid, by = "GID_2")

lights <- read_rds("C:/Users/eman7/OneDrive/Documents/gha_nightlight_mean.rds")

#night lights 
lights_2014 <- lights %>% 
  filter(year == 2015)%>%
  select(NAME_2, mean.nl = mean)

#map of night lights 
reg_nl <- gha_shp%>%
  tidylog::left_join(lights_2014, by = "NAME_2")


ggplot(data = reg_nl) +
  geom_sf(aes(fill = log(mean.nl))) +
  scale_fill_viridis_c(option = "viridis", na.value = "grey50") +
  theme_minimal() +
  labs(fill = "NL radiance", title = "Log NL radiance by Region")


#population grid
pop_grid <-geodata::population(2010, res = "10", path = tempdir())

gha_avg_pop  <- terra::extract(
  pop_grid,
  gha_shp,
  fun = mean,
  na.rm = TRUE,
  exact = TRUE
)

gha_shp$avg_pop <- gha_avg_pop[,2]

ggplot(data = gha_shp) +
  geom_sf(aes(fill = log(avg_pop))) +
  scale_fill_viridis_c(option = "viridis", na.value = "grey50") +
  theme_minimal() +
  labs(fill = "Average Pop Den", title = "Pop Den by Region")

gha_avg_pop <- gha_shp %>%
  select(
    avg_pop, NAME_2
  )%>%
sf::st_drop_geometry()

total_dat <- total_dat%>%
  tidylog::left_join(lights_2014, by = "NAME_2")%>%
  tidylog::left_join(gha_avg_pop, by = "NAME_2")%>%
  mutate(any_aid = if_else(is.na(total_aid), 0, 1))


aid_dat <- total_dat %>% 
  filter(any_aid == 1)


summary(lm(log(mean.nl) ~ any_aid * GID_1 - 1, dat = total_dat))

#admin 2
summary(lm(log(mean.nl) ~ frag_index * mean.sgqi + donor_count + projects + total_aid + log(avg_pop), dat = aid_dat))
#admin 1

# distance to coast 

# Do donors collocate (No)
merged_sf <- sf::st_intersects(gha_shp, aid_points)

gha_shp <- gha_shp %>% 
  mutate(
  num_points = lengths(merged_sf),
  has_points = num_points > 0
)


nb <- gha_shp %>%
  spdep::poly2nb(., queen = TRUE)

lw <- nb2listw(nb, style = "W")  
# Your code here: compute Moran's I for the clust_grid_sf data
spdep::moran(gha_shp$num_points, lw,length(nb), Szero(lw))


clust_localmoran_result <-spdep::localmoran(gha_shp$num_points, lw)

gha_shp$Ii <- clust_localmoran_result[,"Ii"]
  
gha_shp |> ggplot(aes(fill=Ii)) +
    geom_sf() +
    scale_linewidth_manual(values=c(0, 1.5)) +
    theme_classic() 



gha_ppp <- as.ppp(sf::st_transform(aid_points, 3857), W=as.owin(sf::st_transform(sf::st_union(gha_shp), 3857))) # Replace with call to as.ppp()


gha_ppp |> sf::st_as_sf() |> ggplot() +
    geom_sf() +
    theme_classic()

gha_pcf <- spatstat.explore::pcf(gha_ppp, divisor = "d")

plot(gha_pcf)
