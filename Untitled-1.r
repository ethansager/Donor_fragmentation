look<-panel_aid_admin2 %>%
  group_by(GID_0) %>%
  summarise(
    mean_nl_mean = mean(mean_nl, na.rm = TRUE),
    mean_nl_median = median(mean_nl, na.rm = TRUE),
    
    total_aid_admin2_mean = mean(total_aid_admin2, na.rm = TRUE),
    total_aid_admin2_median = median(total_aid_admin2, na.rm = TRUE),

    
    frag_index_admin2_mean = mean(frag_index_admin2, na.rm = TRUE),
    frag_index_admin2_median = median(frag_index_admin2, na.rm = TRUE),
    
    mean_sgq_admin2_mean = mean(mean_sgq_admin2, na.rm = TRUE),
    mean_sgq_admin2_median = median(mean_sgq_admin2, na.rm = TRUE),

  )

look2<-panel_aid_admin1 %>%
  group_by(GID_0) %>%
  summarise(
    mean_nl_mean = mean(mean_nl, na.rm = TRUE),
    mean_nl_median = median(mean_nl, na.rm = TRUE),
    
    total_aid_admin2_mean = mean(total_aid_admin1, na.rm = TRUE),
    total_aid_admin2_median = median(total_aid_admin1, na.rm = TRUE),

    
    frag_index_admin2_mean = mean(frag_index_admin1, na.rm = TRUE),
    frag_index_admin2_median = median(frag_index_admin1, na.rm = TRUE),
    
    mean_sgq_admin2_mean = mean(mean_sgq_admin1, na.rm = TRUE),
    mean_sgq_admin2_median = median(mean_sgq_admin1, na.rm = TRUE),

  )

panel_aid_admin2 <- read_csv("01_panel_data/panel_aid_admin2.csv")

panel_aid_admin2 <- panel_aid_admin2 %>%
select(-disaster_count.x, -disaster_dummy.x)%>%
rename(disaster_dummy = disaster_dummy.y, 
 disaster_count = disaster_count.y)

write_csv(panel_aid_admin2, "01_panel_data/panel_aid_admin2.csv")
