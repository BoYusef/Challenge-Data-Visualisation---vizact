

myblue = "#004aad"

df <- DEP_mois_hosp  %>%
  left_join(donnee_population %>% select(code_insee, pop),
            by = c('dep' = 'code_insee'),
            keep = FALSE
  ) %>%
  group_by(dep) %>%
  arrange(jour) %>%
  mutate(hosp = cumsum(incid_hosp),
         hosp_rel = (hosp/pop)*100000) %>%
  ungroup


df %>%
  group_by(mois_annee) %>%
  e_charts(dep_name, timeline = TRUE) %>%
  e_bar(hosp_rel, realtimeSort = TRUE, itemStyle = list(
    borderColor = "#FAFAFA", borderWidth = '0.1', color = myblue, borderRadius = 2)
  ) %>%
  e_legend(show = FALSE) %>%
  e_flip_coords() %>%
  e_y_axis(inverse = TRUE,
           min = 0,
           max = 9)  %>%
  e_labels(position = "insideRight", 
           formatter = htmlwidgets::JS("
      function(params){
        return(Math.round(params.value[0]))
      }
    ") ) %>%
  e_timeline_opts(autoPlay = TRUE, top = "55")  %>%
  e_grid(top = 100) %>%
  e_title(paste0("Hospitalisation pour 100000 habitants par mois"), 
          subtext = "10 départements les plus atteints", 
          left = "center", top = 10)

