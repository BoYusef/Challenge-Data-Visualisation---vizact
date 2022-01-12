
myred = "#FF5733"

df <- DEP_mois_hosp  %>%
  left_join(donnee_population %>% select(code_insee, pop),
            by = c('dep' = 'code_insee'),
            keep = FALSE
  ) %>%
  group_by(dep) %>%
  arrange(jour) %>%
  mutate(rea = cumsum(incid_rea),
         rea_rel = (rea/pop)*100000) %>%
  ungroup


df %>%
  group_by(mois_annee) %>%
  e_charts(dep_name, timeline = TRUE) %>%
  e_bar(rea_rel, realtimeSort = TRUE, itemStyle = list(
    borderColor = "#FAFAFA", borderWidth = '0.1', color = myred, borderRadius = 2)
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
  e_title(paste0("Reanimations pour 100000 habitants par mois"), 
          subtext = "10 départements les plus atteints", 
          left = "center", top = 10)

