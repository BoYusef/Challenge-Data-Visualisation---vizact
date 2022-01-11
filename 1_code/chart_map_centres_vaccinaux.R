

### Traitement des donnees

DEP_rea <- DEP_hosp %>%
  left_join(donnee_population %>% select(code_insee, pop),
            by = c('dep' = 'code_insee'),
            keep = FALSE
  ) %>%
  mutate(dep = as.numeric(dep)) %>%
  left_join(dep_corres %>% select('hc_a2', 'code_insee_dep', 'hc-a2'), 
            by = c('dep' = 'code_insee_dep'),
            keep = FALSE
  ) %>%
  group_by(dep, dep_name, hc_a2) %>%
  summarise(reg_name = first(reg_name),
            `hc-a2` = first(`hc-a2`),
            hosp = sum(incid_hosp, na.rm = T),
            pop = mean(pop),
            rea = sum(incid_rea, na.rm = T),
            hosp_rel = (hosp/pop)*100000 ,
            rea_rel = (rea/pop)*100000 ,
            rad = sum(incid_rad, na.rm = T),
            hop_inf = sum(hop_inf, na.rm = T)) %>%
  ungroup


DEP_tamp <- vaccin_etb_dep %>%
  left_join(dep,
            by = c('dep' = 'code'),
            keep = FALSE
  ) %>%
  mutate(dep = as.numeric(dep)) %>%
  left_join(dep_corres %>% select('hc_a2', 'code_insee_dep', 'hc-a2'), 
            by = c('dep' = 'code_insee_dep'),
            keep = FALSE
  ) %>%
  left_join(DEP_rea %>% select(dep, rea_rel),
            keep = FALSE)

##Make the graph


#leave only necessary variables from state df
dt.st <- DEP_tamp %>%
  transmute(value = nbr_etbl_pour_100mill_hab, code = `hc_a2`, dep = dep,  nbr_etbl_pour_100mill_hab = nbr_etbl_pour_100mill_hab, rea_rel = rea_rel, Count = Count, stlabel = dep_name,
            hc_dataLabel = case_when(
              nbr_etbl_pour_100mill_hab == max(nbr_etbl_pour_100mill_hab, na.rm = T) ~ glue(" <center> <i> {stlabel} </i> <br> Centres :{round(nbr_etbl_pour_100mill_hab)} <br> Rea : {round(rea_rel)} </center>"),
              nbr_etbl_pour_100mill_hab == min(nbr_etbl_pour_100mill_hab, na.rm = T) ~ glue(" <center> <i> {stlabel} </i> <br> Centres :{round(nbr_etbl_pour_100mill_hab)} <br> Rea : {round(rea_rel)} </center>"),
              dep == 75 ~ glue(" <center> <i> {stlabel}</i> <br>  Centres : {round(nbr_etbl_pour_100mill_hab)} <br> Rea : {round(rea_rel)} </center>"),
              TRUE ~ "" # No label
            ))
ds.st <- list_parse(dt.st)


#tooltim for state map has to be coded separately from tooltip for county
highchart(type = 'map') %>%
  hc_add_series(
    mapData = frgeoson_dep, data = ds.st, joinBy = c("hc-a2","code"),
    borderWidth = 0.1, borderColor = "#FAFAFA", dataLabels = list(enabled = TRUE, allowOverlap = TRUE, formatter = JS("function(){return(this.point.hc_dataLabel)}")),
    tooltip = list(
      useHTML = TRUE,
      headerFormat = "<p>",
      pointFormat = paste0("<b>{point.stlabel}</b><br>",
                           "<b style=\"color:#1874CD\">Centre vaccinal (pour 100000):</b> {point.nbr_etbl_pour_100mill_hab:.0f}<br>",
                           "<b style=\"color:red\">Réanimations (pour 100000):</b> {point.rea_rel:.0f}<br>"
      ),
      footerFormat = "</p>"
    )) %>%
  hc_title(text = "Nombre de centres vaccinaux pour 100000 habitants", 
           style = list(
             fontWeight = "bold",
             fontSize = "28px"
           ), 
           align = "left"
  ) %>%
  hc_subtitle(
    text = "Paris n'a que 3 centres vaccinaux pour 100000 alors qu'il a le plus fort taux de réanimations", 
    style = list(
      color = "#000000", 
      fontStyle = "italic",
      fontSize = "18px"
    ), 
    align = "left"
  ) %>%
  hc_plotOptions(map = list(states = list(hover = list(color = '#03989E')))) %>%
  hc_colorAxis(minColor = "#a7bcdc", maxColor = "#004aad") %>%
  hc_legend(enabled = TRUE,
            layout = 'vertical',
            align = 'right',
            verticalAlign = 'middle'
  ) %>%
  hc_tooltip(
    positioner = JS(
      "function () { return { x: 0, y: 350}; }")
  ) %>%
  hc_mapNavigation(
    enabled = TRUE,
    enableMouseWheelZoom = TRUE,
    enableDoubleClickZoom = TRUE
  ) %>%
  hc_add_theme(hc_theme_hcCookbook)


