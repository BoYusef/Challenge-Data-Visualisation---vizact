

### Traitement des donnees
DEP_tamp <- DEP_hosp %>%
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
            hosp_rel = (hosp/pop)*100000 ,
            rea = sum(incid_rea, na.rm = T),
            rad = sum(incid_rad, na.rm = T),
            hop_inf = sum(hop_inf, na.rm = T)) %>%
  ungroup

REG_tamp <- DEP_tamp %>%
  group_by(`hc-a2`) %>%
  summarise(reg_name = first(reg_name),
            hosp = sum(hosp, na.rm = T),
            pop = sum(pop, na.rm = T),
            hosp_rel = (hosp/pop)*100000,
            rea = sum(rea, na.rm = T),
            rad = sum(rad, na.rm = T),
            hop_inf = sum(hop_inf, na.rm = T)) %>%
  ungroup


##On recupere les codes departements
dep_code = ""
for (i in 1:103) {
  dep_code = rbind(dep_code, frgeoson_dep$features[[i]]$properties$`hc-a2`)
}

dep_code <- as.data.frame(dep_code) %>% left_join(dep_corres %>% select('hc_a2', 'hc-a2'), 
                                                  by = c('V1' = 'hc_a2'),
                                                  keep = FALSE
) %>% filter(!is.na(`hc-a2`))


build_series <- function(reg) {
  
  list_dep <- dep_corres %>%
    filter(`hc-a2` == reg) %>%
    select(hc_a2)
  
  #subset frgeoson_dep
  frgeoson_dep.subset <- frgeoson_dep
  frgeoson_dep.subset$features <- frgeoson_dep$features[which(dep_code$`hc-a2` == reg)]
  
  #subset frgeoson_dep data
  data_filter <- filter(DEP_tamp, `hc-a2` == reg) %>%
    transmute(value = hosp_rel, `hc-a2` = hc_a2, cnlabel = dep_name, stlabel = reg_name, hosp_rel = hosp_rel, hosp = hosp, rea = rea) %>%
    list_parse()
  
  #build series
  list(
    id = reg,
    type = "map",
    mapData = frgeoson_dep.subset,
    data = data_filter,
    joinBy = 'hc-a2',
    dataLabels = list(enabled = TRUE, format = '{point.name}'),
    tooltip = list(
      useHTML = TRUE,
      headerFormat = "<p>",
      pointFormat = paste0("<b>{point.stlabel}, {point.cnlabel}</b><br>",
                           "<b style=\"color:#1874CD\">Hospitalisations (pour 100000):</b> {point.hosp_rel:.0f}<br>",
                           "<b style=\"color:#1874CD\">Hospitalisations :</b> {point.hosp:.0f}<br>",
                           "<b style=\"color:red\">Reanimations :</b> {point.rea:.0f}<br>"
      ),
      footerFormat = "</p>")
  )
}




##Make the graph


#leave only necessary variables from departement 
dt.st <- REG_tamp %>%
  transmute(value = hosp_rel, code = `hc-a2`, hosp_rel = hosp_rel, hosp = hosp, rea = rea, stlabel = reg_name, drilldown = `hc-a2`,
            hc_dataLabel = case_when(
              hosp_rel == max(hosp_rel, na.rm = T) ~ glue(" ({stlabel}) : <i> {round(hosp_rel)} pour 100000</i> "),
              hosp_rel == min(hosp_rel, na.rm = T) ~ glue(" ({stlabel}) : <i> {round(hosp_rel)} pour 100000</i> "),
              TRUE ~ "" # No label
            ))
ds.st <- list_parse(dt.st)

#create drilldown series
series.list <- lapply(dt.st$drilldown, build_series)

#tooltim for region map has to be coded separately from tooltip for departement
chart_map_hosp <- highchart(type = 'map') %>%
  hc_add_series(
    mapData = frgeoson_reg, data = ds.st, joinBy = c("hc-a2","code"),
    borderWidth = 0.1, borderColor = "#FAFAFA", dataLabels = list(enabled = TRUE, formatter = JS("function(){return(this.point.hc_dataLabel)}")),
    tooltip = list(
      useHTML = TRUE,
      headerFormat = "<p>",
      pointFormat = paste0("<b>{point.stlabel}</b><br>",
                           "<b style=\"color:#1874CD\">Hospitalisations (pour 100000):</b> {point.hosp_rel:.0f}<br>",
                           "<b style=\"color:#1874CD\">Hospitalisations :</b> {point.hosp:.0f}<br>",
                           "<b style=\"color:red\">Reanimations :</b> {point.rea:.0f}<br>"
      ),
      footerFormat = "</p>"
    )) %>%
  hc_title(text = "Nombre de cas d'hospitalisations pour 100000 habitants", 
           style = list(
             fontWeight = "bold",
             fontSize = "28px"
           ), 
           align = "left"
  ) %>%
  hc_subtitle(
    text = "Depuis Mars 2020 <br> La Guyane présente le plus fort taux d'hospitalisation", 
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
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series = series.list,
    activeDataLabelStyle = list(
      color = '#FFFFFF',
      textDecoration = 'none'
    )
  ) %>%
  hc_tooltip(
    positioner = JS(
      "function () { return { x: 0, y: 350}; }")
  ) %>%
  hc_add_theme(hc_theme_hcCookbook)

chart_map_hosp

