
mygold = "#FDB927"
myblue = "#a7bcdc"
mypurple = "#552583"
myred = "#FF5733"
myredlight = "#f0d5d1"

pop_region <- donnee_population %>%
  left_join(dep %>% select(code, region_code),
            by = c('code_insee' = 'code'),
            keep = FALSE
  ) %>%
  mutate(region_code = as.numeric(region_code)) %>%
  group_by(region_code) %>%
  summarise(pop = sum(pop, na.rm = T)) %>%
  ungroup



data_reg <- REG_hosp %>%
  left_join(vagues_COVID, by = c("jour"), keep = FALSE) %>%
  left_join(pop_region,
            by = c('reg' = 'region_code'),
            keep = FALSE
  ) %>%
  group_by(reg, vague) %>%
  summarise(nomReg = first(nomReg),
            rea = sum(incid_rea, na.rm = T),
            pop = mean(pop)) %>%
  ungroup %>%
  mutate(rea = (rea/pop)*100000) %>%
  pivot_wider(id_cols = c("reg", "nomReg"), 
              names_from = vague,
              values_from = c("rea", "pop"))

data <- DEP_hosp %>%
              left_join(vagues_COVID, by = c("jour"), keep = FALSE) %>%
              left_join(donnee_population %>% select(code_insee, pop),
                        by = c("dep" = "code_insee"), keep = FALSE) %>%
              group_by(dep,vague) %>%
              summarise(dep_name = first(dep_name),
                        reg = first(region_code),
                        nomReg = first(reg_name),
                        rea = sum(incid_rea, na.rm = T),
                        pop = mean(pop)) %>%
              ungroup %>%
              mutate(rea = (rea/pop)*100000) %>%
              pivot_wider(id_cols = c("dep", "reg", "dep_name", "nomReg"), 
                          names_from = vague,
                          values_from = c("rea", "pop")) %>%
              group_by(reg) %>%
              slice_max(rea_5, n = 5) %>%
              arrange(rea_2) 
              

##select 5 most dep during wave 5
data_filter = data_reg %>%
              slice_max(rea_5, n = 5) %>%
              arrange(rea_5) 

dep_drilldown <- data %>%
  mutate(reg = ifelse(reg == "COM", 999, as.numeric(reg))) %>%
  group_nest(reg) %>%
  mutate(
    id = reg,
    type = "column",
    data = map(data, mutate, name = dep_name, y = rea_5),
    data = map(data, list_parse)
  )

Chart = 
  data_filter %>%
  hchart(
    "column", 
    hcaes(x = nomReg, y = rea_2, name = reg, drilldown = reg), 
    name = "VAGUE 2", 
    showInLegend = TRUE, 
    color = myredlight,
    pointPlacement = -0.2,
    borderColor = "transparent",
    borderRadius = 2,
    dataLabels = list(
      enabled = FALSE
    )
  ) %>% 
  hc_plotOptions(
    series = list(
      grouping = FALSE,
      borderwidth = 0,
      cursor = "pointer"
    )
  ) %>%
  hc_add_series(data_filter,
                "column", 
                hcaes(x = nomReg, y = rea_5, name = reg, drilldown = reg), 
                name = "VAGUE 5", 
                showInLegend = TRUE, 
                color = myred,
                borderColor = "transparent",
                borderRadius = 2,
                dataLabels = list(
                  enabled = TRUE,
                  inside = TRUE,
                  formatter = JS("function(){return(Math.round(this.point.rea_5))}")
                )
  ) %>%
  hc_chart(style = list(fontFamily = "Rubik")) %>%
  hc_title(
    text = "Taux de réanimations dans les 5 régions les plus atteintes", 
    style = list(
      fontWeight = "bold",
      fontSize = "28px"
    ), 
    align = "left"
  ) %>%
  hc_subtitle(
    text = "VAGUE 2 et 5", 
    style = list(
      fontStyle = "italic",
      fontSize = "18px"
    ), 
    align = "left"
  ) %>%
  hc_yAxis(
    gridLineWidth = 0, 
    labels = list(style = list(color =  "#000000")),
    title = list(text = "Nombre de réanimations", style = list(color = "#000000")),
    showFirstLabel = FALSE
  ) %>%
  hc_xAxis(
    labels = list(style = list(color =  "#000000")),
    title = list(text= ""),
    lineWidth = 0,
    tickWidth = 0
  ) %>%
  hc_tooltip(
    shared = TRUE,
    shape = "square",
    borderWidth = 0
  )%>%
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series = list_parse(dep_drilldown)
  )

Chart



