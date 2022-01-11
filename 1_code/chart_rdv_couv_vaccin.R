
data <- nb_rdv_dep %>%
  rename('dep' = "departement") %>%
  left_join(dep,
            by = c('dep' = 'code'),
            keep = FALSE
  ) %>%
  left_join(vaccin_etb_dep %>% select(dep, Count, nbr_etbl_pour_100mill_hab),
            keep = FALSE
  ) %>%
  left_join(DEP_vaccine %>% select(dep, n_tot_complet, couv_tot_complet),
            by = 'dep',
            keep = FALSE
  ) %>%
  mutate(dep = as.numeric(dep)) %>%
  left_join(dep_corres %>% select('hc_a2', 'code_insee_dep', 'hc-a2'), 
            by = c('dep' = 'code_insee_dep'),
            keep = FALSE
  ) %>%
  arrange(desc(n_tot_complet)) %>%
  slice_max(n_tot_complet, n = 10)

myblue = "#a7bcdc"
mygreen = "#03989E"

Chart_rdv = 
  data %>%
  hchart(
    "bar", 
    hcaes(x = dep_name, y = nb_rdv), 
    name = "Rendez-vous pris", 
    showInLegend = TRUE, 
    color = myblue,
    pointPadding = 0.3,
    borderColor = "transparent"
  ) %>%
  hc_add_series(
    data,
    "bar",
    hcaes(x = dep_name, y = n_tot_complet), 
    name = "Nombre de Vaccinés (couverture vaccinale)", 
    showInLegend = TRUE,
    color = mygreen,
    pointPadding = 0.4,
    borderColor = "transparent",
    dataLabels = list(
      enabled = TRUE,
      formatter = JS("function() {return(this.point.n_tot_complet + ' (' + this.point.couv_tot_complet + '%)')}")
    )
  ) %>%
  hc_tooltip(
    shared = TRUE
  ) %>% 
  hc_plotOptions(
    series = list(
      grouping = FALSE,
      borderwidth = 0
    )
  ) %>%
  hc_title(
    text = "Rendez-vous de vaccination pris et nombre de vaccinés", 
    style = list(
      fontWeight = "bold",
      fontSize = "28px"
    ), 
    align = "left"
  ) %>%
  hc_subtitle(
    text = "Une couverture vaccinale accrue grâce à des vaccinations sans rendez-vous", 
    style = list(
      fontStyle = "italic",
      fontSize = "18px"
    ), 
    align = "left"
  ) %>%
  hc_yAxis(
    gridLineWidth = 0, 
    labels = list(style = list(color =  "#000000")),
    title = list(text = "", style = list(color = "#000000")),
    showFirstLabel = FALSE
  ) %>%
  hc_xAxis(
    labels = list(style = list(color =  "#000000")),
    title = list(text= "Departement"),
    lineWidth = 0,
    tickWidth = 0
  ) %>%
  hc_add_theme(hc_theme_hcCookbook)

Chart_rdv
