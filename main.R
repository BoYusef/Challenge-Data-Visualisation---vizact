# ********************************************************************************#
#       COMPETITION : Challenge Data Visualisation en Actuariat                  #
#      ORGANISATEUR : Institut des Actuaires                                     #
#             TITLE : Covid et Inegalites de sante                               #
#       DESCRIPTION :                                                            #
#         CAPITAINE : DANIS JIOGUE                                               #
#    AUTRES MEMBRES : TOSSOU RODRIGUE, FOUZIA ELAOUNI, YOUSSOUF BANCE            #
# ********************************************************************************#

Packages_lib = "C:/Packages"

.libPaths(Packages_lib)
########################
### Packages 
if(!is.element('pacman',.packages(all.available = TRUE))){
   install.packages('pacman')
}

pacman::p_load(tidyverse,plotly,tidyverse,cartography,sf,spData,tmap,
               leaflet,ggplot2,highcharter,glue,rmdformats,this.path)

if(!is.element('echarts4r.maps',.packages(all.available = TRUE))){
  remotes::install_github("JohnCoene/echarts4r.maps")
}

setwd(str_replace(this.path(),'main.R',''))


###########################
### Importation des donnees
source('1_code/Importation_traitement_donnees.R')


###########################
### graph : Nombre hospitalisation et rea
#source('1_code/ban2.R')
### Objectif : evolution des hospitalisations suivant les vagues du COVID

### 1er vague : Mars - Mai 2020
### 2e vague : Septembre - Novembre 2020
### 3e vague : Mars - Avril 2021
### 4e vague : Juillet - Aout 2021
### 5e vague : Novembre - Decembre 2021


##

data <- FRA_mois_indicateurs_hosp 

data = 
  data %>%
  mutate(
    # Data Label (same as before)
    hc_dataLabel = case_when(
      hosp == max(hosp) ~ glue("Max Hospitalisation {round(hosp)} <i> ({mois} {annee}) </i>"),
      hosp == min(hosp) ~ glue("Min Hospitalisation {round(hosp)} <i> ({mois} {annee}) </i>"),
      TRUE ~ "" # No label
    ),
    hc_dataLabel_rea = case_when(
      rea == max(rea) ~ glue("Rea : {round(rea)}"),
      rea == min(rea) ~ glue("Rea : {round(rea)}"),
      TRUE ~ "" # No label
    ),
    # Tooltip
    hc_ttip = 
      glue(
        "
        <b> {mois} {annee} </b> <br>
        Hospitalisation: {round(hosp)} <br>
        Situation critique: {round(rea)} <br>
        "
      )
  )

mygold = "#FDB927"
myblue = "#a7bcdc"
mypurple = "#552583"
myred = "#FF5733"
myredlight = "#f0d5d1"

moy_hosp = mean(data$hosp)

Chart_hosp_FRA <- 
  data %>%
  hchart(
    "column", 
    hcaes(x = mois_annee, y = hosp), 
    name = "Hospitalisation", 
    showInLegend = TRUE, 
    color = myblue,
    borderColor = "transparent",
    backgroundColor = 'white',
    dataLabels = list(
      enabled = TRUE,
      formatter = JS(
        "
        function(){return(this.point.hc_dataLabel)}
        "
      ) 
    )
  ) %>%
  hc_add_series(data,
                "column", 
                hcaes(x = mois_annee, y = rea), 
                name = "Reanimations", 
                showInLegend = TRUE, 
                color = myred,
                borderColor = "transparent",
                dataLabels = list(
                  enabled = TRUE,
                  formatter = JS(
                    "
        function(){return(this.point.hc_dataLabel_rea)}
        "
                  ) 
                )
  ) %>%
  hc_chart(style = list(fontFamily = "Rubik")) %>%
  hc_title(
    text = "Hospitalisation et réanimations", 
    style = list(
      color = "#000000", 
      fontWeight = "bold",
      fontSize = "28px"
    ), 
    align = "left"
  ) %>%
  hc_subtitle(
    text = "Mars 2020 ? Decembre 2021", 
    style = list(
      color = "#000000", 
      fontStyle = "italic",
      fontSize = "18px"
    ), 
    align = "left"
  ) %>%
  hc_yAxis(
    gridLineWidth = 0, 
    labels = list(style = list(color =  "#000000")),
    title = list(text = "Nombre de personnes", style = list(color = "#000000")),
    plotLines = list(
      list( 
        value = moy_hosp, 
        color = "#000000", 
        zIndex = 1000,
        label = list(
          text = glue("Moyenne hospitalisation: {round(moy_hosp)}"),
          style = list(color = "#000000",fontSize = "16px",fontWeight = "bold"),
          align = "right"
        )
      ) 
    )
  ) %>%
  hc_xAxis(
    labels = list(style = list(color =  "#000000")),
    title = list(text= ""),
    lineWidth = 0,
    tickWidth = 0,
    # Vagues contamination
    plotBands = list(
      list(
        from = -1,
        to = 3, 
        color = "#e9f1f1",
        label = list(
          text = "1ere vague",
          style = list(fontWeight = "bold", color = "#000000", fontSize = "14px")
        )
      ),
      list(
        from = 6,
        to = 9, 
        color = "#e9f1f1",
        label = list(
          text = "2e vague",
          style = list(fontWeight = "bold", color = "#000000", fontSize = "14px")
        )
      ),
      list(
        from = 11,
        to = 14, 
        color = "#e9f1f1",
        label = list(
          text = "3e vague",
          style = list(fontWeight = "bold", color = "#000000", fontSize = "14px")
        )
      ),
      list(
        from = 16,
        to = 18, 
        color = "#e9f1f1",
        label = list(
          text = "4e vague",
          style = list(fontWeight = "bold", color = "#000000", fontSize = "14px")
        )
      ),
      list(
        from = 19,
        to = 22, 
        color = "#e9f1f1",
        label = list(
          text = "5e vague",
          style = list(fontWeight = "bold", color = "#000000", fontSize = "14px")
        )
      )
    )
  ) %>%
  hc_tooltip(
    formatter = JS(
      "
      function(){return(this.point.hc_ttip)}
      "
    ),
    shape = "square",
    borderWidth = 0
  )

Chart_hosp_FRA


###########################
### graph : chart map hospitalisation
#source('1_code/chart_map_hosp.R')



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
    text = "Depuis Mars 2020 <br> La Guyane pr?sente le plus fort taux d'hospitalisation", 
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


##############################
### graph : map reanimation

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
            rea = sum(incid_rea, na.rm = T),
            hosp_rel = (hosp/pop)*100000 ,
            rea_rel = (rea/pop)*100000 ,
            rad = sum(incid_rad, na.rm = T),
            hop_inf = sum(hop_inf, na.rm = T)) %>%
  ungroup

REG_tamp <- DEP_tamp %>%
  group_by(`hc-a2`) %>%
  summarise(reg_name = first(reg_name),
            hosp = sum(hosp, na.rm = T),
            pop = sum(pop, na.rm = T),
            rea = sum(rea, na.rm = T),
            hosp_rel = (hosp/pop)*100000 ,
            rea_rel = (rea/pop)*100000 ,
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
  
  #subset uscountygeojson
  frgeoson_dep.subset <- frgeoson_dep
  frgeoson_dep.subset$features <- frgeoson_dep$features[which(dep_code$`hc-a2` == reg)]
  
  #subset county data
  data_filter <- filter(DEP_tamp, `hc-a2` == reg) %>%
    transmute(value = rea_rel, `hc-a2` = hc_a2, cnlabel = dep_name, stlabel = reg_name, rea_rel = rea_rel, hosp_rel = hosp_rel, hosp = hosp, rea = rea) %>%
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
                           "<b style=\"color:red\">R?animations (pour 100000):</b> {point.rea_rel:.0f}<br>",
                           "<b style=\"color:#1874CD\">Hospitalisations (pour 100000):</b> {point.hosp_rel:.0f}<br>"
      ),
      footerFormat = "</p>")
  )
}




##Make the graph


#leave only necessary variables from state df
dt.st <- REG_tamp %>%
  transmute(value = rea_rel, code = `hc-a2`, hosp_rel = hosp_rel, rea_rel = rea_rel, hosp = hosp, rea = rea, stlabel = reg_name, drilldown = `hc-a2`,
            hc_dataLabel = case_when(
              rea_rel == max(rea_rel, na.rm = T) ~ glue(" ({stlabel}) : <i> {round(rea_rel)} pour 100000</i> "),
              rea_rel == min(rea_rel, na.rm = T) ~ glue(" ({stlabel}) : <i> {round(rea_rel)} pour 100000</i> "),
              TRUE ~ "" # No label
            ))
ds.st <- list_parse(dt.st)

#create drilldown series
series.list <- lapply(dt.st$drilldown, build_series)

#tooltim for state map has to be coded separately from tooltip for county
map_rea <- highchart(type = 'map') %>%
  hc_add_series(
    mapData = frgeoson_reg, data = ds.st, joinBy = c("hc-a2","code"),
    borderWidth = 0.1, borderColor = "#FAFAFA", dataLabels = list(enabled = TRUE, formatter = JS("function(){return(this.point.hc_dataLabel)}")),
    tooltip = list(
      useHTML = TRUE,
      headerFormat = "<p>",
      pointFormat = paste0("<b>{point.stlabel}</b><br>",
                           "<b style=\"color:red\">R?animations (pour 100000):</b> {point.rea_rel:.0f}<br>",
                           "<b style=\"color:#1874CD\">Hospitalisations (pour 100000):</b> {point.hosp_rel:.0f}<br>"
      ),
      footerFormat = "</p>"
    )) %>%
  hc_title(text = "Nombre de cas de réanimations pour 100000 habitants", 
           style = list(
             fontWeight = "bold",
             fontSize = "28px"
           ), 
           align = "left"
  ) %>%
  hc_subtitle(
    text = "Depuis Mars 2020 <br> Paris pr?sente le plus fort taux de r?animations devant la Guyane (Max taux d'hospitalisation)",
    style = list(
      color = "#000000", 
      fontStyle = "italic",
      fontSize = "18px"
    ), 
    align = "left"
  ) %>%
  hc_plotOptions(map = list(states = list(hover = list(color = '#a7bcdc')))) %>%
  hc_colorAxis(minColor = "#f0d5d1", maxColor = "#FF5733") %>%
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

### Graph : prise de rendez-vous


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

### Graph : centre vaccinaux

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
chart_map_centres_vaccinaux <- highchart(type = 'map') %>%
  hc_add_series(
    mapData = frgeoson_dep, data = ds.st, joinBy = c("hc-a2","code"),
    borderWidth = 0.1, borderColor = "#FAFAFA", dataLabels = list(enabled = TRUE, allowOverlap = TRUE, formatter = JS("function(){return(this.point.hc_dataLabel)}")),
    tooltip = list(
      useHTML = TRUE,
      headerFormat = "<p>",
      pointFormat = paste0("<b>{point.stlabel}</b><br>",
                           "<b style=\"color:#1874CD\">Centre vaccinal (pour 100000):</b> {point.nbr_etbl_pour_100mill_hab:.0f}<br>",
                           "<b style=\"color:red\">R?animations (pour 100000):</b> {point.rea_rel:.0f}<br>"
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
    text = "Paris n'a que 3 centres vaccinaux pour 100000 alors qu'il a le plus fort taux de r?animations", 
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

#### Graph : Couverture vaccinale et taux reanimations

data <- FRA_vaccine_sexe %>%
  mutate(
    Sexe = case_when(
      sexe == 0 ~ "Tous",
      sexe == 1 ~ "Homme",
      sexe == 2 ~ "Femme"
    ),
    jour = as.Date(jour),
    mois = format(jour, "%b"),
    annee = format(jour, "%Y"),
    mois_annee = format(jour, "%Y %m")
  ) %>%
  arrange(jour) %>%
  group_by(mois_annee, sexe, Sexe) %>%
  summarise(mois = last(mois),
            annee = last(annee),
            jour = last(jour),
            couv_dose1 = last(couv_dose1),
            couv_complet = last(couv_complet),
            couv_rappel = last(couv_rappel)) %>%
  ungroup %>%
  left_join(FRA_mois_indicateurs_hosp %>% select(mois_annee, part_rea), keep = FALSE) %>%
  mutate(taux_rea = round(part_rea * 100, 1))



data = 
  data %>%
  mutate(
    # Tooltip
    hc_ttip = 
      glue(
        "
        <b> {mois} {annee} </b> <br>
        Couverture vaccinale : {round(couv_complet)} <br>
        Taux de r?animations : {round(taux_rea)} <br>
        "
      )
  )

mygold = "#FDB927"
myblue = "#a7bcdc"
mypurple = "#552583"
myred = "#FF5733"
myredlight = "#f0d5d1"
mygreen = "#03989E"
mygreenlight = "#e9f1f1"

seuil1 = "70"
seuil2 = "80"
seuil3 = "90"


Chart_couv_FRA = 
  data %>%
  hchart(
    "areaspline", 
    hcaes(x = mois_annee, y = couv_complet), 
    name = "Couverture vaccinale", 
    showInLegend = TRUE, 
    color = mygreen,
    borderWidth = 0.1, 
    borderColor = "#FAFAFA",
    fillOpacity = 0.5,
    dataLabels = list(
      visible = FALSE
    )
  ) %>%
  hc_add_series(
    data,
    "areaspline", 
    hcaes(x = mois_annee, y = taux_rea), 
    name = "Taux de reanimations", 
    showInLegend = TRUE, 
    color = myred,
    borderWidth = 0.1,
    fillOpacity = 0.5,
    borderColor = "transparent"
  ) %>%
  hc_chart(style = list(fontFamily = "Rubik")) %>%
  hc_title(
    text = "Couverture vaccinale et taux de réanimations", 
    style = list(
      color = "#000000", 
      fontWeight = "bold",
      fontSize = "28px"
    ), 
    align = "left"
  ) %>%
  hc_subtitle(
    text = "Décembre 2020 à Décembre 2021", 
    style = list(
      color = "#000000", 
      fontStyle = "italic",
      fontSize = "18px"
    ), 
    align = "left"
  ) %>%
  hc_yAxis(
    gridLineWidth = 0, 
    labels = list(style = list(color =  "#000000")),
    title = list(text = "Proportion (%)", style = list(color = "#000000")),
    plotBands = list(
      list(
        from = 0,
        to = 70, 
        color = myredlight,
        label = list(
          text = "Zone critique",
          style = list(fontWeight = "bold", color = "#000000", fontSize = "14px")
        )
      ),
      list(
        from =70,
        to = 80, 
        color = mygreenlight,
        label = list(
          text = "Transmission du virus stoppée - 70% à 80%",
          style = list(fontWeight = "bold", color = "#000000", fontSize = "14px")
        )
      ),
      list(
        from = 80,
        to = 100, 
        color = "#d3f0f0",
        label = list(
          text = "Retour à la vie normale - 80% à 100%",
          style = list(fontWeight = "bold", color = "#000000", fontSize = "14px")
        )
      )
    )
  ) %>%
  hc_xAxis(
    labels = list(style = list(color =  "#000000")),
    title = list(text= ""),
    startOnTick = TRUE,
    endOnTick = TRUE,
    lineWidth = 0,
    tickWidth = 0
  ) %>% 
  hc_plotOptions(
    series = list(
      grouping = FALSE,
      borderwidth = 0
    )
  ) %>%
  hc_tooltip(
    shared = TRUE,
    shape = "square",
    borderWidth = 0
  )

Chart_couv_FRA

#### Graph : Char bar racing hospitalisation
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


chart_bar_racing_hosp <- df %>%
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


#### Graph : Char bar racing rea


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


chart_bar_racing_rea <-  df %>%
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

#### Couverture vaccinale liquide

## Couverture vaccinale
data <- FRA_vaccine_tot %>%
  filter(sexe == 0)

liquid <- data.frame(val = c(data$couv_tot_complet/100, data$couv_tot_dose1/100, data$couv_tot_rappel/100))

likid <- liquid |> 
  e_charts() |> 
  e_liquid(val) 

### Nombre par tranche d'age
# age ######################################################
url_age="https://www.data.gouv.fr/fr/datasets/r/08c18e08-6780-452d-9b8c-ae244ad529b3"
agdata <- read.csv(url(url_age),
                   sep = ";",dec=".", header = T)
agdata<-filter(agdata, cl_age90 !=0)

agdata_mois <- agdata %>%
  mutate(jour = as.Date(jour),
         mois = format(jour, "%b"),
         annee = format(jour, "%Y"),
         mois_annee = format(jour, "%Y %m"))



agdata_mois <- mutate(agdata_mois,
                      Age = case_when(
                        cl_age90 <= 19 ~ "0 - 19 ans",
                        (cl_age90 >= 20) & (cl_age90 <= 49) ~ "20 - 49 ans",
                        (cl_age90 >= 50) & (cl_age90 <= 69) ~ "50 - 69 ans",
                        cl_age90 >=70 ~ "70 ans et +"
                      ))
bagdata <- agdata_mois %>%
  group_by(mois_annee,Age) %>%dplyr::summarise(bagrea = sum(rea))

donnee_pop_age <- readxl::read_xlsx('0_data/Demographie/demo-pop-sexe-age-effectif.xlsx')

bagdata = bagdata %>%
  left_join(donnee_pop_age, by = c("Age"="Groupe age"), keep = FALSE)

bagdata <- bagdata %>%
  mutate(`nbr_rea_par_age_pour_100mill_hab` = round(bagrea * 100000/Total,2))

#cagdata <- bagdata %>%
#group_by(Date) %>%dplyr::summarise(cagrea = sum(bagrea))
#dagdata <- left_join(bagdata,cagdata,
#by = c("Date" = "Date"))
#datafin <- mutate(dagdata,rea=bagrea/cagrea)


tranch_age <- highchart() %>%
  hc_add_series(data = bagdata,
                mapping = hcaes(x = mois_annee,y=nbr_rea_par_age_pour_100mill_hab,group = Age),
                type = 'spline') %>%
  hc_plotOptions(series = list(
    label = list(connectorAllowed = F))) %>%
  hc_legend(layout = 'vertical',
            align ='right',
            verticalAlign = 'middle') %>%
  hc_responsive(list(
    chartOptions = list(legend = list(layout = 'horizontal',
                                      align = 'center',
                                      verticalAlign = 'bottom'))
  ))%>%
  hc_xAxis(
    categories = unique(bagdata$mois_annee)
  )%>%
  hc_yAxis(title= list(text="Nombre mensuel de cas pour 100 000 hbts"))%>%
  hc_title(text="Taux de réanimation par tranche d'âge",style = list(
    color = "#000000",
    fontWeight = "bold",
    fontSize = "28px"
  ),
  align = "left") %>%
  hc_subtitle(
    text = "Les réanimations chez les moins de 19 ans sont très faibles et constant sur la période observée.",
    style = list(
      color = "#000000",
      fontStyle = "italic",
      fontSize = "18px"
    ),
    align = "left")

tranch_age

#### graph : rea first
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

#### Generation du report (HTML)
rmarkdown::render(
  'Covid_RMD_mod2.Rmd',
  output_file = paste0('2_output/Dataviz_vizact.html'),encoding = 'UTF-8'
)
