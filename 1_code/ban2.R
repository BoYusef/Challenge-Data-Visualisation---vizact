
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

Chart_hosp_FRA = 
  data %>%
  hchart(
    "column", 
    hcaes(x = mois_annee, y = hosp), 
    name = "Hospitalisation", 
    showInLegend = TRUE, 
    color = myblue,
    borderColor = "transparent",
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
    text = "Mars 2020 à Decembre 2021", 
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

