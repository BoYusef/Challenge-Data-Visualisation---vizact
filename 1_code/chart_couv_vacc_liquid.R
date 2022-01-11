
## Couverture vaccinale
data <- FRA_vaccine_tot %>%
  filter(sexe == 0)

liquid <- data.frame(val = c(data$couv_tot_complet/100, data$couv_tot_dose1/100, data$couv_tot_rappel/100))

liquid |> 
  e_charts() |> 
  e_liquid(val) 
