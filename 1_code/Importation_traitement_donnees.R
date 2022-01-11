## table de correspondance
reg <- read.csv("0_data\\table_correspondance_carto\\regions.csv") %>%
  select(code,name)%>%
  rename("reg_name" = "name") 

dep <- read.csv("0_data\\table_correspondance_carto\\departments.csv") %>%
  select(region_code,code,name)%>%
  rename("dep_name" = "name") %>%
  left_join(reg, by = c("region_code"="code"), keep = FALSE)
## chargement des tables de correspondance 
reg_corres <- readxl::read_xlsx('0_data\\table_correspondance_carto\\tab_region_corr.xlsx')
dep_corres <- readxl::read_xlsx('0_data\\table_correspondance_carto\\tab_departement_corr.xlsx') %>%
  left_join(reg_corres %>% select('hc-a2', 'code_insee_reg'), 
            by = c('code_insee_reg'),
            keep = FALSE
  ) %>%
  mutate(code_insee_dep = as.numeric(code_insee_dep))


## Donnees demographique
donnee_population <- readxl::read_xlsx('0_data/Demographie/departement.xlsx')

## Changerment fonction import map
source('1_code/0_functions/imp_donnee_map.R')

########  HOSPITALISATION #######
# (bases journalières)


########  HOSPITALISATION #######
# (bases journalières)



REG_incid_rea <- read.csv2("https://www.data.gouv.fr/fr/datasets/r/a1466f7f-4ece-4158-a373-f5d4db167eb0", dec = ".") %>%
  rename("reg" = "numReg")
REG_indicateurs_hosp <- read.csv2("https://www.data.gouv.fr/fr/datasets/r/46f145d4-9607-45a0-bc3c-86241136ca24", dec = "." )
REG_donnees_hosp <- read.csv2("https://www.data.gouv.fr/fr/datasets/r/08c18e08-6780-452d-9b8c-ae244ad529b3", dec = "." )
REG_new_hosp_hebdo <- read.csv2("https://www.data.gouv.fr/fr/datasets/r/dc7663c7-5da9-4765-a98b-ba4bc9de9079", dec = "." )
REG_hosp <- REG_incid_rea %>%
  left_join(REG_indicateurs_hosp, by = c("reg", "jour"), keep = FALSE) %>%
  left_join(REG_donnees_hosp %>% filter(cl_age90 == 0), by = c("reg", "jour"), keep = FALSE)

DEP_donnees_hosp_sex <- read.csv2("https://www.data.gouv.fr/fr/datasets/r/63352e38-d353-4b54-bfd1-f1b3ee1cabd7", dec = "." )
DEP_hosp_inf <- read.csv2("https://www.data.gouv.fr/fr/datasets/r/41b9bd2a-b5b6-4271-8878-e45a8902ef00", dec = "." ) %>%
  rename("hop_inf" = "nb")
DEP_hosp_incid <- read.csv2("https://www.data.gouv.fr/fr/datasets/r/6fadff46-9efd-4c53-942a-54aca783c30c", dec = ".")
DEP_hosp <- DEP_hosp_incid %>%
  left_join(DEP_hosp_inf, by = c("dep", "jour"), keep = FALSE) %>%
  left_join(dep, by = c("dep" = "code"), keep = FALSE)

FRA_indicateurs_hosp <- DEP_hosp %>%
  group_by(jour) %>%
  summarise(hosp = sum(incid_hosp),
            rea = sum(incid_rea),
            dc = sum(incid_dc),
            rad = sum(incid_rad),
            hop_inf = sum(hop_inf)) %>%
  ungroup %>%
  mutate(part_rea = rea/hosp) 

# (bases mensuelles)
FRA_mois_indicateurs_hosp <- FRA_indicateurs_hosp %>%
  mutate(jour = as.Date(jour),
         mois = format(jour, "%b"),
         annee = format(jour, "%Y"),
         mois_annee = format(jour, "%Y %m")) %>%
  group_by(mois_annee) %>%
  summarise(mois = first(mois),
            annee = first(annee),
            hosp = sum(hosp),
            rea = sum(rea),
            dc = sum(dc),
            rad = sum(rad),
            hop_inf = sum(hop_inf),
            part_rea = rea/hosp) %>%
  ungroup

REG_mois_hosp <- REG_hosp %>%
  mutate(jour = as.Date(jour),
         mois = format(jour, "%b"),
         annee = format(jour, "%Y"),
         mois_annee = format(jour, "%Y %m")) %>%
  group_by(reg,mois_annee) %>%
  summarise(mois = first(mois),
            annee = first(annee),
            nomReg = first(nomReg),
            incid_rea = sum(incid_rea),
            tx_prev_hosp = mean(tx_prev_hosp),
            tx_prev_SC = mean(tx_prev_SC),
            hosp = sum(hosp),
            rea = sum(rea),
            HospConv = sum(HospConv),
            SSR_USLD = sum(SSR_USLD),
            autres = sum(autres),
            rad = sum(rad)) %>%
  ungroup

DEP_mois_hosp <- DEP_hosp %>%
  mutate(jour = as.Date(jour),
         mois = format(jour, "%b"),
         annee = format(jour, "%Y"),
         mois_annee = format(jour, "%Y %m")) %>%
  group_by(dep,mois_annee) %>%
  summarise(mois = first(mois),
            annee = first(annee),
            jour = last(jour),
            dep_name = first(dep_name),
            incid_hosp = sum(incid_hosp),
            incid_rea = sum(incid_rea),
            incid_rad = sum(incid_rad),
            hop_inf = sum(hop_inf)) %>%
  ungroup

# (bases agrégés)
REG_agg_hosp <- REG_hosp %>%
  group_by(reg) %>%
  summarise(nomReg = first(nomReg),
            incid_rea = sum(incid_rea, na.rm = T),
            tx_prev_hosp = mean(tx_prev_hosp, na.rm = T),
            tx_prev_SC = mean(tx_prev_SC, na.rm = T),
            hosp = sum(hosp, na.rm = T),
            rea = sum(rea, na.rm = T),
            HospConv = sum(HospConv, na.rm = T),
            SSR_USLD = sum(SSR_USLD, na.rm = T),
            autres = sum(autres, na.rm = T),
            rad = sum(rad, na.rm = T)) %>%
  ungroup

########  VACCINATION #######
# (bases journalières)
FRA_vaccine <- read.csv2("https://www.data.gouv.fr/fr/datasets/r/b273cf3b-e9de-437c-af55-eda5979e92fc")
DEP_vaccine <- read.csv2("https://www.data.gouv.fr/fr/datasets/r/7969c06d-848e-40cf-9c3c-21b5bd5a874b", dec = ".")
FRA_vaccine_sexe <- read.csv2("https://www.data.gouv.fr/fr/datasets/r/349ca785-cf12-4f4d-9a0a-846d53dce996", dec = ".")
FRA_vaccine_tot <- read.csv2("https://www.data.gouv.fr/fr/datasets/r/e49fec64-0080-4592-bdab-d199de09a7a8", dec = ".")

# (bases mensuelle)
FRA_mois_vaccine <- FRA_vaccine %>%
  filter(vaccin == 0) %>%
  mutate(jour = as.Date(jour),
         mois = format(jour, "%b"),
         annee = format(jour, "%Y"),
         mois_annee = format(jour, "%Y %m")) %>%
  group_by(mois_annee) %>%
  summarise(mois = first(mois),
            annee = first(annee),
            n_dose1 = sum(n_dose1),
            n_dose2 = sum(n_dose2),
            n_dose3 = sum(n_dose3),
            n_dose4 = sum(n_dose4),
            n_rappel = sum(n_rappel)) %>%
  ungroup %>%
  mutate(n_cum_dose1 = cumsum(n_dose1),
         n_cum_dose2 = cumsum(n_dose2),
         n_cum_dose3 = cumsum(n_dose3),
         n_cum_dose4 = cumsum(n_dose4),
         n_cum_rappel = cumsum(n_rappel),
         couv_dose1 = n_cum_dose1 / 67407241,
         couv_dose2 = n_cum_dose2 / 67407241,
         couv_dose3 = n_cum_dose3 / 67407241,
         couv_dose4 = n_cum_dose4 / 67407241)


########  BASE SUR LES VAGUES #######

vagues_COVID <- FRA_indicateurs_hosp %>%
  mutate(
    vague = case_when(
      jour <= "2020-05-31" ~ 1,
      jour >= "2020-09-01" & jour <= "2020-11-30" ~ 2,
      jour >= "2021-03-01" & jour <= "2021-04-30" ~ 3,
      jour >= "2021-07-01" & jour <= "2021-08-31" ~ 4,
      jour >= "2021-11-01" ~ 5,
      TRUE ~ 0 # No label
    )
  ) 

vagues_max_COVID <- vagues_COVID %>%
  group_by(vague) %>%
  filter(hosp == max(hosp)) %>%
  ungroup %>%
  filter(vague != 0) %>%
  select(jour,vague)

## Chargement des maps france (region et departement)

url_dep = "https://code.highcharts.com/mapdata/countries/fr/fr-all-all.geo.json"
url_reg = "https://code.highcharts.com/mapdata/countries/fr/fr-all.geo.json"

frgeoson_reg = download_map(url_reg)
frgeoson_dep = download_map(url_dep)


### Theme graph
hc_theme_hcCookbook = 
  hc_theme_merge(
    hc_theme_smpl(),
    hc_theme(
      colors = list("#f94144","#f3722c","#f8961e","#f9c74f","#90be6d","#43aa8b","#577590"),
      chart = list(style = list(fontFamily = "Roboto")),
      title = list(style = list(fontFamily = "Roboto"), align = "left"),
      subtitle = list(style = list(fontFamily = "Roboto"), align = "left")
    )
  )


######## VACCINATION #######
# (Nombre de structure vaccinale)
DEP_nb_vaccine <- read.csv2("https://www.data.gouv.fr/fr/datasets/r/5cb21a85-b0b0-4a65-a249-806a040ec372")

# Au niveau dep
DEP_nb_vaccine_f = DEP_nb_vaccine %>%
  select(com_insee,com_nom)
DEP_nb_vaccine_f$dep = ifelse(as.numeric(substr(DEP_nb_vaccine_f$com_insee, start = 1, stop = 2))==97,
                              substr(DEP_nb_vaccine_f$com_insee, start = 1, stop = 3),
                              ifelse(nchar(DEP_nb_vaccine_f$com_insee==5),
                                     substr(DEP_nb_vaccine_f$com_insee, start = 1, stop = 2),
                                     substr(DEP_nb_vaccine_f$com_insee, start = 1, stop = 1)))

DEP_nb_vaccine_f$dep = ifelse(is.na(DEP_nb_vaccine_f$dep),
                              substr(DEP_nb_vaccine_f$com_insee, start = 1, stop = 2),
                              DEP_nb_vaccine_f$dep)

vaccin_etb_dep = DEP_nb_vaccine_f %>%
  group_by(dep) %>%
  summarise(Count = n()) %>%
  ungroup

dep_pop <- donnee_population %>%
  select(code_insee,pop)

vaccin_etb_dep = vaccin_etb_dep %>%
  left_join(dep_pop, by = c("dep"="code_insee"), keep = FALSE)

vaccin_etb_dep <- vaccin_etb_dep[complete.cases(vaccin_etb_dep),]

vaccin_etb_dep <- vaccin_etb_dep %>%
  mutate(`nbr_etbl_pour_100mill_hab` = round(Count * 100000/pop,2))


# (Nombre de rdv)
Dep_nb_rdv <- read.csv2("https://www.data.gouv.fr/fr/datasets/r/59aeab47-c364-462c-9087-ce233b6acbbc",
                        sep = ",")

nb_rdv_dep = Dep_nb_rdv %>%
  group_by(departement) %>%
  summarise(nb_rdv = sum(nb))%>%
  ungroup

nb_rdv_dep = nb_rdv_dep %>%
  left_join(dep_pop, by = c("departement"="code_insee"), keep = FALSE)

nb_rdv_dep <- nb_rdv_dep %>%
  mutate(`nbr_rdv_pour_100mill_hab` = round(nb_rdv * 100000/pop,2))