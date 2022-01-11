
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

## Changerment fonction import map
source('1_code/0_functions/imp_donnee_map.R')

########  HOSPITALISATION #######
# (bases journalieres)
  

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
                          incid_hosp = sum(incid_hosp),
                          incid_rea = sum(incid_rea),
                          incid_rad = sum(incid_rad),
                          hop_inf = sum(hop_inf)) %>%
                ungroup

# (bases agrégés)
REG_agg_hosp <- REG_hosp %>%
  group_by(reg) %>%
  summarise(nomReg = first(nomReg),
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

########  VACCINATION #######
# (bases journalières)
FRA_vaccine <- read.csv2("https://www.data.gouv.fr/fr/datasets/r/b273cf3b-e9de-437c-af55-eda5979e92fc")


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
                ) %>%
                group_by(vague) %>%
                filter(hosp == max(hosp)) %>%
                ungroup %>%
                filter(vague != 0) %>%
                select(jour,vague)

