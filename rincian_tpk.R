library(tidyr)
library(data.table)
library(tidyverse)
data_tpk <- data.table(fread("data/daftar_tpk.csv"))
data_tpk <- data_tpk %>%
  select(Kab, Kec, `Desa/Kel`, `No Register`)

data_tpk$`No Register` <- format(data_tpk$`No Register`, scientific = F)

pendampingan_bumil <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/12jGov8gxvlD6_D0ySGkdgiUriOQ2AEYGqIcGprJnra0/edit?gid=1855711036#gid=1855711036")
pendampingan_bumil$no_register_tpk <- format(pendampingan_bumil$no_register_tpk, scientific = F)
pendampingan_bumil <- dplyr::left_join(data_tpk, pendampingan_bumil, 
                                       by = c("Kab" = "kota", "Kec" = "kecamatan", 
                                              "Desa/Kel" = "kelurahan", 
                                              "No Register" = "no_register_tpk")) 
pendampingan_bumil$prov <- "SULAWESI BARAT"
pendampingan_bumil <- pendampingan_bumil %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  mutate(`Keterangan Bumil` = ifelse(total_pendampingan <= 0, "TIDAK MENDAMPINGI", "MENDAMPINGI")) %>% #tambahan
  select(prov, Kab, Kec, `Desa/Kel`, `No Register`, `Keterangan Bumil`) # tambahan

#write.fst(pendampingan_bumil, "data/pendampingan_bumil.fst")

### catin
pendampingan_catin <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1ytoFze2zjF5l_plWwDRe_ZeAELrqtEVEDqjrt408vHc/edit?gid=1855711036#gid=1855711036")
pendampingan_catin$no_register_tpk <- format(pendampingan_catin$no_register_tpk, scientific = F)
pendampingan_catin <- dplyr::left_join(data_tpk, pendampingan_catin, 
                                       by = c("Kab" = "kota", "Kec" = "kecamatan", 
                                              "Desa/Kel" = "kelurahan", 
                                              "No Register" = "no_register_tpk")) 
pendampingan_catin$prov <- "SULAWESI BARAT"
pendampingan_catin <- pendampingan_catin %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  mutate(`Keterangan Catin` = ifelse(total_pendampingan <= 0, "TIDAK MENDAMPINGI", "MENDAMPINGI")) %>% #tambahan
  select(prov, Kab, Kec, `Desa/Kel`, `No Register`, `Keterangan Catin`) # tambahan

write.fst(pendampingan_catin, "data/pendampingan_catin.fst")

### pascasalin
pendampingan_pascasalin <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1YAD01NYkOG_jjrWA-GQ_13na10rdiUop30ceettN6sY/edit?gid=1855711036#gid=1855711036")
pendampingan_pascasalin$no_register_tpk <- format(pendampingan_pascasalin$no_register_tpk, scientific = F)
pendampingan_pascasalin <- dplyr::left_join(data_tpk, pendampingan_pascasalin, 
                                       by = c("Kab" = "kota", "Kec" = "kecamatan", 
                                              "Desa/Kel" = "kelurahan", 
                                              "No Register" = "no_register_tpk")) 
pendampingan_pascasalin$prov <- "SULAWESI BARAT"
pendampingan_pascasalin <- pendampingan_pascasalin %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  mutate(`Keterangan Pascasalin` = ifelse(total_pendampingan <= 0, "TIDAK MENDAMPINGI", "MENDAMPINGI")) %>% #tambahan
  select(prov, Kab, Kec, `Desa/Kel`, `No Register`, `Keterangan Pascasalin`) # tambahan

write.fst(pendampingan_pascasalin, "data/pendampingan_pascasalin.fst")

### baduta
pendampingan_baduta <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1-MChjqWwio75Qe7KMi46nLvxL8u87TXBNoxeABuu6E0/edit?gid=1855711036#gid=1855711036")
pendampingan_baduta$no_register_tpk <- format(pendampingan_baduta$no_register_tpk, scientific = F)
pendampingan_baduta <- dplyr::left_join(data_tpk, pendampingan_baduta, 
                                            by = c("Kab" = "kota", "Kec" = "kecamatan", 
                                                   "Desa/Kel" = "kelurahan", 
                                                   "No Register" = "no_register_tpk")) 
pendampingan_baduta$prov <- "SULAWESI BARAT"
pendampingan_baduta <- pendampingan_baduta %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  mutate(`Keterangan Baduta` = ifelse(total_pendampingan <= 0, "TIDAK MENDAMPINGI", "MENDAMPINGI")) %>% #tambahan
  select(prov, Kab, Kec, `Desa/Kel`, `No Register`, `Keterangan Baduta`) # tambahan

  

write.fst(pendampingan_baduta, "data/pendampingan_baduta.fst")

### TPK MENDAMPINGI
data_pkb <- fst::read.fst("data/data_pkb.fst")

data_tpk_MENDAMPINGI <- pendampingan_baduta %>%
  inner_join(pendampingan_bumil, by = c("prov", "Kab", "Kec", "Desa/Kel", "No Register")) %>%
  inner_join(pendampingan_catin, by = c("prov", "Kab", "Kec", "Desa/Kel", "No Register")) %>%
  inner_join(pendampingan_pascasalin, by = c("prov", "Kab", "Kec", "Desa/Kel", "No Register")) %>%
  mutate(`Keterangan Akhir` = ifelse(
    rowSums(select(., starts_with("Keterangan")) == "MENDAMPINGI") > 0,
    "AKTIF",
    "TIDAK AKTIF"
  )) %>%
  inner_join(data_pkb, by = c("Kec" = "Kecamatan", "Desa/Kel" = "Kelurahan"))
  
openxlsx::write.xlsx(data_tpk_MENDAMPINGI, "data_tpk_MENDAMPINGI.xlsx")
####
# Sintaks berantai

data_pendampingan <- as.data.table(read_fst("data/pendampingan_bumil.fst"))
gt_wil_tpk <- data_pendampingan[, .SD[`No Register` == "7605062002002"]]

gt_bumil_wil <- melt(gt_wil_tpk, id.vars = c("prov"), 
                 measure.vars = c("prov", "Kab", "Kec", "Desa/Kel", "No Register"),
                 variable.name = "Indikator", 
                 value.name = "Nilai")[
                   , .(Indikator, Nilai)
                 ]

gt_bumil_wil$Indikator <- toupper(gt_bumil_wil$Indikator)


gt(gt_bumil_wil) %>%
  tab_style(
    style = list(
      cell_text(align = "left")
    ),
    locations = cells_column_labels(columns = c(Indikator)) # Ganti 'kota' dengan nama kolom yang ingin Anda rata kiri
  ) %>%
  tab_style(
    style = list(
      cell_text(align = "left") # Mengatur teks agar rata kiri
    ),
    locations = cells_body(columns = c(Indikator)) # Terapkan pada semua kolom
  ) %>%
  cols_label(
    Indikator = "",
    Nilai = ""
  ) 


####
pendampingan_baduta <- as.data.table(read_fst("data/capaian_baduta.fst"))
pendampingan_baduta <- pendampingan_baduta[, .SD[no_register_tpk == "7605062001001"]]

pendampingan_bumil <- as.data.table(read_fst("data/capaian_bumil.fst"))
pendampingan_bumil <- pendampingan_bumil[, .SD[no_register_tpk == "7605062001001"]]

pendampingan_pascasalin <- as.data.table(read_fst("data/capaian_pascasalin.fst"))
pendampingan_pascasalin <- pendampingan_pascasalin[, .SD[no_register_tpk == "7605062001001"]]

pendampingan_catin <- as.data.table(read_fst("data/capaian_catin.fst"))
pendampingan_catin <- pendampingan_catin[, .SD[no_register_tpk == "7605062001001"]]

data_spider_pendampingan <- data.table(
  Sasaran = c("Catin", "Bumil", "Pascasalin", "Baduta"),
  Capaian = c(pendampingan_catin$capaian, pendampingan_bumil$capaian_bumil,
              pendampingan_pascasalin$capaian,pendampingan_baduta$capaian_baduta)
)

data_spider_pendampingan[, Capaian := ifelse(Capaian > 100 | is.infinite(Capaian), 100, Capaian)]

plot_ly(type = 'scatterpolar', mode = 'lines+markers') %>%
  add_trace(
    r = data_spider_pendampingan$Capaian,
    theta = data_spider_pendampingan$Sasaran,
    name = 'Sasaran',
    fill = 'toself',
    text = paste0("Capaian ", data_spider_pendampingan$Sasaran, ": ", data_spider_pendampingan$Capaian, "%"),
    hoverinfo = "text"
  ) %>%
  layout(
    polar = list(
      radialaxis = list(visible = TRUE, range = c(0, 100))
    ),
    showlegend = TRUE
  )
