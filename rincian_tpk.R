library(tidyr)
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
  mutate(across(everything(), ~ replace_na(., 0)))

write.fst(pendampingan_bumil, "data/pendampingan_bumil.fst")

### catin
pendampingan_catin <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1ytoFze2zjF5l_plWwDRe_ZeAELrqtEVEDqjrt408vHc/edit?gid=1855711036#gid=1855711036")
pendampingan_catin$no_register_tpk <- format(pendampingan_catin$no_register_tpk, scientific = F)
pendampingan_catin <- dplyr::left_join(data_tpk, pendampingan_catin, 
                                       by = c("Kab" = "kota", "Kec" = "kecamatan", 
                                              "Desa/Kel" = "kelurahan", 
                                              "No Register" = "no_register_tpk")) 
pendampingan_catin$prov <- "SULAWESI BARAT"
pendampingan_catin <- pendampingan_catin %>%
  mutate(across(everything(), ~ replace_na(., 0)))

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
  mutate(across(everything(), ~ replace_na(., 0)))

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
  mutate(across(everything(), ~ replace_na(., 0)))

write.fst(pendampingan_baduta, "data/pendampingan_baduta.fst")

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
