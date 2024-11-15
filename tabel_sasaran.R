library(gt)
library(dplyr)
library(tidyr)
library(data.table)

data_catin_melapor <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1ytoFze2zjF5l_plWwDRe_ZeAELrqtEVEDqjrt408vHc/edit?gid=1849424246#gid=1849424246")
write.fst(data_catin_melapor, "data/capaian_catin.fst")

data_catin_melapor <- data.table(read_fst("data/capaian_catin.fst"))
is.data.table(data_catin_melapor)

# Sintaks berantai
gt_catin <- data_catin_melapor[Kab == "POLEWALI MANDAR" & 
                                 Kec == "TINAMBUNG" & 
                                 `Desa/Kel` %in% c("TINAMBUNG", "KARAMA"), 
                               .(`Target TPK` = sum(target_tpk, na.rm = TRUE), 
                                 `Total Pendampingan` = sum(total_pendampingan, na.rm = TRUE), 
                                 `Total Orang` = sum(total_orang, na.rm = TRUE),
                                 `Rasio Total` = round(sum(total_pendampingan) / sum(total_orang), 2),
                                 Capaian = round(sum(total_orang) / sum(target_tpk) * 100, 2)),
                               by = Prov]

gt_catin <- melt(gt_catin, id.vars = c("Prov"), 
     measure.vars = c("Target TPK", "Total Pendampingan", "Total Orang", "Rasio Total", "Capaian"),
     variable.name = "Indikator", 
     value.name = "Nilai")[
       , .(Indikator, Nilai)
     ]

gt(gt_catin) %>%
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
  )

##pasca
capaian_pasca <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1YAD01NYkOG_jjrWA-GQ_13na10rdiUop30ceettN6sY/edit?gid=1685367814#gid=1685367814")

capaian_pasca <- as.data.table(capaian_pasca)
capaian_pasca$no_register_tpk <- format(capaian_pasca$no_register_tpk, scientific = F)
write.fst(capaian_pasca, "data/capaian_pascasalin.fst")

##BADUTA
capaian_baduta <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1-MChjqWwio75Qe7KMi46nLvxL8u87TXBNoxeABuu6E0/edit?gid=1685367814#gid=1685367814")

capaian_baduta <- as.data.table(capaian_baduta)
capaian_baduta$no_register_tpk <- format(capaian_baduta$no_register_tpk, scientific = F)
write.fst(capaian_baduta, "data/capaian_baduta.fst")


##bumil
capaian_bumil <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/12jGov8gxvlD6_D0ySGkdgiUriOQ2AEYGqIcGprJnra0/edit?gid=1685367814#gid=1685367814")

capaian_bumil <- as.data.table(capaian_bumil)
capaian_bumil$no_register_tpk <- format(capaian_bumil$no_register_tpk, scientific = F)
write.fst(capaian_bumil, "data/capaian_bumil.fst")


