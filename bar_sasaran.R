library(gt)
library(dplyr)
library(tidyr)
library(data.table)
library(fst)
library(plotly)

# baduta
efektifitas_baduta <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1ZF66HwQK2-sKkE5eMRdgXDrQrtIHgd10yha8Jq3oEy0/edit?gid=0#gid=0")
efektifitas_baduta$no_register_tpk <- format(efektifitas_baduta$no_register_tpk, scientific = F)

efektifitas_baduta_dplyr <- efektifitas_baduta %>%
  select(prov, kota, kecamatan, kelurahan, no_register_tpk,keterangan_awal, keterangan_akhir) %>%
  gather("keterangan_waktu", "keterangan_resiko", 6:7) %>%
  group_by(prov, kota, kecamatan, kelurahan, no_register_tpk,keterangan_waktu, keterangan_resiko) %>%
  summarise(total = n()) %>%
  mutate(
    keterangan_waktu = ifelse(keterangan_waktu == "keterangan_akhir", "KUNJUNGAN AKHIR", "KUNJUNGAN AWAL")
  )

efektifitas_baduta_dplyr <- as.data.table(efektifitas_baduta_dplyr)
efektifitas_baduta_dplyr$keterangan_waktu <- factor(efektifitas_baduta_dplyr$keterangan_waktu, 
                                                    levels = c("KUNJUNGAN AWAL", "KUNJUNGAN AKHIR"))

write_fst(efektifitas_baduta_dplyr, "data/efektifitas_baduta.fst")  

efektifitas_baduta_dplyr <- as.data.table(read_fst("data/efektifitas_baduta.fst"))
grafik_baduta <- efektifitas_baduta_dplyr[kota == "POLEWALI MANDAR" & 
                     kecamatan == "TINAMBUNG" & 
                    kelurahan %in% c("TINAMBUNG", "KARAMA"),
                    .(total = sum(total)),
                    by = c("prov", "keterangan_waktu", "keterangan_resiko")]

grafik <- plot_ly(grafik_baduta, 
                  x = ~keterangan_waktu, 
                  y = ~total, 
                  color = ~keterangan_resiko,
                  colors = c("#950606", "#008000"),
                  type = 'bar', 
                  textposition = 'auto',
                  text = ~paste("Total: ", total),
                  hoverinfo = "text") %>%
  layout(title = "Jumlah Resiko Berdasarkan Keterangan Waktu",
         xaxis = list(title = "Kondisi Resiko"),
         yaxis = list(title = "Total"))

# Menampilkan grafik
grafik


# bumil
efektifitas_bumil <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1jRq1xR8E1_o35SnAbkqXlYNKOjsDyRH_wFoteOtvZ3Y/edit?gid=0#gid=0")
efektifitas_bumil$no_register_tpk <- format(efektifitas_bumil$no_register_tpk, scientific = F)

efektifitas_bumil_dplyr <- efektifitas_bumil %>%
  select(prov, kota, kecamatan, kelurahan, no_register_tpk,keterangan_awal, keterangan_akhir) %>%
  gather("keterangan_waktu", "keterangan_resiko", 6:7) %>%
  group_by(prov, kota, kecamatan, kelurahan, no_register_tpk,keterangan_waktu, keterangan_resiko) %>%
  summarise(total = n()) %>%
  mutate(
    keterangan_waktu = ifelse(keterangan_waktu == "keterangan_akhir", "KUNJUNGAN AKHIR", "KUNJUNGAN AWAL")
  )

efektifitas_bumil_dplyr <- as.data.table(efektifitas_bumil_dplyr)
efektifitas_bumil_dplyr$keterangan_waktu <- factor(efektifitas_bumil_dplyr$keterangan_waktu, 
                                                    levels = c("KUNJUNGAN AWAL", "KUNJUNGAN AKHIR"))

write_fst(efektifitas_bumil_dplyr, "data/efektifitas_bumil.fst")  

# pascasalin
efektifitas_pascasalin <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1CJgb_FKwuTOJDqVlFJKX7ClwYQiW9V9crNwhMKJ-NF4/edit?gid=0#gid=0")
efektifitas_pascasalin$no_register_tpk <- format(efektifitas_pascasalin$no_register_tpk, scientific = F)

efektifitas_pascasalin_dplyr <- efektifitas_pascasalin %>%
  select(prov, kota, kecamatan, kelurahan, no_register_tpk,keterangan_awal, keterangan_akhir) %>%
  gather("keterangan_waktu", "keterangan_resiko", 6:7) %>%
  group_by(prov, kota, kecamatan, kelurahan, no_register_tpk,keterangan_waktu, keterangan_resiko) %>%
  summarise(total = n()) %>%
  mutate(
    keterangan_waktu = ifelse(keterangan_waktu == "keterangan_akhir", "KUNJUNGAN AKHIR", "KUNJUNGAN AWAL")
  )

efektifitas_pascasalin_dplyr <- as.data.table(efektifitas_pascasalin_dplyr)
efektifitas_pascasalin_dplyr$keterangan_waktu <- factor(efektifitas_pascasalin_dplyr$keterangan_waktu, 
                                                   levels = c("KUNJUNGAN AWAL", "KUNJUNGAN AKHIR"))
write_fst(efektifitas_pascasalin_dplyr, "data/efektifitas_pascasalin.fst")  

# catin
efektifitas_catin <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1hF0ednEfWhk5s6JSN_w9KRInvUxYLBxfkmBiJW1KRX0/edit?usp=drive_web&ouid=113383871394990368284")
efektifitas_catin$no_register_tpk <- format(efektifitas_catin$no_register_tpk, scientific = F)

efektifitas_catin_dplyr <- efektifitas_catin %>%
  select(prov, kota, kecamatan, kelurahan, no_register_tpk,keterangan_awal, keterangan_akhir) %>%
  gather("keterangan_waktu", "keterangan_resiko", 6:7) %>%
  group_by(prov, kota, kecamatan, kelurahan, no_register_tpk,keterangan_waktu, keterangan_resiko) %>%
  summarise(total = n()) %>%
  mutate(
    keterangan_waktu = ifelse(keterangan_waktu == "keterangan_akhir", "KUNJUNGAN AKHIR", "KUNJUNGAN AWAL")
  )

efektifitas_catin_dplyr <- as.data.table(efektifitas_catin_dplyr)
efektifitas_catin_dplyr$keterangan_waktu <- factor(efektifitas_catin_dplyr$keterangan_waktu, 
                                                        levels = c("KUNJUNGAN AWAL", "KUNJUNGAN AKHIR"))

write_fst(efektifitas_catin_dplyr, "data/efektifitas_catin.fst")  



