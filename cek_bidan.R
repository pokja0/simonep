
nama_bidan_2023_1 <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1VyUfySxBWXDN4j0qWwm8erY2t2YE4AnFU40NQMfG4M8/edit?gid=1890152065#gid=1890152065")
nama_bidan_2023_1 <- nama_bidan_2023_1 %>%
  dplyr::select(KODE,NAMA...7)
nama_bidan_2023_2 <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1VyUfySxBWXDN4j0qWwm8erY2t2YE4AnFU40NQMfG4M8/edit?gid=1490432708#gid=1490432708")
nama_bidan_2023_2 <- nama_bidan_2023_2 %>%
  dplyr::select(KODE,NAMA...7)

nama_bidan_terlatih <- rbind(nama_bidan_2023_1, nama_bidan_2023_2)
length(unique(nama_bidan_terlatih$KODE))

bidan_2023 <- readxl::read_excel("data/yan-kb bidan terlatih 23.xlsx")
bidan_2023 <- bidan_2023 %>%
  filter(`Kode Faskes` %in% nama_bidan_terlatih$KODE)

nama_bidan_terlatih %>%
  filter(!(KODE %in% bidan_2023$`Kode Faskes`))

length(unique(bidan_2023$`Kode Faskes`))

nama_bidan_2024 <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1VyUfySxBWXDN4j0qWwm8erY2t2YE4AnFU40NQMfG4M8/edit?gid=0#gid=0")
nama_bidan_2024 <- nama_bidan_2024 %>% 
  select(KODE, NAMA...7)

pelayanan_kb_2024 <- readxl::read_excel("data/YAN KB Bidan Terlatih 2024.xlsx")
pelayanan_kb_2024 <- pelayanan_kb_2024 %>%
  filter(`Kode Faskes` %in% nama_bidan_2024$KODE)

unique(pelayanan_kb_2024$`Kode Faskes`)

cek <- nama_bidan_2024 %>%
  filter(!(KODE %in% pelayanan_kb_2024$`Kode Faskes`))
