library(stringr)

pelayanan_kb_2024 <- readxl::read_excel("data/faskes-pelayanan-2024-1.xlsx")
pelayanan_kb_2024 <- pelayanan_kb_2024 %>%
  mutate(KODE = gsub(" ", "", KODE))

bidan_2024 <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1VyUfySxBWXDN4j0qWwm8erY2t2YE4AnFU40NQMfG4M8/edit?gid=0#gid=0")

bidan_2024 <- bidan_2024 %>%
  select(NAMA...7, KODE, NAMA...3)

colnames(bidan_2024) <- c("NAMA FASKES", "REGISTER", "NAMA")

bidan_2024$REGISTER <- as.character(bidan_2024$REGISTER)

pelayanan_kb_2024 <- pelayanan_kb_2024 %>%
  filter(KODE %in% bidan_2024$REGISTER)

fwrite(pelayanan_kb_2024, "data/pelayanan_kb_2024.csv")


####
pelayanan_kb_2023 <- readxl::read_excel("data/faskes-pelayanan-2023-1.xlsx")
pelayanan_kb_2023 <- pelayanan_kb_2023 %>%
  mutate(KODE = gsub(" ", "", KODE)) 

pelayanan_kb_2024 <- readxl::read_excel("data/faskes-pelayanan-2024-1.xlsx")
pelayanan_kb_2024 <- pelayanan_kb_2024 %>%
  mutate(KODE = gsub(" ", "", KODE)) %>%
  select(KODE, `NAMA FASKES`, KABUPATEN, KECAMATAN)%>%
  filter(!grepl("^\\d{1}$", KODE)) %>%
  distinct()

pelayanan_kb_2023 <- pelayanan_kb_2023 %>%
  dplyr::left_join(pelayanan_kb_2024, by = c("KODE", "NAMA FASKES"))

bidan_2023 <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1VyUfySxBWXDN4j0qWwm8erY2t2YE4AnFU40NQMfG4M8/edit?gid=1173219117#gid=1173219117")

bidan_2023 <- bidan_2023 %>%
  select(NAMA...7, KODE, NAMA...4)

colnames(bidan_2023) <- c("NAMA FASKES", "REGISTER", "NAMA")

bidan_2023$REGISTER <- as.character(bidan_2023$REGISTER)

pelayanan_kb_2023 <- pelayanan_kb_2023 %>%
  filter(KODE %in% bidan_2023$REGISTER)

bidan_2023 %>%
  filter(!(REGISTER %in% pelayanan_kb_2023$KODE))

fwrite(pelayanan_kb_2024, "data/pelayanan_kb_2023.csv")


##

bidan <- fst::read.fst("/home/hi/Documents/projects/profil_desa_r/data/data_faskes_siga.fst")
bidan$`NIK Bidan` <- format(bidan$`NIK Bidan`)
