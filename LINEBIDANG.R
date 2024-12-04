data_bidan <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1VyUfySxBWXDN4j0qWwm8erY2t2YE4AnFU40NQMfG4M8/edit?gid=78122984#gid=78122984")
data_bidan$KODE <- as.character(data_bidan$KODE)
data_bidan$TAHUN <- factor(data_bidan$TAHUN)

fwrite(data_bidan, "data/daftar_bidan_terlatih.csv")
data_bidan <- fread("data/daftar_bidan_terlatih.csv")


# data_capaian <- readxl::read_excel("data/faskes-pelayanan-2023-ulang.xlsx")
# data_capaian$KODE <- gsub(" ", "", data_capaian$KODE)
# data_capaian <- data_capaian %>%
#   filter(KODE %in% data_bidan$KODE)
# 
# data_capaian$BULAN <- factor(data_capaian$BULAN, levels = unique(data_capaian$BULAN))
# data_capaian$`JUMLAH PESERTA KB BARU` <- as.integer(data_capaian$`JUMLAH PESERTA KB BARU`)
# 
# data_capaian <- data_capaian %>%
#   mutate(
#     `JUMLAH PESERTA KB BARU` = ifelse(is.na(`JUMLAH PESERTA KB BARU`), 0, `JUMLAH PESERTA KB BARU`)
#   )
# 
# fwrite(data_capaian, "data/pelayanan_faskes_jejaring.csv")

data_bidan <- fread("data/daftar_bidan_terlatih.csv")
data_capaian <- fread("data/pelayanan_faskes_jejaring.csv")
data_capaian$BULAN <- factor(data_capaian$BULAN, levels = unique(data_capaian$BULAN))

data_line <- data_capaian %>%
  filter(KODE == data_bidan$KODE[89], TAHUN == data_bidan$TAHUN[89]) %>%
  mutate(MKJP = as.integer(`1 BATANG`) + as.integer(`2 BATANG`) + as.integer(IUD))

# Membuat grafik garis dengan dua garis dan legenda
fig <- plot_ly(data_line) %>%
  add_trace(x = ~BULAN, y = ~MKJP, type = 'scatter', mode = 'lines+markers+text',
            hovertemplate = paste("BULAN: %{x}<br>MKJP: %{y}<br>PB: ", data_line$`JUMLAH PESERTA KB BARU`, "<extra></extra>"),
            text = ~MKJP, textposition = 'top center', line = list(shape = "linear"),
            textfont = list(size = 16), marker = list(size = 12), name = 'MKJP') %>%
  add_trace(x = ~BULAN, y = ~`JUMLAH PESERTA KB BARU`, type = 'scatter', mode = 'lines+markers+text',
            hovertemplate = paste("BULAN: %{x}<br>MKJP: ", data_line$MKJP, "<br>PB: %{y}<extra></extra>"),
            text = ~`JUMLAH PESERTA KB BARU`, textposition = 'top center', line = list(shape = "linear"),
            textfont = list(size = 16), marker = list(size = 12), name = 'Peserta Baru') %>%
  layout(title = paste("Tren Capaian Peserta KB Baru \n", data_bidan$`NAMA BIDAN`[89], 
         "\n Tahun", data_bidan$TAHUN[89]),
         yaxis = list(title = "Jumlah", range = c(0, max(data_line$`JUMLAH PESERTA KB BARU`) + 3)))

# Menampilkan grafik
fig


#bar
warna <- c("MKJP" = "orange", "NON MKJP" = "#90e0ef")

data_bar <- data_line %>%
  group_by(`NAMA FASKES`) %>%
  summarise(MKJP = sum(MKJP),
            `NON MKJP` = sum(`JUMLAH PESERTA KB BARU`) - sum(MKJP)) %>%
  pivot_longer(
    cols = c("MKJP", "NON MKJP"),
    names_to = "JENIS",
    values_to = "JUMLAH"
  )
# Membuat grafik batang dengan plotly
fig <- plot_ly(data_bar, x = ~JENIS, y = ~JUMLAH, type = 'bar', 
               color = ~JENIS, colors = warna,
               textfont = list(size = 15, color = 'black'),
               text = ~paste("Jumlah: ", JUMLAH), hoverinfo = 'text') %>%
  layout(title = paste("Jumlah Pelayanan KB Berdasarkan Jenis \n", data_bidan$`NAMA BIDAN`[89], 
                       "\n Tahun", data_bidan$TAHUN[89]),
         xaxis = list(title = "Jenis"),
         yaxis = list(title = "Jumlah", range = c(0, max(data_bar$JUMLAH) + 10)))

# Menampilkan grafik
fig
