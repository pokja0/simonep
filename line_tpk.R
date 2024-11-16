capaian_baduta <- as.data.table(read_fst("data/pendampingan_baduta.fst"))

dt_melted <- capaian_baduta[`No Register` == "7605072001002", 
                            .SD, 
                            .SDcols = grep("total_orang", names(capaian_baduta)[1:29], value = TRUE)]
dt_melted <- melt(dt_melted, 
                  measure.vars = grep("total_orang", names(dt_melted), value = TRUE),
                  variable.name = "bulan", 
                  value.name = "Total")

# Mengatur label bulan dari Januari hingga Agustus sesuai urutan yang benar
bulan_labels <- c("Januari", "Februari", "Maret", "April", "Mei", "Juni", "Juli", "Agustus")

# Menambahkan kolom Bulan dengan urutan Januari hingga Agustus
dt_melted$Bulan <- factor(rep(bulan_labels, length.out = nrow(dt_melted)), levels = bulan_labels)

# Membuat line plot menggunakan plotly
# Memuat plotly
library(plotly)

# Membuat line plot menggunakan plotly
fig <- plot_ly(dt_melted, x = ~Bulan, y = ~Total, type = 'scatter', mode = 'lines+markers', 
               line = list(color = 'blue')) %>%
  layout(title = "Total per Bulan",
         xaxis = list(title = "Bulan"),
         yaxis = list(title = "Total"),
         showlegend = FALSE,
         shapes = list(
           # Shape untuk Januari hingga April
           list(
             type = "rect",
             x0 = 0, x1 = 3,  # Januari hingga April
             y0 = min(dt_melted$Total), y1 = max(dt_melted$Total),
             line = list(color = "transparent"),
             fillcolor = "rgba(0, 0, 255, 0.2)"  # Biru transparan
           ),
           # Shape untuk Mei hingga Agustus
           list(
             type = "rect",
             x0 = 3, x1 = 7,  # Mei hingga Agustus
             y0 = min(dt_melted$Total), y1 = max(dt_melted$Total),
             line = list(color = "transparent"),
             fillcolor = "rgba(255, 0, 0, 0.2)"  # Merah transparan
           )
         ),
         annotations = list(
           # Label untuk "Sebelum Pelatihan" di area biru
           list(
             x = 1.5,  # Posisi tengah dari Januari hingga April
             y = max(dt_melted$Total * 0.8),  # Posisi tinggi (y) persegi panjang
             text = "Sebelum Pelatihan",  # Teks label
             showarrow = FALSE,
             font = list(size = 14, color = 'black'),  # Teks biru
             align = "center",
             verticalalign = "top"
           ),
           # Label untuk "Setelah Pelatihan" di area merah
           list(
             x = 5.5,  # Posisi tengah dari Mei hingga Agustus
             y = max(dt_melted$Total * 0.8),  # Posisi tinggi (y) persegi panjang
             text = "Setelah Pelatihan",  # Teks label
             showarrow = FALSE,
             font = list(size = 14, color = 'black'),  # Teks pink
             align = "center",
             verticalalign = "top"
           )
         )
  )

# Menampilkan plot
fig


fig
# Lihat hasilnya
print(dt_melted)

