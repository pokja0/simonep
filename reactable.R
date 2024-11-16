library(reactablefmtr)

data_tabel_sasaran <- data.table(read_fst("data/capaian_catin.fst"))
kolom <-  c("Prov","Kab", "Kec","Desa/Kel","no_register_tpk","total_pendampingan","total_orang",
            "Rasio Total","target_tpk","capaian")
colnames(data_tabel_sasaran) <- kolom

data_tabel_sasaran[, keterangan := ifelse(capaian <= 0, "Tidak", "Mendampingi")]

data_pkb <- data.table(read_fst("data/data_pkb.fst"))
data_tabel_sasaran <- data_tabel_sasaran[data_pkb, on = c("Kec" = "Kecamatan", "Desa/Kel" = "Kelurahan")]
data_tabel_sasaran <- data_tabel_sasaran[capaian >= as.integer("0") &
                                           capaian <as.integer("1000")]
# data_pkb <- data_pkb %>%
#   select(Kecamatan, Kelurahan, `Nama PKB`)
# write.fst(data_pkb, "data/data_pkb.fst")


reactable(
  data_tabel_sasaran,
  defaultColDef = colDef(
    align = "left",
    minWidth = 140,
    headerStyle = list(background = "#7393B3", color = "black"),
    style = list(color = "black") # Atur teks hitam untuk semua sel
  ),
  filterable = TRUE,
  showPageSizeOptions = TRUE,
  bordered = TRUE, striped = TRUE, highlight = TRUE,
  resizable = TRUE,
  defaultSorted = c("capaian"),
  theme = reactableTheme(
    borderColor = "#808080",
    stripedColor = "#f6f8fa",
    highlightColor = "#f0f5f9",
    style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
  ),
  groupBy = c("Prov", "Kab", "Kec", "Desa/Kel"),
  columns = list(
    Prov = colDef(name = "Provinsi"),
    Kab = colDef(name = "Kabupaten"),        
    Kec = colDef(name = "Kecamatan"),
    `Desa/Kel` = colDef(name = "Desa/Kelurahan"),
    no_register_tpk = colDef(aggregate = "count", name = "No Register"),
    total_pendampingan = colDef(aggregate = "sum", name =  "Pendampingan"),
    total_orang = colDef(aggregate = "sum", name = "Sasaran Terdampingi"),
    `Rasio Total` = colDef(name = "Rasio Pendampingan",
                           # Calculate the aggregate Avg.Price as `sum(Price) / sum(Units)`
                           aggregate = JS("function(values, rows) {
            let totalPrice = 0
            let totalUnits = 0
            rows.forEach(function(row) {
              totalPrice += row['total_pendampingan']
              totalUnits += row['total_orang']
            })
            return totalPrice / totalUnits
          }"),
                           format = colFormat(locales = "fr-FR", digits = 2)
    ),
    target_tpk = colDef(aggregate = "sum", name = "Target", 
                        format =  colFormat(separators = TRUE, locales = "id-ID")),
    capaian = colDef(name = "Capaian",
                     # Calculate the aggregate Avg.Price as `sum(Price) / sum(Units)`
                     aggregate = JS("function(values, rows) {
            let totalPrice = 0
            let totalUnits = 0
            rows.forEach(function(row) {
              totalPrice += row['total_orang']
              totalUnits += row['target_tpk']
            })
            return totalPrice / totalUnits * 100
          }"),
          format = colFormat(locales = "id-ID", digits = 2)
    ),
    keterangan = colDef(aggregate = "frequency", name = "Keterangan")
  )
)
