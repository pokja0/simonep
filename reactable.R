library(reactablefmtr)

data_tabel_sasaran <- data.table(read_fst("data/capaian_catin.fst"))

data_tabel_sasaran$no_register_tpk <- as.character(data_tabel_sasaran$no_register_tpk)
sum(is.na(data_tabel_sasaran))
str(data_tabel_sasaran)

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

library(reactable)

reactable(
  data_tabel_sasaran,
  groupBy = c("Prov", "Kab", "Kec", "Desa/Kel"),
  columns = list(
    Prov = colDef(name = "Provinsi"),
    Kab = colDef(name = "Kabupaten"),        
    Kec = colDef(name = "Kecamatan"),
    `Desa/Kel` = colDef(name = "Desa/Kelurahan"),
    no_register_tpk = colDef(aggregate = "count", name = "No Register"),
    total_pendampingan = colDef(aggregate = "sum", name = "Pendampingan"),
    total_orang = colDef(aggregate = "sum", name = "Sasaran Terdampingi"),
    target_tpk = colDef(
      aggregate = "sum",
      name = "Target",
      format = colFormat(separators = TRUE, locales = "id-ID")
    ),
    capaian = colDef(
      aggregate = "mean",
      name = "Capaian",
      format = colFormat(separators = TRUE, locales = "id-ID", digits = 2)
    ),
    keterangan = colDef(aggregate = "frequency", name = "Keterangan"),
    `Nama PKB` = colDef(aggregate = "count", name = "Nama PKB")
  )
)

