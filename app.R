# Install dan load bs4Dash terlebih dahulu
# install.packages("bs4Dash")

library(shiny)
library(bs4Dash)
library(fst)
library(data.table)
library(bslib)
library(plotly)
library(gt)
library(reactablefmtr)

data_wilayah <- read_fst("data/data_daftar_desa.fst")
data_wilayah <- data.table(data_wilayah)
data_pkb <- data.table(read_fst("data/data_pkb.fst"))


# UI: Define the user interface
ui <- dashboardPage(
  header = dashboardHeader(title = "SIMONEP"),  # Header with title
  sidebar = dashboardSidebar(   # Sidebar with navigation
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Data Analysis", tabName = "data_analysis", icon = icon("chart-bar")),
      menuItem("Settings", tabName = "settings", icon = icon("cogs"))
    )
  ),
  body = dashboardBody(  # Main content area
    tabItems(
      tabItem(tabName = "home",
              jumbotron(
                title = "SIMONEP",
                status = "info",
                lead = "Sistem Informasi Monitoring Lalala",
                btnName = "Download Panduan",
                href = NULL
                
              ),
              bs4Card(
                width = 12, title = "Pilih Wilayah", closable = F, collapsible = TRUE,
                fluidRow(
                  column(4, 
                         selectInput("pilih_kab", "Daftar Kabupaten",
                                     choices = c("SEMUA KABUPATEN", "PASANGKAYU", "MAMUJU TENGAH",
                                                 "MAMUJU", "MAJENE", "POLEWALI MANDAR", "MAMASA"))
                         ),
                  column(4, selectInput("pilih_kec", "Daftar Kecamatan", choices = NULL)),
                  column(4, selectInput("pilih_desa_kel", "Pilih Desa/Kel", choices = NULL))
                ),
                fluidRow(
                  input_task_button(
                    label_busy = "Sedang Proses",
                    id = "cari_rekap",
                    label = "Cari"
                  )
                ),
                h5(textOutput("tes_input_rekap"), style="text-align: center;"),
                fluidRow(
                  infoBox(
                    title = "Jumlah TPK",
                    width = 5,
                    value = textOutput("jumlah_tpk"),
                    icon = icon("clock"),
                    color = "primary"
                  ),
                  infoBox(
                    title = "Status Orientasi",
                    width = 5,
                    value = "100%",
                    icon = icon("clock"),
                    color = "primary"
                  )
                )
              ),
            bs4Card(width = 12, title = "Capaian Pendampingan Sasaran Wilayah", closable = F, collapsible = TRUE,
              fluidRow(
                column(
                  3,
                  gt_output("capaian_catin")
                ),
                column(
                  3,
                  gt_output("capaian_pascasalin")
                ),
                column(
                  3, 
                  gt_output("capaian_baduta")
                ),
                column(
                  3,
                  gt_output("capaian_bumil")
                )
              )
            ),
            bs4Card(
              width = 12, title = "Status Resiko Pendampingan Sasaran Di Wilayah", closable = F, collapsible = TRUE,
              fluidRow(
                column(
                  6,
                  plotlyOutput("efektif_catin")
                ),
                column(
                  6,
                  plotlyOutput("efektif_pascasalin")
                ),
              ),
              br(),
              fluidRow(
                column(
                  6,
                  plotlyOutput("efektif_baduta")
                ),
                column(
                  6,
                  plotlyOutput("efektif_bumil")
                )
              ) #fluidro
            ), #bscard
            bs4Card(
              width = 12, title = "Tabel Cakupan Pendampingan Sasaran Di Wilayah", closable = F, collapsible = TRUE,
              fluidRow(
                column(
                  4,
                  selectInput("pilih_sasaran", "Pilih Sasaran Pendampingan", 
                              choices = c("Calon Pengantin", "Ibu Hamil", "Ibu Pascasalin", "Bayi Dibawah 2 Tahun"))
                ),
                column(
                  4,
                  selectInput("pilih_rentang_capaian", 
                              "Rentang Capaian", 
                              choices = c("Semua", "0%", "Dibawah 50%", "50% Keatas", "80% Keatas"))
                )
              ),
              fluidRow(
                input_task_button(
                  label_busy = "Sedang Proses",
                  id = "cari_tabel",
                  label = "Cari"
                )
              ),
              br(),
              fluidRow(
                column(
                  12,
                  uiOutput("cakupan_pendampingan")
                )
              )
            )
      ),
      tabItem(tabName = "data_analysis",
              fluidRow(
                box(title = "Data Plot", status = "primary", solidHeader = TRUE, width = 6,
                    plotOutput("data_plot")),
                box(title = "Data Table", status = "warning", solidHeader = TRUE, width = 6,
                    DT::dataTableOutput("data_table"))
              )
      ),
      tabItem(tabName = "settings",
              fluidRow(
                box(title = "Settings", status = "success", solidHeader = TRUE, width = 12,
                    "Adjust the settings for your analysis.")
              )
      )
    )
  ),
  controlbar = NULL,
  footer = dashboardFooter(  # Footer with copyright or other information
    "Shiny App with bs4Dash - Developed by Your Name"
  )
)

# Server: Define server logic
server <- function(input, output, session) {
  
  #input
  observe({
    if(input$pilih_kab == "SEMUA KABUPATEN"){
      pilihan_kec = "SEMUA KECAMATAN"
    } else{
      
      pilihan_kec = data_wilayah[, .(KABUPATEN, KECAMATAN)]
      pilihan_kec = data_wilayah[KABUPATEN == input$pilih_kab, .(KABUPATEN, KECAMATAN)]
      pilihan_kec = c("SEMUA KECAMATAN", pilihan_kec$KECAMATAN)
    }
    
    updateSelectInput(session, "pilih_kec",
                      choices = pilihan_kec,
                      selected = pilihan_kec[1])
    
  })
  
  observe({
    if(input$pilih_kec == "SEMUA KECAMATAN"){
      pilihan_desa = "SEMUA DESA/KELURAHAN"
    } else{
      
      pilihan_desa = data_wilayah[, .(KECAMATAN, KELURAHAN)]
      pilihan_desa = data_wilayah[KECAMATAN == input$pilih_kec, .(KELURAHAN, KECAMATAN)]
      pilihan_desa = c("SEMUA DESA/KELURAHAN", pilihan_desa$KELURAHAN)
    }
    
    updateSelectInput(session, "pilih_desa_kel",
                      choices = pilihan_desa,
                      selected = pilihan_desa[1])
    
  })
  
  # Simpan hasil unique data_poktan$KABUPATEN di luar observeEvent
  unique_kabupaten <- unique(data_wilayah$KABUPATEN)
  
  # Reactive untuk memilih kabupaten
  value_filter_kab <- reactiveVal()
  
  observeEvent(input$cari_rekap, {
    kondisi_input = input$pilih_kab
    
    # Cek apakah yang dipilih adalah 'SEMUA KABUPATEN' atau satu kabupaten tertentu
    if (kondisi_input == "SEMUA KABUPATEN") {
      # Tidak perlu menggunakan unique setiap kali, karena sudah disimpan sebelumnya
      filter_kabupaten <- unique_kabupaten
    } else {
      filter_kabupaten <- kondisi_input
    }
    
    # Set nilai reaktif
    value_filter_kab(filter_kabupaten)
  })
  
  # Reactive untuk mendapatkan daftar kecamatan per kabupaten
  daftar_kecamatan_reaktif <- reactive({
    filter_kabupaten = value_filter_kab()  # Mengambil filter kabupaten yang telah diset
    
    # Jika filter kabupaten ada, ambil kecamatan yang sesuai
    if (length(filter_kabupaten) > 0) {
      # Menggunakan data.table untuk filter dan mengambil kecamatan unik
      daftar_kecamatan = data_wilayah[KABUPATEN %in% filter_kabupaten, unique(KECAMATAN)]
    } else {
      daftar_kecamatan = character(0)  # Mengembalikan kecamatan kosong jika tidak ada kabupaten yang dipilih
    }
    
    return(daftar_kecamatan)
  })
  
  # Reactive untuk filter kecamatan
  value_filter_kec <- reactiveVal()
  
  observeEvent(input$cari_rekap, {
    kondisi_input = input$pilih_kec
    daftar_kecamatan = daftar_kecamatan_reaktif()  # Ambil daftar kecamatan berdasarkan kabupaten
    
    if (kondisi_input == "SEMUA KECAMATAN") {
      # Jika "SEMUA KECAMATAN", ambil semua kecamatan yang tersedia
      filter_kecamatan = daftar_kecamatan
    } else {
      # Jika kecamatan dipilih spesifik
      filter_kecamatan = kondisi_input
    }
    
    # Update nilai reaktif untuk kecamatan
    value_filter_kec(filter_kecamatan)
  })
  
  # Reactive untuk mendapatkan daftar desa/kelurahan berdasarkan kabupaten dan kecamatan
  daftar_desa_kel_reaktif <- reactive({
    filter_kabupaten = value_filter_kab()  # Mengambil filter kabupaten yang telah diset
    filter_kecamatan = value_filter_kec()  # Mengambil filter kecamatan yang telah diset
    
    # Jika filter kabupaten dan kecamatan ada, ambil desa/kelurahan yang sesuai
    if (length(filter_kabupaten) > 0 && length(filter_kecamatan) > 0) {
      daftar_kel = data_wilayah[
        KABUPATEN %in% filter_kabupaten & KECAMATAN %in% filter_kecamatan,
        unique(KELURAHAN)
      ]
    } else {
      daftar_kel = character(0)  # Jika kabupaten atau kecamatan kosong, kembalikan desa/kelurahan kosong
    }
    
    return(daftar_kel)
  })
  
  # Reactive untuk filter desa/kelurahan
  value_filter_desa_kel <- reactiveVal()
  
  observeEvent(input$cari_rekap, {
    kondisi_input = input$pilih_desa_kel
    daftar_kel = daftar_desa_kel_reaktif()  # Ambil daftar desa/kelurahan berdasarkan kabupaten dan kecamatan
    
    if (kondisi_input == "SEMUA DESA/KELURAHAN") {
      # Jika "SEMUA DESA/KELURAHAN", ambil semua desa/kelurahan yang sesuai
      filter_desa_kel = daftar_kel
    } else {
      # Jika desa/kelurahan dipilih spesifik
      filter_desa_kel = kondisi_input
    }
    
    # Update nilai reaktif untuk desa/kelurahan
    value_filter_desa_kel(filter_desa_kel)
  })
  #akhir inpur
  
  ##judul
  values <- reactiveValues(default = 0)
  
  observeEvent(input$cari_rekap,{
    values$default <- input$cari_rekap
  })
  
  teks_judul_rekap <- eventReactive(input$cari_rekap, {
    if(input$pilih_kab == "SEMUA KABUPATEN"){
      nama_daerah = "SULAWESI BARAT"
      tingkat_daerah = "PROVINSI"
    } else if(input$pilih_kec == "SEMUA KECAMATAN"){
      nama_daerah = input$pilih_kab
      tingkat_daerah = "KABUPATEN"
    } else if(input$pilih_desa_kel == "SEMUA DESA/KELURAHAN"){
      nama_daerah = input$pilih_kec
      tingkat_daerah = "KECAMATAN"
    } else{
      nama_daerah = value_filter_desa_kel()
      tingkat_daerah = "DESA/KELURAHAN"
    }
    teks <- paste("PROFIL", tingkat_daerah, nama_daerah)
    # if(tingkat_daerah == "KELURAHAN"){
    #   teks <- paste0("PROFIL DESA/", tingkat_daerah, " ", nama_daerah, " - ", input$bulan_rekap)
    # } else{
    #   teks <- paste("PROFIL", tingkat_daerah, nama_daerah, "-", input$bulan_rekap)
    # }
    
    
  })
  
  output$tes_input_rekap <- renderText({
    if(values$default == 0){
      teks = "Klik Cari Untuk Menampilkan Halaman"
    }
    else{
      teks = teks_judul_rekap()
    }
  })
  ##akhir judul
  
  #menu rekap wilayah
  tim_pendamping_keluarga <- fread("data/daftar_tpk.csv")
  tim_pendamping_keluarga <- data.table(tim_pendamping_keluarga)
  
  output$jumlah_tpk <- renderText({
    nrow(tim_pendamping_keluarga[
      Kab %in% value_filter_kab() & Kec %in% value_filter_kec() & `Desa/Kel` %in% value_filter_desa_kel(),
    ])
  })
  
  output$capaian_catin <- render_gt({
    data_catin_melapor <- data.table(read_fst("data/capaian_catin.fst"))
    is.data.table(data_catin_melapor)
    
    # Sintaks berantai
    gt_catin <- data_catin_melapor[Kab %in% value_filter_kab() & 
                                     Kec %in% value_filter_kec() & 
                                     `Desa/Kel` %in% value_filter_desa_kel(), 
                                   .(`Estimasi Catin` = sum(target_tpk, na.rm = TRUE), 
                                     `Pendampingan` = sum(total_pendampingan, na.rm = TRUE), 
                                     `Catin Terdampingi` = sum(total_orang, na.rm = TRUE),
                                     `Rasio Pendampingan` = round(sum(total_pendampingan) / sum(total_orang), 2),
                                     Capaian = round(sum(total_orang) / sum(target_tpk) * 100, 2)),
                                   by = Prov]
    
    gt_catin <- melt(gt_catin, id.vars = c("Prov"), 
                     measure.vars = c("Estimasi Catin", "Pendampingan", "Catin Terdampingi", "Rasio Pendampingan", "Capaian"),
                     variable.name = "Indikator", 
                     value.name = "Total")[
                       , .(Indikator, Total)
                     ]
    
    gt(gt_catin) %>%
      tab_header(
        title = md("**Calon Pengantin**") # Ganti judul sesuai kebutuhan
      ) %>%
      fmt_integer(
        columns = c(Total), 
        rows = Indikator %in% c("Estimasi Catin", "Pendampingan", "Catin Terdampingi"),
        use_seps = TRUE, 
        sep_mark = "."
      ) %>%
      fmt_number(
        columns = c(Total),
        use_seps = TRUE, 
        sep_mark = ".",
        rows = Indikator %in% c("Rasio Pendampingan", "Capaian"),
        decimals = 2, 
        dec_mark = ","
      ) %>%
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
  })
  
  output$capaian_pascasalin <- render_gt({
    data_pascasalin_melapor <- data.table(read_fst("data/capaian_pascasalin.fst"))
    is.data.table(data_pascasalin_melapor)
    
    # Sintaks berantai
    gt_pascasalin <- data_pascasalin_melapor[Kab %in% value_filter_kab() & 
                                     Kec %in% value_filter_kec() & 
                                     `Desa/Kel` %in% value_filter_desa_kel(), 
                                   .(`Estimasi Pascasalin` = sum(target_tpk, na.rm = TRUE), 
                                     `Pendampingan` = sum(total_pendampingan, na.rm = TRUE), 
                                     `Pascasalin Terdampingi` = sum(total_orang, na.rm = TRUE),
                                     `Rasio Pendampingan` = round(sum(total_pendampingan) / sum(total_orang), 2),
                                     Capaian = round(sum(total_orang) / sum(target_tpk) * 100, 2)),
                                   by = Prov]
    
    gt_pascasalin <- melt(gt_pascasalin, id.vars = c("Prov"), 
                     measure.vars = c("Estimasi Pascasalin", "Pendampingan", "Pascasalin Terdampingi", 
                                      "Rasio Pendampingan", "Capaian"),
                     variable.name = "Indikator", 
                     value.name = "Total")[
                       , .(Indikator, Total)
                     ]
    
    gt(gt_pascasalin) %>%
      tab_header(
        title = md("**Ibu Pascasalin**") # Ganti judul sesuai kebutuhan
      ) %>%
      fmt_integer(
        columns = c(Total), 
        rows = Indikator %in% c("Estimasi pascasalin", "Pendampingan", "Pascasalin Terdampingi"),
        use_seps = TRUE, 
        sep_mark = "."
      ) %>%
      fmt_number(
        columns = c(Total),
        use_seps = TRUE, 
        sep_mark = ".",
        rows = Indikator %in% c("Rasio Pendampingan", "Capaian"),
        decimals = 2, 
        dec_mark = ","
      ) %>%
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
  })
  
  output$capaian_baduta <- render_gt({
    data_baduta_melapor <- data.table(read_fst("data/capaian_baduta.fst"))
    is.data.table(data_baduta_melapor)
    
    # Sintaks berantai
    gt_baduta <- data_baduta_melapor[Kab %in% value_filter_kab() & 
                                               Kec %in% value_filter_kec() & 
                                               `Desa/Kel` %in% value_filter_desa_kel(), 
                                             .(`Estimasi Baduta` = sum(target_tpk, na.rm = TRUE), 
                                               `Pendampingan` = sum(total_pendampingan, na.rm = TRUE), 
                                               `Baduta Terdampingi` = sum(total_baduta, na.rm = TRUE),
                                               `Rasio Pendampingan` = round(sum(total_pendampingan) / sum(total_baduta), 2),
                                               Capaian = round(sum(total_baduta) / sum(target_tpk) * 100, 2)),
                                             by = Prov]
    
    gt_baduta <- melt(gt_baduta, id.vars = c("Prov"), 
                          measure.vars = c("Estimasi Baduta", "Pendampingan", "Baduta Terdampingi", 
                                           "Rasio Pendampingan", "Capaian"),
                          variable.name = "Indikator", 
                          value.name = "Total")[
                            , .(Indikator, Total)
                          ]
    
    gt(gt_baduta) %>%
      tab_header(
        title = md("**Baduta**") # Ganti judul sesuai kebutuhan
      ) %>%
      fmt_integer(
        columns = c(Total), 
        rows = Indikator %in% c("Estimasi Baduta", "Pendampingan", "Baduta Terdampingi"),
        use_seps = TRUE, 
        sep_mark = "."
      ) %>%
      fmt_number(
        columns = c(Total),
        use_seps = TRUE, 
        sep_mark = ".",
        rows = Indikator %in% c("Rasio Pendampingan", "Capaian"),
        decimals = 2, 
        dec_mark = ","
      ) %>%
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
  })
  
  output$capaian_bumil <- render_gt({
    data_bumil_melapor <- data.table(read_fst("data/capaian_bumil.fst"))
    is.data.table(data_bumil_melapor)
    
    # Sintaks berantai
    gt_bumil <- data_bumil_melapor[Kab %in% value_filter_kab() & 
                                     Kec %in% value_filter_kec() & 
                                     `Desa/Kel` %in% value_filter_desa_kel(), 
                                   .(`Estimasi Bumil` = sum(target_tpk_bumil, na.rm = TRUE), 
                                     `Pendampingan` = sum(total_pendampingan_bumil, na.rm = TRUE), 
                                     `Bumil Terdampingi` = sum(total_bumil, na.rm = TRUE),
                                     `Rasio Pendampingan` = round(sum(total_pendampingan_bumil, na.rm = TRUE) / sum(total_bumil, na.rm = TRUE), 2),
                                     Capaian = round(sum(total_bumil, na.rm = TRUE) / sum(target_tpk_bumil, na.rm = TRUE) * 100, 2)),
                                   by = Prov]
    
    gt_bumil <- melt(gt_bumil, id.vars = c("Prov"), 
                     measure.vars = c("Estimasi Bumil", "Pendampingan", "Bumil Terdampingi",
                                      "Rasio Pendampingan", "Capaian"),
                     variable.name = "Indikator", 
                     value.name = "Total")[
                       , .(Indikator, Total)
                     ]
    
    gt(gt_bumil) %>%
      tab_header(
        title = md("**Ibu Hamil**") # Ganti judul sesuai kebutuhan
      ) %>%
      fmt_integer(
        columns = c(Total), 
        rows = Indikator %in% c("Estimasi bumil", "Pendampingan", "Bumil Terdampingi"),
        use_seps = TRUE, 
        sep_mark = "."
      ) %>%
      fmt_number(
        columns = c(Total),
        use_seps = TRUE, 
        sep_mark = ".",
        rows = Indikator %in% c("Rasio Pendampingan", "Capaian"),
        decimals = 2, 
        dec_mark = ","
      ) %>%
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
  })
  
  output$efektif_catin <- renderPlotly({
    efektifitas_catin_dplyr <- as.data.table(read_fst("data/efektifitas_catin.fst"))
    grafik_catin <- efektifitas_catin_dplyr[kota %in% value_filter_kab() & 
                                                kecamatan %in% value_filter_kec() & 
                                                kelurahan %in% value_filter_desa_kel(),
                                              .(total = sum(total)),
                                              by = c("prov", "keterangan_waktu", "keterangan_resiko")]
    
    grafik <- plot_ly(grafik_catin, 
                      x = ~keterangan_waktu, 
                      y = ~total, 
                      color = ~keterangan_resiko,
                      colors = c("#950606", "#008000"),
                      type = 'bar', 
                      textposition = 'auto',
                      text = ~paste(keterangan_resiko, ": ", total),
                      hoverinfo = "text") %>%
      layout(title = "Status Resiko Calon Pengantian",
             xaxis = list(title = "Kondisi Resiko"),
             yaxis = list(title = "Total"))
    
    # Menampilkan grafik
    grafik
    
  })
  
  output$efektif_pascasalin <- renderPlotly({
    efektifitas_pascasalin_dplyr <- as.data.table(read_fst("data/efektifitas_pascasalin.fst"))
    grafik_pascasalin <- efektifitas_pascasalin_dplyr[kota %in% value_filter_kab() & 
                                                kecamatan %in% value_filter_kec() & 
                                                kelurahan %in% value_filter_desa_kel(),
                                              .(total = sum(total)),
                                              by = c("prov", "keterangan_waktu", "keterangan_resiko")]
    
    grafik <- plot_ly(grafik_pascasalin, 
                      x = ~keterangan_waktu, 
                      y = ~total, 
                      color = ~keterangan_resiko,
                      colors = c("#950606", "#008000"),
                      type = 'bar', 
                      textposition = 'auto',
                      text = ~paste(keterangan_resiko, ": ", total),
                      hoverinfo = "text") %>%
      layout(title = "Status Resiko Ibu Pascasalin",
             xaxis = list(title = "Kondisi Resiko"),
             yaxis = list(title = "Total"))
    
    # Menampilkan grafik
    grafik
    
  })
  
  output$efektif_baduta <- renderPlotly({
    efektifitas_baduta_dplyr <- as.data.table(read_fst("data/efektifitas_baduta.fst"))
    grafik_baduta <- efektifitas_baduta_dplyr[kota %in% value_filter_kab() & 
                                                kecamatan %in% value_filter_kec() & 
                                                kelurahan %in% value_filter_desa_kel(),
                                              .(total = sum(total)),
                                              by = c("prov", "keterangan_waktu", "keterangan_resiko")]
    
    grafik <- plot_ly(grafik_baduta, 
                      x = ~keterangan_waktu, 
                      y = ~total, 
                      color = ~keterangan_resiko,
                      colors = c("#950606", "#008000"),
                      type = 'bar', 
                      textposition = 'auto',
                      text = ~paste(keterangan_resiko, ": ", total),
                      hoverinfo = "text") %>%
      layout(title = "Status Resiko Baduta",
             xaxis = list(title = "Kondisi Resiko"),
             yaxis = list(title = "Total"))
    
    # Menampilkan grafik
    grafik
    
  })
  
  output$efektif_bumil <- renderPlotly({
    efektifitas_bumil_dplyr <- as.data.table(read_fst("data/efektifitas_bumil.fst"))
    grafik_bumil <- efektifitas_bumil_dplyr[kota %in% value_filter_kab() & 
                                                kecamatan %in% value_filter_kec() & 
                                                kelurahan %in% value_filter_desa_kel(),
                                              .(total = sum(total)),
                                              by = c("prov", "keterangan_waktu", "keterangan_resiko")]
    
    grafik <- plot_ly(grafik_bumil, 
                      x = ~keterangan_waktu, 
                      y = ~total, 
                      color = ~keterangan_resiko,
                      colors = c("#950606", "#008000"),
                      type = 'bar', 
                      textposition = 'auto',
                      text = ~paste(keterangan_resiko, ": ", total),
                      hoverinfo = "text") %>%
      layout(title = "Status Resiko Pendampingan Ibu Hamil",
             xaxis = list(title = "Kondisi Resiko"),
             yaxis = list(title = "Total"))
    
    # Menampilkan grafik
    grafik
    
  })
  
  nama_dataset <- eventReactive(input$cari_tabel,{
    #choices = c("Calon Pengantin", "Ibu Hamil", "Ibu Pascasalin", "Bayi Dibawah 2 Tahun")
    nama_dataset = input$pilih_sasaran
    if(nama_dataset == "Calon Pengantin"){
      data_tabel_sasaran <- data.table(read_fst("data/capaian_catin.fst"))
    } else if(nama_dataset == "Ibu Hamil"){
      data_tabel_sasaran <- data.table(read_fst("data/capaian_bumil.fst"))
    } else if(nama_dataset == "Ibu Pascasalin"){
      data_tabel_sasaran <- data.table(read_fst("data/capaian_pascasalin.fst"))
    } else{
      data_tabel_sasaran <- data.table(read_fst("data/capaian_baduta.fst"))
    }
    
    kolom <-  c("Prov","Kab", "Kec","Desa/Kel","no_register_tpk","total_pendampingan","total_orang",
                "Rasio Total","target_tpk","capaian")
    
    colnames(data_tabel_sasaran) <- kolom
    
    data_tabel_sasaran[, keterangan := ifelse(capaian <= 0, "Tidak", "Mendampingi")]
    
    #data_pkb <- data.table(read_fst("data/data_pkb.fst"))
    data_tabel_sasaran <- data_tabel_sasaran[data_pkb, on = c("Kec" = "Kecamatan", "Desa/Kel" = "Kelurahan")]
    #choices = c("Semua", "0%", "Dibawah 50%", "50% Keatas", "80% Keatas"))

    if(input$pilih_rentang_capaian == "Semua"){
      data_tabel_sasaran = data_tabel_sasaran
    } else if(input$pilih_rentang_capaian == "0%"){
      data_tabel_sasaran = data_tabel_sasaran[capaian <= 0]
    } else if(input$pilih_rentang_capaian == "Dibawah 50%"){
      data_tabel_sasaran = data_tabel_sasaran[capaian < 50]
    } else if(input$pilih_rentang_capaian == "50% Keatas"){
      data_tabel_sasaran = data_tabel_sasaran[capaian >= 50]
    } else{
      data_tabel_sasaran = data_tabel_sasaran[capaian >= 80]
    }
    data_tabel_sasaran
    
  })
  
  judul_dataset <- eventReactive(input$cari_tabel,{
    judul_tabel <- paste("Cakupan Pendampingan ", input$pilih_sasaran)
  })
  
  subjudul_dataset <- eventReactive(input$cari_tabel,{
    paste("Capaian ", input$pilih_rentang_capaian)
  })
  
  output$cakupan_pendampingan <- renderUI({
    judul_tabel <- judul_dataset()
    subjudul_tabel <-subjudul_dataset()
   tabel <-  reactable(
      nama_dataset(),
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
   
   tabel %>%
     add_title(judul_tabel, align = "left") %>%
     add_subtitle(subjudul_tabel)
    
  })
  #akhir rekap wilayah
  
  
}

# Run the app
shinyApp(ui = ui, server = server)
