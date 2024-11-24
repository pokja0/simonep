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
library(waiter)
library(collapse)

data_wilayah <- read_fst("data/data_daftar_desa.fst")
data_wilayah <- data.table(data_wilayah)
data_pkb <- data.table(read_fst("data/data_pkb.fst"))
data_tpk <- data.table(fread("data/daftar_tpk.csv"))

csvDownloadButton <- function(id, filename = "data.csv", label = "Download as CSV") {
  tags$button(
    tagList(icon("download"), label),
    onclick = sprintf("Reactable.downloadDataCSV('%s', '%s')", id, filename)
  )
}


# UI: Define the user interface
ui <- dashboardPage(
  preloader = list(html = tagList(spin_1(), "Loading ..."), color = "#343a40"),
  header = dashboardHeader(title = "SIMONEP"),  # Header with title
  sidebar = dashboardSidebar(   # Sidebar with navigation
    sidebarMenu(
      menuItem("Monitoring TPK", tabName = "home", icon = icon("home")),
      menuItem("TPK", tabName = "tpk", icon = icon("chart-bar")),
      menuItem("Bidan Terlatih", tabName = "bidan", icon = icon("cogs")),
      menuItem("PPKS", tabName = "ppks", icon = icon("cogs"))
    )
  ),
  body = dashboardBody(  # Main content area
    tabItems(
      tabItem(tabName = "home",
              jumbotron(
                title = "SIMONEP",
                status = "info",btnName = NULL,
                lead = "Sistem Informasi Monitoring dan Evaluasi Program",
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
                br(),
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
      tabItem(tabName = "tpk",
              bs4Card(
                width = 12, title = "Pilih Wilayah", closable = F, collapsible = TRUE,
                fluidRow(
                  column(3, 
                         selectInput("pilih_kab_tpk", "Daftar Kabupaten",
                                     choices = c("PASANGKAYU", "MAMUJU TENGAH",
                                                 "MAMUJU", "MAJENE", "POLEWALI MANDAR", "MAMASA"))
                  ),
                  column(3, selectInput("pilih_kec_tpk", "Daftar Kecamatan", choices = NULL)),
                  column(3, selectInput("pilih_desa_kel_tpk", "Pilih Desa/Kel", choices = NULL)),
                  column(3, selectInput("pilih_tpk", "Pilih Register TPK", choices = NULL))
                ),
                fluidRow(
                  input_task_button(
                    label_busy = "Sedang Proses",
                    id = "cari_tpk",
                    label = "Cari"
                  )
                )
              ),
              fluidRow(
                box(title = "Profil Wilayah", status = "primary", solidHeader = TRUE, width = 4,
                    gt_output("tabel_tpk")),
                box(title = "Capaian Pendampingan (%)", status = "warning", solidHeader = TRUE, width = 8, height = "250",
                    plotlyOutput("radar_tpk", height = "100%"))
              ),
              fluidRow(
                column(
                  6,
                  plotlyOutput("line_catin")
                ),
                column(
                  6,
                  plotlyOutput("line_pascasalin")
                )
              ),
              br(),
              fluidRow(
                column(
                  6,
                  plotlyOutput("line_baduta")
                ),
                column(
                  6,
                  plotlyOutput("line_bumil")
                )
              )
      ),
      tabItem(tabName = "bidan",
              fluidRow(
                box(title = "Pelatihan Bidan", status = "success", solidHeader = TRUE, width = 12,
                    "On Progress")
              )
      ),
      tabItem(tabName = "ppks",
              box(width = 12,
                tabsetPanel(type = "pills",
                  tabPanel("Gambaran Umum", 
                           br(),
                             fluidRow(
                               column(
                                 4,
                                 selectInput("tahun_ppks", "Pilih Tahun", 
                                             choices = c("2023", "2024"))
                               ),
                               column(
                                 4,
                                 selectInput("bulan_tidak_lapor_ppks", 
                                             "Rentang Bulan Tidak Lapor", 
                                             choices = c(1:10), selected = 6)
                               )
                             ),
                             fluidRow(
                               input_task_button(
                                 label_busy = "Sedang Proses",
                                 id = "cari_ppks_umum",
                                 label = "Cari"
                               )
                             ),
                             br(),
                             fluidRow(
                               box(title = "Kecamatan Tidak Memiliki PPKS",
                                 uiOutput("tabel_tidak_memiliki_ppks"),
                                 br(),
                                 uiOutput("download_data_ppks_tidak")
                               ),
                               box(title = "Kecamatan Tidak Melapor PPKS",
                                 uiOutput("tabel_tidak_melapor_ppks"),
                                 uiOutput("download_data_ppks_tidak_lapor")
                               )
                             )
                           ),
                  tabPanel("Pelayanan PPKS", verbatimTextOutput("summary"))
                )
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
      teks = ""
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
  
  #tpk
  
  #input tpk
  observe({
      pilihan_kec = data_wilayah[, .(KABUPATEN, KECAMATAN)]
      pilihan_kec = data_wilayah[KABUPATEN == input$pilih_kab_tpk, .(KABUPATEN, KECAMATAN)]
      pilihan_kec = c(pilihan_kec$KECAMATAN)
    
    updateSelectInput(session, "pilih_kec_tpk",
                      choices = pilihan_kec,
                      selected = pilihan_kec[1])
    
  })
  
  observe({
      pilihan_desa = data_wilayah[, .(KECAMATAN, KELURAHAN)]
      pilihan_desa = data_wilayah[KECAMATAN == input$pilih_kec_tpk, .(KELURAHAN, KECAMATAN)]
      pilihan_desa = c(pilihan_desa$KELURAHAN)
    
    updateSelectInput(session, "pilih_desa_kel_tpk",
                      choices = pilihan_desa,
                      selected = pilihan_desa[1])
    
  })
  
  observe({
    pilihan_tpk = data_tpk[, .(Kec, `Desa/Kel`, `No Register`)]
    pilihan_tpk = data_tpk[Kec == input$pilih_kec_tpk & `Desa/Kel` == input$pilih_desa_kel_tpk, .(Kec, `Desa/Kel`, `No Register`)]
    pilihan_tpk = c(pilihan_tpk$`No Register`)
    updateSelectInput(session, "pilih_tpk",
                      choices = pilihan_tpk,
                      selected = pilihan_tpk[1])
    
  })
  #akhir input tpk
  
  data_tpk_cari <- eventReactive(input$cari_tpk, {
    data_pendampingan <- as.data.table(read_fst("data/pendampingan_bumil.fst"))
    gt_wil_tpk <- data_pendampingan[, .SD[`No Register` == input$pilih_tpk]]
    
    gt_wil_tpk <- melt(gt_wil_tpk, id.vars = c("prov"), 
                         measure.vars = c("prov", "Kab", "Kec", "Desa/Kel", "No Register"),
                         variable.name = "Indikator", 
                         value.name = "Nilai")[
                           , .(Indikator, Nilai)
                         ]
    
    gt_wil_tpk$Indikator <- toupper(gt_wil_tpk$Indikator)
    gt_wil_tpk
    
  })
  
  output$tabel_tpk <- render_gt({
    gt_wil_tpk = data_tpk_cari()
    
    gt(gt_wil_tpk) %>%
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
  })
  
  data_spider <- eventReactive(input$cari_tpk, {
    pendampingan_baduta <- as.data.table(read_fst("data/capaian_baduta.fst"))
    pendampingan_baduta <- pendampingan_baduta[, .SD[no_register_tpk == input$pilih_tpk]]
    
    pendampingan_bumil <- as.data.table(read_fst("data/capaian_bumil.fst"))
    pendampingan_bumil <- pendampingan_bumil[, .SD[no_register_tpk == input$pilih_tpk]]
    
    pendampingan_pascasalin <- as.data.table(read_fst("data/capaian_pascasalin.fst"))
    pendampingan_pascasalin <- pendampingan_pascasalin[, .SD[no_register_tpk == input$pilih_tpk]]
    
    pendampingan_catin <- as.data.table(read_fst("data/capaian_catin.fst"))
    pendampingan_catin <- pendampingan_catin[, .SD[no_register_tpk == input$pilih_tpk]]
    
    data_spider_pendampingan <- data.table(
      Sasaran = c("Catin", "Bumil", "Pascasalin", "Baduta"),
      Capaian = c(pendampingan_catin$capaian, pendampingan_bumil$capaian_bumil,
                  pendampingan_pascasalin$capaian,pendampingan_baduta$capaian_baduta)
    )
    
    data_spider_pendampingan[, Capaian := ifelse(Capaian > 100 | is.infinite(Capaian), 100, Capaian)]
  })
  
  output$radar_tpk <- renderPlotly({
    data_spider_pendampingan =  data_spider()
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
    
   
  })
  
  tpk_terpilih <- eventReactive(input$cari_tpk,{
    tpk = input$pilih_tpk
  })
  
  output$line_catin <- renderPlotly({
    capaian_catin <- as.data.table(read_fst("data/pendampingan_catin.fst"))
    
    dt_melted <- capaian_catin[`No Register` == tpk_terpilih(), 
                                .SD, 
                                .SDcols = grep("total_orang", names(capaian_catin)[1:29], value = TRUE)]
    dt_melted <- melt(dt_melted, 
                      measure.vars = grep("total_orang", names(dt_melted), value = TRUE),
                      variable.name = "bulan", 
                      value.name = "Total")
    
    # Mengatur label bulan dari Januari hingga Agustus sesuai urutan yang benar
    bulan_labels <- c("Januari", "Februari", "Maret", "April", "Mei", "Juni", "Juli", "Agustus")
    
    # Menambahkan kolom Bulan dengan urutan Januari hingga Agustus
    dt_melted$Bulan <- factor(rep(bulan_labels, length.out = nrow(dt_melted)), levels = bulan_labels)
    
    # Membuat line plot menggunakan plotly
    fig <- plot_ly(dt_melted, x = ~Bulan, y = ~Total, type = 'scatter', mode = 'lines+markers', 
                   line = list(color = 'blue')) %>%
      layout(title = "Tren catin Terdampingi",
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
    
  })
  
  output$line_pascasalin <- renderPlotly({
    capaian_pascasalin <- as.data.table(read_fst("data/pendampingan_pascasalin.fst"))
    
    dt_melted <- capaian_pascasalin[`No Register` == tpk_terpilih(), 
                               .SD, 
                               .SDcols = grep("total_orang", names(capaian_pascasalin)[1:29], value = TRUE)]
    dt_melted <- melt(dt_melted, 
                      measure.vars = grep("total_orang", names(dt_melted), value = TRUE),
                      variable.name = "bulan", 
                      value.name = "Total")
    
    # Mengatur label bulan dari Januari hingga Agustus sesuai urutan yang benar
    bulan_labels <- c("Januari", "Februari", "Maret", "April", "Mei", "Juni", "Juli", "Agustus")
    
    # Menambahkan kolom Bulan dengan urutan Januari hingga Agustus
    dt_melted$Bulan <- factor(rep(bulan_labels, length.out = nrow(dt_melted)), levels = bulan_labels)
    
    # Membuat line plot menggunakan plotly
    fig <- plot_ly(dt_melted, x = ~Bulan, y = ~Total, type = 'scatter', mode = 'lines+markers', 
                   line = list(color = 'blue')) %>%
      layout(title = "Tren pascasalin Terdampingi",
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
    
  })
  
  output$line_baduta <- renderPlotly({
    capaian_baduta <- as.data.table(read_fst("data/pendampingan_baduta.fst"))
    
    dt_melted <- capaian_baduta[`No Register` == tpk_terpilih(), 
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
    fig <- plot_ly(dt_melted, x = ~Bulan, y = ~Total, type = 'scatter', mode = 'lines+markers', 
                   line = list(color = 'blue')) %>%
      layout(title = "Tren Baduta Terdampingi",
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
    
  })
  
  output$line_bumil <- renderPlotly({
    capaian_bumil <- as.data.table(read_fst("data/pendampingan_bumil.fst"))
    
    dt_melted <- capaian_bumil[`No Register` == tpk_terpilih(), 
                                .SD, 
                                .SDcols = grep("total_orang", names(capaian_bumil)[1:29], value = TRUE)]
    dt_melted <- melt(dt_melted, 
                      measure.vars = grep("total_orang", names(dt_melted), value = TRUE),
                      variable.name = "bulan", 
                      value.name = "Total")
    
    # Mengatur label bulan dari Januari hingga Agustus sesuai urutan yang benar
    bulan_labels <- c("Januari", "Februari", "Maret", "April", "Mei", "Juni", "Juli", "Agustus")
    
    # Menambahkan kolom Bulan dengan urutan Januari hingga Agustus
    dt_melted$Bulan <- factor(rep(bulan_labels, length.out = nrow(dt_melted)), levels = bulan_labels)
    
    # Membuat line plot menggunakan plotly
    fig <- plot_ly(dt_melted, x = ~Bulan, y = ~Total, type = 'scatter', mode = 'lines+markers', 
                   line = list(color = 'blue')) %>%
      layout(title = "Tren Jumlah Ibu Hamil Terdampingi",
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
    
  })
  #akhir tpk
  
  ### bidan
  
  ### akhir bidan
  
  ### ppks
  pelayanan_ppks <- fread("data/pelayanan_ppks.csv")
  pelayanan_ppks$`YANG ADA` <- factor(pelayanan_ppks$`YANG ADA`) 
  pelayanan_ppks$TAHUN <- factor(pelayanan_ppks$TAHUN) 
#   selectInput("tahun_ppks", "Pilih Tahun", 
#               choices = c("2023", "2024"))
#   ),
# column(
#   4,
#   selectInput("bulan_tidak_lapor_ppks", 
#               "Rentang Bulan Tidak Lapor", 
#               choices = c(1:12))
# )
# ),
# fluidRow(
#   input_task_button(
#     label_busy = "Sedang Proses",
#     id = "cari_ppks_umum",
#     label = "Cari"
#   )
  tidak_memiliki_ppks  <- eventReactive(input$cari_ppks_umum, {
    if(input$tahun_ppks == "2024"){
      BULAN1 = "OKTOBER"
    } else{
      BULAN1 = "DESEMBER"
    }
    tidak_memiliki_ppks <- pelayanan_ppks |>
      fsubset(TAHUN == input$tahun_ppks) |>
      fsubset(BULAN == BULAN1) |>
      fsubset(`YANG ADA` == "0") |>
      select(KABUPATEN, KECAMATAN)
    tidak_memiliki_ppks
  })
  
  subtitle_ppks <- eventReactive(input$cari_ppks_umum, {
    if(input$tahun_ppks == "2024"){
      subtitle = paste0("OKTOBER - ", "2024")
    } else{
      subtitle = paste0("DESEMBER - ", "2023")
    }
    subtitle
  })
  
  output$tabel_tidak_memiliki_ppks <- renderUI({
    
    tabel <- reactable(
      tidak_memiliki_ppks(),
      groupBy = "KABUPATEN",
      columns = list(
        KECAMATAN = colDef(aggregate = "count")
      )
    )
    tabel %>%
    #  add_title("KECAMATAN YANG TIDAK MEMILIKI PPKS", align = "left",font_size = 26) %>%
      add_subtitle(subtitle_ppks(), font_size = 16)
  })
  
  output$download_data_ppks_tidak <- renderUI({
    req(input$cari_ppks_umum)
    downloadButton("downloadData", "Download")
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("kec-tidak-memiliki-ppks-", subtitle_ppks(), ".csv", sep="")
    },
    content = function(file) {
      fwrite(tidak_memiliki_ppks(), file)
    }
  )
  
  tidak_melapor_ppks <- eventReactive(input$cari_ppks_umum,{
    if(input$tahun_ppks == "2024"){
      jumlah_bulan <- 10 - as.integer(input$bulan_tidak_lapor_ppks)
    } else{
      jumlah_bulan <- 10 - as.integer(input$bulan_tidak_lapor_ppks)
    }

    tidak_melapor_ppks <- pelayanan_ppks %>%
      fsubset(TAHUN == input$tahun_ppks) |>
      fgroup_by(KABUPATEN, KECAMATAN) %>%
      fsummarise(`YANG LAPOR` = sum(`YANG LAPOR`)) |>
      fsubset(`YANG LAPOR` <= jumlah_bulan)
    tidak_melapor_ppks
  })
  
  subtitle_tidak_lapor_ppks <- eventReactive(input$cari_ppks_umum, {
    if(input$tahun_ppks == "2024"){
      subtitle = paste0("OKTOBER - ", "2024")
    } else{
      subtitle = paste0("DESEMBER - ", "2023")
    }
    subtitle <- paste0("Selama ", input$bulan_tidak_lapor_ppks,
                      " Bulan Pada Tahun ", input$tahun_ppks)
  })
  
  output$tabel_tidak_melapor_ppks <- renderUI({
    tabel <- reactable(
      tidak_melapor_ppks(),
      groupBy = "KABUPATEN",
      theme = reactableTheme(
        borderColor = "#dfe2e5",
        stripedColor = "#f6f8fa",
        highlightColor = "#f0f5f9",
        cellPadding = "8px 12px",
        style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
        searchInputStyle = list(width = "100%")
      ),
      columns = list(
        KECAMATAN = colDef(aggregate = "count")
      )
    )
    tabel %>%
     # add_title("KECAMATAN YANG TIDAK MELAPOR PPKS", align = "left",font_size = 26) %>%
      add_subtitle(subtitle_tidak_lapor_ppks(),font_size = 16)
  })
  
  output$download_data_ppks_tidak_lapor <- renderUI({
    req(input$cari_ppks_umum)
    downloadButton("downloadData_tidak_lapor", "Download")
  })
  
  output$downloadData_tidak_lapor <- downloadHandler(
    filename = function() {
      paste("kec-tidak-lapor-ppks-", subtitle_tidak_lapor_ppks(), ".csv", sep="")
    },
    content = function(file) {
      fwrite(tidak_melapor_ppks(), file)
    }
  )
  
  ### akhir ppks
}

# Run the app
shinyApp(ui = ui, server = server)
