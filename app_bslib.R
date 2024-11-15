library(bslib)
library(data.table)
library(fst)
library(bsicons)

data_wilayah <- read_fst("data/data_daftar_desa.fst")
data_wilayah <- data.table(data_wilayah)

ui <- page_navbar(
  title = "Penguins dashboard",
  theme = bs_theme(
    version = 5,
    bootswatch = "minty"  # You can change this theme (e.g., "cosmo", "darkly")
  ),
  nav_spacer(),
  nav_panel(
    "Rekap Wilayah", 
    page_fluid(
      layout_columns(
        selectInput("pilih_kab", "Pilih Kabupaten", 
                    choices = c("SEMUA KABUPATEN", "PASANGKAYU", "MAMUJU TENGAH",
                                "MAMUJU", "MAJENE", "POLEWALI MANDAR", "MAMASA")),
        selectInput("pilih_kec", "Pilih Kecamatan", choices = NULL),
        selectInput("pilih_desa_kel", "Pilih Desa/Kel", choices = NULL),
      ), #layoyt_column input 1
      layout_columns(
        fixed_width = TRUE,col_widths = 2,
        input_task_button(
          label_busy = "Sedang Proses",
          id = "cari_rekap",
          label = "Cari"
        )
      ), #layout column input 2
      layout_columns(
        col_widths = 3,
        value_box(
          title = "Jumlah TPK",
          value = textOutput("jumlah_tpk"),
          showcase = bs_icon("clock")
        ),
        value_box(
          title = "Status Orientasi",
          value = "100%",
          showcase = bs_icon("clock")
        )
      ) #layout colimns value box
    )
  ),
  nav_panel("Bill Depth", "B"),
  nav_panel("Body Mass", "C"),
)

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
  
  #menu rekap wilayah
  tim_pendamping_keluarga <- fread("data/daftar_tpk.csv")
  tim_pendamping_keluarga <- data.table(tim_pendamping_keluarga)
  
  output$jumlah_tpk <- renderText({
    nrow(tim_pendamping_keluarga[
      Kab %in% value_filter_kab() & Kec %in% value_filter_kec() & `Desa/Kel` %in% value_filter_desa_kel(),
    ])
    
  }) 
  
  #akhir rekap wilayah
  

}

shinyApp(ui, server)