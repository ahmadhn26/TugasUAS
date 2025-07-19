# ===============================================================================
# DASHBOARD ANALITIKA SOVI NUSANTARA - VERSI DIPERBAIKI
# ===============================================================================
# Dashboard R Shiny untuk Analisis Komprehensif Data Social Vulnerability Index
# Nama Unik: "SOVI-STAT EXPLORER: Dashboard Analitika Kerentanan Sosial Indonesia"
# ===============================================================================

# ===============================================================================
# 1. PEMUATAN LIBRARY YANG DIPERLUKAN
# ===============================================================================
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)
library(ggplot2)
library(DT)
library(rmarkdown)
library(officer)
library(car)
library(plotly)
library(corrplot)
library(RColorBrewer)
library(gridExtra)
library(viridis)
library(cluster)
library(sf)
library(leaflet)
library(mapview)
library(RColorBrewer)
library(mapview)
library(spdep)






# ===============================================================================
# 2. PEMUATAN DAN PERSIAPAN DATA - DIPERBAIKI
# ===============================================================================

# Fungsi untuk memuat data dengan penanganan error
load_data <- function() {
  tryCatch({
    # Coba muat data dari file lokal
    sovi_data <- read.csv("data/sovi_data.csv", stringsAsFactors = FALSE)
    distance_data <- read.csv("data/distance.csv", stringsAsFactors = FALSE)
    
    
    
    list(
      sovi = sovi_data,
      distance = distance_data,
      status = "success",
      message = "Data berhasil dimuat dari file lokal"
    )
  }, error = function(e) {
    # Jika gagal, buat data dummy berdasarkan struktur asli TANPA variabel tambahan
    set.seed(123)
    n <- 511  # Sesuai dengan data asli
    
    # Buat data dummy HANYA dengan variabel yang ada di dataset asli
    sovi_dummy <- data.frame(
      DISTRICTCODE = 1101:(1100 + n),
      CHILDREN = round(runif(n, 5, 20), 2),
      FEMALE = round(runif(n, 45, 55), 2),
      ELDERLY = round(runif(n, 1, 8), 2),
      FHEAD = round(runif(n, 10, 30), 2),
      FAMILYSIZE = round(runif(n, 3, 6), 2),
      NOELECTRIC = round(runif(n, 0, 5), 2),
      LOWEDU = round(runif(n, 15, 40), 2),
      GROWTH = round(runif(n, 0.5, 3), 2),
      POVERTY = round(runif(n, 5, 35), 2),
      ILLITERATE = round(runif(n, 2, 15), 2),
      NOTRAINING = round(runif(n, 85, 99), 2),
      DPRONE = round(runif(n, 30, 95), 2),
      RENTED = round(runif(n, 2, 10), 2),
      NOSEWER = round(runif(n, 10, 50), 2),
      TAPWATER = round(runif(n, 3, 25), 2),
      POPULATION = round(runif(n, 50000, 500000)),
      stringsAsFactors = FALSE
    )
    
    # Hitung SOVI Score berdasarkan variabel lain
    sovi_dummy$SOVI_SCORE <- with(sovi_dummy, 
                                  0.2 * (POVERTY/100) + 0.15 * (ILLITERATE/100) + 0.1 * (NOELECTRIC/100) + 
                                    0.1 * (LOWEDU/100) + 0.15 * (CHILDREN/100) + 0.1 * (ELDERLY/100) + 
                                    0.1 * (FHEAD/100) + 0.1 * (NOSEWER/100) + rnorm(n, 0, 0.05))
    sovi_dummy$SOVI_SCORE <- pmax(0.1, pmin(0.9, sovi_dummy$SOVI_SCORE))
    
    # Buat matriks jarak berdasarkan data asli
    distance_dummy <- matrix(runif(n*n, 0, 1000), nrow = n, ncol = n)
    diag(distance_dummy) <- 0
    # Buat simetris
    distance_dummy[lower.tri(distance_dummy)] <- t(distance_dummy)[lower.tri(distance_dummy)]
    colnames(distance_dummy) <- paste0("DIST_", 1:n)
    rownames(distance_dummy) <- paste0("DIST_", 1:n)
    distance_dummy <- as.data.frame(distance_dummy)
    
    list(
      sovi = sovi_dummy,
      distance = distance_dummy,
      status = "dummy",
      message = "File data tidak ditemukan. Menggunakan data dummy berdasarkan struktur asli untuk demonstrasi."
    )
  })
}

# Fungsi ini sekarang hanya bertugas memuat file, tidak lebih.
# HANYA MEMBACA FILE, TIDAK LEBIH
load_spatial_data <- function() {
  tryCatch({
    spatial_data <- st_read("data/peta_sovi_sederhana.gpkg", quiet = TRUE)
    list(spatial = spatial_data, status = "success", message = "Data spasial berhasil dimuat")
  }, error = function(e) {
    list(spatial = NULL, status = "error", message = paste("Gagal memuat data spasial:", e$message))
  })
}

# ===============================================================================
# PEMUATAN DAN PENGGABUNGAN DATA - VERSI PERBAIKAN FINAL
# ===============================================================================

# 1. Muat data non-spasial (dari sovi_data.csv)
data_result <- load_data()
sovi_data <- data_result$sovi
distance_data <- data_result$distance

# 2. Buat atau muat Matriks Bobot Spasial
weights_rds_path <- "data/spatial_weights_from_distance.rds"
if (file.exists(weights_rds_path)) {
  spatial_weights <- readRDS(weights_rds_path)
  print("Matriks bobot spasial berhasil dimuat dari file .rds.")
} else {
  print("File .rds tidak ditemukan. Membuat matriks bobot spasial dari distance.csv...")
  if (exists("distance_data") && is.data.frame(distance_data)) {
    dist_matrix_full <- as.matrix(distance_data[, -1])
    inv_dist_matrix <- 1 / (dist_matrix_full + 1e-9)
    diag(inv_dist_matrix) <- 0
    spatial_weights <- mat2listw(inv_dist_matrix, style = "W")
    saveRDS(spatial_weights, file = weights_rds_path)
    print(paste("Matriks bobot spasial telah dibuat dan disimpan di:", weights_rds_path))
  } else {
    stop("Data 'distance.csv' tidak dapat dimuat atau tidak valid untuk membuat matriks bobot.")
  }
}
# Simpan matriks jarak mentah untuk nanti
dist_matrix_raw <- as.matrix(distance_data[, -1])

# 3. Muat data spasial MENTAH
spatial_result <- load_spatial_data()
spatial_data_raw <- spatial_result$spatial

# 4. Lakukan RENAME dengan nama kolom yang sudah kita pastikan dari konsol
spatial_data_renamed <- spatial_data_raw %>%
  rename(
    DISTRICTCODE = KODE_BERSIH,
    DISTRICT_NAME = WADMKK,
    PROVINCE_NAME = WADMPR
  )

# --- LANGKAH PENGGABUNGAN ---

# 5. Ambil kolom kunci dan geometri yang dibutuhkan
spatial_data_geom_clean <- spatial_data_renamed %>%
  select(DISTRICTCODE, DISTRICT_NAME, PROVINCE_NAME, geom)

# 6. Pastikan tipe data kolom kunci sama
sovi_data$DISTRICTCODE <- as.character(sovi_data$DISTRICTCODE)
spatial_data_geom_clean$DISTRICTCODE <- as.character(spatial_data_geom_clean$DISTRICTCODE)

# 7. Gabungkan geometri dengan atribut dari sovi_data
spatial_data <- left_join(spatial_data_geom_clean, sovi_data, by = "DISTRICTCODE")

# --- AKHIR DARI PENGGABUNGAN ---


# Buat daftar pilihan peta dari data yang SUDAH digabung
pilihan_peta_numerik <- names(spatial_data)[sapply(st_drop_geometry(spatial_data), is.numeric)]

# HAPUS BARIS INI KARENA SUDAH DITANGANI DI ATAS
# spatial_weights <- readRDS("data/spatial_weights_from_distance.rds")

# Metadata variabel berdasarkan struktur asli
metadata <- data.frame(
  Variabel = c("DISTRICTCODE", "CHILDREN", "FEMALE", "ELDERLY", "FHEAD", "FAMILYSIZE", 
               "NOELECTRIC", "LOWEDU", "GROWTH", "POVERTY", "ILLITERATE", "NOTRAINING",
               "DPRONE", "RENTED", "NOSEWER", "TAPWATER", "POPULATION", "SOVI_SCORE"),
  Deskripsi = c("Kode distrik/kabupaten", "Persentase anak-anak", "Persentase perempuan", 
                "Persentase lansia", "Persentase kepala keluarga perempuan", "Rata-rata ukuran keluarga",
                "Persentase tanpa listrik", "Persentase pendidikan rendah", "Tingkat pertumbuhan penduduk",
                "Persentase kemiskinan", "Persentase buta huruf", "Persentase tanpa pelatihan",
                "Kerawanan bencana", "Persentase rumah sewa", "Persentase tanpa saluran pembuangan",
                "Persentase akses air bersih", "Jumlah populasi", "Skor kerentanan sosial"),
  Tipe = c("Numerik", "Numerik", "Numerik", "Numerik", "Numerik", "Numerik", 
           "Numerik", "Numerik", "Numerik", "Numerik", "Numerik", "Numerik",
           "Numerik", "Numerik", "Numerik", "Numerik", "Numerik", "Numerik"),
  Range = c("1101-1611", "5-20%", "45-55%", "1-8%", "10-30%", "3-6 orang",
            "0-5%", "15-40%", "0.5-3%", "5-35%", "2-15%", "85-99%",
            "30-95", "2-10%", "10-50%", "3-25%", "50K-500K", "0.1-0.9"),
  stringsAsFactors = FALSE
)

# ===============================================================================
# 3. ANTARMUKA PENGGUNA (UI) - DIPERBAIKI
# ===============================================================================

ui <- dashboardPage(
  skin = "purple",
  
  dashboardHeader(
    title = "SOVI-STAT EXPLORER",
    titleWidth = 350,
    tags$li(class = "dropdown",
            tags$a(href = "#", 
                   style = "color: white; font-size: 12px; padding: 15px;",
                   "Dashboard Analitika Kerentanan Sosial Indonesia"))
  ),
  
  dashboardSidebar(
    width = 350,
    sidebarMenu(
      id = "sidebar",
      menuItem("🏠 Beranda", tabName = "beranda", icon = icon("home")),
      menuItem("📊 Manajemen Data", tabName = "manajemen", icon = icon("database")),
      menuItem("🔍 Eksplorasi Data", tabName = "eksplorasi", icon = icon("chart-pie"),
               menuSubItem("Statistik Deskriptif", tabName = "deskriptif", icon = icon("table")),
               menuSubItem("Visualisasi & Grafik", tabName = "visualisasi", icon = icon("chart-bar")),
               menuSubItem("Analisis Spasial", tabName = "spatial", icon = icon("project-diagram"))  # DIGANTI dari peta
      ),
      menuItem("✅ Uji Asumsi Data", tabName = "asumsi", icon = icon("tasks")),
      menuItem("📈 Statistik Inferensia", icon = icon("calculator"),
               menuSubItem("Uji Beda Rata-rata", tabName = "uji_rata", icon = icon("balance-scale")),
               menuSubItem("Uji Proporsi & Ragam", tabName = "uji_prop_var", icon = icon("percentage")),
               menuSubItem("ANOVA (1 & 2 Arah)", tabName = "anova", icon = icon("sort-amount-down"))
      ),
      menuItem("📉 Regresi Linear Berganda", tabName = "regresi", icon = icon("chart-line"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
      .content-wrapper, .right-side {
        background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%);
      }
      
      .interpretation-box {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        border-left: 5px solid #ffd700;
        padding: 20px;
        border-radius: 10px;
        margin: 15px 0;
        box-shadow: 0 4px 15px rgba(0,0,0,0.1);
      }
      
      .interpretation-box h4 {
        color: #ffd700;
        margin-top: 0;
        font-weight: bold;
      }
      
      .custom-box {
        box-shadow: 0 4px 20px rgba(0,0,0,0.1);
        border-radius: 12px;
        border-top: 3px solid #667eea;
      }
      
      .download-section {
        background: linear-gradient(135deg, #a8edea 0%, #fed6e3 100%);
        padding: 15px;
        border-radius: 10px;
        margin: 10px 0;
      }
      
      .main-header .navbar {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
      }
      
      .sidebar-menu > li.active > a {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
      }
      
      .btn-primary {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        border: none;
      }
      
      .btn-success {
        background: linear-gradient(135deg, #11998e 0%, #38ef7d 100%);
        border: none;
      }
      
      .btn-warning {
        background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%);
        border: none;
      }
    "))
    ),
    
    conditionalPanel(
      condition = "true",
      div(
        style = if(data_result$status == "dummy") {
          "background: linear-gradient(135deg, #ffeaa7 0%, #fab1a0 100%); border: 2px solid #fdcb6e; padding: 15px; margin: 15px; border-radius: 10px; color: #2d3436;"
        } else {
          "background: linear-gradient(135deg, #00b894 0%, #00cec9 100%); border: 2px solid #00b894; padding: 15px; margin: 15px; border-radius: 10px; color: white;"
        },
        icon(if(data_result$status == "dummy") "exclamation-triangle" else "check-circle", class = "fa-2x"),
        h4(style = "margin: 10px 0;", if(data_result$status == "dummy") "Mode Demonstrasi" else "Data Berhasil Dimuat"),
        p(data_result$message, style = "margin: 0; font-size: 14px;"),
        p(paste("Status Data Spasial:", spatial_result$status), style = "margin: 5px 0; font-size: 12px;"),
        p(spatial_result$message, style = "margin: 0; font-size: 12px;")
      )
    ),
    
    tabItems(
      # =====================================================================
      # TAB BERANDA - SAMA SEPERTI SEBELUMNYA
      # =====================================================================
      tabItem(tabName = "beranda",
              fluidRow(
                box(
                  title = "🎯 SOVI-STAT EXPLORER: Dashboard Analitika Kerentanan Sosial Indonesia", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  class = "custom-box",
                  div(
                    style = "font-size: 16px; line-height: 1.8;",
                    div(
                      style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; padding: 20px; border-radius: 10px; margin-bottom: 20px;",
                      h3("🌟 Selamat Datang di Platform Analisis SOVI Terdepan", style = "margin-top: 0; color: #ffd700;"),
                      p("Dashboard ini merupakan solusi komprehensif untuk analisis Social Vulnerability Index (SOVI) di Indonesia dengan teknologi R Shiny terdepan.")
                    ),
                    
                    div(
                      style = "display: grid; grid-template-columns: 1fr 1fr; gap: 20px; margin: 20px 0;",
                      div(
                        style = "background: white; padding: 20px; border-radius: 10px; box-shadow: 0 4px 15px rgba(0,0,0,0.1);",
                        h4("🚀 Fitur Unggulan:", style = "color: #667eea; margin-top: 0;"),
                        tags$ul(
                          style = "list-style-type: none; padding-left: 0;",
                          tags$li("✅ Manajemen data dengan transformasi otomatis"),
                          tags$li("📊 Eksplorasi data dengan visualisasi interaktif"),
                          tags$li("🗺️ Analisis spasial menggunakan matriks jarak"),
                          tags$li("🔬 Uji asumsi statistik komprehensif"),
                          tags$li("📈 Analisis inferensia lengkap (T-test, ANOVA, dll)"),
                          tags$li("📉 Regresi linear berganda dengan diagnostik"),
                          tags$li("📄 Export laporan Word/PDF otomatis")
                        )
                      ),
                      div(
                        style = "background: white; padding: 20px; border-radius: 10px; box-shadow: 0 4px 15px rgba(0,0,0,0.1);",
                        h4("📋 Informasi Dataset:", style = "color: #667eea; margin-top: 0;"),
                        div(
                          style = "background: #f8f9fa; padding: 15px; border-radius: 8px;",
                          p(strong("Jumlah Observasi: "), nrow(sovi_data)),
                          p(strong("Jumlah Variabel: "), ncol(sovi_data)),
                          p(strong("Cakupan Wilayah: "), "511 Distrik"),
                          p(strong("Periode Data: "), "2023-2024"),
                          p(strong("Status: "), span(data_result$status, style = if(data_result$status == "dummy") "color: #e17055;" else "color: #00b894;"))
                        )
                      )
                    )
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "📚 Metadata Variabel Lengkap", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 12,
                  class = "custom-box",
                  DT::dataTableOutput("metadata_table"),
                  br(),
                  div(
                    class = "download-section",
                    h4("📥 Download Options", style = "margin-top: 0; color: #2d3436;"),
                    div(
                      style = "display: flex; gap: 10px; flex-wrap: wrap;",
                      downloadButton("download_metadata", "📄 Metadata (Word)", 
                                     class = "btn-primary", icon = icon("download")),
                      downloadButton("download_beranda_full", "📋 Laporan Beranda Lengkap (Word)", 
                                     class = "btn-success", icon = icon("file-word"))
                    )
                  )
                )
              )
      ),
      
      # =====================================================================
      # TAB MANAJEMEN DATA - DIPERBAIKI
      # =====================================================================
      tabItem(tabName = "manajemen",
              fluidRow(
                box(
                  title = "⚙️ Kontrol Transformasi Data", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 4,
                  class = "custom-box",
                  selectInput("var_numeric", "Pilih Variabel Numerik:",
                              choices = names(select_if(sovi_data, is.numeric)),
                              selected = names(select_if(sovi_data, is.numeric))[1]),
                  numericInput("n_categories", "Jumlah Kategori:", 
                               value = 3, min = 2, max = 5, step = 1),
                  selectInput("method_cat", "Metode Kategorisasi:",
                              choices = c("Quantile" = "quantile", 
                                          "Equal Width" = "equal",
                                          "Manual" = "manual")),
                  conditionalPanel(
                    condition = "input.method_cat == 'manual'",
                    textInput("manual_breaks", "Breakpoints (pisahkan dengan koma):", 
                              placeholder = "0.2, 0.5, 0.8")
                  ),
                  br(),
                  div(
                    class = "download-section",
                    h5("📥 Download Data", style = "margin-top: 0;"),
                    downloadButton("download_transformed", "📊 Data Transformasi (CSV)", 
                                   class = "btn-success", icon = icon("download")),
                    br(), br(),
                    downloadButton("download_manajemen_full", "📋 Laporan Manajemen (Word)", 
                                   class = "btn-primary", icon = icon("file-word"))
                  )
                ),
                
                box(
                  title = "📊 Perbandingan Data Original vs Transformasi", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 8,
                  class = "custom-box",
                  DT::dataTableOutput("comparison_table")
                )
              ),
              
              fluidRow(
                box(
                  title = "📈 Ringkasan Statistik Original", 
                  status = "warning", 
                  solidHeader = TRUE,
                  width = 6,
                  class = "custom-box",
                  verbatimTextOutput("summary_original"),
                  div(class = "interpretation-box",
                      h4("🔍 Interpretasi Statistik Deskriptif"),
                      textOutput("interpretation_summary")
                  )
                ),
                
                box(
                  title = "📋 Tabel Frekuensi Kategori", 
                  status = "success", 
                  solidHeader = TRUE,
                  width = 6,
                  class = "custom-box",
                  verbatimTextOutput("frequency_table"),
                  div(class = "interpretation-box",
                      h4("📊 Interpretasi Distribusi Kategori"),
                      textOutput("interpretation_frequency")
                  )
                )
              )
      ),
      
      # =====================================================================
      # TAB STATISTIK DESKRIPTIF - SAMA SEPERTI SEBELUMNYA
      # =====================================================================
      tabItem(tabName = "deskriptif",
              fluidRow(
                box(
                  title = "📊 Statistik Deskriptif Komprehensif", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  class = "custom-box",
                  tabsetPanel(
                    tabPanel("📈 Ringkasan Umum",
                             verbatimTextOutput("descriptive_stats_full"),
                             div(class = "interpretation-box",
                                 h4("🔍 Interpretasi Statistik Deskriptif"),
                                 textOutput("interpretation_descriptive")
                             )
                    ),
                    tabPanel("📊 Statistik per Variabel",
                             selectInput("desc_var", "Pilih Variabel:",
                                         choices = names(select_if(sovi_data, is.numeric))),
                             verbatimTextOutput("single_var_stats"),
                             div(class = "interpretation-box",
                                 h4("📋 Interpretasi Variabel Terpilih"),
                                 textOutput("interpretation_single_var")
                             )
                    ),
                    tabPanel("🔗 Matriks Korelasi",
                             plotOutput("correlation_matrix", height = "500px"),
                             div(class = "interpretation-box",
                                 h4("🔗 Interpretasi Korelasi"),
                                 textOutput("interpretation_correlation")
                             )
                    )
                  ),
                  br(),
                  div(
                    class = "download-section",
                    h4("📥 Download Options", style = "margin-top: 0;"),
                    div(
                      style = "display: flex; gap: 10px; flex-wrap: wrap;",
                      downloadButton("download_descriptive", "📄 Statistik Deskriptif (Word)", 
                                     class = "btn-primary", icon = icon("download")),
                      downloadButton("download_correlation_plot", "🖼️ Matriks Korelasi (JPG)", 
                                     class = "btn-warning", icon = icon("image")),
                      downloadButton("download_deskriptif_full", "📋 Laporan Lengkap (Word)", 
                                     class = "btn-success", icon = icon("file-word"))
                    )
                  )
                )
              )
      ),
      
      # =====================================================================
      # TAB VISUALISASI - DIPERBAIKI
      # =====================================================================
      tabItem(tabName = "visualisasi",
              fluidRow(
                box(
                  title = "🎨 Kontrol Visualisasi", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 3,
                  class = "custom-box",
                  selectInput("plot_var", "Pilih Variabel:",
                              choices = names(select_if(sovi_data, is.numeric))),
                  selectInput("plot_type", "Jenis Plot:",
                              choices = c("Histogram" = "hist", 
                                          "Boxplot" = "box", 
                                          "Density Plot" = "density",
                                          "Scatter Plot" = "scatter",
                                          "Violin Plot" = "violin")),
                  conditionalPanel(
                    condition = "input.plot_type == 'scatter'",
                    selectInput("scatter_y", "Variabel Y:",
                                choices = names(select_if(sovi_data, is.numeric)))
                  ),
                  conditionalPanel(
                    condition = "input.plot_type %in% c('box', 'violin')",
                    selectInput("group_var_plot", "Variabel Kelompok:",
                                choices = c("Tidak ada" = "none"))  # DIPERBAIKI: hanya opsi "none"
                  ),
                  selectInput("color_theme", "Tema Warna:",
                              choices = c("Default" = "default",
                                          "Viridis" = "viridis", 
                                          "Plasma" = "plasma",
                                          "Set1" = "set1")),
                  br(),
                  div(
                    class = "download-section",
                    h5("📥 Download", style = "margin-top: 0;"),
                    downloadButton("download_plot", "🖼️ Plot (JPG)", 
                                   class = "btn-success", icon = icon("download"))
                  )
                ),
                
                box(
                  title = "📊 Visualisasi Data Interaktif", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 9,
                  class = "custom-box",
                  plotlyOutput("data_plot_interactive", height = "500px"),
                  div(class = "interpretation-box",
                      h4("📈 Interpretasi Visualisasi"),
                      textOutput("interpretation_plot")
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "📊 Dashboard Multi-Plot", 
                  status = "warning", 
                  solidHeader = TRUE,
                  width = 12,
                  class = "custom-box",
                  plotOutput("multi_plot_dashboard", height = "600px"),
                  br(),
                  div(
                    class = "download-section",
                    h4("📥 Download Options", style = "margin-top: 0;"),
                    div(
                      style = "display: flex; gap: 10px; flex-wrap: wrap;",
                      downloadButton("download_multi_plot", "🖼️ Multi-Plot (JPG)", 
                                     class = "btn-warning", icon = icon("image")),
                      downloadButton("download_visualisasi_full", "📋 Laporan Visualisasi (Word)", 
                                     class = "btn-primary", icon = icon("file-word"))
                    )
                  )
                )
              )
      ),
      
      # =====================================================================
      # TAB PETA INTERAKTIF - FITUR EKSPLORASI DATA
      # =====================================================================
      tabItem(tabName = "spatial",
              fluidRow(
                box(
                  title = "🗺️ Kontrol Peta Interaktif",
                  status = "primary", solidHeader = TRUE, width = 4, class = "custom-box",
                  selectInput("map_var", "Pilih Variabel untuk Pemetaan:",
                              choices = pilihan_peta_numerik,
                              selected = "POVERTY"
                  ),
                  selectInput("color_palette", "Palet Warna:",
                              choices = c("Viridis" = "viridis", "Plasma" = "plasma", "Inferno" = "inferno", "Blues" = "Blues", "Reds" = "Reds"),
                              selected = "viridis"
                  )
                ),
                box(
                  title = "🗺️ Peta Interaktif SOVI Indonesia",
                  status = "info", solidHeader = TRUE, width = 8, class = "custom-box",
                  leafletOutput("sovi_map", height = "600px"),
                  br(),
                  div(class = "interpretation-box",
                      h4("📊 Interpretasi Peta"),
                      textOutput("map_interpretation"),
                      div(class = "download-section",
                          downloadButton("download_map_interpretation", "📥 Download Interpretasi (Word)", class = "btn-primary", icon = icon("file-word"))
                      )
                  ),
                  br(),
                  div(class = "interpretation-box",
                      h4("📊 Statistik Peta"),
                      verbatimTextOutput("map_stats"),
                      div(class = "download-section",
                          downloadButton("download_map_stats", "📥 Download Statistik (Word)", class = "btn-success", icon = icon("file-word"))
                      )
                  )
                )
              ),
              
              # Baris baru untuk Analisis
              fluidRow(
                box(
                  title = "🔬 Analisis Autokorelasi Spasial (Moran's I)",
                  status = "success", solidHeader = TRUE, width = 12, class = "custom-box",
                  
                  # Tombol untuk menjalankan analisis
                  actionButton("run_moran", "Jalankan Uji Moran's I pada Variabel Terpilih", icon = icon("calculator"), class = "btn-success"),
                  
                  hr(), # Garis pemisah
                  
                  # Tempat untuk menampilkan hasil
                  verbatimTextOutput("moran_test_result"),
                  
                  # Tempat untuk interpretasi
                  div(class = "interpretation-box",
                      h4("📊 Interpretasi Hasil Uji Moran's I"),
                      textOutput("moran_interpretation")
                  ),
                  div(class = "download-section",
                      downloadButton("download_moran_result", "📥 Download Hasil Moran's I (Word)", class = "btn-primary", icon = icon("file-word")),
                      downloadButton("download_moran_plot", "🖼️ Plot Moran's I (JPG)", class = "btn-warning", icon = icon("image")),
                      downloadButton("download_moran_interpretation", "📥 Download Interpretasi Moran's I (Word)", class = "btn-success", icon = icon("file-word"))
                  )
                )
              )
      ),
      
      # =====================================================================
      # TAB UJI ASUMSI - DIPERBAIKI
      # =====================================================================
      tabItem(tabName = "asumsi",
              fluidRow(
                box(
                  title = "⚙️ Kontrol Uji Asumsi", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 4,
                  class = "custom-box",
                  selectInput("assumption_var", "Pilih Variabel untuk Uji Normalitas:",
                              choices = names(select_if(sovi_data, is.numeric))),
                  # DIPERBAIKI: Hanya gunakan variabel kategori yang dibuat dari manajemen data
                  selectInput("group_var", "Pilih Variabel Kelompok untuk Uji Homogenitas:",
                              choices = c("Tidak ada" = "none")),  # Akan diupdate secara dinamis
                  numericInput("alpha_level", "Tingkat Signifikansi (α):", 
                               value = 0.05, min = 0.01, max = 0.1, step = 0.01),
                  br(),
                  div(
                    class = "download-section",
                    h5("📥 Download", style = "margin-top: 0;"),
                    downloadButton("download_assumptions", "📄 Laporan Uji Asumsi (Word)", 
                                   class = "btn-primary", icon = icon("download"))
                  ),
                  div(class = "download-section",
                      downloadButton("download_normality_plot", "🖼️ Plot Normalitas (JPG)", class = "btn-warning", icon = icon("image")),
                      downloadButton("download_normality_interpretation", "📥 Interpretasi Normalitas (Word)", class = "btn-success", icon = icon("file-word"))
                  ),
                  div(class = "download-section",
                      downloadButton("download_homogeneity_plot", "🖼️ Plot Homogenitas (JPG)", class = "btn-warning", icon = icon("image")),
                      downloadButton("download_homogeneity_interpretation", "📥 Interpretasi Homogenitas (Word)", class = "btn-success", icon = icon("file-word"))
                  )
                ),
                
                box(
                  title = "✅ Hasil Uji Asumsi Statistik", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 8,
                  class = "custom-box",
                  tabsetPanel(
                    tabPanel("📊 Uji Normalitas",
                             verbatimTextOutput("normality_test"),
                             plotOutput("normality_plot", height = "300px"),
                             div(class = "interpretation-box",
                                 h4("📈 Interpretasi Uji Normalitas"),
                                 textOutput("normality_interpretation")
                             )
                    ),
                    tabPanel("⚖️ Uji Homogenitas",
                             verbatimTextOutput("homogeneity_test"),
                             plotOutput("homogeneity_plot", height = "300px"),
                             div(class = "interpretation-box",
                                 h4("⚖️ Interpretasi Uji Homogenitas"),
                                 textOutput("homogeneity_interpretation")
                             )
                    )
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "📋 Ringkasan Uji Asumsi", 
                  status = "warning", 
                  solidHeader = TRUE,
                  width = 12,
                  class = "custom-box",
                  DT::dataTableOutput("assumption_summary_table"),
                  br(),
                  downloadButton("download_asumsi_full", "📋 Laporan Lengkap Uji Asumsi (Word)", 
                                 class = "btn-success", icon = icon("file-word"))
                )
              )
      ),
      
      # =====================================================================
      # TAB UJI BEDA RATA-RATA - DIPERBAIKI
      # =====================================================================
      tabItem(tabName = "uji_rata",
              fluidRow(
                box(
                  title = "⚙️ Kontrol Uji Beda Rata-rata", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 4,
                  class = "custom-box",
                  selectInput("t_test_var", "Pilih Variabel Numerik:",
                              choices = names(select_if(sovi_data, is.numeric))),
                  selectInput("t_test_type", "Jenis Uji:",
                              choices = c("One Sample T-Test (1 Kelompok)" = "one_sample",
                                          "Two Sample T-Test (2 Kelompok)" = "two_sample",
                                          "Paired T-Test (Berpasangan)" = "paired")),
                  conditionalPanel(
                    condition = "input.t_test_type == 'one_sample'",
                    numericInput("mu_value", "Nilai μ₀ (Hipotesis):", value = 0)
                  ),
                  conditionalPanel(
                    condition = "input.t_test_type %in% c('two_sample', 'paired')",
                    selectInput("group_var_t", "Variabel Kelompok:",
                                choices = c("Tidak ada" = "none"))  # Akan diupdate secara dinamis
                  ),
                  numericInput("alpha_t", "Tingkat Signifikansi (α):", 
                               value = 0.05, min = 0.01, max = 0.1, step = 0.01),
                  checkboxInput("equal_var", "Asumsi Ragam Sama", value = TRUE),
                  br(),
                  div(
                    class = "download-section",
                    h5("📥 Download", style = "margin-top: 0;"),
                    downloadButton("download_t_test", "📄 Hasil Uji T (Word)", 
                                   class = "btn-primary", icon = icon("download"))
                  ),
                  div(class = "download-section",
                      downloadButton("download_ttest_plot", "🖼️ Plot T-Test (JPG)", class = "btn-warning", icon = icon("image")),
                      downloadButton("download_ttest_interpretation", "📥 Interpretasi T-Test (Word)", class = "btn-success", icon = icon("file-word"))
                  )
                ),
                
                box(
                  title = "📊 Hasil Uji Beda Rata-rata", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 8,
                  class = "custom-box",
                  verbatimTextOutput("t_test_result"),
                  plotOutput("t_test_plot", height = "300px"),
                  div(class = "interpretation-box",
                      h4("📈 Interpretasi Hasil Uji T"),
                      textOutput("t_test_interpretation")
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "📋 Ringkasan Statistik Kelompok", 
                  status = "success", 
                  solidHeader = TRUE,
                  width = 12,
                  class = "custom-box",
                  DT::dataTableOutput("group_summary_table"),
                  br(),
                  downloadButton("download_uji_rata_full", "📋 Laporan Lengkap Uji Rata-rata (Word)", 
                                 class = "btn-success", icon = icon("file-word"))
                )
              )
      ),
      
      # =====================================================================
      # TAB UJI PROPORSI & RAGAM - DIPERBAIKI
      # =====================================================================
      tabItem(tabName = "uji_prop_var",
              fluidRow(
                box(
                  title = "⚙️ Kontrol Uji Proporsi & Ragam", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 4,
                  class = "custom-box",
                  selectInput("test_type_pv", "Jenis Uji:",
                              choices = c("Uji Ragam 1 Kelompok" = "var_one",
                                          "Uji Ragam 2 Kelompok" = "var_two")),  # DIPERBAIKI: Hapus uji proporsi
                  
                  selectInput("var_test_var", "Variabel Numerik:",
                              choices = names(select_if(sovi_data, is.numeric))),
                  conditionalPanel(
                    condition = "input.test_type_pv == 'var_one'",
                    numericInput("var_null", "Ragam H₀:", value = 1, min = 0.01, step = 0.01)
                  ),
                  conditionalPanel(
                    condition = "input.test_type_pv == 'var_two'",
                    selectInput("group_var_var", "Variabel Kelompok:",
                                choices = c("Tidak ada" = "none"))  # Akan diupdate secara dinamis
                  ),
                  
                  numericInput("alpha_pv", "Tingkat Signifikansi (α):", 
                               value = 0.05, min = 0.01, max = 0.1, step = 0.01),
                  br(),
                  div(
                    class = "download-section",
                    h5("📥 Download", style = "margin-top: 0;"),
                    downloadButton("download_prop_var", "📄 Hasil Uji (Word)", 
                                   class = "btn-primary", icon = icon("download"))
                  ),
                  div(class = "download-section",
                      downloadButton("download_propvar_plot", "🖼️ Plot Ragam (JPG)", class = "btn-warning", icon = icon("image")),
                      downloadButton("download_propvar_interpretation", "📥 Interpretasi Ragam (Word)", class = "btn-success", icon = icon("file-word"))
                  )
                ),
                
                box(
                  title = "📊 Hasil Uji Ragam", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 8,
                  class = "custom-box",
                  verbatimTextOutput("prop_var_result"),
                  plotOutput("prop_var_plot", height = "300px"),
                  div(class = "interpretation-box",
                      h4("📊 Interpretasi Hasil"),
                      textOutput("prop_var_interpretation")
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "📋 Ringkasan Statistik", 
                  status = "warning", 
                  solidHeader = TRUE,
                  width = 12,
                  class = "custom-box",
                  DT::dataTableOutput("prop_var_summary_table"),
                  br(),
                  downloadButton("download_prop_var_full", "📋 Laporan Lengkap (Word)", 
                                 class = "btn-success", icon = icon("file-word"))
                )
              )
      ),
      
      # =====================================================================
      # TAB ANOVA - DIPERBAIKI
      # =====================================================================
      tabItem(tabName = "anova",
              fluidRow(
                box(
                  title = "⚙️ Kontrol ANOVA", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 4,
                  class = "custom-box",
                  selectInput("anova_type", "Jenis ANOVA:",
                              choices = c("One-Way ANOVA (1 Arah)" = "one_way")),  # DIPERBAIKI: Hanya one-way
                  selectInput("anova_y", "Variabel Dependen (Y):",
                              choices = names(select_if(sovi_data, is.numeric))),
                  selectInput("anova_x1", "Faktor 1 (X1):",
                              choices = c("Tidak ada" = "none")),  # Akan diupdate secara dinamis
                  numericInput("alpha_anova", "Tingkat Signifikansi (α):", 
                               value = 0.05, min = 0.01, max = 0.1, step = 0.01),
                  br(),
                  div(
                    class = "download-section",
                    h5("📥 Download", style = "margin-top: 0;"),
                    downloadButton("download_anova", "📄 Hasil ANOVA (Word)", 
                                   class = "btn-primary", icon = icon("download"))
                  ),
                  div(class = "download-section",
                      downloadButton("download_anova_plot", "🖼️ Plot ANOVA (JPG)", class = "btn-warning", icon = icon("image")),
                      downloadButton("download_anova_interpretation", "📥 Interpretasi ANOVA (Word)", class = "btn-success", icon = icon("file-word"))
                  )
                ),
                
                box(
                  title = "📊 Hasil ANOVA", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 8,
                  class = "custom-box",
                  verbatimTextOutput("anova_result"),
                  div(class = "interpretation-box",
                      h4("📈 Interpretasi Hasil ANOVA"),
                      textOutput("anova_interpretation")
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "📊 Visualisasi ANOVA", 
                  status = "warning", 
                  solidHeader = TRUE,
                  width = 6,
                  class = "custom-box",
                  plotOutput("anova_plot", height = "400px")
                ),
                
                box(
                  title = "📋 Uji Lanjut (Post-Hoc)", 
                  status = "success", 
                  solidHeader = TRUE,
                  width = 6,
                  class = "custom-box",
                  conditionalPanel(
                    condition = "output.show_posthoc",
                    verbatimTextOutput("posthoc_result"),
                    div(class = "interpretation-box",
                        h4("🔍 Interpretasi Uji Lanjut"),
                        textOutput("posthoc_interpretation")
                    )
                  ),
                  conditionalPanel(
                    condition = "!output.show_posthoc",
                    div(
                      style = "text-align: center; padding: 50px; color: #666;",
                      icon("info-circle", class = "fa-3x"),
                      h4("Uji lanjut akan muncul jika ANOVA signifikan")
                    )
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "📋 Ringkasan Lengkap ANOVA", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  class = "custom-box",
                  DT::dataTableOutput("anova_summary_table"),
                  br(),
                  downloadButton("download_anova_full", "📋 Laporan Lengkap ANOVA (Word)", 
                                 class = "btn-success", icon = icon("file-word"))
                )
              )
      ),
      
      # =====================================================================
      # TAB REGRESI LINEAR BERGANDA - SAMA SEPERTI SEBELUMNYA
      # =====================================================================
      tabItem(tabName = "regresi",
              fluidRow(
                box(
                  title = "⚙️ Kontrol Regresi Linear Berganda", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 4,
                  class = "custom-box",
                  selectInput("reg_y", "Variabel Dependen (Y):",
                              choices = names(select_if(sovi_data, is.numeric))),
                  checkboxGroupInput("reg_x", "Variabel Independen (X):",
                                     choices = names(select_if(sovi_data, is.numeric)),
                                     selected = names(select_if(sovi_data, is.numeric))[1:3]),
                  checkboxInput("include_interaction", "Sertakan Interaksi", value = FALSE),
                  conditionalPanel(
                    condition = "input.include_interaction",
                    selectInput("interaction_vars", "Pilih 2 Variabel untuk Interaksi:",
                                choices = NULL, multiple = TRUE)
                  ),
                  numericInput("alpha_reg", "Tingkat Signifikansi (α):", 
                               value = 0.05, min = 0.01, max = 0.1, step = 0.01),
                  br(),
                  div(
                    class = "download-section",
                    h5("📥 Download", style = "margin-top: 0;"),
                    downloadButton("download_regression", "📄 Laporan Regresi (Word)", 
                                   class = "btn-primary", icon = icon("download"))
                  ),
                  div(class = "download-section",
                      downloadButton("download_regression_scatter", "🖼️ Plot Regresi (JPG)", class = "btn-warning", icon = icon("image")),
                      downloadButton("download_regression_scatter_interpretation", "📥 Interpretasi Plot Regresi (Word)", class = "btn-success", icon = icon("file-word"))
                  ),
                  div(class = "download-section",
                      downloadButton("download_regression_diag", "🖼️ Plot Diagnostik (JPG)", class = "btn-warning", icon = icon("image"))
                  ),
                  div(class = "download-section",
                      downloadButton("download_regression_interpretation", "📥 Interpretasi Model Regresi (Word)", class = "btn-success", icon = icon("file-word"))
                  )
                ),
                
                box(
                  title = "📊 Hasil Regresi Linear Berganda", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 8,
                  class = "custom-box",
                  verbatimTextOutput("regression_summary"),
                  div(class = "interpretation-box",
                      h4("📈 Interpretasi Model Regresi"),
                      textOutput("regression_interpretation")
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "📈 Plot Regresi", 
                  status = "success", 
                  solidHeader = TRUE,
                  width = 6,
                  class = "custom-box",
                  plotOutput("regression_scatter_plot", height = "400px"),
                  div(class = "interpretation-box",
                      h4("📊 Interpretasi Plot Regresi"),
                      textOutput("regression_plot_interpretation")
                  )
                ),
                
                box(
                  title = "📊 Plot Diagnostik Regresi", 
                  status = "warning", 
                  solidHeader = TRUE,
                  width = 6,
                  class = "custom-box",
                  plotOutput("regression_plots", height = "400px")
                )
              ),
              
              fluidRow(
                box(
                  title = "🔍 Uji Asumsi Regresi", 
                  status = "warning", 
                  solidHeader = TRUE,
                  width = 6,
                  class = "custom-box",
                  tabsetPanel(
                    tabPanel("Linearitas",
                             verbatimTextOutput("linearity_test"),
                             div(class = "interpretation-box",
                                 h4("📏 Interpretasi Linearitas"),
                                 textOutput("linearity_interpretation")
                             )
                    ),
                    tabPanel("Multikolinearitas",
                             verbatimTextOutput("multicollinearity_test"),
                             div(class = "interpretation-box",
                                 h4("🔗 Interpretasi Multikolinearitas"),
                                 textOutput("multicollinearity_interpretation")
                             )
                    ),
                    tabPanel("Heteroskedastisitas",
                             verbatimTextOutput("heteroscedasticity_test"),
                             div(class = "interpretation-box",
                                 h4("📊 Interpretasi Heteroskedastisitas"),
                                 textOutput("heteroscedasticity_interpretation")
                             )
                    )
                  )
                ),
                
                box(
                  title = "📋 Ringkasan Lengkap Model", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 6,
                  class = "custom-box",
                  DT::dataTableOutput("regression_summary_table"),
                  br(),
                  div(
                    class = "download-section",
                    h4("📥 Download Options", style = "margin-top: 0;"),
                    div(
                      style = "display: flex; gap: 10px; flex-wrap: wrap;",
                      downloadButton("download_regression_plots", "🖼️ Plot Diagnostik (JPG)", 
                                     class = "btn-warning", icon = icon("image")),
                      downloadButton("download_regresi_full", "📋 Laporan Lengkap Regresi (Word)", 
                                     class = "btn-success", icon = icon("file-word"))
                    )
                  )
                )
              )
      )
    )
  )
)

# ===============================================================================
# 4. LOGIKA SERVER - DIPERBAIKI
# ===============================================================================

server <- function(input, output, session) {
  
  # =====================================================================
  # REACTIVE VALUES DAN FUNGSI HELPER
  # =====================================================================
  
  # Reactive untuk menyimpan variabel kategori yang dibuat
  categorical_vars <- reactiveValues(data = list())
  
  observe({
    if(!is.null(input$reg_x) && length(input$reg_x) >= 2) {
      updateSelectInput(session, "interaction_vars",
                        choices = input$reg_x,
                        selected = input$reg_x[1:min(2, length(input$reg_x))])
    }
  })
  
  # Update pilihan variabel kelompok berdasarkan variabel kategori yang tersedia
  observe({
    cat_choices <- c("Tidak ada" = "none")
    if(length(categorical_vars$data) > 0) {
      cat_choices <- c(cat_choices, names(categorical_vars$data))
    }
    
    updateSelectInput(session, "group_var", choices = cat_choices)
    updateSelectInput(session, "group_var_t", choices = cat_choices)
    updateSelectInput(session, "group_var_var", choices = cat_choices)
    updateSelectInput(session, "anova_x1", choices = cat_choices)
  })
  
  create_interpretation <- function(test_result, test_type, alpha = 0.05, additional_info = NULL) {
    if(is.null(test_result) || (is.list(test_result) && is.null(test_result$p.value))) {
      return("Tidak dapat menentukan interpretasi karena data tidak memadai.")
    }
    
    p_value <- if("p.value" %in% names(test_result)) test_result$p.value else NA
    
    if(is.na(p_value)) return("Tidak dapat menentukan interpretasi.")
    
    significance <- if(p_value < alpha) "signifikan" else "tidak signifikan"
    decision <- if(p_value < alpha) "menolak H₀" else "gagal menolak H₀"
    
    base_interpretation <- paste0("Berdasarkan hasil analisis, diperoleh nilai p-value = ", 
                                  round(p_value, 4), ". Karena p-value ", 
                                  if(p_value < alpha) "lebih kecil" else "lebih besar atau sama dengan",
                                  " dari α = ", alpha, ", maka hasil uji ", significance, 
                                  " dan kita ", decision, ".")
    
    specific_interpretation <- switch(test_type,
                                      "normalitas" = if(p_value >= alpha) {
                                        " Hal ini menunjukkan bahwa data mengikuti distribusi normal, sehingga asumsi normalitas terpenuhi untuk analisis parametrik."
                                      } else {
                                        " Hal ini menunjukkan bahwa data tidak mengikuti distribusi normal, sehingga perlu dipertimbangkan penggunaan uji non-parametrik."
                                      },
                                      "homogenitas" = if(p_value >= alpha) {
                                        " Hal ini menunjukkan bahwa ragam antar kelompok homogen (sama), sehingga asumsi homogenitas ragam terpenuhi."
                                      } else {
                                        " Hal ini menunjukkan bahwa ragam antar kelompok tidak homogen (berbeda), sehingga perlu dipertimbangkan koreksi atau uji alternatif."
                                      },
                                      "t-test" = if(p_value < alpha) {
                                        " Hal ini menunjukkan bahwa terdapat perbedaan rata-rata yang signifikan secara statistik."
                                      } else {
                                        " Hal ini menunjukkan bahwa tidak terdapat perbedaan rata-rata yang signifikan secara statistik."
                                      },
                                      "ANOVA" = if(p_value < alpha) {
                                        " Hal ini menunjukkan bahwa terdapat perbedaan rata-rata yang signifikan antar kelompok, sehingga diperlukan uji lanjut (post-hoc) untuk mengetahui kelompok mana yang berbeda."
                                      } else {
                                        " Hal ini menunjukkan bahwa tidak terdapat perbedaan rata-rata yang signifikan antar kelompok."
                                      },
                                      ""
    )
    
    return(paste0(base_interpretation, specific_interpretation))
  }
  
  interpret_descriptive <- function(summary_stats, var_name) {
    if(is.null(summary_stats)) return("Data tidak tersedia untuk interpretasi.")
    
    mean_val <- summary_stats["Mean"]
    median_val <- summary_stats["Median"]
    
    if(is.na(mean_val) || is.na(median_val)) {
      var_data <- sovi_data[[var_name]]
      var_data <- var_data[!is.na(var_data)]
      mean_val <- mean(var_data)
      median_val <- median(var_data)
      sd_val <- sd(var_data)
    } else {
      var_data <- sovi_data[[var_name]]
      var_data <- var_data[!is.na(var_data)]
      sd_val <- sd(var_data)
    }
    
    skewness <- if(mean_val > median_val) "positif (ekor kanan)" else if(mean_val < median_val) "negatif (ekor kiri)" else "simetris"
    variability <- if(sd_val/mean_val < 0.1) "rendah" else if(sd_val/mean_val < 0.3) "sedang" else "tinggi"
    
    paste0("Variabel ", var_name, " memiliki rata-rata ", round(mean_val, 3), 
           " dan median ", round(median_val, 3), ". Distribusi data menunjukkan kecenderungan ", skewness,
           ". Variabilitas data tergolong ", variability, " dengan standar deviasi ", round(sd_val, 3), 
           ". Interpretasi ini menunjukkan karakteristik distribusi yang spesifik untuk variabel ini dalam konteks analisis SOVI.")
  }
  
  # =====================================================================
  # TAB BERANDA - SAMA SEPERTI SEBELUMNYA
  # =====================================================================
  
  output$metadata_table <- DT::renderDataTable({
    DT::datatable(metadata, 
                  options = list(pageLength = 15, scrollX = TRUE, dom = 'Bfrtip'),
                  class = 'cell-border stripe hover',
                  rownames = FALSE) %>%
      DT::formatStyle(columns = 1:ncol(metadata), fontSize = '12px')
  })
  
  output$download_metadata <- downloadHandler(
    filename = function() {
      paste0("metadata_variabel_SOVI_", Sys.Date(), ".docx")
    },
    content = function(file) {
      doc <- read_docx()
      doc <- doc %>%
        body_add_par("METADATA VARIABEL DATASET SOVI", style = "heading 1") %>%
        body_add_par("SOVI-STAT EXPLORER: Dashboard Analitika Kerentanan Sosial Indonesia", style = "heading 2") %>%
        body_add_par(paste("Tanggal:", Sys.Date()), style = "Normal") %>%
        body_add_par(paste("Jumlah Observasi:", nrow(sovi_data)), style = "Normal") %>%
        body_add_par(paste("Jumlah Variabel:", ncol(sovi_data)), style = "Normal") %>%
        body_add_par("", style = "Normal") %>%
        body_add_table(metadata, style = "table_template") %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("Keterangan:", style = "heading 2") %>%
        body_add_par("Dataset ini berisi informasi Social Vulnerability Index (SOVI) untuk berbagai wilayah di Indonesia. SOVI merupakan indeks yang mengukur tingkat kerentanan sosial suatu wilayah terhadap bencana dan perubahan lingkungan.", style = "Normal")
      
      print(doc, target = file)
    }
  )
  
  output$download_beranda_full <- downloadHandler(
    filename = function() {
      paste0("laporan_beranda_lengkap_", Sys.Date(), ".docx")
    },
    content = function(file) {
      doc <- read_docx()
      doc <- doc %>%
        body_add_par("LAPORAN BERANDA LENGKAP", style = "heading 1") %>%
        body_add_par("SOVI-STAT EXPLORER: Dashboard Analitika Kerentanan Sosial Indonesia", style = "heading 2") %>%
        body_add_par(paste("Tanggal Pembuatan:", Sys.Date()), style = "Normal") %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("1. INFORMASI UMUM", style = "heading 2") %>%
        body_add_par(paste("Status Data:", data_result$status), style = "Normal") %>%
        body_add_par(paste("Pesan:", data_result$message), style = "Normal") %>%
        body_add_par(paste("Jumlah Observasi:", nrow(sovi_data)), style = "Normal") %>%
        body_add_par(paste("Jumlah Variabel:", ncol(sovi_data)), style = "Normal") %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("2. METADATA VARIABEL", style = "heading 2") %>%
        body_add_table(metadata, style = "table_template") %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("3. RINGKASAN STATISTIK DASAR", style = "heading 2")
      
      numeric_vars <- select_if(sovi_data, is.numeric)
      summary_stats <- capture.output(summary(numeric_vars))
      for(line in summary_stats) {
        doc <- doc %>% body_add_par(line, style = "Normal")
      }
      
      print(doc, target = file)
    }
  )
  
  # =====================================================================
  # TAB MANAJEMEN DATA - DIPERBAIKI
  # =====================================================================
  
  transformed_data <- reactive({
    req(input$var_numeric, input$n_categories, input$method_cat)
    
    var_data <- sovi_data[[input$var_numeric]]
    var_data <- var_data[!is.na(var_data)]
    
    if(length(var_data) == 0) return(NULL)
    
    if(input$method_cat == "quantile") {
      breaks <- quantile(var_data, probs = seq(0, 1, length.out = input$n_categories + 1), na.rm = TRUE)
    } else if(input$method_cat == "equal") {
      breaks <- seq(min(var_data, na.rm = TRUE), max(var_data, na.rm = TRUE), length.out = input$n_categories + 1)
    } else if(input$method_cat == "manual" && input$manual_breaks != "") {
      manual_breaks <- as.numeric(unlist(strsplit(input$manual_breaks, ",")))
      if(length(manual_breaks) == input$n_categories - 1) {
        breaks <- c(min(var_data, na.rm = TRUE), manual_breaks, max(var_data, na.rm = TRUE))
      } else {
        breaks <- quantile(var_data, probs = seq(0, 1, length.out = input$n_categories + 1), na.rm = TRUE)
      }
    } else {
      breaks <- quantile(var_data, probs = seq(0, 1, length.out = input$n_categories + 1), na.rm = TRUE)
    }
    
    labels <- paste0("Kategori_", 1:input$n_categories)
    
    original_var <- sovi_data[[input$var_numeric]]
    categorized <- cut(original_var, breaks = breaks, labels = labels, include.lowest = TRUE)
    
    # Simpan variabel kategori ke reactive values
    cat_var_name <- paste0(input$var_numeric, "_CAT")
    categorical_vars$data[[cat_var_name]] <- categorized
    
    valid_indices <- !is.na(original_var)
    
    comparison_df <- data.frame(
      Index = which(valid_indices),
      Original = original_var[valid_indices],
      Kategori = categorized[valid_indices],
      stringsAsFactors = FALSE
    )
    
    return(list(
      data = comparison_df,
      breaks = breaks,
      labels = labels,
      method = input$method_cat,
      var_name = cat_var_name
    ))
  })
  
  output$comparison_table <- DT::renderDataTable({
    data <- transformed_data()
    if(is.null(data)) return(NULL)
    
    DT::datatable(data$data, 
                  options = list(pageLength = 15, scrollX = TRUE),
                  class = 'cell-border stripe hover',
                  rownames = FALSE) %>%
      DT::formatRound(columns = "Original", digits = 3)
  })
  
  output$summary_original <- renderPrint({
    req(input$var_numeric)
    var_data <- sovi_data[[input$var_numeric]]
    summary(var_data)
  })
  
  output$frequency_table <- renderPrint({
    data <- transformed_data()
    if(is.null(data)) return("Data tidak tersedia")
    
    freq_table <- table(data$data$Kategori, useNA = "ifany")
    prop_table <- prop.table(freq_table) * 100
    
    result <- data.frame(
      Kategori = names(freq_table),
      Frekuensi = as.numeric(freq_table),
      Persentase = round(as.numeric(prop_table), 2),
      stringsAsFactors = FALSE
    )
    
    print(result)
  })
  
  output$interpretation_summary <- renderText({
    req(input$var_numeric)
    var_data <- sovi_data[[input$var_numeric]]
    var_data <- var_data[!is.na(var_data)]
    
    if(length(var_data) == 0) return("Data tidak tersedia untuk interpretasi.")
    
    summary_stats <- summary(var_data)
    interpret_descriptive(summary_stats, input$var_numeric)
  })
  
  output$interpretation_frequency <- renderText({
    data <- transformed_data()
    if(is.null(data)) return("Data tidak tersedia untuk interpretasi.")
    
    freq_table <- table(data$data$Kategori, useNA = "ifany")
    most_frequent <- names(freq_table)[which.max(freq_table)]
    least_frequent <- names(freq_table)[which.min(freq_table)]
    total_obs <- sum(freq_table)
    
    paste0("Berdasarkan kategorisasi menggunakan metode ", data$method, 
           " pada variabel ", input$var_numeric, ", dari total ", total_obs, " observasi, ",
           "kategori dengan frekuensi tertinggi adalah ", most_frequent, 
           " dengan ", max(freq_table), " observasi (", round(max(freq_table)/total_obs*100, 1), "%), ",
           "sedangkan kategori dengan frekuensi terendah adalah ", least_frequent, 
           " dengan ", min(freq_table), " observasi (", round(min(freq_table)/total_obs*100, 1), "%). ",
           "Distribusi ini menunjukkan pola sebaran data yang dapat digunakan untuk analisis lebih lanjut ",
           "seperti uji beda rata-rata antar kelompok atau ANOVA.")
  })
  
  output$download_transformed <- downloadHandler(
    filename = function() {
      paste0("data_transformasi_", input$var_numeric, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- transformed_data()
      if(!is.null(data)) {
        write.csv(data$data, file, row.names = FALSE)
      }
    }
  )
  
  output$download_manajemen_full <- downloadHandler(
    filename = function() {
      paste0("laporan_manajemen_data_", Sys.Date(), ".docx")
    },
    content = function(file) {
      doc <- read_docx()
      doc <- doc %>%
        body_add_par("LAPORAN MANAJEMEN DATA", style = "heading 1") %>%
        body_add_par("SOVI-STAT EXPLORER", style = "heading 2") %>%
        body_add_par(paste("Tanggal:", Sys.Date()), style = "Normal") %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("1. INFORMASI TRANSFORMASI", style = "heading 2") %>%
        body_add_par(paste("Variabel yang ditransformasi:", input$var_numeric), style = "Normal") %>%
        body_add_par(paste("Metode kategorisasi:", input$method_cat), style = "Normal") %>%
        body_add_par(paste("Jumlah kategori:", input$n_categories), style = "Normal")
      
      data <- transformed_data()
      if(!is.null(data)) {
        doc <- doc %>%
          body_add_par("", style = "Normal") %>%
          body_add_par("2. HASIL TRANSFORMASI", style = "heading 2") %>%
          body_add_table(head(data$data, 20), style = "table_template")
      }
      
      print(doc, target = file)
    }
  )
  
  # =====================================================================
  # TAB STATISTIK DESKRIPTIF - DIPERBAIKI DENGAN INTERPRETASI LEBIH SPESIFIK
  # =====================================================================
  
  output$descriptive_stats_full <- renderPrint({
    numeric_data <- select_if(sovi_data, is.numeric)
    summary(numeric_data)
  })
  
  output$single_var_stats <- renderPrint({
    req(input$desc_var)
    var_data <- sovi_data[[input$desc_var]]
    
    stats <- list(
      "Jumlah Observasi" = length(var_data),
      "Jumlah Missing" = sum(is.na(var_data)),
      "Mean" = mean(var_data, na.rm = TRUE),
      "Median" = median(var_data, na.rm = TRUE),
      "Std Deviation" = sd(var_data, na.rm = TRUE),
      "Variance" = var(var_data, na.rm = TRUE),
      "Minimum" = min(var_data, na.rm = TRUE),
      "Maximum" = max(var_data, na.rm = TRUE),
      "Q1" = quantile(var_data, 0.25, na.rm = TRUE),
      "Q3" = quantile(var_data, 0.75, na.rm = TRUE),
      "IQR" = IQR(var_data, na.rm = TRUE),
      "Skewness" = round((mean(var_data, na.rm = TRUE) - median(var_data, na.rm = TRUE)) / sd(var_data, na.rm = TRUE), 3)
    )
    
    stats
  })
  
  output$correlation_matrix <- renderPlot({
    numeric_data <- select_if(sovi_data, is.numeric)
    if(ncol(numeric_data) < 2) return(NULL)
    
    cor_matrix <- cor(numeric_data, use = "complete.obs")
    corrplot(cor_matrix, method = "color", type = "upper", 
             order = "hclust", tl.cex = 0.8, tl.col = "black",
             col = colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))(200))
  })
  
  output$interpretation_descriptive <- renderText({
    numeric_data <- select_if(sovi_data, is.numeric)
    n_vars <- ncol(numeric_data)
    n_obs <- nrow(numeric_data)
    
    # Hitung statistik agregat
    means <- sapply(numeric_data, mean, na.rm = TRUE)
    sds <- sapply(numeric_data, sd, na.rm = TRUE)
    cv <- sds / means  # Coefficient of variation
    
    high_var_vars <- names(cv[cv > 0.5])
    low_var_vars <- names(cv[cv < 0.2])
    
    paste0("Dataset SOVI memiliki ", n_vars, " variabel numerik dengan ", n_obs, " observasi wilayah. ",
           "Analisis statistik deskriptif menunjukkan variabilitas yang beragam antar indikator kerentanan sosial. ",
           if(length(high_var_vars) > 0) {
             paste0("Variabel dengan variabilitas tinggi (CV > 0.5) meliputi: ", 
                    paste(high_var_vars[1:min(3, length(high_var_vars))], collapse = ", "), 
                    ", menunjukkan disparitas yang signifikan antar wilayah. ")
           } else "",
           if(length(low_var_vars) > 0) {
             paste0("Variabel dengan variabilitas rendah (CV < 0.2) meliputi: ", 
                    paste(low_var_vars[1:min(3, length(low_var_vars))], collapse = ", "), 
                    ", menunjukkan kondisi yang relatif homogen. ")
           } else "",
           "Informasi ini penting untuk memahami pola kerentanan sosial sebelum melakukan analisis inferensia.")
  })
  
  output$interpretation_single_var <- renderText({
    req(input$desc_var)
    var_data <- sovi_data[[input$desc_var]]
    var_data <- var_data[!is.na(var_data)]
    
    if(length(var_data) == 0) return("Data tidak tersedia untuk interpretasi.")
    
    mean_val <- mean(var_data)
    median_val <- median(var_data)
    sd_val <- sd(var_data)
    
    # Interpretasi yang lebih spesifik berdasarkan nama variabel
    var_context <- switch(input$desc_var,
                          "POVERTY" = "tingkat kemiskinan",
                          "CHILDREN" = "persentase anak-anak",
                          "ELDERLY" = "persentase lansia",
                          "ILLITERATE" = "tingkat buta huruf",
                          "NOELECTRIC" = "akses listrik yang terbatas",
                          "LOWEDU" = "tingkat pendidikan rendah",
                          "SOVI_SCORE" = "skor kerentanan sosial",
                          paste("indikator", tolower(input$desc_var)))
    
    interpret_descriptive(c(Mean = mean_val, Median = median_val), input$desc_var) %>%
      paste0(" Dalam konteks SOVI, variabel ini mengukur ", var_context, 
             " yang merupakan komponen penting dalam menentukan tingkat kerentanan sosial suatu wilayah.")
  })
  
  output$interpretation_correlation <- renderText({
    numeric_data <- select_if(sovi_data, is.numeric)
    if(ncol(numeric_data) < 2) return("Tidak cukup variabel numerik untuk analisis korelasi.")
    
    cor_matrix <- cor(numeric_data, use = "complete.obs")
    high_cor <- which(abs(cor_matrix) > 0.7 & abs(cor_matrix) < 1, arr.ind = TRUE)
    
    if(nrow(high_cor) > 0) {
      var_pairs <- paste(rownames(cor_matrix)[high_cor[,1]], 
                         colnames(cor_matrix)[high_cor[,2]], sep = " - ")
      cor_values <- cor_matrix[high_cor]
      
      paste0("Matriks korelasi mengungkap hubungan antar indikator kerentanan sosial. ",
             "Terdapat ", nrow(high_cor), " pasang variabel dengan korelasi tinggi (|r| > 0.7): ",
             paste(paste0(var_pairs[1:min(3, length(var_pairs))], " (r=", round(cor_values[1:min(3, length(cor_values))], 3), ")"), 
                   collapse = ", "), 
             if(length(var_pairs) > 3) " dan lainnya." else ".",
             " Korelasi tinggi ini mengindikasikan keterkaitan erat antar dimensi kerentanan sosial dan ",
             "perlu dipertimbangkan dalam pemodelan untuk menghindari multikolinearitas.")
    } else {
      paste0("Matriks korelasi menunjukkan tidak ada korelasi yang sangat tinggi (|r| > 0.7) antar indikator SOVI, ",
             "yang mengindikasikan bahwa setiap variabel mengukur dimensi kerentanan sosial yang relatif independen. ",
             "Hal ini menguntungkan untuk analisis regresi karena risiko multikolinearitas rendah.")
    }
  })
  
  output$download_descriptive <- downloadHandler(
    filename = function() {
      paste0("statistik_deskriptif_", Sys.Date(), ".docx")
    },
    content = function(file) {
      doc <- read_docx()
      doc <- doc %>%
        body_add_par("LAPORAN STATISTIK DESKRIPTIF", style = "heading 1") %>%
        body_add_par("SOVI-STAT EXPLORER", style = "heading 2") %>%
        body_add_par(paste("Tanggal:", Sys.Date()), style = "Normal") %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("1. RINGKASAN STATISTIK", style = "heading 2")
      
      numeric_data <- select_if(sovi_data, is.numeric)
      summary_stats <- capture.output(summary(numeric_data))
      for(line in summary_stats) {
        doc <- doc %>% body_add_par(line, style = "Normal")
      }
      
      print(doc, target = file)
    }
  )
  
  output$download_correlation_plot <- downloadHandler(
    filename = function() {
      paste0("matriks_korelasi_", Sys.Date(), ".jpg")
    },
    content = function(file) {
      jpeg(file, width = 800, height = 600, quality = 95)
      numeric_data <- select_if(sovi_data, is.numeric)
      if(ncol(numeric_data) >= 2) {
        cor_matrix <- cor(numeric_data, use = "complete.obs")
        corrplot(cor_matrix, method = "color", type = "upper", 
                 order = "hclust", tl.cex = 0.8, tl.col = "black",
                 col = colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))(200))
      }
      dev.off()
    }
  )
  
  output$download_deskriptif_full <- downloadHandler(
    filename = function() {
      paste0("laporan_deskriptif_lengkap_", Sys.Date(), ".docx")
    },
    content = function(file) {
      doc <- read_docx()
      doc <- doc %>%
        body_add_par("LAPORAN STATISTIK DESKRIPTIF LENGKAP", style = "heading 1") %>%
        body_add_par("SOVI-STAT EXPLORER", style = "heading 2") %>%
        body_add_par(paste("Tanggal:", Sys.Date()), style = "Normal") %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("1. INFORMASI UMUM", style = "heading 2") %>%
        body_add_par(paste("Jumlah Observasi:", nrow(sovi_data)), style = "Normal") %>%
        body_add_par(paste("Jumlah Variabel Numerik:", ncol(select_if(sovi_data, is.numeric))), style = "Normal") %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("2. STATISTIK DESKRIPTIF", style = "heading 2")
      
      numeric_data <- select_if(sovi_data, is.numeric)
      summary_stats <- capture.output(summary(numeric_data))
      for(line in summary_stats) {
        doc <- doc %>% body_add_par(line, style = "Normal")
      }
      
      doc <- doc %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("3. INTERPRETASI", style = "heading 2") %>%
        body_add_par("Dataset SOVI menunjukkan karakteristik distribusi yang beragam antar indikator kerentanan sosial. Variabilitas yang tinggi pada beberapa indikator mengindikasikan disparitas kondisi sosial-ekonomi antar wilayah yang perlu mendapat perhatian dalam perumusan kebijakan.", style = "Normal")
      
      print(doc, target = file)
    }
  )
  
  # =====================================================================
  # TAB VISUALISASI - DIPERBAIKI DENGAN INTERPRETASI LEBIH SPESIFIK
  # =====================================================================
  
  output$data_plot_interactive <- renderPlotly({
    req(input$plot_var, input$plot_type)
    
    var_data <- sovi_data[[input$plot_var]]
    
    if(input$plot_type == "hist") {
      p <- ggplot(sovi_data, aes_string(x = input$plot_var)) +
        geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7, color = "white") +
        theme_minimal() +
        labs(title = paste("Histogram", input$plot_var),
             x = input$plot_var, y = "Frekuensi")
    } else if(input$plot_type == "box") {
      p <- ggplot(sovi_data, aes_string(y = input$plot_var)) +
        geom_boxplot(fill = "lightblue", alpha = 0.7) +
        theme_minimal() +
        labs(title = paste("Boxplot", input$plot_var), y = input$plot_var)
    } else if(input$plot_type == "density") {
      p <- ggplot(sovi_data, aes_string(x = input$plot_var)) +
        geom_density(fill = "orange", alpha = 0.7) +
        theme_minimal() +
        labs(title = paste("Density Plot", input$plot_var))
    } else if(input$plot_type == "scatter" && !is.null(input$scatter_y)) {
      p <- ggplot(sovi_data, aes_string(x = input$plot_var, y = input$scatter_y)) +
        geom_point(alpha = 0.6, color = "darkgreen") +
        geom_smooth(method = "lm", se = TRUE, color = "red") +
        theme_minimal() +
        labs(title = paste("Scatter Plot:", input$plot_var, "vs", input$scatter_y))
    } else if(input$plot_type == "violin") {
      p <- ggplot(sovi_data, aes_string(y = input$plot_var)) +
        geom_violin(fill = "purple", alpha = 0.7) +
        theme_minimal() +
        labs(title = paste("Violin Plot", input$plot_var), y = input$plot_var)
    } else {
      return(NULL)
    }
    
    ggplotly(p)
  })
  
  output$multi_plot_dashboard <- renderPlot({
    req(input$plot_var)
    
    var_data <- sovi_data[[input$plot_var]]
    
    p1 <- ggplot(sovi_data, aes_string(x = input$plot_var)) +
      geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7, color = "white") +
      theme_minimal() +
      labs(title = "Histogram", x = input$plot_var, y = "Frekuensi")
    
    p2 <- ggplot(sovi_data, aes_string(y = input$plot_var)) +
      geom_boxplot(fill = "lightcoral", alpha = 0.7) +
      theme_minimal() +
      labs(title = "Boxplot", y = input$plot_var)
    
    p3 <- ggplot(sovi_data, aes_string(x = input$plot_var)) +
      geom_density(fill = "lightgreen", alpha = 0.7) +
      theme_minimal() +
      labs(title = "Density Plot", x = input$plot_var, y = "Density")
    
    p4 <- ggplot(sovi_data, aes_string(sample = input$plot_var)) +
      stat_qq() + stat_qq_line() +
      theme_minimal() +
      labs(title = "Q-Q Plot", x = "Theoretical Quantiles", y = "Sample Quantiles")
    
    grid.arrange(p1, p2, p3, p4, ncol = 2)
  })
  
  output$interpretation_plot <- renderText({
    req(input$plot_var, input$plot_type)
    
    var_data <- sovi_data[[input$plot_var]]
    var_data <- var_data[!is.na(var_data)]
    
    if(length(var_data) == 0) return("Data tidak tersedia untuk interpretasi.")
    
    mean_val <- mean(var_data)
    median_val <- median(var_data)
    sd_val <- sd(var_data)
    
    # Konteks spesifik berdasarkan variabel
    var_context <- switch(input$plot_var,
                          "POVERTY" = "tingkat kemiskinan wilayah",
                          "CHILDREN" = "proporsi anak-anak dalam populasi",
                          "ELDERLY" = "proporsi lansia dalam populasi",
                          "ILLITERATE" = "tingkat buta huruf",
                          "SOVI_SCORE" = "skor kerentanan sosial keseluruhan",
                          paste("indikator", tolower(input$plot_var)))
    
    base_interpretation <- paste0("Visualisasi menunjukkan distribusi ", var_context, " di seluruh wilayah analisis. ")
    
    if(input$plot_type == "hist") {
      skewness <- if(mean_val > median_val) "positif (ekor kanan)" else if(mean_val < median_val) "negatif (ekor kiri)" else "relatif simetris"
      paste0(base_interpretation, "Histogram menunjukkan distribusi dengan kecenderungan ", skewness, ". ",
             "Rata-rata (", round(mean_val, 3), ") dan median (", round(median_val, 3), ") memberikan gambaran pemusatan data. ",
             if(skewness == "positif (ekor kanan)") {
               "Distribusi ini mengindikasikan bahwa sebagian besar wilayah memiliki nilai rendah, namun terdapat beberapa wilayah dengan nilai sangat tinggi yang perlu perhatian khusus."
             } else if(skewness == "negatif (ekor kiri)") {
               "Distribusi ini menunjukkan bahwa sebagian besar wilayah memiliki nilai tinggi, dengan beberapa wilayah memiliki kondisi yang lebih baik."
             } else {
               "Distribusi yang simetris menunjukkan sebaran yang merata antar wilayah."
             })
    } else if(input$plot_type == "box") {
      q1 <- quantile(var_data, 0.25)
      q3 <- quantile(var_data, 0.75)
      iqr <- q3 - q1
      outliers <- length(var_data[var_data < (q1 - 1.5*iqr) | var_data > (q3 + 1.5*iqr)])
      
      paste0(base_interpretation, "Boxplot menunjukkan median pada ", round(median_val, 3), 
             " dengan IQR ", round(iqr, 3), ". ",
             if(outliers > 0) {
               paste0("Terdapat ", outliers, " wilayah outlier yang memiliki kondisi ekstrem dan memerlukan analisis lebih lanjut.")
             } else {
               "Tidak terdapat outlier yang signifikan, menunjukkan distribusi yang relatif normal."
             })
    } else if(input$plot_type == "density") {
      paste0(base_interpretation, "Density plot menunjukkan estimasi distribusi probabilitas dengan puncak sekitar ", 
             round(mean_val, 3), ". Bentuk kurva memberikan gambaran tentang konsentrasi wilayah pada rentang nilai tertentu.")
    } else if(input$plot_type == "scatter") {
      if(!is.null(input$scatter_y)) {
        y_data <- sovi_data[[input$scatter_y]]
        correlation <- cor(var_data, y_data, use = "complete.obs")
        cor_strength <- if(abs(correlation) > 0.7) "kuat" else if(abs(correlation) > 0.3) "sedang" else "lemah"
        cor_direction <- if(correlation > 0) "positif" else "negatif"
        
        paste0(base_interpretation, "Scatter plot mengungkap hubungan ", cor_direction, " yang ", cor_strength, 
               " antara kedua indikator (r = ", round(correlation, 3), "). ",
               if(abs(correlation) > 0.5) {
                 "Korelasi yang cukup kuat ini menunjukkan keterkaitan erat antar dimensi kerentanan sosial."
               } else {
                 "Korelasi yang lemah menunjukkan bahwa kedua indikator mengukur aspek kerentanan yang berbeda."
               })
      } else {
        base_interpretation
      }
    } else {
      base_interpretation
    }
  })
  
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("plot_", input$plot_var, "_", input$plot_type, "_", Sys.Date(), ".jpg")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), width = 10, height = 6, dpi = 300)
    }
  )
  
  output$download_multi_plot <- downloadHandler(
    filename = function() {
      paste0("multi_plot_", input$plot_var, "_", Sys.Date(), ".jpg")
    },
    content = function(file) {
      jpeg(file, width = 1200, height = 800, quality = 95)
      
      var_data <- sovi_data[[input$plot_var]]
      
      p1 <- ggplot(sovi_data, aes_string(x = input$plot_var)) +
        geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7, color = "white") +
        theme_minimal() +
        labs(title = "Histogram", x = input$plot_var, y = "Frekuensi")
      
      p2 <- ggplot(sovi_data, aes_string(y = input$plot_var)) +
        geom_boxplot(fill = "lightcoral", alpha = 0.7) +
        theme_minimal() +
        labs(title = "Boxplot", y = input$plot_var)
      
      p3 <- ggplot(sovi_data, aes_string(x = input$plot_var)) +
        geom_density(fill = "lightgreen", alpha = 0.7) +
        theme_minimal() +
        labs(title = "Density Plot", x = input$plot_var, y = "Density")
      
      p4 <- ggplot(sovi_data, aes_string(sample = input$plot_var)) +
        stat_qq() + stat_qq_line() +
        theme_minimal() +
        labs(title = "Q-Q Plot", x = "Theoretical Quantiles", y = "Sample Quantiles")
      
      grid.arrange(p1, p2, p3, p4, ncol = 2)
      dev.off()
    }
  )
  
  output$download_visualisasi_full <- downloadHandler(
    filename = function() {
      paste0("laporan_visualisasi_", Sys.Date(), ".docx")
    },
    content = function(file) {
      doc <- read_docx()
      doc <- doc %>%
        body_add_par("LAPORAN VISUALISASI DATA", style = "heading 1") %>%
        body_add_par("SOVI-STAT EXPLORER", style = "heading 2") %>%
        body_add_par(paste("Tanggal:", Sys.Date()), style = "Normal") %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("1. INFORMASI VISUALISASI", style = "heading 2") %>%
        body_add_par(paste("Variabel yang divisualisasikan:", input$plot_var), style = "Normal") %>%
        body_add_par(paste("Jenis plot:", input$plot_type), style = "Normal") %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("2. INTERPRETASI", style = "heading 2")
      
      interpretation <- if(!is.null(input$plot_var) && !is.null(input$plot_type)) {
        var_data <- sovi_data[[input$plot_var]]
        var_data <- var_data[!is.na(var_data)]
        if(length(var_data) > 0) {
          paste0("Visualisasi menunjukkan karakteristik distribusi indikator kerentanan sosial ", input$plot_var, 
                 " dengan rata-rata ", round(mean(var_data), 3), " dan standar deviasi ", round(sd(var_data), 3), ". ",
                 "Pola distribusi ini memberikan wawasan penting tentang variabilitas kondisi sosial antar wilayah.")
        } else {
          "Data tidak tersedia untuk interpretasi."
        }
      } else {
        "Pilih variabel dan jenis plot untuk melihat interpretasi."
      }
      
      doc <- doc %>% body_add_par(interpretation, style = "Normal")
      
      print(doc, target = file)
    }
  )
  
  # =====================================================================
  # TAB PETA INTERAKTIF - FITUR EKSPLORASI DATA
  # =====================================================================
  
  # Reactive untuk data peta
  map_data <- reactive({
    req(input$map_var)
    
    if(spatial_result$status == "dummy") {
      return(NULL)
    }
    
    # Ambil data spasial dan variabel yang dipilih
    spatial_data_filtered <- spatial_data %>%
      select(DISTRICTCODE, DISTRICT_NAME, PROVINCE_NAME, geom, !!sym(input$map_var)) %>%
      filter(!is.na(!!sym(input$map_var)))
    
    if(nrow(spatial_data_filtered) == 0) {
      return(NULL)
    }
    
    spatial_data_filtered
  })
  
  # =====================================================================
  # PETA INTERAKTIF - VERSI ANTI-ERROR
  # =====================================================================
  output$sovi_map <- renderLeaflet({
    
    # 1. Pastikan variabel peta sudah dipilih
    req(input$map_var)
    
    # 2. Ambil data peta yang sudah digabung
    map_data_to_render <- spatial_data
    
    # 3. Pengecekan data yang kuat
    # Pastikan data tidak NULL dan punya baris
    if (is.null(map_data_to_render) || nrow(map_data_to_render) == 0) {
      # Tampilkan peta kosong jika tidak ada data sama sekali
      return(leaflet() %>% addTiles() %>% setView(lng = 118, lat = -2, zoom = 5) %>%
               addControl("Data spasial tidak tersedia.", position = "topright"))
    }
    
    # 4. Filter data berdasarkan variabel yang dipilih dan hilangkan NA
    # Ini penting agar palet warna dan popup tidak error
    map_data_filtered <- map_data_to_render %>%
      filter(!is.na(.data[[input$map_var]]))
    
    if (nrow(map_data_filtered) == 0) {
      return(leaflet() %>% addTiles() %>% setView(lng = 118, lat = -2, zoom = 5) %>%
               addControl(paste("Tidak ada data valid untuk variabel:", input$map_var), position = "topright"))
    }
    
    # 5. Render peta
    var_values <- map_data_filtered[[input$map_var]]
    pal <- colorNumeric(palette = input$color_palette, domain = var_values, na.color = "transparent")
    
    popup_content <- paste0(
      "<strong>", map_data_filtered$DISTRICT_NAME, "</strong><br>",
      "Provinsi: ", map_data_filtered$PROVINCE_NAME, "<br>",
      input$map_var, ": ", round(var_values, 2)
    )
    
    leaflet(map_data_filtered) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% # Menggunakan basemap default yang pasti ada
      addPolygons(
        fillColor = ~pal(var_values),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7, # Menggunakan nilai opacity default
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = lapply(popup_content, htmltools::HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = pal,
        values = var_values,
        opacity = 0.7,
        title = input$map_var,
        position = "bottomright"
      )
  })
  
  
  # Output interpretasi peta
  output$map_interpretation <- renderText({
    req(input$map_var, map_data())
    
    map_data_filtered <- map_data()
    
    if(is.null(map_data_filtered) || nrow(map_data_filtered) == 0) {
      return("Tidak ada data yang tersedia untuk interpretasi peta.")
    }
    
    var_values <- map_data_filtered[[input$map_var]]
    
    # Analisis pola spasial
    mean_val <- mean(var_values, na.rm = TRUE)
    high_regions <- sum(var_values > mean_val, na.rm = TRUE)
    low_regions <- sum(var_values <= mean_val, na.rm = TRUE)
    
    # Identifikasi wilayah dengan nilai ekstrem
    q95 <- quantile(var_values, 0.95, na.rm = TRUE)
    q05 <- quantile(var_values, 0.05, na.rm = TRUE)
    extreme_high <- sum(var_values >= q95, na.rm = TRUE)
    extreme_low <- sum(var_values <= q05, na.rm = TRUE)
    
    paste0("Peta interaktif menampilkan distribusi spasial variabel ", input$map_var, " di seluruh wilayah Indonesia. ",
           "Dari ", length(var_values), " wilayah yang dianalisis, ", high_regions, " wilayah memiliki nilai di atas rata-rata (", round(mean_val, 3), "), ",
           "sedangkan ", low_regions, " wilayah memiliki nilai di bawah rata-rata. ",
           "Terdapat ", extreme_high, " wilayah dengan nilai sangat tinggi (percentile 95+) dan ", extreme_low, " wilayah dengan nilai sangat rendah (percentile 5-). ",
           "Pola spasial ini mengindikasikan adanya variasi kerentanan sosial yang signifikan antar wilayah, ",
           "yang dapat digunakan untuk mengidentifikasi area prioritas dalam program pengurangan kerentanan sosial.")
  })
  
  # Output statistik peta
  output$map_stats <- renderPrint({
    req(input$map_var, map_data())
    
    map_data_filtered <- map_data()
    
    if(is.null(map_data_filtered) || nrow(map_data_filtered) == 0) {
      cat("Tidak ada data yang tersedia untuk variabel yang dipilih.")
      return()
    }
    
    var_values <- map_data_filtered[[input$map_var]]
    
    cat("=== STATISTIK DESKRIPTIF PETA ===\n")
    cat("Variabel:", input$map_var, "\n")
    cat("Jumlah Wilayah:", length(var_values), "\n")
    cat("Nilai Minimum:", round(min(var_values, na.rm = TRUE), 4), "\n")
    cat("Nilai Maksimum:", round(max(var_values, na.rm = TRUE), 4), "\n")
    cat("Rata-rata:", round(mean(var_values, na.rm = TRUE), 4), "\n")
    cat("Median:", round(median(var_values, na.rm = TRUE), 4), "\n")
    cat("Standar Deviasi:", round(sd(var_values, na.rm = TRUE), 4), "\n")
    cat("Nilai yang Hilang:", sum(is.na(var_values)), "\n")
    
    # Quartiles
    quartiles <- quantile(var_values, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
    cat("Q1 (25%):", round(quartiles[1], 4), "\n")
    cat("Q2 (50%):", round(quartiles[2], 4), "\n")
    cat("Q3 (75%):", round(quartiles[3], 4), "\n")
    
    # Range dan IQR
    cat("Range:", round(max(var_values, na.rm = TRUE) - min(var_values, na.rm = TRUE), 4), "\n")
    cat("IQR:", round(quartiles[3] - quartiles[1], 4), "\n")
  })
  
  # Output interpretasi statistik peta
  output$map_stats_interpretation <- renderText({
    req(input$map_var, map_data())
    
    map_data_filtered <- map_data()
    
    if(is.null(map_data_filtered) || nrow(map_data_filtered) == 0) {
      return("Tidak ada data yang tersedia untuk interpretasi.")
    }
    
    var_values <- map_data_filtered[[input$map_var]]
    
    mean_val <- mean(var_values, na.rm = TRUE)
    median_val <- median(var_values, na.rm = TRUE)
    sd_val <- sd(var_values, na.rm = TRUE)
    min_val <- min(var_values, na.rm = TRUE)
    max_val <- max(var_values, na.rm = TRUE)
    
    # Tentukan karakteristik distribusi
    skewness <- if(mean_val > median_val) "positif (ekor kanan)" else if(mean_val < median_val) "negatif (ekor kiri)" else "simetris"
    variability <- if(sd_val/mean_val < 0.1) "rendah" else if(sd_val/mean_val < 0.3) "sedang" else "tinggi"
    
    # Interpretasi berdasarkan jenis variabel
    var_interpretation <- switch(input$map_var,
                                 "children" = "Persentase anak-anak dalam populasi",
                                 "female" = "Persentase perempuan dalam populasi", 
                                 "elderly" = "Persentase lansia dalam populasi",
                                 "fhead" = "Persentase kepala keluarga perempuan",
                                 "familysize" = "Rata-rata ukuran keluarga",
                                 "noelectric" = "Persentase rumah tangga tanpa listrik",
                                 "lowedu" = "Persentase penduduk dengan pendidikan rendah",
                                 "growth" = "Tingkat pertumbuhan penduduk",
                                 "poverty" = "Persentase kemiskinan",
                                 "illiterate" = "Persentase buta huruf",
                                 "notraining" = "Persentase tanpa pelatihan",
                                 "dprone" = "Indeks kerawanan bencana",
                                 "rented" = "Persentase rumah sewa",
                                 "nosewer" = "Persentase tanpa saluran pembuangan",
                                 "tapwater" = "Persentase akses air bersih",
                                 "Indikator kerentanan sosial"
    )
    
    paste0("Berdasarkan analisis spasial untuk variabel ", input$map_var, " (", var_interpretation, "), ",
           "ditemukan bahwa nilai rata-rata adalah ", round(mean_val, 3), " dengan median ", round(median_val, 3), ". ",
           "Distribusi data menunjukkan kecenderungan ", skewness, " dengan variabilitas ", variability, 
           " (standar deviasi: ", round(sd_val, 3), "). ",
           "Nilai berkisar dari ", round(min_val, 3), " hingga ", round(max_val, 3), 
           " yang menunjukkan ", if(max_val - min_val > 2*sd_val) "variasi yang tinggi" else "variasi yang sedang",
           " antar wilayah. Interpretasi ini penting untuk memahami pola kerentanan sosial di berbagai wilayah Indonesia.")
  })
  
  # Output tabel data peta
  output$map_data_table <- DT::renderDataTable({
    req(input$map_var, map_data())
    
    map_data_filtered <- map_data()
    
    if(is.null(map_data_filtered) || nrow(map_data_filtered) == 0) {
      return(DT::datatable(data.frame(Message = "Tidak ada data yang tersedia")))
    }
    
    # Siapkan data untuk tabel (tanpa geometry)
    table_data <- map_data_filtered %>%
      st_drop_geometry() %>%
      select(DISTRICTCODE, DISTRICT_NAME, PROVINCE_NAME, !!sym(input$map_var)) %>%
      arrange(desc(!!sym(input$map_var)))
    
    DT::datatable(table_data,
                  options = list(pageLength = 10, scrollX = TRUE),
                  class = 'cell-border stripe hover',
                  rownames = FALSE) %>%
      DT::formatRound(columns = input$map_var, digits = 2)
  })
  
  # Download handlers untuk peta
  output$download_map <- downloadHandler(
    filename = function() {
      paste0("laporan_peta_", input$map_var, "_", Sys.Date(), ".docx")
    },
    content = function(file) {
      req(input$map_var, map_data())
      
      map_data_filtered <- map_data()
      
      if(is.null(map_data_filtered) || nrow(map_data_filtered) == 0) {
        doc <- read_docx() %>%
          body_add_par("LAPORAN PETA SOVI", style = "heading 1") %>%
          body_add_par("Tidak ada data yang tersedia untuk variabel yang dipilih.", style = "Normal")
        print(doc, target = file)
        return()
      }
      
      var_values <- map_data_filtered[[input$map_var]]
      
      doc <- read_docx() %>%
        body_add_par("LAPORAN PETA INTERAKTIF SOVI", style = "heading 1") %>%
        body_add_par(paste("Variabel:", input$map_var), style = "Normal") %>%
        body_add_par(paste("Tanggal:", Sys.Date()), style = "Normal") %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("STATISTIK DESKRIPTIF", style = "heading 2") %>%
        body_add_par(paste("Jumlah Wilayah:", length(var_values)), style = "Normal") %>%
        body_add_par(paste("Rata-rata:", round(mean(var_values, na.rm = TRUE), 4)), style = "Normal") %>%
        body_add_par(paste("Median:", round(median(var_values, na.rm = TRUE), 4)), style = "Normal") %>%
        body_add_par(paste("Standar Deviasi:", round(sd(var_values, na.rm = TRUE), 4)), style = "Normal") %>%
        body_add_par(paste("Minimum:", round(min(var_values, na.rm = TRUE), 4)), style = "Normal") %>%
        body_add_par(paste("Maksimum:", round(max(var_values, na.rm = TRUE), 4)), style = "Normal")
      
      print(doc, target = file)
    }
  )
  
  output$download_map_data <- downloadHandler(
    filename = function() {
      paste0("data_peta_", input$map_var, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(input$map_var, map_data())
      
      map_data_filtered <- map_data()
      
      if(is.null(map_data_filtered) || nrow(map_data_filtered) == 0) {
        write.csv(data.frame(Message = "Tidak ada data yang tersedia"), file, row.names = FALSE)
        return()
      }
      
      # Export data tanpa geometry
      export_data <- map_data_filtered %>%
        st_drop_geometry() %>%
        select(DISTRICTCODE, DISTRICT_NAME, PROVINCE_NAME, !!sym(input$map_var))
      
      write.csv(export_data, file, row.names = FALSE)
    }
  )
  
  output$download_spatial_full <- downloadHandler(
    filename = function() {
      paste0("laporan_lengkap_peta_", Sys.Date(), ".docx")
    },
    content = function(file) {
      req(input$map_var, map_data())
      
      map_data_filtered <- map_data()
      
      if(is.null(map_data_filtered) || nrow(map_data_filtered) == 0) {
        doc <- read_docx() %>%
          body_add_par("LAPORAN LENGKAP PETA SOVI", style = "heading 1") %>%
          body_add_par("Tidak ada data yang tersedia untuk variabel yang dipilih.", style = "Normal")
        print(doc, target = file)
        return()
      }
      
      var_values <- map_data_filtered[[input$map_var]]
      
      doc <- read_docx() %>%
        body_add_par("LAPORAN LENGKAP PETA INTERAKTIF SOVI", style = "heading 1") %>%
        body_add_par("SOVI-STAT EXPLORER", style = "heading 2") %>%
        body_add_par(paste("Tanggal:", Sys.Date()), style = "Normal") %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("1. RINGKASAN EKSEKUTIF", style = "heading 2") %>%
        body_add_par("Laporan ini menyajikan hasil analisis spasial menggunakan peta interaktif untuk mengidentifikasi pola kerentanan sosial di berbagai wilayah Indonesia.", style = "Normal") %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("2. METODOLOGI", style = "heading 2") %>%
        body_add_par(paste("Variabel yang dianalisis:", input$map_var), style = "Normal") %>%
        body_add_par(paste("Jumlah wilayah:", length(var_values)), style = "Normal") %>%
        body_add_par("Peta interaktif digunakan untuk visualisasi distribusi spasial indikator kerentanan sosial.", style = "Normal") %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("3. STATISTIK DESKRIPTIF", style = "heading 2") %>%
        body_add_par(paste("Rata-rata:", round(mean(var_values, na.rm = TRUE), 4)), style = "Normal") %>%
        body_add_par(paste("Median:", round(median(var_values, na.rm = TRUE), 4)), style = "Normal") %>%
        body_add_par(paste("Standar Deviasi:", round(sd(var_values, na.rm = TRUE), 4)), style = "Normal") %>%
        body_add_par(paste("Range:", round(max(var_values, na.rm = TRUE) - min(var_values, na.rm = TRUE), 4)), style = "Normal") %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("4. KESIMPULAN", style = "heading 2") %>%
        body_add_par("Analisis spasial mengungkap variasi kerentanan sosial antar wilayah yang dapat digunakan untuk strategi intervensi yang lebih terarah dan efektif.", style = "Normal")
      
      print(doc, target = file)
    }
  )
  
  # =====================================================================
  # SISA KODE SERVER SAMA SEPERTI SEBELUMNYA DENGAN PERBAIKAN MINOR
  # =====================================================================
  
  # Untuk menghemat ruang, saya akan melanjutkan dengan bagian-bagian penting lainnya
  # yang memerlukan perbaikan sesuai dengan masalah yang diidentifikasi
  
  # TAB UJI ASUMSI - Perbaikan untuk menggunakan variabel kategori yang dibuat
  output$normality_test <- renderPrint({
    req(input$assumption_var)
    
    var_data <- sovi_data[[input$assumption_var]]
    var_data <- var_data[!is.na(var_data)]
    
    if(length(var_data) < 3) {
      return("Data tidak mencukupi untuk uji normalitas (minimal 3 observasi).")
    }
    
    tryCatch({
      if(length(var_data) >= 5000) {
        ks_test <- ks.test(var_data, "pnorm", mean(var_data), sd(var_data))
        list(
          "Kolmogorov-Smirnov Test" = ks_test,
          "Interpretasi" = if(ks_test$p.value >= input$alpha_level) "Data mengikuti distribusi normal" else "Data tidak mengikuti distribusi normal"
        )
      } else {
        sw_test <- shapiro.test(var_data)
        list(
          "Shapiro-Wilk Test" = sw_test,
          "Interpretasi" = if(sw_test$p.value >= input$alpha_level) "Data mengikuti distribusi normal" else "Data tidak mengikuti distribusi normal"
        )
      }
    }, error = function(e) {
      paste("Error dalam uji normalitas:", e$message)
    })
  })
  
  output$normality_plot <- renderPlot({
    req(input$assumption_var)
    
    var_data <- sovi_data[[input$assumption_var]]
    var_data <- var_data[!is.na(var_data)]
    
    if(length(var_data) < 3) return(NULL)
    
    par(mfrow = c(1, 2))
    
    hist(var_data, main = paste("Histogram", input$assumption_var), 
         xlab = input$assumption_var, col = "lightblue", border = "white")
    curve(dnorm(x, mean = mean(var_data), sd = sd(var_data)), add = TRUE, col = "red", lwd = 2)
    
    qqnorm(var_data, main = paste("Q-Q Plot", input$assumption_var))
    qqline(var_data, col = "red", lwd = 2)
    
    par(mfrow = c(1, 1))
  })
  
  output$homogeneity_test <- renderPrint({
    req(input$assumption_var, input$group_var)
    
    if(input$group_var == "none" || !input$group_var %in% names(categorical_vars$data)) {
      return("Pilih variabel kelompok yang valid dari hasil kategorisasi di menu Manajemen Data.")
    }
    
    var_data <- sovi_data[[input$assumption_var]]
    group_data <- categorical_vars$data[[input$group_var]]
    
    complete_cases <- !is.na(var_data) & !is.na(group_data)
    var_data <- var_data[complete_cases]
    group_data <- group_data[complete_cases]
    
    if(length(var_data) < 3) {
      return("Data tidak mencukupi untuk uji homogenitas.")
    }
    
    group_counts <- table(group_data)
    if(any(group_counts < 2)) {
      return("Beberapa kelompok memiliki observasi kurang dari 2. Uji homogenitas tidak dapat dilakukan.")
    }
    
    tryCatch({
      bartlett_test <- bartlett.test(var_data, group_data)
      
      if(require(car, quietly = TRUE)) {
        levene_test <- leveneTest(var_data, group_data)
        list(
          "Bartlett Test" = bartlett_test,
          "Levene Test" = levene_test,
          "Interpretasi Bartlett" = if(bartlett_test$p.value >= input$alpha_level) "Ragam homogen antar kelompok" else "Ragam tidak homogen antar kelompok",
          "Interpretasi Levene" = if(levene_test$`Pr(>F)`[1] >= input$alpha_level) "Ragam homogen antar kelompok" else "Ragam tidak homogen antar kelompok"
        )
      } else {
        list(
          "Bartlett Test" = bartlett_test,
          "Interpretasi" = if(bartlett_test$p.value >= input$alpha_level) "Ragam homogen antar kelompok" else "Ragam tidak homogen antar kelompok"
        )
      }
    }, error = function(e) {
      paste("Error dalam uji homogenitas:", e$message)
    })
  })
  
  output$homogeneity_plot <- renderPlot({
    req(input$assumption_var, input$group_var)
    
    if(input$group_var == "none" || !input$group_var %in% names(categorical_vars$data)) return(NULL)
    
    var_data <- sovi_data[[input$assumption_var]]
    group_data <- categorical_vars$data[[input$group_var]]
    
    complete_cases <- !is.na(var_data) & !is.na(group_data)
    plot_data <- data.frame(
      Variable = var_data[complete_cases],
      Group = group_data[complete_cases]
    )
    
    if(nrow(plot_data) < 3) return(NULL)
    
    par(mfrow = c(1, 2))
    
    boxplot(Variable ~ Group, data = plot_data, 
            main = paste("Boxplot", input$assumption_var, "by", input$group_var),
            xlab = input$group_var, ylab = input$assumption_var,
            col = rainbow(length(unique(plot_data$Group))))
    
    group_means <- tapply(plot_data$Variable, plot_data$Group, mean, na.rm = TRUE)
    group_vars <- tapply(plot_data$Variable, plot_data$Group, var, na.rm = TRUE)
    
    plot(group_means, group_vars, 
         main = "Mean vs Variance by Group",
         xlab = "Group Means", ylab = "Group Variances",
         pch = 19, col = "blue")
    text(group_means, group_vars, names(group_means), pos = 3, cex = 0.8)
    
    par(mfrow = c(1, 1))
  })
  
  output$normality_interpretation <- renderText({
    req(input$assumption_var)
    
    var_data <- sovi_data[[input$assumption_var]]
    var_data <- var_data[!is.na(var_data)]
    
    if(length(var_data) < 3) {
      return("Data tidak mencukupi untuk interpretasi uji normalitas.")
    }
    
    tryCatch({
      if(length(var_data) >= 5000) {
        ks_test <- ks.test(var_data, "pnorm", mean(var_data), sd(var_data))
        create_interpretation(ks_test, "normalitas", input$alpha_level)
      } else {
        sw_test <- shapiro.test(var_data)
        create_interpretation(sw_test, "normalitas", input$alpha_level)
      }
    }, error = function(e) {
      "Tidak dapat memberikan interpretasi karena error dalam perhitungan uji normalitas."
    })
  })
  
  output$homogeneity_interpretation <- renderText({
    req(input$assumption_var, input$group_var)
    
    if(input$group_var == "none" || !input$group_var %in% names(categorical_vars$data)) {
      return("Pilih variabel kelompok yang valid untuk interpretasi uji homogenitas.")
    }
    
    var_data <- sovi_data[[input$assumption_var]]
    group_data <- categorical_vars$data[[input$group_var]]
    
    complete_cases <- !is.na(var_data) & !is.na(group_data)
    var_data <- var_data[complete_cases]
    group_data <- group_data[complete_cases]
    
    if(length(var_data) < 3) {
      return("Data tidak mencukupi untuk interpretasi uji homogenitas.")
    }
    
    group_counts <- table(group_data)
    if(any(group_counts < 2)) {
      return("Beberapa kelompok memiliki observasi kurang dari 2. Interpretasi tidak dapat diberikan.")
    }
    
    tryCatch({
      bartlett_test <- bartlett.test(var_data, group_data)
      create_interpretation(bartlett_test, "homogenitas", input$alpha_level)
    }, error = function(e) {
      "Tidak dapat memberikan interpretasi karena error dalam perhitungan uji homogenitas."
    })
  })
  
  output$assumption_summary_table <- DT::renderDataTable({
    req(input$assumption_var)
    
    var_data <- sovi_data[[input$assumption_var]]
    var_data <- var_data[!is.na(var_data)]
    
    if(length(var_data) < 3) {
      return(data.frame(Pesan = "Data tidak mencukupi untuk ringkasan"))
    }
    
    tryCatch({
      # Uji normalitas
      if(length(var_data) >= 5000) {
        norm_test <- ks.test(var_data, "pnorm", mean(var_data), sd(var_data))
        norm_test_name <- "Kolmogorov-Smirnov"
      } else {
        norm_test <- shapiro.test(var_data)
        norm_test_name <- "Shapiro-Wilk"
      }
      
      summary_df <- data.frame(
        "Uji" = paste("Normalitas (", norm_test_name, ")", sep = ""),
        "Statistik" = round(norm_test$statistic, 4),
        "P-value" = round(norm_test$p.value, 4),
        "Keputusan" = if(norm_test$p.value >= input$alpha_level) "Terima H₀ (Normal)" else "Tolak H₀ (Tidak Normal)",
        "Alpha" = input$alpha_level,
        stringsAsFactors = FALSE
      )
      
      # Tambahkan uji homogenitas jika ada variabel kelompok
      if(input$group_var != "none" && input$group_var %in% names(categorical_vars$data)) {
        group_data <- categorical_vars$data[[input$group_var]]
        complete_cases <- !is.na(var_data) & !is.na(group_data)
        
        if(sum(complete_cases) >= 3) {
          bartlett_test <- bartlett.test(var_data[complete_cases], group_data[complete_cases])
          
          homo_row <- data.frame(
            "Uji" = "Homogenitas (Bartlett)",
            "Statistik" = round(bartlett_test$statistic, 4),
            "P-value" = round(bartlett_test$p.value, 4),
            "Keputusan" = if(bartlett_test$p.value >= input$alpha_level) "Terima H₀ (Homogen)" else "Tolak H₀ (Tidak Homogen)",
            "Alpha" = input$alpha_level,
            stringsAsFactors = FALSE
          )
          
          summary_df <- rbind(summary_df, homo_row)
        }
      }
      
      DT::datatable(summary_df, 
                    options = list(pageLength = 10, scrollX = TRUE),
                    class = 'cell-border stripe hover',
                    rownames = FALSE)
    }, error = function(e) {
      data.frame(Error = paste("Error:", e$message))
    })
  })
  # Download handlers untuk uji asumsi
  output$download_assumptions <- downloadHandler(
    filename = function() {
      paste0("laporan_uji_asumsi_", input$assumption_var, "_", Sys.Date(), ".docx")
    },
    content = function(file) {
      doc <- read_docx()
      doc <- doc %>%
        body_add_par("LAPORAN UJI ASUMSI STATISTIK", style = "heading 1") %>%
        body_add_par("SOVI-STAT EXPLORER", style = "heading 2") %>%
        body_add_par(paste("Tanggal:", Sys.Date()), style = "Normal") %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("1. INFORMASI UJI", style = "heading 2") %>%
        body_add_par(paste("Variabel yang diuji:", input$assumption_var), style = "Normal") %>%
        body_add_par(paste("Variabel kelompok:", input$group_var), style = "Normal") %>%
        body_add_par(paste("Tingkat signifikansi:", input$alpha_level), style = "Normal") %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("2. HASIL UJI NORMALITAS", style = "heading 2")
      
      # Tambahkan hasil uji normalitas
      var_data <- sovi_data[[input$assumption_var]]
      var_data <- var_data[!is.na(var_data)]
      
      if(length(var_data) >= 3) {
        norm_output <- capture.output({
          if(length(var_data) >= 5000) {
            print(ks.test(var_data, "pnorm", mean(var_data), sd(var_data)))
          } else {
            print(shapiro.test(var_data))
          }
        })
        
        for(line in norm_output) {
          doc <- doc %>% body_add_par(line, style = "Normal")
        }
      }
      
      # =====================================================================
      # BAGIAN YANG HILANG DAN DIPERBAIKI ADA DI SINI
      # =====================================================================
      doc <- doc %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("3. HASIL UJI HOMOGENITAS", style = "heading 2")
      
      # Tambahkan hasil uji homogenitas
      if(input$group_var != "none" && input$group_var %in% names(categorical_vars$data)) {
        group_data <- categorical_vars$data[[input$group_var]]
        complete_cases <- !is.na(var_data) & !is.na(group_data)
        
        # Pastikan ada cukup data setelah menghilangkan NA
        if(sum(complete_cases) >= 3 && length(unique(group_data[complete_cases])) > 1) {
          
          # Pastikan setiap grup punya cukup data
          group_counts <- table(group_data[complete_cases])
          if(all(group_counts >= 2)) {
            homo_output <- capture.output({
              print(bartlett.test(var_data[complete_cases], group_data[complete_cases]))
            })
            
            for(line in homo_output) {
              doc <- doc %>% body_add_par(line, style = "Normal")
            }
          } else {
            doc <- doc %>% body_add_par("Uji homogenitas tidak dapat dilakukan karena salah satu grup memiliki kurang dari 2 observasi.", style = "Normal")
          }
        } else {
          doc <- doc %>% body_add_par("Data tidak mencukupi atau variabel kelompok tidak valid untuk uji homogenitas.", style = "Normal")
        }
      } else {
        doc <- doc %>% body_add_par("Tidak ada variabel kelompok yang dipilih untuk uji homogenitas.", style = "Normal")
      }
      # =====================================================================
      # AKHIR DARI BAGIAN YANG DIPERBAIKI
      # =====================================================================
      
      print(doc, target = file)
    }
  )
  
  output$download_asumsi_full <- downloadHandler(
    filename = function() {
      paste0("laporan_lengkap_uji_asumsi_", Sys.Date(), ".docx")
    },
    content = function(file) {
      doc <- read_docx()
      doc <- doc %>%
        body_add_par("LAPORAN LENGKAP UJI ASUMSI STATISTIK", style = "heading 1") %>%
        body_add_par("SOVI-STAT EXPLORER", style = "heading 2") %>%
        body_add_par(paste("Tanggal:", Sys.Date()), style = "Normal") %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("1. RINGKASAN EKSEKUTIF", style = "heading 2") %>%
        body_add_par("Laporan ini menyajikan hasil uji asumsi statistik yang diperlukan sebelum melakukan analisis inferensia pada data SOVI.", style = "Normal") %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("2. METODOLOGI", style = "heading 2") %>%
        body_add_par("Uji normalitas menggunakan Shapiro-Wilk (n<5000) atau Kolmogorov-Smirnov (n≥5000).", style = "Normal") %>%
        body_add_par("Uji homogenitas menggunakan Bartlett Test dan Levene Test.", style = "Normal") %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("3. KESIMPULAN", style = "heading 2") %>%
        body_add_par("Hasil uji asumsi menentukan pemilihan metode analisis statistik yang tepat untuk data SOVI.", style = "Normal")
      
      print(doc, target = file)
    }
  )
  
  # =====================================================================
  # TAB UJI BEDA RATA-RATA - DIPERBAIKI
  # =====================================================================
  
  t_test_result <- reactive({
    req(input$t_test_var, input$t_test_type)
    
    var_data <- sovi_data[[input$t_test_var]]
    var_data <- var_data[!is.na(var_data)]
    
    if(length(var_data) < 2) return(NULL)
    
    tryCatch({
      if(input$t_test_type == "one_sample") {
        t.test(var_data, mu = input$mu_value, conf.level = 1 - input$alpha_t)
      } else if(input$t_test_type == "two_sample") {
        if(input$group_var_t == "none" || !input$group_var_t %in% names(categorical_vars$data)) {
          return(list(error = "Pilih variabel kelompok yang valid dari hasil kategorisasi."))
        }
        
        group_data <- categorical_vars$data[[input$group_var_t]]
        complete_cases <- !is.na(var_data) & !is.na(group_data)
        var_clean <- var_data[complete_cases]
        group_clean <- group_data[complete_cases]
        
        group_levels <- unique(group_clean)
        if(length(group_levels) != 2) {
          return(list(error = "Variabel kelompok harus memiliki tepat 2 kategori untuk two-sample t-test."))
        }
        
        group1_data <- var_clean[group_clean == group_levels[1]]
        group2_data <- var_clean[group_clean == group_levels[2]]
        
        if(length(group1_data) < 2 || length(group2_data) < 2) {
          return(list(error = "Setiap kelompok harus memiliki minimal 2 observasi."))
        }
        
        t.test(group1_data, group2_data, var.equal = input$equal_var, conf.level = 1 - input$alpha_t)
      } else if(input$t_test_type == "paired") {
        if(input$group_var_t == "none" || !input$group_var_t %in% names(categorical_vars$data)) {
          return(list(error = "Pilih variabel kelompok yang valid dari hasil kategorisasi."))
        }
        
        group_data <- categorical_vars$data[[input$group_var_t]]
        complete_cases <- !is.na(var_data) & !is.na(group_data)
        var_clean <- var_data[complete_cases]
        group_clean <- group_data[complete_cases]
        
        group_levels <- unique(group_clean)
        if(length(group_levels) != 2) {
          return(list(error = "Variabel kelompok harus memiliki tepat 2 kategori untuk paired t-test."))
        }
        
        group1_data <- var_clean[group_clean == group_levels[1]]
        group2_data <- var_clean[group_clean == group_levels[2]]
        
        if(length(group1_data) != length(group2_data)) {
          min_length <- min(length(group1_data), length(group2_data))
          group1_data <- group1_data[1:min_length]
          group2_data <- group2_data[1:min_length]
        }
        
        if(length(group1_data) < 2) {
          return(list(error = "Tidak cukup pasangan data untuk paired t-test."))
        }
        
        t.test(group1_data, group2_data, paired = TRUE, conf.level = 1 - input$alpha_t)
      }
    }, error = function(e) {
      list(error = paste("Error dalam uji t:", e$message))
    })
  })
  
  output$t_test_result <- renderPrint({
    result <- t_test_result()
    if(is.null(result)) {
      return("Pilih parameter yang valid untuk uji t.")
    }
    if("error" %in% names(result)) {
      return(result$error)
    }
    result
  })
  
  output$t_test_plot <- renderPlot({
    req(input$t_test_var, input$t_test_type)
    
    var_data <- sovi_data[[input$t_test_var]]
    var_data <- var_data[!is.na(var_data)]
    
    if(length(var_data) < 2) return(NULL)
    
    if(input$t_test_type == "one_sample") {
      hist(var_data, main = paste("Distribusi", input$t_test_var), 
           xlab = input$t_test_var, col = "lightblue", border = "white")
      abline(v = mean(var_data), col = "red", lwd = 2, lty = 1)
      abline(v = input$mu_value, col = "blue", lwd = 2, lty = 2)
      legend("topright", c("Sample Mean", "H₀ Mean"), 
             col = c("red", "blue"), lty = c(1, 2), lwd = 2)
    } else if(input$t_test_type %in% c("two_sample", "paired")) {
      if(input$group_var_t != "none" && input$group_var_t %in% names(categorical_vars$data)) {
        group_data <- categorical_vars$data[[input$group_var_t]]
        complete_cases <- !is.na(var_data) & !is.na(group_data)
        
        plot_data <- data.frame(
          Variable = var_data[complete_cases],
          Group = group_data[complete_cases]
        )
        
        if(nrow(plot_data) > 0) {
          boxplot(Variable ~ Group, data = plot_data,
                  main = paste("Perbandingan", input$t_test_var, "antar Kelompok"),
                  xlab = input$group_var_t, ylab = input$t_test_var,
                  col = rainbow(length(unique(plot_data$Group))))
        }
      }
    }
  })
  
  output$t_test_interpretation <- renderText({
    result <- t_test_result()
    if(is.null(result) || "error" %in% names(result)) {
      return("Tidak dapat memberikan interpretasi karena error dalam perhitungan atau parameter tidak valid.")
    }
    
    var_context <- switch(input$t_test_var,
                          "POVERTY" = "tingkat kemiskinan",
                          "SOVI_SCORE" = "skor kerentanan sosial",
                          "CHILDREN" = "persentase anak-anak",
                          "ELDERLY" = "persentase lansia",
                          paste("indikator", tolower(input$t_test_var)))
    
    base_interpretation <- create_interpretation(result, "t-test", input$alpha_t)
    
    specific_context <- switch(input$t_test_type,
                               "one_sample" = paste0(" Uji ini membandingkan rata-rata ", var_context, 
                                                     " dalam sampel (", round(result$estimate, 3), 
                                                     ") dengan nilai hipotesis (", input$mu_value, ")."),
                               "two_sample" = paste0(" Uji ini membandingkan rata-rata ", var_context, 
                                                     " antara dua kelompok berdasarkan ", input$group_var_t, "."),
                               "paired" = paste0(" Uji ini membandingkan ", var_context, 
                                                 " pada pengukuran berpasangan berdasarkan ", input$group_var_t, "."))
    
    practical_implication <- if(result$p.value < input$alpha_t) {
      paste0(" Dalam konteks SOVI, perbedaan yang signifikan ini menunjukkan adanya disparitas ", 
             var_context, " yang perlu mendapat perhatian dalam perumusan kebijakan.")
    } else {
      paste0(" Dalam konteks SOVI, tidak adanya perbedaan yang signifikan menunjukkan kondisi ", 
             var_context, " yang relatif homogen antar kelompok yang dibandingkan.")
    }
    
    paste0(base_interpretation, specific_context, practical_implication)
  })
  
  output$group_summary_table <- DT::renderDataTable({
    req(input$t_test_var)
    
    var_data <- sovi_data[[input$t_test_var]]
    
    if(input$t_test_type == "one_sample") {
      summary_df <- data.frame(
        "Statistik" = c("N", "Mean", "Std Dev", "Min", "Max", "Median"),
        "Nilai" = c(
          length(var_data[!is.na(var_data)]),
          round(mean(var_data, na.rm = TRUE), 4),
          round(sd(var_data, na.rm = TRUE), 4),
          round(min(var_data, na.rm = TRUE), 4),
          round(max(var_data, na.rm = TRUE), 4),
          round(median(var_data, na.rm = TRUE), 4)
        ),
        stringsAsFactors = FALSE
      )
    } else if(input$t_test_type %in% c("two_sample", "paired")) {
      if(input$group_var_t != "none" && input$group_var_t %in% names(categorical_vars$data)) {
        group_data <- categorical_vars$data[[input$group_var_t]]
        complete_cases <- !is.na(var_data) & !is.na(group_data)
        var_clean <- var_data[complete_cases]
        group_clean <- group_data[complete_cases]
        
        group_summary <- aggregate(var_clean, by = list(Group = group_clean), 
                                   FUN = function(x) c(
                                     N = length(x),
                                     Mean = round(mean(x), 4),
                                     SD = round(sd(x), 4),
                                     Min = round(min(x), 4),
                                     Max = round(max(x), 4),
                                     Median = round(median(x), 4)
                                   ))
        
        summary_df <- data.frame(
          "Kelompok" = group_summary$Group,
          "N" = group_summary$x[, "N"],
          "Mean" = group_summary$x[, "Mean"],
          "Std_Dev" = group_summary$x[, "SD"],
          "Min" = group_summary$x[, "Min"],
          "Max" = group_summary$x[, "Max"],
          "Median" = group_summary$x[, "Median"],
          stringsAsFactors = FALSE
        )
      } else {
        summary_df <- data.frame(Pesan = "Pilih variabel kelompok yang valid")
      }
    } else {
      summary_df <- data.frame(Pesan = "Pilih jenis uji yang valid")
    }
    
    DT::datatable(summary_df, 
                  options = list(pageLength = 10, scrollX = TRUE),
                  class = 'cell-border stripe hover',
                  rownames = FALSE)
  })
  
  output$download_t_test <- downloadHandler(
    filename = function() {
      paste0("hasil_uji_t_", input$t_test_var, "_", Sys.Date(), ".docx")
    },
    content = function(file) {
      doc <- read_docx()
      doc <- doc %>%
        body_add_par("LAPORAN UJI BEDA RATA-RATA (T-TEST)", style = "heading 1") %>%
        body_add_par("SOVI-STAT EXPLORER", style = "heading 2") %>%
        body_add_par(paste("Tanggal:", Sys.Date()), style = "Normal") %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("1. INFORMASI UJI", style = "heading 2") %>%
        body_add_par(paste("Variabel yang diuji:", input$t_test_var), style = "Normal") %>%
        body_add_par(paste("Jenis uji:", input$t_test_type), style = "Normal") %>%
        body_add_par(paste("Tingkat signifikansi:", input$alpha_t), style = "Normal")
      
      result <- t_test_result()
      if(!is.null(result) && !"error" %in% names(result)) {
        doc <- doc %>%
          body_add_par("", style = "Normal") %>%
          body_add_par("2. HASIL UJI", style = "heading 2")
        
        result_output <- capture.output(print(result))
        for(line in result_output) {
          doc <- doc %>% body_add_par(line, style = "Normal")
        }
      }
      
      print(doc, target = file)
    }
  )
  
  output$download_uji_rata_full <- downloadHandler(
    filename = function() {
      paste0("laporan_lengkap_uji_rata_", Sys.Date(), ".docx")
    },
    content = function(file) {
      doc <- read_docx()
      doc <- doc %>%
        body_add_par("LAPORAN LENGKAP UJI BEDA RATA-RATA", style = "heading 1") %>%
        body_add_par("SOVI-STAT EXPLORER", style = "heading 2") %>%
        body_add_par(paste("Tanggal:", Sys.Date()), style = "Normal") %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("1. RINGKASAN EKSEKUTIF", style = "heading 2") %>%
        body_add_par("Laporan ini menyajikan hasil uji beda rata-rata untuk menganalisis perbedaan indikator kerentanan sosial antar kelompok atau kondisi.", style = "Normal") %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("2. METODOLOGI", style = "heading 2") %>%
        body_add_par(paste("Jenis uji yang digunakan:", input$t_test_type), style = "Normal") %>%
        body_add_par("Uji t digunakan untuk membandingkan rata-rata dengan asumsi distribusi normal.", style = "Normal") %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("3. KESIMPULAN", style = "heading 2") %>%
        body_add_par("Hasil uji beda rata-rata memberikan wawasan tentang disparitas kondisi kerentanan sosial yang dapat digunakan untuk prioritas intervensi.", style = "Normal")
      
      print(doc, target = file)
    }
  )
  
  # =====================================================================
  # TAB UJI PROPORSI & RAGAM - DIPERBAIKI
  # =====================================================================
  
  prop_var_test_result <- reactive({
    req(input$test_type_pv, input$var_test_var)
    
    var_data <- sovi_data[[input$var_test_var]]
    var_data <- var_data[!is.na(var_data)]
    
    if(length(var_data) < 2) return(NULL)
    
    tryCatch({
      if(input$test_type_pv == "var_one") {
        # Uji ragam satu kelompok (Chi-square test untuk variance)
        n <- length(var_data)
        sample_var <- var(var_data)
        chi_stat <- (n - 1) * sample_var / input$var_null
        p_value <- 2 * min(pchisq(chi_stat, df = n - 1), 1 - pchisq(chi_stat, df = n - 1))
        
        list(
          statistic = chi_stat,
          parameter = n - 1,
          p.value = p_value,
          estimate = sample_var,
          null.value = input$var_null,
          method = "One-sample test for variance",
          data.name = input$var_test_var
        )
      } else if(input$test_type_pv == "var_two") {
        if(input$group_var_var == "none" || !input$group_var_var %in% names(categorical_vars$data)) {
          return(list(error = "Pilih variabel kelompok yang valid dari hasil kategorisasi."))
        }
        
        group_data <- categorical_vars$data[[input$group_var_var]]
        complete_cases <- !is.na(var_data) & !is.na(group_data)
        var_clean <- var_data[complete_cases]
        group_clean <- group_data[complete_cases]
        
        group_levels <- unique(group_clean)
        if(length(group_levels) != 2) {
          return(list(error = "Variabel kelompok harus memiliki tepat 2 kategori untuk uji ragam dua kelompok."))
        }
        
        group1_data <- var_clean[group_clean == group_levels[1]]
        group2_data <- var_clean[group_clean == group_levels[2]]
        
        if(length(group1_data) < 2 || length(group2_data) < 2) {
          return(list(error = "Setiap kelompok harus memiliki minimal 2 observasi."))
        }
        
        var.test(group1_data, group2_data, conf.level = 1 - input$alpha_pv)
      }
    }, error = function(e) {
      list(error = paste("Error dalam uji ragam:", e$message))
    })
  })
  
  output$prop_var_result <- renderPrint({
    result <- prop_var_test_result()
    if(is.null(result)) {
      return("Pilih parameter yang valid untuk uji ragam.")
    }
    if("error" %in% names(result)) {
      return(result$error)
    }
    result
  })
  
  output$prop_var_plot <- renderPlot({
    req(input$test_type_pv, input$var_test_var)
    
    var_data <- sovi_data[[input$var_test_var]]
    var_data <- var_data[!is.na(var_data)]
    
    if(length(var_data) < 2) return(NULL)
    
    if(input$test_type_pv == "var_one") {
      hist(var_data, main = paste("Distribusi", input$var_test_var), 
           xlab = input$var_test_var, col = "lightgreen", border = "white")
      abline(v = mean(var_data), col = "red", lwd = 2)
      legend("topright", "Sample Mean", col = "red", lty = 1, lwd = 2)
    } else if(input$test_type_pv == "var_two") {
      if(input$group_var_var != "none" && input$group_var_var %in% names(categorical_vars$data)) {
        group_data <- categorical_vars$data[[input$group_var_var]]
        complete_cases <- !is.na(var_data) & !is.na(group_data)
        
        plot_data <- data.frame(
          Variable = var_data[complete_cases],
          Group = group_data[complete_cases]
        )
        
        if(nrow(plot_data) > 0) {
          par(mfrow = c(1, 2))
          
          boxplot(Variable ~ Group, data = plot_data,
                  main = paste("Boxplot", input$var_test_var),
                  xlab = input$group_var_var, ylab = input$var_test_var,
                  col = rainbow(length(unique(plot_data$Group))))
          
          group_vars <- tapply(plot_data$Variable, plot_data$Group, var, na.rm = TRUE)
          barplot(group_vars, main = "Perbandingan Ragam antar Kelompok",
                  ylab = "Ragam", col = rainbow(length(group_vars)))
          
          par(mfrow = c(1, 1))
        }
      }
    }
  })
  
  output$prop_var_interpretation <- renderText({
    result <- prop_var_test_result()
    if(is.null(result) || "error" %in% names(result)) {
      return("Tidak dapat memberikan interpretasi karena error dalam perhitungan atau parameter tidak valid.")
    }
    
    var_context <- switch(input$var_test_var,
                          "POVERTY" = "tingkat kemiskinan",
                          "SOVI_SCORE" = "skor kerentanan sosial",
                          "CHILDREN" = "persentase anak-anak",
                          paste("indikator", tolower(input$var_test_var)))
    
    if(input$test_type_pv == "var_one") {
      paste0("Uji ragam satu kelompok untuk ", var_context, " menghasilkan statistik χ² = ", 
             round(result$statistic, 4), " dengan p-value = ", round(result$p.value, 4), ". ",
             if(result$p.value < input$alpha_pv) {
               paste0("Hasil signifikan menunjukkan bahwa ragam sampel (", round(result$estimate, 4), 
                      ") berbeda secara signifikan dari ragam yang dihipotesiskan (", input$var_null, "). ")
             } else {
               paste0("Hasil tidak signifikan menunjukkan bahwa ragam sampel (", round(result$estimate, 4), 
                      ") tidak berbeda secara signifikan dari ragam yang dihipotesiskan (", input$var_null, "). ")
             },
             "Dalam konteks SOVI, ragam menunjukkan tingkat variabilitas atau disparitas ", var_context, " antar wilayah.")
    } else {
      paste0("Uji ragam dua kelompok (F-test) untuk ", var_context, " menghasilkan statistik F = ", 
             round(result$statistic, 4), " dengan p-value = ", round(result$p.value, 4), ". ",
             if(result$p.value < input$alpha_pv) {
               "Hasil signifikan menunjukkan bahwa ragam kedua kelompok berbeda secara signifikan. "
             } else {
               "Hasil tidak signifikan menunjukkan bahwa ragam kedua kelompok tidak berbeda secara signifikan. "
             },
             "Perbedaan ragam mengindikasikan tingkat variabilitas ", var_context, 
             " yang berbeda antar kelompok, yang penting untuk memahami heterogenitas kondisi sosial.")
    }
  })
  
  output$prop_var_summary_table <- DT::renderDataTable({
    req(input$var_test_var)
    
    var_data <- sovi_data[[input$var_test_var]]
    
    if(input$test_type_pv == "var_one") {
      summary_df <- data.frame(
        "Statistik" = c("N", "Mean", "Variance", "Std Dev", "Min", "Max"),
        "Nilai" = c(
          length(var_data[!is.na(var_data)]),
          round(mean(var_data, na.rm = TRUE), 4),
          round(var(var_data, na.rm = TRUE), 4),
          round(sd(var_data, na.rm = TRUE), 4),
          round(min(var_data, na.rm = TRUE), 4),
          round(max(var_data, na.rm = TRUE), 4)
        ),
        stringsAsFactors = FALSE
      )
    } else if(input$test_type_pv == "var_two") {
      if(input$group_var_var != "none" && input$group_var_var %in% names(categorical_vars$data)) {
        group_data <- categorical_vars$data[[input$group_var_var]]
        complete_cases <- !is.na(var_data) & !is.na(group_data)
        var_clean <- var_data[complete_cases]
        group_clean <- group_data[complete_cases]
        
        group_summary <- aggregate(var_clean, by = list(Group = group_clean), 
                                   FUN = function(x) c(
                                     N = length(x),
                                     Mean = round(mean(x), 4),
                                     Variance = round(var(x), 4),
                                     SD = round(sd(x), 4),
                                     Min = round(min(x), 4),
                                     Max = round(max(x), 4)
                                   ))
        
        summary_df <- data.frame(
          "Kelompok" = group_summary$Group,
          "N" = group_summary$x[, "N"],
          "Mean" = group_summary$x[, "Mean"],
          "Variance" = group_summary$x[, "Variance"],
          "Std_Dev" = group_summary$x[, "SD"],
          "Min" = group_summary$x[, "Min"],
          "Max" = group_summary$x[, "Max"],
          stringsAsFactors = FALSE
        )
      } else {
        summary_df <- data.frame(Pesan = "Pilih variabel kelompok yang valid")
      }
    } else {
      summary_df <- data.frame(Pesan = "Pilih jenis uji yang valid")
    }
    
    DT::datatable(summary_df, 
                  options = list(pageLength = 10, scrollX = TRUE),
                  class = 'cell-border stripe hover',
                  rownames = FALSE)
  })
  
  output$download_prop_var <- downloadHandler(
    filename = function() {
      paste0("hasil_uji_ragam_", input$var_test_var, "_", Sys.Date(), ".docx")
    },
    content = function(file) {
      doc <- read_docx()
      doc <- doc %>%
        body_add_par("LAPORAN UJI RAGAM", style = "heading 1") %>%
        body_add_par("SOVI-STAT EXPLORER", style = "heading 2") %>%
        body_add_par(paste("Tanggal:", Sys.Date()), style = "Normal") %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("1. INFORMASI UJI", style = "heading 2") %>%
        body_add_par(paste("Variabel yang diuji:", input$var_test_var), style = "Normal") %>%
        body_add_par(paste("Jenis uji:", input$test_type_pv), style = "Normal") %>%
        body_add_par(paste("Tingkat signifikansi:", input$alpha_pv), style = "Normal")
      
      result <- prop_var_test_result()
      if(!is.null(result) && !"error" %in% names(result)) {
        doc <- doc %>%
          body_add_par("", style = "Normal") %>%
          body_add_par("2. HASIL UJI", style = "heading 2")
        
        result_output <- capture.output(print(result))
        for(line in result_output) {
          doc <- doc %>% body_add_par(line, style = "Normal")
        }
      }
      
      print(doc, target = file)
    }
  )
  
  output$download_prop_var_full <- downloadHandler(
    filename = function() {
      paste0("laporan_lengkap_uji_ragam_", Sys.Date(), ".docx")
    },
    content = function(file) {
      doc <- read_docx()
      doc <- doc %>%
        body_add_par("LAPORAN LENGKAP UJI RAGAM", style = "heading 1") %>%
        body_add_par("SOVI-STAT EXPLORER", style = "heading 2") %>%
        body_add_par(paste("Tanggal:", Sys.Date()), style = "Normal") %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("1. RINGKASAN EKSEKUTIF", style = "heading 2") %>%
        body_add_par("Laporan ini menyajikan hasil uji ragam untuk menganalisis variabilitas indikator kerentanan sosial.", style = "Normal") %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("2. METODOLOGI", style = "heading 2") %>%
        body_add_par("Uji ragam menggunakan distribusi Chi-square untuk satu kelompok dan F-test untuk dua kelompok.", style = "Normal") %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("3. KESIMPULAN", style = "heading 2") %>%
        body_add_par("Hasil uji ragam memberikan informasi tentang homogenitas atau heterogenitas kondisi kerentanan sosial.", style = "Normal")
      
      print(doc, target = file)
    }
  )
  
  # =====================================================================
  # TAB ANOVA - DIPERBAIKI
  # =====================================================================
  
  anova_result <- reactive({
    req(input$anova_y, input$anova_x1)
    
    if(input$anova_x1 == "none" || !input$anova_x1 %in% names(categorical_vars$data)) {
      return(list(error = "Pilih variabel faktor yang valid dari hasil kategorisasi."))
    }
    
    y_data <- sovi_data[[input$anova_y]]
    x1_data <- categorical_vars$data[[input$anova_x1]]
    
    complete_cases <- !is.na(y_data) & !is.na(x1_data)
    y_clean <- y_data[complete_cases]
    x1_clean <- x1_data[complete_cases]
    
    if(length(y_clean) < 3) {
      return(list(error = "Data tidak mencukupi untuk ANOVA (minimal 3 observasi)."))
    }
    
    group_counts <- table(x1_clean)
    if(any(group_counts < 2)) {
      return(list(error = "Setiap kelompok harus memiliki minimal 2 observasi."))
    }
    
    if(length(unique(x1_clean)) < 2) {
      return(list(error = "Variabel faktor harus memiliki minimal 2 kategori."))
    }
    
    tryCatch({
      anova_data <- data.frame(y = y_clean, x1 = as.factor(x1_clean))
      
      if(input$anova_type == "one_way") {
        model <- aov(y ~ x1, data = anova_data)
        anova_summary <- summary(model)
        
        list(
          model = model,
          summary = anova_summary,
          data = anova_data,
          type = "one_way"
        )
      }
    }, error = function(e) {
      list(error = paste("Error dalam ANOVA:", e$message))
    })
  })
  
  output$anova_result <- renderPrint({
    result <- anova_result()
    if(is.null(result)) {
      return("Pilih parameter yang valid untuk ANOVA.")
    }
    if("error" %in% names(result)) {
      return(result$error)
    }
    result$summary
  })
  
  output$anova_plot <- renderPlot({
    result <- anova_result()
    if(is.null(result) || "error" %in% names(result)) return(NULL)
    
    if(result$type == "one_way") {
      par(mfrow = c(2, 2))
      
      # Boxplot
      boxplot(y ~ x1, data = result$data,
              main = paste("Boxplot", input$anova_y, "by", input$anova_x1),
              xlab = input$anova_x1, ylab = input$anova_y,
              col = rainbow(length(unique(result$data$x1))))
      
      # Means plot
      group_means <- tapply(result$data$y, result$data$x1, mean)
      group_se <- tapply(result$data$y, result$data$x1, function(x) sd(x)/sqrt(length(x)))
      
      plot(1:length(group_means), group_means, 
           main = "Group Means with Error Bars",
           xlab = "Groups", ylab = "Mean",
           pch = 19, col = "blue", cex = 1.5,
           xlim = c(0.5, length(group_means) + 0.5),
           ylim = c(min(group_means - group_se), max(group_means + group_se)))
      arrows(1:length(group_means), group_means - group_se,
             1:length(group_means), group_means + group_se,
             angle = 90, code = 3, length = 0.1)
      axis(1, at = 1:length(group_means), labels = names(group_means))
      
      # Residuals plot
      plot(fitted(result$model), residuals(result$model),
           main = "Residuals vs Fitted",
           xlab = "Fitted Values", ylab = "Residuals",
           pch = 19, col = "red")
      abline(h = 0, lty = 2)
      
      # Q-Q plot of residuals
      qqnorm(residuals(result$model), main = "Q-Q Plot of Residuals")
      qqline(residuals(result$model), col = "red")
      
      par(mfrow = c(1, 1))
    }
  })
  
  output$anova_interpretation <- renderText({
    result <- anova_result()
    if(is.null(result) || "error" %in% names(result)) {
      return("Tidak dapat memberikan interpretasi karena error dalam perhitungan atau parameter tidak valid.")
    }
    
    f_stat <- result$summary[[1]]$`F value`[1]
    p_value <- result$summary[[1]]$`Pr(>F)`[1]
    
    var_context <- switch(input$anova_y,
                          "POVERTY" = "tingkat kemiskinan",
                          "SOVI_SCORE" = "skor kerentanan sosial",
                          "CHILDREN" = "persentase anak-anak",
                          paste("indikator", tolower(input$anova_y)))
    
    factor_context <- switch(input$anova_x1,
                             paste("kategori", gsub("_CAT$", "", input$anova_x1)))
    
    base_interpretation <- paste0("ANOVA satu arah untuk ", var_context, " berdasarkan ", factor_context, 
                                  " menghasilkan statistik F = ", round(f_stat, 4), 
                                  " dengan p-value = ", round(p_value, 4), ". ")
    
    significance_interpretation <- if(p_value < input$alpha_anova) {
      paste0("Hasil signifikan menunjukkan bahwa terdapat perbedaan rata-rata ", var_context, 
             " yang signifikan antar ", factor_context, ". Hal ini mengindikasikan bahwa ", 
             factor_context, " berpengaruh terhadap ", var_context, " dalam konteks kerentanan sosial.")
    } else {
      paste0("Hasil tidak signifikan menunjukkan bahwa tidak terdapat perbedaan rata-rata ", var_context, 
             " yang signifikan antar ", factor_context, ". Hal ini mengindikasikan bahwa ", 
             var_context, " relatif homogen antar ", factor_context, ".")
    }
    
    practical_implication <- if(p_value < input$alpha_anova) {
      " Perbedaan yang signifikan ini menunjukkan perlunya strategi intervensi yang berbeda untuk setiap kategori dalam upaya mengurangi kerentanan sosial."
    } else {
      " Kondisi yang homogen ini menunjukkan bahwa strategi intervensi dapat diterapkan secara seragam tanpa perlu diferensiasi berdasarkan kategori ini."
    }
    
    paste0(base_interpretation, significance_interpretation, practical_implication)
  })
  
  # Post-hoc test
  posthoc_result <- reactive({
    result <- anova_result()
    if(is.null(result) || "error" %in% names(result)) return(NULL)
    
    p_value <- result$summary[[1]]$`Pr(>F)`[1]
    if(p_value >= input$alpha_anova) return(NULL)  # Only do post-hoc if ANOVA is significant
    
    tryCatch({
      TukeyHSD(result$model)
    }, error = function(e) {
      NULL
    })
  })
  
  output$show_posthoc <- reactive({
    !is.null(posthoc_result())
  })
  outputOptions(output, "show_posthoc", suspendWhenHidden = FALSE)
  
  output$posthoc_result <- renderPrint({
    posthoc <- posthoc_result()
    if(is.null(posthoc)) return("Uji lanjut tidak diperlukan karena ANOVA tidak signifikan.")
    posthoc
  })
  
  output$posthoc_interpretation <- renderText({
    posthoc <- posthoc_result()
    if(is.null(posthoc)) return("")
    
    # Extract significant comparisons
    posthoc_table <- posthoc[[1]]
    significant_pairs <- rownames(posthoc_table)[posthoc_table[, "p adj"] < input$alpha_anova]
    
    if(length(significant_pairs) == 0) {
      return("Meskipun ANOVA signifikan, uji Tukey HSD tidak menunjukkan perbedaan yang signifikan antar pasangan kelompok setelah koreksi multiple comparison.")
    }
    
    paste0("Uji lanjut Tukey HSD mengidentifikasi ", length(significant_pairs), 
           " pasangan kelompok yang berbeda signifikan: ", 
           paste(significant_pairs[1:min(3, length(significant_pairs))], collapse = ", "),
           if(length(significant_pairs) > 3) " dan lainnya." else ".",
           " Hasil ini menunjukkan kelompok-kelompok spesifik yang memiliki perbedaan rata-rata signifikan dalam konteks kerentanan sosial.")
  })
  
  output$anova_summary_table <- DT::renderDataTable({
    result <- anova_result()
    if(is.null(result) || "error" %in% names(result)) {
      return(data.frame(Pesan = "ANOVA tidak tersedia"))
    }
    
    # Create summary table
    anova_table <- result$summary[[1]]
    
    summary_df <- data.frame(
      "Source" = c("Between Groups", "Within Groups", "Total"),
      "Df" = c(anova_table$Df[1], anova_table$Df[2], sum(anova_table$Df)),
      "Sum_Sq" = c(round(anova_table$`Sum Sq`[1], 4), round(anova_table$`Sum Sq`[2], 4), 
                   round(sum(anova_table$`Sum Sq`), 4)),
      "Mean_Sq" = c(round(anova_table$`Mean Sq`[1], 4), round(anova_table$`Mean Sq`[2], 4), NA),
      "F_value" = c(round(anova_table$`F value`[1], 4), NA, NA),
      "P_value" = c(round(anova_table$`Pr(>F)`[1], 4), NA, NA),
      stringsAsFactors = FALSE
    )
    
    DT::datatable(summary_df, 
                  options = list(pageLength = 10, scrollX = TRUE),
                  class = 'cell-border stripe hover',
                  rownames = FALSE)
  })
  
  output$download_anova <- downloadHandler(
    filename = function() {
      paste0("hasil_anova_", input$anova_y, "_", Sys.Date(), ".docx")
    },
    content = function(file) {
      doc <- read_docx()
      doc <- doc %>%
        body_add_par("LAPORAN ANOVA", style = "heading 1") %>%
        body_add_par("SOVI-STAT EXPLORER", style = "heading 2") %>%
        body_add_par(paste("Tanggal:", Sys.Date()), style = "Normal") %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("1. INFORMASI ANALISIS", style = "heading 2") %>%
        body_add_par(paste("Variabel dependen:", input$anova_y), style = "Normal") %>%
        body_add_par(paste("Faktor:", input$anova_x1), style = "Normal") %>%
        body_add_par(paste("Jenis ANOVA:", input$anova_type), style = "Normal") %>%
        body_add_par(paste("Tingkat signifikansi:", input$alpha_anova), style = "Normal")
      
      result <- anova_result()
      if(!is.null(result) && !"error" %in% names(result)) {
        doc <- doc %>%
          body_add_par("", style = "Normal") %>%
          body_add_par("2. HASIL ANOVA", style = "heading 2")
        
        anova_output <- capture.output(print(result$summary))
        for(line in anova_output) {
          doc <- doc %>% body_add_par(line, style = "Normal")
        }
      }
      
      print(doc, target = file)
    }
  )
  
  output$download_anova_full <- downloadHandler(
    filename = function() {
      paste0("laporan_lengkap_anova_", Sys.Date(), ".docx")
    },
    content = function(file) {
      doc <- read_docx()
      doc <- doc %>%
        body_add_par("LAPORAN LENGKAP ANOVA", style = "heading 1") %>%
        body_add_par("SOVI-STAT EXPLORER", style = "heading 2") %>%
        body_add_par(paste("Tanggal:", Sys.Date()), style = "Normal") %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("1. RINGKASAN EKSEKUTIF", style = "heading 2") %>%
        body_add_par("Laporan ini menyajikan hasil Analysis of Variance (ANOVA) untuk menganalisis perbedaan rata-rata indikator kerentanan sosial antar kelompok.", style = "Normal") %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("2. METODOLOGI", style = "heading 2") %>%
        body_add_par("ANOVA digunakan untuk menguji perbedaan rata-rata lebih dari dua kelompok dengan asumsi normalitas dan homogenitas ragam.", style = "Normal") %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("3. KESIMPULAN", style = "heading 2") %>%
        body_add_par("Hasil ANOVA memberikan wawasan tentang variabilitas indikator kerentanan sosial antar kategori yang dapat digunakan untuk strategi intervensi yang tepat sasaran.", style = "Normal")
      
      print(doc, target = file)
    }
  )
  
  # =====================================================================
  # TAB REGRESI LINEAR BERGANDA - SAMA SEPERTI SEBELUMNYA
  # =====================================================================
  
  regression_model <- reactive({
    req(input$reg_y, input$reg_x)
    
    if(length(input$reg_x) == 0) return(NULL)
    
    # Prepare data
    y_data <- sovi_data[[input$reg_y]]
    x_data <- sovi_data[, input$reg_x, drop = FALSE]
    
    # Remove rows with missing values
    complete_data <- cbind(y = y_data, x_data)
    complete_data <- complete_data[complete.cases(complete_data), ]
    
    if(nrow(complete_data) < length(input$reg_x) + 2) {
      return(list(error = "Data tidak mencukupi untuk regresi linear berganda."))
    }
    
    tryCatch({
      # Create formula
      if(input$include_interaction && !is.null(input$interaction_vars) && length(input$interaction_vars) == 2) {
        formula_str <- paste(input$reg_y, "~", paste(input$reg_x, collapse = " + "), 
                             "+", paste(input$interaction_vars, collapse = " * "))
      } else {
        formula_str <- paste(input$reg_y, "~", paste(input$reg_x, collapse = " + "))
      }
      
      formula_obj <- as.formula(formula_str)
      
      # Fit model
      model <- lm(formula_obj, data = sovi_data)
      
      list(
        model = model,
        data = complete_data,
        formula = formula_str
      )
    }, error = function(e) {
      list(error = paste("Error dalam regresi:", e$message))
    })
  })
  
  output$regression_summary <- renderPrint({
    result <- regression_model()
    if(is.null(result)) {
      return("Pilih variabel yang valid untuk regresi.")
    }
    if("error" %in% names(result)) {
      return(result$error)
    }
    summary(result$model)
  })
  
  output$regression_interpretation <- renderText({
    result <- regression_model()
    if(is.null(result) || "error" %in% names(result)) {
      return("Tidak dapat memberikan interpretasi karena error dalam perhitungan atau parameter tidak valid.")
    }
    
    model_summary <- summary(result$model)
    r_squared <- model_summary$r.squared
    adj_r_squared <- model_summary$adj.r.squared
    f_stat <- model_summary$fstatistic[1]
    f_p_value <- pf(f_stat, model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE)
    
    significant_vars <- rownames(model_summary$coefficients)[model_summary$coefficients[, "Pr(>|t|)"] < input$alpha_reg]
    significant_vars <- significant_vars[significant_vars != "(Intercept)"]
    
    var_context <- switch(input$reg_y,
                          "POVERTY" = "tingkat kemiskinan",
                          "SOVI_SCORE" = "skor kerentanan sosial keseluruhan",
                          "CHILDREN" = "persentase anak-anak",
                          paste("indikator", tolower(input$reg_y)))
    
    paste0("Model regresi linear berganda untuk ", var_context, " menunjukkan R² = ", round(r_squared, 4), 
           " (R² adjusted = ", round(adj_r_squared, 4), "), yang berarti ", round(r_squared * 100, 1), 
           "% variabilitas ", var_context, " dapat dijelaskan oleh variabel prediktor dalam model. ",
           "Uji F menghasilkan statistik F = ", round(f_stat, 4), " dengan p-value = ", round(f_p_value, 4), 
           if(f_p_value < input$alpha_reg) {
             ", menunjukkan bahwa model secara keseluruhan signifikan. "
           } else {
             ", menunjukkan bahwa model secara keseluruhan tidak signifikan. "
           },
           if(length(significant_vars) > 0) {
             paste0("Variabel yang berpengaruh signifikan terhadap ", var_context, " adalah: ", 
                    paste(significant_vars[1:min(3, length(significant_vars))], collapse = ", "),
                    if(length(significant_vars) > 3) " dan lainnya." else ".")
           } else {
             paste0("Tidak ada variabel prediktor yang berpengaruh signifikan terhadap ", var_context, ".")
           },
           " Dalam konteks SOVI, model ini dapat digunakan untuk memahami faktor-faktor yang berkontribusi terhadap kerentanan sosial.")
  })
  
  output$regression_scatter_plot <- renderPlot({
    result <- regression_model()
    if(is.null(result) || "error" %in% names(result)) return(NULL)
    
    # Plot actual vs predicted
    predicted <- fitted(result$model)
    actual <- sovi_data[[input$reg_y]][!is.na(sovi_data[[input$reg_y]])]
    
    if(length(predicted) != length(actual)) {
      # Match lengths
      min_length <- min(length(predicted), length(actual))
      predicted <- predicted[1:min_length]
      actual <- actual[1:min_length]
    }
    
    plot(actual, predicted, 
         main = paste("Actual vs Predicted:", input$reg_y),
         xlab = "Actual Values", ylab = "Predicted Values",
         pch = 19, col = "blue", alpha = 0.6)
    abline(0, 1, col = "red", lwd = 2, lty = 2)
    
    # Add correlation
    correlation <- cor(actual, predicted, use = "complete.obs")
    legend("topleft", paste("r =", round(correlation, 3)), bty = "n")
  })
  
  output$regression_plot_interpretation <- renderText({
    result <- regression_model()
    if(is.null(result) || "error" %in% names(result)) {
      return("Plot tidak tersedia karena error dalam model regresi.")
    }
    
    predicted <- fitted(result$model)
    actual <- sovi_data[[input$reg_y]][!is.na(sovi_data[[input$reg_y]])]
    
    if(length(predicted) != length(actual)) {
      min_length <- min(length(predicted), length(actual))
      predicted <- predicted[1:min_length]
      actual <- actual[1:min_length]
    }
    
    correlation <- cor(actual, predicted, use = "complete.obs")
    
    paste0("Plot actual vs predicted menunjukkan korelasi r = ", round(correlation, 3), 
           " antara nilai aktual dan prediksi model. ",
           if(correlation > 0.8) {
             "Korelasi yang tinggi ini menunjukkan bahwa model memiliki kemampuan prediksi yang baik."
           } else if(correlation > 0.5) {
             "Korelasi yang sedang ini menunjukkan bahwa model memiliki kemampuan prediksi yang cukup baik."
           } else {
             "Korelasi yang rendah ini menunjukkan bahwa model memiliki kemampuan prediksi yang terbatas."
           },
           " Titik-titik yang mendekati garis diagonal (y=x) menunjukkan prediksi yang akurat, ",
           "sedangkan titik yang menyebar jauh dari garis menunjukkan residual yang besar.")
  })
  
  output$regression_plots <- renderPlot({
    result <- regression_model()
    if(is.null(result) || "error" %in% names(result)) return(NULL)
    
    par(mfrow = c(2, 2))
    plot(result$model)
    par(mfrow = c(1, 1))
  })
  
  # Assumption tests for regression
  output$linearity_test <- renderPrint({
    result <- regression_model()
    if(is.null(result) || "error" %in% names(result)) {
      return("Model regresi tidak tersedia untuk uji linearitas.")
    }
    
    # Rainbow test for linearity
    tryCatch({
      if(require(lmtest, quietly = TRUE)) {
        rainbow_test <- raintest(result$model)
        list(
          "Rainbow Test for Linearity" = rainbow_test,
          "Interpretasi" = if(rainbow_test$p.value >= 0.05) {
            "Asumsi linearitas terpenuhi"
          } else {
            "Asumsi linearitas tidak terpenuhi"
          }
        )
      } else {
        "Package lmtest tidak tersedia untuk uji linearitas."
      }
    }, error = function(e) {
      paste("Error dalam uji linearitas:", e$message)
    })
  })
  
  output$linearity_interpretation <- renderText({
    result <- regression_model()
    if(is.null(result) || "error" %in% names(result)) {
      return("Tidak dapat memberikan interpretasi uji linearitas.")
    }
    
    tryCatch({
      if(require(lmtest, quietly = TRUE)) {
        rainbow_test <- raintest(result$model)
        create_interpretation(rainbow_test, "linearitas", 0.05)
      } else {
        "Interpretasi tidak tersedia karena package lmtest tidak terinstal."
      }
    }, error = function(e) {
      "Tidak dapat memberikan interpretasi karena error dalam perhitungan."
    })
  })
  
  output$multicollinearity_test <- renderPrint({
    result <- regression_model()
    if(is.null(result) || "error" %in% names(result)) {
      return("Model regresi tidak tersedia untuk uji multikolinearitas.")
    }
    
    tryCatch({
      if(require(car, quietly = TRUE)) {
        vif_values <- vif(result$model)
        list(
          "Variance Inflation Factor (VIF)" = vif_values,
          "Interpretasi" = if(any(vif_values > 10)) {
            "Terdapat multikolinearitas yang serius (VIF > 10)"
          } else if(any(vif_values > 5)) {
            "Terdapat multikolinearitas moderat (VIF > 5)"
          } else {
            "Tidak terdapat multikolinearitas yang serius (VIF < 5)"
          }
        )
      } else {
        "Package car tidak tersedia untuk uji multikolinearitas."
      }
    }, error = function(e) {
      paste("Error dalam uji multikolinearitas:", e$message)
    })
  })
  
  output$multicollinearity_interpretation <- renderText({
    result <- regression_model()
    if(is.null(result) || "error" %in% names(result)) {
      return("Tidak dapat memberikan interpretasi uji multikolinearitas.")
    }
    
    tryCatch({
      if(require(car, quietly = TRUE)) {
        vif_values <- vif(result$model)
        max_vif <- max(vif_values)
        
        paste0("Uji multikolinearitas menggunakan Variance Inflation Factor (VIF) menunjukkan nilai tertinggi = ", 
               round(max_vif, 3), ". ",
               if(max_vif > 10) {
                 "Nilai VIF > 10 mengindikasikan multikolinearitas yang serius. Hal ini dapat menyebabkan ketidakstabilan koefisien regresi dan interpretasi yang menyesatkan. Pertimbangkan untuk menghapus atau menggabungkan variabel yang berkorelasi tinggi."
               } else if(max_vif > 5) {
                 "Nilai VIF > 5 mengindikasikan multikolinearitas moderat. Meskipun tidak kritis, perlu diperhatikan dalam interpretasi hasil regresi."
               } else {
                 "Nilai VIF < 5 menunjukkan tidak ada masalah multikolinearitas yang serius. Variabel prediktor relatif independen satu sama lain."
               })
      } else {
        "Interpretasi tidak tersedia karena package car tidak terinstal."
      }
    }, error = function(e) {
      "Tidak dapat memberikan interpretasi karena error dalam perhitungan."
    })
  })
  
  output$heteroscedasticity_test <- renderPrint({
    result <- regression_model()
    if(is.null(result) || "error" %in% names(result)) {
      return("Model regresi tidak tersedia untuk uji heteroskedastisitas.")
    }
    
    tryCatch({
      if(require(lmtest, quietly = TRUE)) {
        bp_test <- bptest(result$model)
        list(
          "Breusch-Pagan Test" = bp_test,
          "Interpretasi" = if(bp_test$p.value >= 0.05) {
            "Asumsi homoskedastisitas terpenuhi"
          } else {
            "Terdapat heteroskedastisitas"
          }
        )
      } else {
        "Package lmtest tidak tersedia untuk uji heteroskedastisitas."
      }
    }, error = function(e) {
      paste("Error dalam uji heteroskedastisitas:", e$message)
    })
  })
  
  output$heteroscedasticity_interpretation <- renderText({
    result <- regression_model()
    if(is.null(result) || "error" %in% names(result)) {
      return("Tidak dapat memberikan interpretasi uji heteroskedastisitas.")
    }
    
    tryCatch({
      if(require(lmtest, quietly = TRUE)) {
        bp_test <- bptest(result$model)
        create_interpretation(bp_test, "heteroskedastisitas", 0.05)
      } else {
        "Interpretasi tidak tersedia karena package lmtest tidak terinstal."
      }
    }, error = function(e) {
      "Tidak dapat memberikan interpretasi karena error dalam perhitungan."
    })
  })
  
  output$regression_summary_table <- DT::renderDataTable({
    result <- regression_model()
    if(is.null(result) || "error" %in% names(result)) {
      return(data.frame(Pesan = "Model regresi tidak tersedia"))
    }
    
    model_summary <- summary(result$model)
    coef_table <- model_summary$coefficients
    
    summary_df <- data.frame(
      "Variable" = rownames(coef_table),
      "Coefficient" = round(coef_table[, "Estimate"], 4),
      "Std_Error" = round(coef_table[, "Std. Error"], 4),
      "t_value" = round(coef_table[, "t value"], 4),
      "P_value" = round(coef_table[, "Pr(>|t|)"], 4),
      "Significance" = ifelse(coef_table[, "Pr(>|t|)"] < 0.001, "***",
                              ifelse(coef_table[, "Pr(>|t|)"] < 0.01, "**",
                                     ifelse(coef_table[, "Pr(>|t|)"] < 0.05, "*",
                                            ifelse(coef_table[, "Pr(>|t|)"] < 0.1, ".", "")))),
      stringsAsFactors = FALSE
    )
    
    DT::datatable(summary_df, 
                  options = list(pageLength = 15, scrollX = TRUE),
                  class = 'cell-border stripe hover',
                  rownames = FALSE)
  })
  
  output$download_regression <- downloadHandler(
    filename = function() {
      paste0("laporan_regresi_", input$reg_y, "_", Sys.Date(), ".docx")
    },
    content = function(file) {
      doc <- read_docx()
      doc <- doc %>%
        body_add_par("LAPORAN REGRESI LINEAR BERGANDA", style = "heading 1") %>%
        body_add_par("SOVI-STAT EXPLORER", style = "heading 2") %>%
        body_add_par(paste("Tanggal:", Sys.Date()), style = "Normal") %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("1. INFORMASI MODEL", style = "heading 2") %>%
        body_add_par(paste("Variabel dependen:", input$reg_y), style = "Normal") %>%
        body_add_par(paste("Variabel independen:", paste(input$reg_x, collapse = ", ")), style = "Normal") %>%
        body_add_par(paste("Interaksi:", if(input$include_interaction) "Ya" else "Tidak"), style = "Normal") %>%
        body_add_par(paste("Tingkat signifikansi:", input$alpha_reg), style = "Normal")
      
      result <- regression_model()
      if(!is.null(result) && !"error" %in% names(result)) {
        doc <- doc %>%
          body_add_par("", style = "Normal") %>%
          body_add_par("2. RINGKASAN MODEL", style = "heading 2")
        
        model_output <- capture.output(summary(result$model))
        for(line in model_output) {
          doc <- doc %>% body_add_par(line, style = "Normal")
        }
      }
      
      print(doc, target = file)
    }
  )
  
  output$download_regression_plots <- downloadHandler(
    filename = function() {
      paste0("plot_diagnostik_regresi_", input$reg_y, "_", Sys.Date(), ".jpg")
    },
    content = function(file) {
      result <- regression_model()
      if(!is.null(result) && !"error" %in% names(result)) {
        jpeg(file, width = 1200, height = 800, quality = 95)
        par(mfrow = c(2, 2))
        plot(result$model)
        par(mfrow = c(1, 1))
        dev.off()
      }
    }
  )
  
  output$download_regresi_full <- downloadHandler(
    filename = function() {
      paste0("laporan_lengkap_regresi_", Sys.Date(), ".docx")
    },
    content = function(file) {
      doc <- read_docx()
      doc <- doc %>%
        body_add_par("LAPORAN LENGKAP REGRESI LINEAR BERGANDA", style = "heading 1") %>%
        body_add_par("SOVI-STAT EXPLORER", style = "heading 2") %>%
        body_add_par(paste("Tanggal:", Sys.Date()), style = "Normal") %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("1. RINGKASAN EKSEKUTIF", style = "heading 2") %>%
        body_add_par("Laporan ini menyajikan hasil analisis regresi linear berganda untuk mengidentifikasi faktor-faktor yang berpengaruh terhadap indikator kerentanan sosial.", style = "Normal") %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("2. METODOLOGI", style = "heading 2") %>%
        body_add_par("Regresi linear berganda digunakan untuk menganalisis hubungan antara satu variabel dependen dengan beberapa variabel independen.", style = "Normal") %>%
        body_add_par("Model diasumsikan memenuhi asumsi linearitas, independensi, homoskedastisitas, dan normalitas residual.", style = "Normal") %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("3. HASIL UTAMA", style = "heading 2")
      
      result <- regression_model()
      if(!is.null(result) && !"error" %in% names(result)) {
        model_summary <- summary(result$model)
        doc <- doc %>%
          body_add_par(paste("R-squared:", round(model_summary$r.squared, 4)), style = "Normal") %>%
          body_add_par(paste("Adjusted R-squared:", round(model_summary$adj.r.squared, 4)), style = "Normal") %>%
          body_add_par(paste("F-statistic:", round(model_summary$fstatistic[1], 4)), style = "Normal")
      }
      
      doc <- doc %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("4. KESIMPULAN", style = "heading 2") %>%
        body_add_par("Model regresi memberikan wawasan tentang faktor-faktor yang berkontribusi terhadap kerentanan sosial dan dapat digunakan untuk prediksi serta perumusan kebijakan.", style = "Normal")
      
      print(doc, target = file)
    }
  )
  
  # =====================================================================
  # MORAN'S I - VERSI PERBAIKAN REAKTIVITAS FINAL
  # =====================================================================
  
  # 1. BUAT REAKTIF UNTUK KALKULASI
  # eventReactive akan berjalan HANYA ketika input$run_moran di-klik
  moran_calculation <- eventReactive(input$run_moran, {
    
    # Pastikan variabel dari dropdown ada
    req(input$map_var)
    
    # Ambil data dari kolom yang TEPAT
    var_to_test <- sovi_data[[input$map_var]]
    
    # Dapatkan indeks baris yang valid (bukan NA)
    valid_indices <- which(!is.na(var_to_test))
    
    # Cek jika data cukup
    if (length(valid_indices) < 2) {
      return(list(error = "Data tidak mencukupi untuk analisis (kurang dari 2 nilai valid)."))
    }
    
    # Filter variabel yang akan diuji
    var_filtered <- var_to_test[valid_indices]
    
    # Buat ulang matriks bobot yang sudah difilter
    dist_matrix_filtered <- dist_matrix_raw[valid_indices, valid_indices]
    inv_dist_filtered <- 1 / (dist_matrix_filtered + 1e-9)
    diag(inv_dist_filtered) <- 0
    weights_for_test <- mat2listw(inv_dist_filtered, style = "W")
    
    # Jalankan uji dan kembalikan hasilnya
    tryCatch({
      moran.test(var_filtered, listw = weights_for_test, na.action = na.fail)
    }, error = function(e) {
      list(error = paste("Gagal menjalankan Uji Moran's I:", e$message))
    })
  })
  
  # 2. RENDER OUTPUT TEKS BERDASARKAN HASIL REAKTIF
  output$moran_test_result <- renderPrint({
    # Panggil hasil kalkulasi reaktif
    result <- moran_calculation()
    
    # Jika ada error, tampilkan, jika tidak, print hasilnya
    if (!is.null(result$error)) {
      result$error
    } else {
      print(result)
    }
  })
  
  # 3. RENDER INTERPRETASI BERDASARKAN HASIL REAKTIF
  output$moran_interpretation <- renderText({
    # Panggil hasil kalkulasi reaktif
    result <- moran_calculation()
    
    # Jangan tampilkan apa-apa jika ada error
    if (!is.null(result$error)) {
      return("")
    }
    
    # Buat interpretasi
    p_val <- result$p.value
    moran_I <- result$estimate[1]
    
    if (p_val < 0.05) {
      if (moran_I > 0) {
        paste("Hasilnya signifikan (p < 0.05). Nilai Moran's I positif (", round(moran_I, 4), ") menunjukkan adanya pola MENGELOMPOK (clustered). Wilayah dengan nilai serupa (tinggi-tinggi atau rendah-rendah) cenderung berdekatan.")
      } else {
        paste("Hasilnya signifikan (p < 0.05). Nilai Moran's I negatif (", round(moran_I, 4), ") menunjukkan adanya pola MENYEBAR (dispersed). Wilayah dengan nilai tinggi cenderung berdekatan dengan wilayah bernilai rendah (pola seperti papan catur).")
      }
    } else {
      paste("Hasilnya tidak signifikan (p >= 0.05). Tidak ada cukup bukti adanya pola spasial. Distribusi nilai variabel di seluruh wilayah cenderung ACAK (random).")
    }
  })

  # Download handler untuk interpretasi peta
  output$download_map_interpretation <- downloadHandler(
    filename = function() {
      paste0("interpretasi_peta_", input$map_var, "_", Sys.Date(), ".docx")
    },
    content = function(file) {
      req(input$map_var, map_data())
      map_data_filtered <- map_data()
      interpretation <- if(is.null(map_data_filtered) || nrow(map_data_filtered) == 0) {
        "Tidak ada data yang tersedia untuk interpretasi peta."
      } else {
        var_values <- map_data_filtered[[input$map_var]]
        mean_val <- mean(var_values, na.rm = TRUE)
        high_regions <- sum(var_values > mean_val, na.rm = TRUE)
        low_regions <- sum(var_values <= mean_val, na.rm = TRUE)
        q95 <- quantile(var_values, 0.95, na.rm = TRUE)
        q05 <- quantile(var_values, 0.05, na.rm = TRUE)
        extreme_high <- sum(var_values >= q95, na.rm = TRUE)
        extreme_low <- sum(var_values <= q05, na.rm = TRUE)
        paste0("Peta interaktif menampilkan distribusi spasial variabel ", input$map_var, " di seluruh wilayah Indonesia. ",
               "Dari ", length(var_values), " wilayah yang dianalisis, ", high_regions, " wilayah memiliki nilai di atas rata-rata (", round(mean_val, 3), "), ",
               "sedangkan ", low_regions, " wilayah memiliki nilai di bawah rata-rata. ",
               "Terdapat ", extreme_high, " wilayah dengan nilai sangat tinggi (percentile 95+) dan ", extreme_low, " wilayah dengan nilai sangat rendah (percentile 5-). ",
               "Pola spasial ini mengindikasikan adanya variasi kerentanan sosial yang signifikan antar wilayah, ",
               "yang dapat digunakan untuk mengidentifikasi area prioritas dalam program pengurangan kerentanan sosial.")
      }
      doc <- officer::read_docx() %>%
        officer::body_add_par("INTERPRETASI PETA SOVI", style = "heading 1") %>%
        officer::body_add_par(interpretation, style = "Normal")
      print(doc, target = file)
    }
  )

  # Download handler untuk statistik peta
  output$download_map_stats <- downloadHandler(
    filename = function() {
      paste0("statistik_peta_", input$map_var, "_", Sys.Date(), ".docx")
    },
    content = function(file) {
      req(input$map_var, map_data())
      map_data_filtered <- map_data()
      interpretation <- if(is.null(map_data_filtered) || nrow(map_data_filtered) == 0) {
        "Tidak ada data yang tersedia untuk interpretasi peta."
      } else {
        var_values <- map_data_filtered[[input$map_var]]
        mean_val <- mean(var_values, na.rm = TRUE)
        high_regions <- sum(var_values > mean_val, na.rm = TRUE)
        low_regions <- sum(var_values <= mean_val, na.rm = TRUE)
        q95 <- quantile(var_values, 0.95, na.rm = TRUE)
        q05 <- quantile(var_values, 0.05, na.rm = TRUE)
        extreme_high <- sum(var_values >= q95, na.rm = TRUE)
        extreme_low <- sum(var_values <= q05, na.rm = TRUE)
        paste0("Peta interaktif menampilkan distribusi spasial variabel ", input$map_var, " di seluruh wilayah Indonesia. ",
               "Dari ", length(var_values), " wilayah yang dianalisis, ", high_regions, " wilayah memiliki nilai di atas rata-rata (", round(mean_val, 3), "), ",
               "sedangkan ", low_regions, " wilayah memiliki nilai di bawah rata-rata. ",
               "Terdapat ", extreme_high, " wilayah dengan nilai sangat tinggi (percentile 95+) dan ", extreme_low, " wilayah dengan nilai sangat rendah (percentile 5-). ",
               "Pola spasial ini mengindikasikan adanya variasi kerentanan sosial yang signifikan antar wilayah, ",
               "yang dapat digunakan untuk mengidentifikasi area prioritas dalam program pengurangan kerentanan sosial.")
      }
      doc <- officer::read_docx() %>%
        officer::body_add_par("STATISTIK PETA SOVI", style = "heading 1") %>%
        officer::body_add_par(interpretation, style = "Normal")
      print(doc, target = file)
    }
  )

  # 1. Moran's I
  downloadHandler(
    filename = function() paste0("hasil_moran_", input$map_var, "_", Sys.Date(), ".docx"),
    content = function(file) {
      result <- moran_calculation()
      doc <- officer::read_docx() %>%
        officer::body_add_par("HASIL UJI MORAN'S I", style = "heading 1") %>%
        officer::body_add_par(capture.output(print(result)), style = "Normal")
      print(doc, target = file)
    }
  )
  # Plot Moran's I
  downloadHandler(
    filename = function() paste0("plot_moran_", input$map_var, "_", Sys.Date(), ".jpg"),
    content = function(file) {
      result <- moran_calculation()
      jpeg(file, width = 800, height = 600, quality = 95)
      if (!is.null(result$estimate)) {
        barplot(result$res, main = "Moran's I Plot", col = "skyblue")
      } else {
        plot.new(); text(0.5, 0.5, "Tidak ada plot.")
      }
      dev.off()
    }
  )
  # Interpretasi Moran's I
  downloadHandler(
    filename = function() paste0("interpretasi_moran_", input$map_var, "_", Sys.Date(), ".docx"),
    content = function(file) {
      result <- moran_calculation()
      doc <- officer::read_docx() %>%
        officer::body_add_par("INTERPRETASI MORAN'S I", style = "heading 1") %>%
        officer::body_add_par(output$moran_interpretation(), style = "Normal")
      print(doc, target = file)
    }
  )
  # 2. Uji Asumsi (Normalitas, Homogenitas)
  downloadHandler(
    filename = function() paste0("plot_normalitas_", input$assumption_var, "_", Sys.Date(), ".jpg"),
    content = function(file) {
      jpeg(file, width = 800, height = 600, quality = 95)
      print(output$normality_plot())
      dev.off()
    }
  )
  downloadHandler(
    filename = function() paste0("interpretasi_normalitas_", input$assumption_var, "_", Sys.Date(), ".docx"),
    content = function(file) {
      doc <- officer::read_docx() %>%
        officer::body_add_par("INTERPRETASI UJI NORMALITAS", style = "heading 1") %>%
        officer::body_add_par(output$normality_interpretation(), style = "Normal")
      print(doc, target = file)
    }
  )
  downloadHandler(
    filename = function() paste0("plot_homogenitas_", input$assumption_var, "_", Sys.Date(), ".jpg"),
    content = function(file) {
      jpeg(file, width = 800, height = 600, quality = 95)
      print(output$homogeneity_plot())
      dev.off()
    }
  )
  downloadHandler(
    filename = function() paste0("interpretasi_homogenitas_", input$assumption_var, "_", Sys.Date(), ".docx"),
    content = function(file) {
      doc <- officer::read_docx() %>%
        officer::body_add_par("INTERPRETASI UJI HOMOGENITAS", style = "heading 1") %>%
        officer::body_add_par(output$homogeneity_interpretation(), style = "Normal")
      print(doc, target = file)
    }
  )
  # 3. Inferensia (T-Test, ANOVA, Proporsi/Varian)
  downloadHandler(
    filename = function() paste0("plot_ttest_", input$t_test_var, "_", Sys.Date(), ".jpg"),
    content = function(file) {
      jpeg(file, width = 800, height = 600, quality = 95)
      print(output$t_test_plot())
      dev.off()
    }
  )
  downloadHandler(
    filename = function() paste0("interpretasi_ttest_", input$t_test_var, "_", Sys.Date(), ".docx"),
    content = function(file) {
      doc <- officer::read_docx() %>%
        officer::body_add_par("INTERPRETASI UJI T-TEST", style = "heading 1") %>%
        officer::body_add_par(output$t_test_interpretation(), style = "Normal")
      print(doc, target = file)
    }
  )
  downloadHandler(
    filename = function() paste0("plot_propvar_", input$var_test_var, "_", Sys.Date(), ".jpg"),
    content = function(file) {
      jpeg(file, width = 800, height = 600, quality = 95)
      print(output$prop_var_plot())
      dev.off()
    }
  )
  downloadHandler(
    filename = function() paste0("interpretasi_propvar_", input$var_test_var, "_", Sys.Date(), ".docx"),
    content = function(file) {
      doc <- officer::read_docx() %>%
        officer::body_add_par("INTERPRETASI UJI RAGAM", style = "heading 1") %>%
        officer::body_add_par(output$prop_var_interpretation(), style = "Normal")
      print(doc, target = file)
    }
  )
  downloadHandler(
    filename = function() paste0("plot_anova_", input$anova_y, "_", Sys.Date(), ".jpg"),
    content = function(file) {
      jpeg(file, width = 800, height = 600, quality = 95)
      print(output$anova_plot())
      dev.off()
    }
  )
  downloadHandler(
    filename = function() paste0("interpretasi_anova_", input$anova_y, "_", Sys.Date(), ".docx"),
    content = function(file) {
      doc <- officer::read_docx() %>%
        officer::body_add_par("INTERPRETASI ANOVA", style = "heading 1") %>%
        officer::body_add_par(output$anova_interpretation(), style = "Normal")
      print(doc, target = file)
    }
  )
  # 4. Regresi
  downloadHandler(
    filename = function() paste0("plot_regresi_", input$reg_y, "_", Sys.Date(), ".jpg"),
    content = function(file) {
      jpeg(file, width = 800, height = 600, quality = 95)
      print(output$regression_scatter_plot())
      dev.off()
    }
  )
  downloadHandler(
    filename = function() paste0("interpretasi_plot_regresi_", input$reg_y, "_", Sys.Date(), ".docx"),
    content = function(file) {
      doc <- officer::read_docx() %>%
        officer::body_add_par("INTERPRETASI PLOT REGRESI", style = "heading 1") %>%
        officer::body_add_par(output$regression_plot_interpretation(), style = "Normal")
      print(doc, target = file)
    }
  )
  downloadHandler(
    filename = function() paste0("plot_diag_regresi_", input$reg_y, "_", Sys.Date(), ".jpg"),
    content = function(file) {
      jpeg(file, width = 800, height = 600, quality = 95)
      print(output$regression_plots())
      dev.off()
    }
  )
  downloadHandler(
    filename = function() paste0("interpretasi_regresi_", input$reg_y, "_", Sys.Date(), ".docx"),
    content = function(file) {
      doc <- officer::read_docx() %>%
        officer::body_add_par("INTERPRETASI MODEL REGRESI", style = "heading 1") %>%
        officer::body_add_par(output$regression_interpretation(), style = "Normal")
      print(doc, target = file)
    }
  )
}

# ===============================================================================
# 5. MENJALANKAN APLIKASI
# ===============================================================================

# Jalankan aplikasi Shiny
shinyApp(ui = ui, server = server)

# ===============================================================================
# CATATAN PENGGUNAAN:
# ===============================================================================
# 1. Pastikan semua library telah terinstal
# 2. Siapkan file data:
#    - sovi_data.csv: Data utama SOVI dengan struktur yang sesuai
#    - distance.csv: Matriks jarak antar wilayah
# 3. Letakkan file data dalam folder "data/" di direktori aplikasi
# 4. Jalankan aplikasi dengan: shiny::runApp()
# 
# STRUKTUR FOLDER:
# project_folder/
# ├── app.R (file ini)
# └── data/
#     ├── sovi_data.csv
#     └── distance.csv
#
# FITUR UTAMA YANG TELAH DIPERBAIKI:
# - Integrasi matriks jarak untuk analisis spasial
# - Penghapusan variabel artifisial yang tidak ada di data asli
# - Interpretasi yang lebih spesifik dan dinamis
# - Analisis klaster menggantikan peta yang tidak berfungsi
# - Sistem manajemen variabel kategori yang dinamis
# - Validasi data yang lebih baik
# - Export laporan Word yang komprehensif
# 
# ANALISIS YANG TERSEDIA:
# 1. Manajemen Data: Transformasi dan kategorisasi variabel
# 2. Statistik Deskriptif: Ringkasan dan korelasi
# 3. Visualisasi: Plot interaktif dengan interpretasi
# 4. Analisis Spasial: Clustering dan Moran's I menggunakan matriks jarak
# 5. Uji Asumsi: Normalitas dan homogenitas
# 6. Uji Inferensia: T-test, ANOVA, uji ragam
# 7. Regresi: Model linear berganda dengan diagnostik
# 
# KEUNGGULAN APLIKASI:
# - Kompatibel dengan struktur data SOVI yang sebenarnya
# - Menggunakan matriks jarak yang disediakan
# - Interpretasi kontekstual untuk setiap analisis
# - Interface yang user-friendly dengan tema modern
# - Export otomatis ke format Word dan gambar
# - Validasi input yang komprehensif
# - Sistem peringatan untuk data dummy vs real
# 
# TROUBLESHOOTING:
# - Jika data tidak ditemukan, aplikasi akan menggunakan data dummy
# - Pastikan struktur data sesuai dengan metadata yang didefinisikan
# - Untuk analisis spasial, pastikan matriks jarak tersedia
# - Variabel kategori harus dibuat melalui menu Manajemen Data
# 
# PENGEMBANGAN LANJUTAN:
# - Tambahkan metode clustering lainnya (DBSCAN, Gaussian Mixture)
# - Implementasikan analisis spasial lanjutan (Local Moran's I, Geary's C)
# - Tambahkan validasi data yang lebih komprehensif
# - Integrasikan dengan database eksternal
# - Tambahkan fitur machine learning untuk prediksi
# ===============================================================================