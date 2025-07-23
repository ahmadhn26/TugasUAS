# ===============================================================================
# DASHBOARD ANALITIKA SOVI NUSANTARA - VERSI DIPERBAIKI + SLM
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
library(spatialreg)




# ===============================================================================
# 2. PEMUATAN DATA BERSIH (CLEAN DATA LOADING)
# ===============================================================================

# --- Blok tryCatch untuk memuat data berundsih atau data dummy jika gagal ---
data_load_result <- tryCatch({
  
  # 1. Muat data spasial master yang sudah bersih (479 baris)
  spatial_data <- st_read("data/data_master_clean2.gpkg", quiet = TRUE)
  
  # 2. Muat matriks jarak yang sudah bersih (479x479)
  dist_matrix_raw <- readRDS("data/distance_matrix_clean1.rds")
  
  # 3. Buat versi non-spasial untuk analisis statistik (tabel biasa)
  sovi_data <- st_drop_geometry(spatial_data)
  
  # 4. Beri pesan sukses
  list(
    spatial_data = spatial_data,
    sovi_data = sovi_data,
    dist_matrix_raw = dist_matrix_raw,
    status = "clean",
    message = "Data bersih yang telah disinkronkan berhasil dimuat."
  )
  
}, error = function(e) {
  # Jika file bersih tidak ditemukan, buat data dummy untuk demonstrasi
  warning("File data bersih tidak ditemukan. Memuat data dummy. Pesan error: ", e$message)
  set.seed(123)
  n <- 479 # Sesuaikan dengan jumlah data bersih
  
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
    SOVI_SCORE = round(runif(n, 0.1, 0.9), 2),
    WADMKK = paste("Kabupaten Dummy", 1:n),
    WADMPR = "Provinsi Dummy",
    KODE_BERSIH = 1101:(1100 + n),
    stringsAsFactors = FALSE
  )
  
  distance_dummy <- matrix(runif(n*n, 0, 1000), nrow = n, ncol = n)
  diag(distance_dummy) <- 0
  
  list(
    spatial_data = NULL, # Tidak ada data spasial untuk mode dummy
    sovi_data = sovi_dummy,
    dist_matrix_raw = distance_dummy,
    status = "dummy",
    message = "File data bersih tidak ditemukan. Menggunakan data dummy untuk demonstrasi."
  )
})

# --- Menetapkan variabel final dari hasil pemuatan data ---
spatial_data    <- data_load_result$spatial_data
sovi_data       <- data_load_result$sovi_data
dist_matrix_raw <- data_load_result$dist_matrix_raw
data_result     <- list(status = data_load_result$status, message = data_load_result$message)

# Buat daftar pilihan peta dari data spasial yang SUDAH dimuat
# Pastikan data spasial ada sebelum membuat pilihan
if (!is.null(spatial_data)) {
  pilihan_peta_numerik <- names(spatial_data)[sapply(st_drop_geometry(spatial_data), is.numeric)]
} else {
  pilihan_peta_numerik <- names(sovi_data)[sapply(sovi_data, is.numeric)]
}

# Membuat matriks bobot spasial dari matriks jarak yang sudah bersih
# Ini akan digunakan untuk Moran's I
inv_dist_matrix <- 1 / (dist_matrix_raw + 1e-9)
diag(inv_dist_matrix) <- 0
spatial_weights <- mat2listw(inv_dist_matrix, style = "W")




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
               menuSubItem("Analisis Spasial", tabName = "spatial", icon = icon("project-diagram"))  
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
        style = if(data_result$status == "dummy" || data_load_result$status == "dummy") { # Sedikit modifikasi untuk dummy mode
          "background: linear-gradient(135deg, #ffeaa7 0%, #fab1a0 100%); border: 2px solid #fdcb6e; padding: 15px; margin: 15px; border-radius: 10px; color: #2d3436;"
        } else {
          "background: linear-gradient(135deg, #00b894 0%, #00cec9 100%); border: 2px solid #00b894; padding: 15px; margin: 15px; border-radius: 10px; color: white;"
        },
        icon(if(data_result$status == "dummy" || data_load_result$status == "dummy") "exclamation-triangle" else "check-circle", class = "fa-2x"),
        h4(style = "margin: 10px 0;", if(data_result$status == "dummy" || data_load_result$status == "dummy") "Mode Demonstrasi" else "Data Berhasil Dimuat"),
        p(data_result$message, style = "margin: 0; font-size: 14px;")
        # Dua baris yang merujuk ke 'spatial_result' sudah dihapus
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
              
              # --- BARIS 1: PETA INTERAKTIF ---
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
                  leafletOutput("sovi_map", height = "600px")
                )
              ),
              
              # --- BARIS 2: ANALISIS MORAN'S I ---
              fluidRow(
                box(
                  title = "🔬 Analisis Autokorelasi Spasial (Moran's I)",
                  status = "success", solidHeader = TRUE, width = 12, class = "custom-box",
                  
                  actionButton("run_moran", "Jalankan Uji Moran's I", icon = icon("calculator"), class = "btn-success"),
                  hr(),
                  verbatimTextOutput("moran_test_result"),
                  div(class = "interpretation-box",
                      h4("📊 Interpretasi Hasil Uji Moran's I"),
                      textOutput("moran_interpretation")
                  ),
                  
                  # Tombol Download untuk Moran's I
                  div(class = "download-section",
                      downloadButton("download_moran_report", "Download Laporan Moran's I (Word)", class = "btn-primary")
                  )
                )
              ),
              
              # --- BARIS 3: ANALISIS CLUSTERING ---
              fluidRow(
                box(
                  title = "🔬 Analisis Clustering (K-Means)",
                  status = "purple", solidHeader = TRUE, width = 12, class = "custom-box",
                  
                  column(
                    width = 4,
                    h4("Pengaturan Clustering", style = "margin-top: 0;"),
                    selectizeInput("cluster_vars", "Pilih Variabel untuk Clustering:", choices = NULL, multiple = TRUE),
                    numericInput("cluster_k", "Jumlah Klaster (K):", value = 4, min = 2, max = 10),
                    actionButton("run_cluster", "🚀 Jalankan Analisis Klaster", icon = icon("cogs"), class = "btn-success"),
                    div(class = "interpretation-box", style = "margin-top: 20px;",
                        h4("💡 Interpretasi Klaster"),
                        textOutput("cluster_interpretation_text")
                    )
                  ),
                  
                  column(
                    width = 8,
                    h4("Peta Hasil Clustering", style = "margin-top: 0;"),
                    leafletOutput("cluster_map", height = "550px"),
                    
                    br(),
                    h4("Ringkasan Karakteristik Klaster"),
                    DT::dataTableOutput("cluster_summary_table"),
                    
                    # Tombol Download untuk Clustering
                    div(class = "download-section",
                        h5("Download Hasil Clustering", style="margin-top:0;"),
                        downloadButton("download_cluster_map_jpg", "Peta Klaster (JPG)", class="btn-warning"),
                        downloadButton("download_cluster_report", "Laporan Klaster (Word)", class="btn-primary")
                    )
                  )
                )
              ),
              
              # --- [PENAMBAHAN SLM] --- BARIS 4: ANALISIS SPATIAL LAG MODEL (SLM) ---
              fluidRow(
                box(
                  title = "📈 Analisis Spatial Lag Model (SLM)",
                  status = "danger", solidHeader = TRUE, width = 12, class = "custom-box",
                  
                  # --- Kolom Kiri untuk Input ---
                  column(
                    width = 4,
                    h4("Pengaturan Model SLM"),
                    p("Gunakan model ini untuk melihat pengaruh variabel independen (X) terhadap dependen (Y) dengan memperhitungkan efek spasial."),
                    
                    selectInput("slm_y", "Pilih Variabel Dependen (Y):",
                                choices = pilihan_peta_numerik,
                                selected = "POVERTY"),
                    
                    selectizeInput("slm_x", "Pilih Variabel Independen (X):",
                                   choices = pilihan_peta_numerik,
                                   selected = c("LOWEDU", "NOELECTRIC"),
                                   multiple = TRUE),
                    
                    actionButton("run_slm", "Jalankan Model SLM", icon = icon("rocket"), class = "btn-danger"),
                    
                    br(),br(),
                    div(class = "download-section",
                        downloadButton("download_slm_report", "Download Laporan SLM (Word)", class = "btn-primary")
                    )
                  ),
                  
                  # --- Kolom Kanan untuk Output ---
                  column(
                    width = 8,
                    h4("Ringkasan Hasil Model SLM"),
                    verbatimTextOutput("slm_summary"),
                    
                    div(class = "interpretation-box",
                        h4("🔍 Interpretasi Hasil SLM"),
                        textOutput("slm_interpretation")
                    )
                  )
                )
              ),
              
              # --- BARIS 4: DOWNLOAD GABUNGAN ---
              fluidRow(
                box(
                  title = "📥 Download Laporan Gabungan",
                  status = "danger", solidHeader = TRUE, width = 12, class = "custom-box",
                  p("Download seluruh hasil analisis di halaman ini (Moran's I dan Clustering) dalam satu dokumen Word."),
                  downloadButton("download_spatial_full_report", "Download Laporan Lengkap Analisis Spasial (Word)", class = "btn-danger")
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
                  
                  selectInput("group_var", "Pilih Variabel Kelompok untuk Uji Homogenitas:",
                              choices = c("Tidak ada" = "none")),
                  
                  numericInput("alpha_level", "Tingkat Signifikansi (α):",
                               value = 0.05, min = 0.01, max = 0.1, step = 0.01),
                  br(),
                  
                  # --- BAGIAN DOWNLOAD BARU ---
                  div(
                    class = "download-section",
                    h5("📥 Download Hasil Individual", style = "margin-top: 0;"),
                    
                    # Tombol Download Plot
                    downloadButton("download_assumption_plots", "Plot (PNG)", class = "btn-warning"),
                    
                    # Tombol Download Laporan Teks
                    downloadButton("download_assumptions", "Laporan Teks (Word)", class = "btn-primary")
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
                  status = "primary", solidHeader = TRUE, width = 4, class = "custom-box",
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
                                choices = c("Tidak ada" = "none"))
                  ),
                  numericInput("alpha_t", "Tingkat Signifikansi (α):", 
                               value = 0.05, min = 0.01, max = 0.1, step = 0.01),
                  checkboxInput("equal_var", "Asumsi Ragam Sama", value = TRUE)
                ),
                
                box(
                  title = "📊 Hasil Uji Beda Rata-rata", 
                  status = "info", solidHeader = TRUE, width = 8, class = "custom-box",
                  verbatimTextOutput("t_test_result"),
                  div(class = "interpretation-box",
                      h4("📈 Interpretasi Hasil Uji T"),
                      textOutput("t_test_interpretation")
                  ),
                  # Tombol download individual
                  div(class="download-section",
                      downloadButton("download_ttest_report", "Laporan Teks (Word)", class="btn-primary"),
                      downloadButton("download_ttest_plot", "Plot (PNG)", class="btn-warning")
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "📋 Ringkasan Statistik & Laporan Lengkap", 
                  status = "success", solidHeader = TRUE, width = 12, class = "custom-box",
                  DT::dataTableOutput("group_summary_table"),
                  br(),
                  downloadButton("download_uji_rata_full", "📋 Download Laporan Lengkap (Word)", class = "btn-success", icon = icon("file-word"))
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
                  status = "primary", solidHeader = TRUE, width = 4, class = "custom-box",
                  
                  # Kontrol input Anda ada di sini (tidak berubah)
                  selectInput("test_type_pv", "Jenis Uji:",
                              choices = c("Uji Ragam 1 Kelompok" = "var_one", "Uji Ragam 2 Kelompok" = "var_two",
                                          "Uji Proporsi 1 Kelompok" = "prop_one", "Uji Proporsi 2 Kelompok (Chi-Square)" = "prop_two")),
                  conditionalPanel(
                    condition = "input.test_type_pv == 'var_one' || input.test_type_pv == 'var_two'",
                    selectInput("var_test_var", "Variabel Numerik (untuk Uji Ragam):", choices = names(select_if(sovi_data, is.numeric))),
                    conditionalPanel("input.test_type_pv == 'var_one'", numericInput("var_null", "Ragam H₀:", value = 1, min = 0.01)),
                    conditionalPanel("input.test_type_pv == 'var_two'", selectInput("group_var_var", "Variabel Kelompok:", choices = c("Tidak ada" = "none")))
                  ),
                  conditionalPanel(
                    condition = "input.test_type_pv == 'prop_one' || input.test_type_pv == 'prop_two'",
                    selectInput("prop_var_categorical", "Variabel Kategorikal:", choices = c("Buat variabel di Man. Data" = "")),
                    conditionalPanel("input.test_type_pv == 'prop_one'",
                                     selectInput("prop_success_level", "Pilih Kategori 'Sukses':", choices = c("Pilih variabel dulu" = "")),
                                     numericInput("prop_null_value", "Proporsi H₀ (0-1):", value = 0.5, min = 0, max = 1, step = 0.01)),
                    conditionalPanel("input.test_type_pv == 'prop_two'", selectInput("prop_group_var", "Variabel Kelompok:", choices = c("Buat variabel di Man. Data" = "")))
                  ),
                  numericInput("alpha_pv", "Tingkat Signifikansi (α):", value = 0.05, min = 0.01, max = 0.1, step = 0.01)
                ),
                
                box(
                  title = "📊 Hasil Uji & Plot", 
                  status = "info", solidHeader = TRUE, width = 8, class = "custom-box",
                  verbatimTextOutput("prop_var_result"),
                  plotOutput("prop_var_plot", height = "300px"),
                  div(class = "interpretation-box",
                      h4("📊 Interpretasi Hasil"),
                      textOutput("prop_var_interpretation")
                  ),
                  # Tombol download individual yang BENAR
                  div(class="download-section",
                      downloadButton("download_prop_var_report", "Laporan Teks (Word)", class="btn-primary"),
                      downloadButton("download_prop_var_plot", "Plot (PNG)", class="btn-warning")
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "📋 Ringkasan Statistik & Laporan Lengkap", 
                  status = "warning", solidHeader = TRUE, width = 12, class = "custom-box",
                  DT::dataTableOutput("prop_var_summary_table"),
                  br(),
                  downloadButton("download_prop_var_full", "📋 Download Laporan Lengkap (Word)", class = "btn-success", icon = icon("file-word"))
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
                  status = "primary", solidHeader = TRUE, width = 12, class = "custom-box",
                  
                  column(width = 4,
                         selectInput("anova_type", "Jenis ANOVA:",
                                     choices = c("One-Way ANOVA (1 Arah)" = "one_way",
                                                 "Two-Way ANOVA (2 Arah)" = "two_way"))
                  ),
                  column(width = 4,
                         selectInput("anova_y", "Variabel Dependen (Y):",
                                     choices = names(select_if(sovi_data, is.numeric))),
                         selectInput("anova_x1", "Faktor 1 (X1):",
                                     choices = c("Tidak ada" = "none"))
                  ),
                  column(width = 4,
                         conditionalPanel(
                           condition = "input.anova_type == 'two_way'",
                           selectInput("anova_x2", "Faktor 2 (X2):",
                                       choices = c("Tidak ada" = "none"))
                         ),
                         numericInput("alpha_anova", "Tingkat Signifikansi (α):", 
                                      value = 0.05, min = 0.01, max = 0.1, step = 0.01)
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "📊 Hasil Analisis ANOVA & Uji Lanjut",
                  status = "info", solidHeader = TRUE, width = 12, class = "custom-box",
                  
                  column(width = 7,
                         h4("Tabel ANOVA"),
                         verbatimTextOutput("anova_result"),
                         div(class = "interpretation-box",
                             h4("📈 Interpretasi Hasil ANOVA"),
                             textOutput("anova_interpretation")
                         )
                  ),
                  column(width = 5,
                         h4("Uji Lanjut (Post-Hoc)"),
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
                  ),
                  
                  # Tombol download untuk laporan teks
                  column(width=12,
                         div(class = "download-section", style="margin-top: 15px;",
                             downloadButton("download_anova_report", "Download Laporan Teks (Word)", class="btn-primary")
                         )
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "📊 Visualisasi & Laporan Lengkap",
                  status = "purple", solidHeader = TRUE, width = 12, class = "custom-box",
                  plotOutput("anova_plot", height = "400px"),
                  br(),
                  # Tombol download untuk plot dan laporan lengkap
                  div(class = "download-section",
                      downloadButton("download_anova_plot", "Download Plot (PNG)", class="btn-warning"),
                      downloadButton("download_anova_full", "📋 Download Laporan Lengkap (Word)", class = "btn-success", icon = icon("file-word"))
                  )
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
                  )
                ),
                
                box(
                  title = "📊 Hasil Regresi Linear Berganda", 
                  status = "info", solidHeader = TRUE, width = 8, class = "custom-box",
                  verbatimTextOutput("regression_summary"),
                  div(class = "interpretation-box",
                      h4("📈 Interpretasi Model Regresi"),
                      textOutput("regression_interpretation")
                  ),
                  # Tombol download untuk ringkasan model
                  div(class = "download-section",
                      downloadButton("download_summary_report", "Download Ringkasan Model (Word)", class="btn-primary")
                  )
                )
              ),
              
              
              fluidRow(
                box(
                  title = "📈 Plot Regresi (Actual vs. Predicted)", 
                  status = "success", solidHeader = TRUE, width = 6, class = "custom-box",
                  plotOutput("regression_scatter_plot", height = "400px"),
                  div(class = "interpretation-box",
                      h4("📊 Interpretasi Plot Regresi"),
                      textOutput("regression_plot_interpretation")
                  ),
                  # Tombol download untuk plot regresi
                  div(class = "download-section",
                      downloadButton("download_scatter_plot_jpg", "Download Plot (JPG)", class="btn-warning")
                  )
                ),
                
                box(
                  title = "📊 Plot Diagnostik Regresi", 
                  status = "warning", solidHeader = TRUE, width = 6, class = "custom-box",
                  plotOutput("regression_plots", height = "400px"),
                  # Tombol download untuk plot diagnostik
                  div(class = "download-section",
                      downloadButton("download_regression_plots", "Download Plot Diagnostik (JPG)", class="btn-warning")
                  )
                )
              ),
              
              
              fluidRow(
                box(
                  title = "🔍 Uji Asumsi Regresi", 
                  status = "warning", solidHeader = TRUE, width = 12, class = "custom-box",
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
                  ),
                  # Tombol download untuk laporan asumsi
                  div(class = "download-section",
                      downloadButton("download_assumption_report", "Download Laporan Asumsi (Word)", class="btn-primary")
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "📋 Download Laporan Lengkap",
                  status = "danger", solidHeader = TRUE, width = 12, class = "custom-box",
                  p("Download seluruh hasil analisis di halaman ini dalam satu dokumen Word."),
                  downloadButton("download_regresi_full", "Download Laporan Lengkap Regresi (Word)", class="btn-danger")
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
  
  # LETAKKAN KODE INI SETELAH 'reactiveValues'
  # Observer untuk mengupdate pilihan input secara dinamis
  observe({
    # Dapatkan nama variabel kategori yang sudah dibuat di tab Manajemen Data
    cat_vars_available <- names(categorical_vars$data)
    if (is.null(cat_vars_available)) {
      cat_vars_available <- c("Buat variabel di Man. Data" = "")
    }
    
    # Update pilihan untuk variabel yang akan diuji proporsinya
    updateSelectInput(session, "prop_var_categorical", choices = cat_vars_available)
    
    # Update pilihan untuk variabel kelompok pada uji 2 proporsi
    updateSelectInput(session, "prop_group_var", choices = cat_vars_available)
  })
  
  # Observer untuk mengupdate level/kategori 'sukses' berdasarkan variabel yang dipilih
  observe({
    req(input$prop_var_categorical)
    
    # Dapatkan level unik dari variabel kategori yang dipilih
    var_data <- categorical_vars$data[[input$prop_var_categorical]]
    if (!is.null(var_data)) {
      selected_var_levels <- levels(as.factor(var_data))
      updateSelectInput(session, "prop_success_level", choices = selected_var_levels)
    }
  })
  
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

  
  # --- Objek Reaktif untuk Interpretasi Summary ---
  interpretation_summary_reactive <- reactive({
    req(input$var_numeric)
    var_data <- na.omit(sovi_data[[input$var_numeric]])
    validate(need(length(var_data) > 0, "Data tidak tersedia."))
    
    summary_stats <- summary(var_data)
    interpret_descriptive(summary_stats, input$var_numeric)
  })
  
  # --- Objek Reaktif untuk Interpretasi Frekuensi ---
  interpretation_frequency_reactive <- reactive({
    data <- transformed_data()
    validate(need(!is.null(data), "Data transformasi belum dibuat."))
    
    freq_table <- table(data$data$Kategori, useNA = "ifany")
    most_frequent <- names(freq_table)[which.max(freq_table)]
    least_frequent <- names(freq_table)[which.min(freq_table)]
    total_obs <- sum(freq_table)
    
    paste0("Berdasarkan kategorisasi menggunakan metode '", data$method, 
           "', kategori dengan frekuensi tertinggi adalah ", most_frequent, 
           " (", max(freq_table), " observasi), sedangkan yang terendah adalah ", least_frequent, 
           " (", min(freq_table), " observasi).")
  })
  
  # --- Render Teks di UI (sekarang hanya memanggil reactive) ---
  output$interpretation_summary <- renderText({
    interpretation_summary_reactive()
  })
  
  output$interpretation_frequency <- renderText({
    interpretation_frequency_reactive()
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
      paste0("laporan_manajemen_data_", input$var_numeric, "_", Sys.Date(), ".docx")
    },
    content = function(file) {
      shinyjs::show("loading_overlay"); on.exit(shinyjs::hide("loading_overlay"))
      
      # 1. Siapkan semua data yang dibutuhkan
      params <- list(
        Variabel = input$var_numeric,
        Metode = input$method_cat,
        Jumlah_Kategori = input$n_categories,
        Breakpoints = if (input$method_cat == 'manual') input$manual_breaks else "Otomatis"
      )
      
      summary_output <- capture.output(summary(sovi_data[[input$var_numeric]]))
      summary_interp <- interpretation_summary_reactive()
      
      freq_output <- capture.output(print(table(transformed_data()$data$Kategori)))
      freq_interp <- interpretation_frequency_reactive()
      
      comparison_data <- head(transformed_data()$data, 20)
      
      # 2. Susun Dokumen Word
      doc <- read_docx() %>%
        body_add_par("Laporan Manajemen & Transformasi Data", style = "heading 1") %>%
        body_add_par(paste("Tanggal Analisis:", Sys.Date())) %>%
        body_add_break() %>%
        
        body_add_par("1. Parameter Transformasi", style = "heading 2") %>%
        body_add_table(data.frame(Parameter = names(params), Nilai = unlist(params)), style = "Table Professional") %>%
        body_add_break() %>%
        
        body_add_par("2. Ringkasan Variabel Asli", style = "heading 2")
      for(line in summary_output) { doc <- doc %>% body_add_par(line) }
      doc <- doc %>% 
        body_add_par("Interpretasi:", style = "heading 3") %>%
        body_add_par(summary_interp) %>%
        body_add_break() %>%
        
        body_add_par("3. Ringkasan Variabel Hasil Transformasi", style = "heading 2")
      for(line in freq_output) { doc <- doc %>% body_add_par(line) }
      doc <- doc %>% 
        body_add_par("Interpretasi:", style = "heading 3") %>%
        body_add_par(freq_interp) %>%
        body_add_break() %>%
        
        body_add_par("4. Pratinjau Data Transformasi (20 Baris Pertama)", style = "heading 2") %>%
        body_add_table(comparison_data, style = "Table Professional")
      
      # 3. Simpan file
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
  # TAB UJI ASUMSI - (VERSI REAKTIF UNTUK DOWNLOAD)
  # =====================================================================
  
  # --- Objek Reaktif untuk Perhitungan ---
  normality_test_reactive <- reactive({
    req(input$assumption_var)
    var_data <- sovi_data[[input$assumption_var]]
    var_data <- na.omit(var_data)
    
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
  
  homogeneity_test_reactive <- reactive({
    req(input$assumption_var, input$group_var)
    
    if(input$group_var == "none" || !input$group_var %in% names(categorical_vars$data)) {
      return("Pilih variabel kelompok yang valid dari hasil kategorisasi.")
    }
    
    var_data <- sovi_data[[input$assumption_var]]
    group_data <- categorical_vars$data[[input$group_var]]
    
    complete_cases <- !is.na(var_data) & !is.na(group_data)
    var_data <- var_data[complete_cases]
    group_data <- group_data[complete_cases]
    
    if(length(unique(group_data)) < 2 || any(table(group_data) < 2)) {
      return("Setiap kelompok harus memiliki minimal 2 observasi.")
    }
    
    tryCatch({
      bartlett_test <- bartlett.test(var_data ~ group_data)
      levene_test <- leveneTest(var_data ~ group_data)
      list(
        "Bartlett Test" = bartlett_test,
        "Levene Test" = levene_test
      )
    }, error = function(e) {
      paste("Error dalam uji homogenitas:", e$message)
    })
  })
  
  # --- Objek Reaktif untuk Plot ---
  homogeneity_plot_object <- reactive({
    req(input$assumption_var, input$group_var != "none")
    
    var_data <- sovi_data[[input$assumption_var]]
    group_data <- categorical_vars$data[[input$group_var]]
    
    plot_data <- data.frame(Variable = var_data, Group = group_data) %>% na.omit()
    
    p1 <- ggplot(plot_data, aes(x = Group, y = Variable, fill = Group)) +
      geom_boxplot() +
      labs(title = paste("Boxplot", input$assumption_var), x = input$group_var) +
      theme_minimal()
    
    p2 <- ggplot(plot_data, aes(x = .data$Variable)) +
      geom_density(aes(fill = .data$Group), alpha = 0.5) +
      labs(title = "Distribusi per Kelompok") +
      theme_minimal()
    
    grid.arrange(p1, p2, ncol = 2)
  })
  
  
  # --- Objek Reaktif BARU untuk Plot Normalitas (versi ggplot2) ---
  normality_plots_object <- reactive({
    req(input$assumption_var)
    
    df <- data.frame(value = na.omit(sovi_data[[input$assumption_var]]))
    
    # Plot 1: Histogram dengan kurva densitas
    p1 <- ggplot(df, aes(x = value)) +
      geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "lightblue", color = "white", alpha = 0.7) +
      geom_density(color = "red", linewidth = 1) +
      labs(title = paste("Histogram", input$assumption_var)) +
      theme_minimal()
    
    # Plot 2: Q-Q Plot
    p2 <- ggplot(df, aes(sample = value)) +
      stat_qq() +
      stat_qq_line(color = "red", linewidth = 1) +
      labs(title = paste("Q-Q Plot", input$assumption_var)) +
      theme_minimal()
    
    # Gabungkan kedua plot
    grid.arrange(p1, p2, ncol = 2)
  })
  
  
  # --- Render Output untuk UI ---
  output$normality_test <- renderPrint({ normality_test_reactive() })
  output$homogeneity_test <- renderPrint({ homogeneity_test_reactive() })
  
  output$normality_plot <- renderPlot({
    normality_plots_object()
  })
  
  output$homogeneity_plot <- renderPlot({
    if(input$group_var != "none") {
      homogeneity_plot_object()
    }
  })
  
  # --- Objek Reaktif untuk Teks Interpretasi Normalitas ---
  normality_interpretation_reactive <- reactive({
    # Ambil hasil dari reactive test
    test_result_list <- normality_test_reactive()
    
    # Cek jika hasilnya adalah pesan error (string)
    if (is.character(test_result_list)) {
      return(test_result_list)
    }
    
    # Ekstrak hasil tes yang sebenarnya
    test_res <- test_result_list[[1]] # Ambil hasil (Shapiro/KS test)
    
    create_interpretation(test_res, "normalitas", alpha = input$alpha_level)
  })
  
  # --- Render Teks di UI ---
  output$normality_interpretation <- renderText({
    normality_interpretation_reactive()
  })
  
  
  # --- Objek Reaktif untuk Teks Interpretasi Homogenitas ---
  homogeneity_interpretation_reactive <- reactive({
    req(input$group_var != "none") # Hanya berjalan jika ada grup
    
    test_result_list <- homogeneity_test_reactive()
    
    if (is.character(test_result_list)) {
      return(test_result_list)
    }
    
    # Gunakan Bartlett test sebagai acuan utama untuk interpretasi
    bartlett_test <- test_result_list[["Bartlett Test"]]
    
    create_interpretation(bartlett_test, "homogenitas", alpha = input$alpha_level)
  })
  
  # --- Render Teks di UI ---
  output$homogeneity_interpretation <- renderText({
    if (input$group_var != "none") {
      homogeneity_interpretation_reactive()
    } else {
      "Pilih variabel kelompok untuk melihat interpretasi."
    }
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

  
  # =====================================================================
  # DOWNLOAD HANDLERS UNTUK UJI ASUMSI (VERSI FINAL)
  # =====================================================================
  
  # --- Download Plot (PNG) ---
  # --- Download Plot (PNG) ---
  output$download_assumption_plots <- downloadHandler(
    filename = function() {
      paste0("plot_uji_asumsi_", input$assumption_var, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      shinyjs::show("loading_overlay")
      on.exit(shinyjs::hide("loading_overlay"))
      
      # Siapkan plot normalitas
      norm_plots <- normality_plots_object()
      
      # Jika ada variabel kelompok, gabungkan plot normalitas dan homogenitas
      if (input$group_var != "none") {
        homo_plots <- homogeneity_plot_object()
        final_plot <- grid.arrange(norm_plots, homo_plots, nrow = 2, heights = c(1, 1))
        # Simpan plot gabungan
        ggsave(file, plot = final_plot, width = 8, height = 7, dpi = 150)
      } else {
        # Jika tidak, simpan plot normalitas saja
        ggsave(file, plot = norm_plots, width = 8, height = 4, dpi = 150)
      }
    }
  )
  
  # --- Download Laporan Teks Individual (Word) ---
  output$download_assumptions <- downloadHandler(
    filename = function() {
      paste0("laporan_teks_asumsi_", input$assumption_var, "_", Sys.Date(), ".docx")
    },
    content = function(file) {
      shinyjs::show("loading_overlay")
      on.exit(shinyjs::hide("loading_overlay"))
      
      # Ambil hasil teks dari reactive
      norm_output <- capture.output(print(normality_test_reactive()))
      
      doc <- read_docx() %>%
        body_add_par("Laporan Teks Uji Asumsi", style = "heading 1") %>%
        body_add_par(paste("Variabel yang diuji:", input$assumption_var)) %>%
        body_add_break() %>%
        body_add_par("Hasil Uji Normalitas", style = "heading 2")
      
      for(line in norm_output) { doc <- doc %>% body_add_par(line) }
      
      # Tambahkan bagian Homogenitas jika ada
      if (input$group_var != "none") {
        homo_output <- capture.output(print(homogeneity_test_reactive()))
        doc <- doc %>%
          body_add_break() %>%
          body_add_par("Hasil Uji Homogenitas", style = "heading 2") %>%
          body_add_par(paste("Variabel Kelompok:", input$group_var))
        
        for(line in homo_output) { doc <- doc %>% body_add_par(line) }
      }
      
      print(doc, target = file)
    }
  )
  
  
  # --- Download Laporan LENGKAP (Gabungan Word) - VERSI FINAL ---
  output$download_asumsi_full <- downloadHandler(
    filename = function() {
      paste0("laporan_lengkap_uji_asumsi_", Sys.Date(), ".docx")
    },
    content = function(file) {
      # Tampilkan notifikasi loading
      shinyjs::show("loading_overlay")
      on.exit(shinyjs::hide("loading_overlay"))
      
      # --- Bagian 1: Siapkan Semua Data dan Plot ---
      
      # a. Ringkasan Normalitas Semua Variabel
      hasil_normalitas <- data.frame(
        Variabel = character(), Metode_Uji = character(), Statistik = numeric(),
        P_Value = numeric(), Keputusan_Alpha_0.05 = character(), stringsAsFactors = FALSE
      )
      for (var_name in names(select_if(sovi_data, is.numeric))) {
        var_data <- na.omit(sovi_data[[var_name]])
        if (length(var_data) >= 3) {
          test_res <- if(length(var_data) >= 5000) ks.test(var_data, "pnorm", mean(var_data), sd(var_data)) else shapiro.test(var_data)
          metode <- if(length(var_data) >= 5000) "Kolmogorov-Smirnov" else "Shapiro-Wilk"
          hasil_normalitas <- rbind(hasil_normalitas, data.frame(
            Variabel = var_name, Metode_Uji = metode, Statistik = round(test_res$statistic, 4),
            P_Value = round(test_res$p.value, 4), Keputusan_Alpha_0.05 = ifelse(test_res$p.value < 0.05, "Tidak Normal", "Normal")
          ))
        }
      }
      
      # b. Detail Normalitas Variabel Terpilih (Plot & Interpretasi)
      norm_interp_selected <- normality_interpretation_reactive()
      temp_norm_plot <- tempfile(fileext = ".png")
      ggsave(temp_norm_plot, plot = normality_plots_object(), width = 8, height = 4)
      
      # c. Detail Homogenitas Variabel Terpilih (jika ada)
      if (input$group_var != "none") {
        homo_output <- capture.output(print(homogeneity_test_reactive()))
        homo_interp <- homogeneity_interpretation_reactive() # <-- SEKARANG INI AMAN
        temp_homo_plot <- tempfile(fileext = ".png")
        ggsave(temp_homo_plot, plot = homogeneity_plot_object(), width = 8, height = 4)
      }
      
      # --- Bagian 2: Susun Dokumen Word ---
      doc <- read_docx() %>%
        body_add_par("Laporan Lengkap Uji Asumsi", style = "heading 1") %>%
        body_add_par(paste("Tanggal Analisis:", Sys.Date())) %>%
        body_add_break() %>%
        body_add_par("1. Ringkasan Uji Normalitas (Semua Variabel)", style = "heading 2") %>%
        body_add_table(hasil_normalitas, style = "Table Professional") %>%
        body_add_break() %>%
        body_add_par("2. Detail Analisis (Variabel Terpilih di UI)", style = "heading 2") %>%
        body_add_par(paste("Variabel Uji:", input$assumption_var)) %>%
        body_add_par("Plot Normalitas:", style = "heading 3") %>%
        body_add_img(src = temp_norm_plot, width = 6, height = 3) %>%
        body_add_par("Interpretasi Normalitas:", style = "heading 3") %>%
        body_add_par(norm_interp_selected)
      
      if (input$group_var != "none") {
        doc <- doc %>%
          body_add_break() %>%
          body_add_par("Hasil Uji Homogenitas", style = "heading 2") %>%
          body_add_par(paste("Variabel Kelompok:", input$group_var)) %>%
          body_add_par("Plot Homogenitas:", style = "heading 3") %>%
          body_add_img(src = temp_homo_plot, width = 6, height = 3) %>%
          body_add_par("Hasil Teks:", style = "heading 3")
        
        for(line in homo_output) { doc <- doc %>% body_add_par(line) }
        
        doc <- doc %>%
          body_add_par("Interpretasi Homogenitas:", style = "heading 3") %>%
          body_add_par(homo_interp)
      }
      
      print(doc, target = file)
    }
  )
  
  
  # =====================================================================
  # TAB UJI BEDA RATA-RATA (VERSI REAKTIF FINAL)
  # =====================================================================
  
  # --- Objek Reaktif untuk Perhitungan ---
  t_test_result <- reactive({
    req(input$t_test_var, input$t_test_type)
    var_data <- sovi_data[[input$t_test_var]]
    
    tryCatch({
      if (input$t_test_type == "one_sample") {
        validate(need(length(na.omit(var_data)) >= 2, "Data tidak cukup."))
        t.test(var_data, mu = input$mu_value, conf.level = 1 - input$alpha_t)
      } else {
        req(input$group_var_t != "none")
        group_data <- categorical_vars$data[[input$group_var_t]]
        validate(need(!is.null(group_data), "Variabel kelompok belum dibuat."))
        
        df_clean <- na.omit(data.frame(var = var_data, group = group_data))
        validate(need(length(unique(df_clean$group)) == 2, "Variabel kelompok harus memiliki 2 kategori."))
        
        group_levels <- unique(df_clean$group)
        group1_data <- df_clean$var[df_clean$group == group_levels[1]]
        group2_data <- df_clean$var[df_clean$group == group_levels[2]]
        
        validate(need(length(group1_data) >= 2 && length(group2_data) >= 2, "Setiap kelompok butuh min. 2 observasi."))
        
        if (input$t_test_type == "two_sample") {
          t.test(group1_data, group2_data, var.equal = input$equal_var, conf.level = 1 - input$alpha_t)
        } else if (input$t_test_type == "paired") {
          validate(need(length(group1_data) == length(group2_data), "Data berpasangan harus punya jumlah sama."))
          t.test(group1_data, group2_data, paired = TRUE, conf.level = 1 - input$alpha_t)
        }
      }
    }, error = function(e) { list(error = paste("Error:", e$message)) })
  })
  
  # --- Objek Reaktif untuk Interpretasi ---
  t_test_interpretation_reactive <- reactive({
    result <- t_test_result()
    validate(need(!is.null(result) && is.null(result$error), "Hasil uji tidak valid."))
    create_interpretation(result, "t-test", input$alpha_t)
  })
  
  # --- Objek Reaktif untuk Tabel Ringkasan ---
  group_summary_table_reactive <- reactive({
    req(input$t_test_var)
    var_data <- sovi_data[[input$t_test_var]]
    
    if(input$t_test_type == "one_sample" || input$group_var_t == "none") {
      df <- na.omit(data.frame(Statistik = c("N", "Mean", "Std Dev"),
                               Nilai = c(length(var_data), mean(var_data), sd(var_data))))
    } else {
      group_data <- categorical_vars$data[[input$group_var_t]]
      df <- na.omit(data.frame(var = var_data, group = group_data)) %>%
        group_by(group) %>%
        summarise(N = n(), Mean = mean(var), Std_Dev = sd(var))
    }
    return(df)
  })
  
  # --- OBJEK REAKTIF BARU UNTUK PLOT ---
  t_test_plot_object <- reactive({
    result <- t_test_result()
    validate(need(!is.null(result) && is.null(result$error), "Plot tidak dapat dibuat karena error pada perhitungan."))
    
    var_data <- na.omit(sovi_data[[input$t_test_var]])
    
    if (input$t_test_type == "one_sample") {
      df <- data.frame(value = var_data)
      sample_mean <- mean(df$value)
      
      # Buat plot ggplot
      p <- ggplot(df, aes(x = value)) +
        geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "#3498db", color = "white", alpha = 0.8) +
        geom_density(color = "#2980b9", linewidth = 1) +
        geom_vline(aes(xintercept = sample_mean, color = "Rata-rata Sampel"), linetype = "solid", linewidth = 1.5) +
        geom_vline(aes(xintercept = input$mu_value, color = "Nilai Hipotesis (μ₀)"), linetype = "dashed", linewidth = 1.5) +
        scale_color_manual(name = "Legenda", values = c("Rata-rata Sampel" = "#e74c3c", "Nilai Hipotesis (μ₀)" = "#2ecc71")) +
        labs(title = paste("Distribusi", input$t_test_var),
             subtitle = paste("Rata-rata Sampel =", round(sample_mean, 2), "vs. Hipotesis μ₀ =", input$mu_value),
             x = input$t_test_var, y = "Densitas") +
        theme_minimal(base_size = 14) +
        theme(legend.position = "bottom")
      
    } else {
      group_data <- categorical_vars$data[[input$group_var_t]]
      df_clean <- na.omit(data.frame(var = var_data, group = group_data))
      
      # Buat plot ggplot
      p <- ggplot(df_clean, aes(x = group, y = var, fill = group)) +
        geom_boxplot(alpha = 0.7) +
        geom_jitter(width = 0.1, alpha = 0.3) +
        labs(title = paste("Perbandingan", input$t_test_var, "antar Kelompok"),
             x = input$group_var_t, y = input$t_test_var) +
        theme_minimal(base_size = 14) + 
        theme(legend.position = "none")
    }
    return(p) # Kembalikan objek plot
  })
  
  
  # --- Render Output untuk UI ---
  output$t_test_result <- renderPrint({ t_test_result() })
  output$t_test_interpretation <- renderText({ t_test_interpretation_reactive() })
  output$group_summary_table <- DT::renderDataTable({
    df <- group_summary_table_reactive()
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    DT::datatable(df, options = list(dom = 't', paging = FALSE), rownames = FALSE) %>%
      DT::formatRound(columns = numeric_cols, digits = 3)
  })
  
  
  
  
  # --- DOWNLOAD HANDLERS ---
  output$download_ttest_report <- downloadHandler(
    filename = function() { paste0("laporan_uji_t_", input$t_test_var, "_", Sys.Date(), ".docx") },
    content = function(file) {
      shinyjs::show("loading_overlay"); on.exit(shinyjs::hide("loading_overlay"))
      
      doc <- read_docx() %>%
        body_add_par("Laporan Uji Beda Rata-rata (T-Test)", style = "heading 1") %>%
        body_add_par(paste("Variabel:", input$t_test_var)) %>%
        body_add_break() %>%
        body_add_par("Hasil Uji", style = "heading 2")
      
      for(line in capture.output(t_test_result())) { doc <- doc %>% body_add_par(line) }
      
      doc <- doc %>% 
        body_add_par("Interpretasi", style = "heading 2") %>%
        body_add_par(t_test_interpretation_reactive())
      
      print(doc, target = file)
    }
  )
  
  output$t_test_plot <- renderPlot({
    print(t_test_plot_object())
  })
  
  output$download_ttest_plot <- downloadHandler(
    filename = function() { paste0("plot_uji_t_", input$t_test_var, "_", Sys.Date(), ".png") },
    content = function(file) {
      # Sekarang ggsave memanggil objek plot secara langsung, ini cara yang benar
      ggsave(file, plot = t_test_plot_object(), width = 8, height = 6, dpi = 150)
    }
  )
  
  output$download_uji_rata_full <- downloadHandler(
    filename = function() { paste0("laporan_lengkap_uji_rata_", Sys.Date(), ".docx") },
    content = function(file) {
      shinyjs::show("loading_overlay"); on.exit(shinyjs::hide("loading_overlay"))
      
      # Simpan plot ke file temporer dengan memanggil objek plot secara langsung
      temp_plot_path <- tempfile(fileext = ".png")
      ggsave(temp_plot_path, plot = t_test_plot_object(), width = 8, height = 6, dpi = 150)
      
      doc <- read_docx() %>%
        body_add_par("Laporan Lengkap Uji Beda Rata-rata", style = "heading 1") %>%
        body_add_par(paste("Tanggal Analisis:", Sys.Date())) %>%
        body_add_break() %>%
        body_add_par("1. Hasil Uji", style = "heading 2")
      
      for(line in capture.output(t_test_result())) { doc <- doc %>% body_add_par(line) }
      
      doc <- doc %>% 
        body_add_par("2. Interpretasi", style = "heading 2") %>%
        body_add_par(t_test_interpretation_reactive()) %>%
        body_add_break() %>%
        body_add_par("3. Plot Perbandingan", style = "heading 2") %>%
        body_add_img(src = temp_plot_path, width = 6, height = 4.5) %>%
        body_add_break() %>%
        body_add_par("4. Ringkasan Statistik Kelompok", style = "heading 2") %>%
        body_add_table(group_summary_table_reactive(), style = "Table Professional")
      
      print(doc, target = file)
    }
  )
  
  # =====================================================================
  # TAB UJI PROPORSI & RAGAM - DIPERBAIKI
  # =====================================================================
  
  # --- Objek Reaktif untuk Perhitungan Uji ---
  prop_var_test_result <- reactive({
    req(input$test_type_pv)
    tryCatch({
      if (input$test_type_pv == "var_one") {
        req(input$var_test_var)
        var_data <- na.omit(sovi_data[[input$var_test_var]])
        validate(need(length(var_data) >= 2, "Data tidak cukup."))
        n <- length(var_data)
        sample_var <- var(var_data)
        chi_stat <- (n - 1) * sample_var / input$var_null
        p_value <- 2 * min(pchisq(chi_stat, df = n - 1), 1 - pchisq(chi_stat, df = n - 1))
        list(statistic = setNames(chi_stat, "chisq"), p.value = p_value,
             estimate = setNames(sample_var, "variance"), null.value = setNames(input$var_null, "variance"),
             method = "One-sample Chi-squared test for variance", data.name = input$var_test_var)
      } else if (input$test_type_pv == "var_two") {
        req(input$var_test_var, input$group_var_var != "none")
        var_data <- sovi_data[[input$var_test_var]]
        group_data <- categorical_vars$data[[input$group_var_var]]
        validate(need(!is.null(group_data), "Variabel kelompok belum dibuat."))
        df_clean <- na.omit(data.frame(var = var_data, group = group_data))
        validate(need(length(unique(df_clean$group)) == 2, "Variabel kelompok harus memiliki 2 kategori."))
        group1_data <- df_clean$var[df_clean$group == unique(df_clean$group)[1]]
        group2_data <- df_clean$var[df_clean$group == unique(df_clean$group)[2]]
        validate(need(length(group1_data) >= 2 && length(group2_data) >= 2, "Setiap kelompok butuh min. 2 observasi."))
        var.test(group1_data, group2_data, conf.level = 1 - input$alpha_pv)
      } else if (input$test_type_pv == "prop_one") {
        req(input$prop_var_categorical, input$prop_success_level, input$prop_null_value)
        cat_data <- na.omit(categorical_vars$data[[input$prop_var_categorical]])
        validate(need(!is.null(cat_data) && length(cat_data) > 0, "Variabel kategorikal belum dibuat atau kosong."))
        x <- sum(cat_data == input$prop_success_level)
        n <- length(cat_data)
        prop.test(x = x, n = n, p = input$prop_null_value, conf.level = 1 - input$alpha_pv)
      } else if (input$test_type_pv == "prop_two") {
        req(input$prop_var_categorical, input$prop_group_var)
        validate(need(input$prop_var_categorical != input$prop_group_var, "Variabel uji dan kelompok tidak boleh sama."))
        var_to_test <- categorical_vars$data[[input$prop_var_categorical]]
        grouping_var <- categorical_vars$data[[input$prop_group_var]]
        validate(need(!is.null(var_to_test) && !is.null(grouping_var), "Variabel belum dibuat."))
        contingency_table <- table(var_to_test, grouping_var)
        validate(need(all(dim(contingency_table) >= 2), "Kedua variabel harus punya min. 2 level/kategori."))
        prop.test(contingency_table, conf.level = 1 - input$alpha_pv)
      }
    }, error = function(e) { list(error = paste("Gagal menjalankan uji:", e$message)) })
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
    } else if(input$test_type_pv == "prop_one") {
      barplot(table(var_data), main = paste("Distribusi", input$var_test_var),
              xlab = "Kategori", ylab = "Frekuensi", col = "lightblue")
      abline(h = input$prop_null, col = "red", lwd = 2, lty = 2)
      legend("topright", paste("Proporsi H₀:", input$prop_null), col = "red", lwd = 2)
    } else if(input$test_type_pv == "prop_two") {
      barplot(table(var_data), main = paste("Distribusi", input$var_test_var),
              xlab = "Kategori", ylab = "Frekuensi", col = "lightblue")
      abline(h = input$prop_null, col = "red", lwd = 2, lty = 2)
      legend("topright", paste("Proporsi H₀:", input$prop_null), col = "red", lwd = 2)
    }
  })
  
  
  prop_var_plot_object <- reactive({
    validate(need(!is.null(prop_var_test_result()) && is.null(prop_var_test_result()$error), "Plot tidak dapat dibuat."))
    
    # Logika untuk Uji Ragam
    if (input$test_type_pv %in% c("var_one", "var_two")) {
      var_data <- na.omit(sovi_data[[input$var_test_var]])
      df <- data.frame(value = var_data)
      
      if (input$test_type_pv == "var_one") {
        ggplot(df, aes(x = value)) +
          geom_histogram(fill = "seagreen3", color = "white", alpha = 0.8) +
          labs(title = paste("Distribusi Variabel", input$var_test_var)) + theme_minimal()
      } else { # var_two
        req(input$group_var_var != "none")
        group_data <- categorical_vars$data[[input$group_var_var]]
        df_grouped <- na.omit(data.frame(value = var_data, group = group_data))
        ggplot(df_grouped, aes(x = group, y = value, fill = group)) +
          geom_boxplot() + theme_minimal() + theme(legend.position = "none") +
          labs(title = "Perbandingan Sebaran antar Kelompok")
      }
      # Logika untuk Uji Proporsi
    } else {
      req(input$prop_var_categorical)
      cat_data <- categorical_vars$data[[input$prop_var_categorical]]
      
      if (input$test_type_pv == "prop_one") {
        df <- as.data.frame(table(cat_data))
        ggplot(df, aes(x = cat_data, y = Freq, fill = cat_data)) +
          geom_bar(stat = "identity") + theme_minimal() + theme(legend.position = "none") +
          labs(title = paste("Distribusi Kategori", input$prop_var_categorical), x = "Kategori", y = "Jumlah")
      } else { # prop_two
        req(input$prop_group_var != "none")
        group_data <- categorical_vars$data[[input$prop_group_var]]
        df_grouped <- na.omit(data.frame(var = cat_data, group = group_data))
        ggplot(df_grouped, aes(x = var, fill = group)) +
          geom_bar(position = "dodge") + theme_minimal() +
          labs(title = "Perbandingan Proporsi antar Kelompok", x = "Kategori Uji", y = "Jumlah")
      }
    }
  })
  

  prop_var_interpretation_reactive <- reactive({
    result <- prop_var_test_result()
    # Hentikan jika ada error atau hasil kosong
    if (is.null(result) || !is.null(result$error)) {
      return("Tidak dapat memberikan interpretasi karena error dalam perhitungan atau parameter tidak valid.")
    }
    
    alpha <- input$alpha_pv
    p_value <- result$p.value
    
    # --- Logika Interpretasi untuk UJI RAGAM (Sudah Benar) ---
    if (input$test_type_pv == "var_one") {
      paste0(
        "Uji ragam satu kelompok untuk variabel ", result$data.name, " menghasilkan statistik χ² = ",
        round(result$statistic, 3), " dengan p-value = ", round(p_value, 4), ". ",
        if (p_value < alpha) {
          paste0("Hasil ini signifikan, menunjukkan bahwa ragam sampel (", round(result$estimate, 3),
                 ") berbeda secara signifikan dari ragam hipotesis (", result$null.value, ").")
        } else {
          paste0("Hasil ini tidak signifikan, menunjukkan bahwa tidak ada cukup bukti untuk menyatakan ragam sampel (", round(result$estimate, 3),
                 ") berbeda dari ragam hipotesis (", result$null.value, ").")
        }
      )
    } else if (input$test_type_pv == "var_two") {
      paste0(
        "Uji F untuk membandingkan ragam dua kelompok menghasilkan statistik F = ",
        round(result$statistic, 3), " dengan p-value = ", round(p_value, 4), ". ",
        if (p_value < alpha) {
          "Hasil ini signifikan, yang berarti ragam antara kedua kelompok berbeda secara statistik (asumsi homogenitas ragam tidak terpenuhi)."
        } else {
          "Hasil ini tidak signifikan, yang berarti tidak ada perbedaan ragam yang cukup berarti antara kedua kelompok (asumsi homogenitas ragam terpenuhi)."
        }
      )
      
      # --- Logika Interpretasi BARU untuk UJI PROPORSI ---
    } else if (input$test_type_pv == "prop_one") {
      paste0(
        "Uji proporsi satu sampel menghasilkan statistik χ² = ", round(result$statistic, 3),
        " dengan p-value = ", round(p_value, 4), ". ",
        "Proporsi sampel untuk kategori '", input$prop_success_level, "' adalah ", round(result$estimate, 3), ". ",
        if (p_value < alpha) {
          paste0("Hasil ini signifikan. Terdapat cukup bukti untuk menyatakan bahwa proporsi populasi sebenarnya berbeda dari nilai hipotesis (", result$null.value, ").")
        } else {
          paste0("Hasil ini tidak signifikan. Tidak ada cukup bukti untuk menyatakan bahwa proporsi populasi sebenarnya berbeda dari nilai hipotesis (", result$null.value, ").")
        }
      )
    } else if (input$test_type_pv == "prop_two") {
      paste0(
        "Uji proporsi dua sampel (Uji Chi-Square) menghasilkan statistik χ² = ", round(result$statistic, 3),
        " dengan p-value = ", round(p_value, 4), ". ",
        if (p_value < alpha) {
          paste0("Hasil ini signifikan. Ini menunjukkan adanya hubungan (asosiasi) yang signifikan antara variabel '",
                 input$prop_var_categorical, "' dan variabel kelompok '", input$prop_group_var, "'.")
        } else {
          paste0("Hasil ini tidak signifikan. Ini menunjukkan tidak ada hubungan (asosiasi) yang signifikan antara variabel '",
                 input$prop_var_categorical, "' dan variabel kelompok '", input$prop_group_var, "'. Kedua variabel tersebut independen.")
        }
      )
    }
  })
  
  # --- Objek Reaktif untuk Tabel Ringkasan (DIPERBAIKI) ---
  prop_var_summary_table_reactive <- reactive({
    # Untuk Uji Ragam
    if (input$test_type_pv %in% c("var_one", "var_two")) {
      req(input$var_test_var)
      var_data <- sovi_data[[input$var_test_var]]
      if (input$test_type_pv == "var_one" || input$group_var_var == "none") {
        df <- na.omit(data.frame(Statistik = c("N", "Mean", "Variance", "Std Dev"),
                                 Nilai = c(length(var_data), mean(var_data), var(var_data), sd(var_data))))
      } else {
        group_data <- categorical_vars$data[[input$group_var_var]]
        df <- na.omit(data.frame(var = var_data, group = group_data)) %>%
          group_by(group) %>%
          summarise(N = n(), Mean = mean(var), Variance = var(var), Std_Dev = sd(var))
      }
      return(df)
      # Untuk Uji Proporsi
    } else { 
      req(input$prop_var_categorical)
      cat_data <- categorical_vars$data[[input$prop_var_categorical]]
      if (input$test_type_pv == "prop_one" || input$prop_group_var == "none") {
        df <- as.data.frame(table(Kategori = cat_data))
        names(df)[2] <- "Frekuensi"
        return(df)
      } else {
        group_data <- categorical_vars$data[[input$prop_group_var]]
        df <- as.data.frame(table(Kategori_Uji = cat_data, Kelompok = group_data))
        names(df)[3] <- "Frekuensi"
        return(df)
      }
    }
  })
  
  # --- Render Output untuk UI ---
  output$prop_var_result <- renderPrint({ prop_var_test_result() })
  output$prop_var_plot <- renderPlot({ print(prop_var_plot_object()) })
  output$prop_var_interpretation <- renderText({ prop_var_interpretation_reactive() })
  output$prop_var_summary_table <- DT::renderDataTable({
    df <- prop_var_summary_table_reactive()
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    DT::datatable(df, options = list(scrollX = TRUE, paging = FALSE, dom = 't'), rownames = FALSE) %>%
      DT::formatRound(columns = numeric_cols, digits = 3)
  })
  
  # --- DOWNLOAD HANDLERS (VERSI LENGKAP) ---
  output$download_prop_var_report <- downloadHandler(
    filename = function() { paste0("laporan_teks_", input$test_type_pv, "_", Sys.Date(), ".docx") },
    content = function(file) {
      shinyjs::show("loading_overlay"); on.exit(shinyjs::hide("loading_overlay"))
      doc <- read_docx() %>%
        body_add_par("Laporan Uji Proporsi & Ragam", style="heading 1") %>%
        body_add_par("Hasil Teks:", style="heading 2")
      for(line in capture.output(prop_var_test_result())) { doc <- doc %>% body_add_par(line) }
      doc <- doc %>% 
        body_add_par("Interpretasi:", style="heading 2") %>%
        body_add_par(prop_var_interpretation_reactive())
      print(doc, target = file)
    }
  )
  
  output$download_prop_var_plot <- downloadHandler(
    filename = function() { paste0("plot_", input$test_type_pv, "_", Sys.Date(), ".png") },
    content = function(file) {
      ggsave(file, plot = prop_var_plot_object(), width = 8, height = 5)
    }
  )
  
  output$download_prop_var_full <- downloadHandler(
    filename = function() { paste0("laporan_lengkap_", input$test_type_pv, "_", Sys.Date(), ".docx") },
    content = function(file) {
      shinyjs::show("loading_overlay"); on.exit(shinyjs::hide("loading_overlay"))
      
      temp_plot_path <- tempfile(fileext = ".png")
      ggsave(temp_plot_path, plot = prop_var_plot_object(), width = 8, height = 5)
      
      doc <- read_docx() %>%
        body_add_par("Laporan Lengkap Uji Proporsi & Ragam", style = "heading 1") %>%
        body_add_break() %>%
        body_add_par("1. Hasil Uji", style = "heading 2")
      
      for(line in capture.output(prop_var_test_result())) { doc <- doc %>% body_add_par(line) }
      
      doc <- doc %>% 
        body_add_par("2. Interpretasi", style = "heading 2") %>%
        body_add_par(prop_var_interpretation_reactive()) %>%
        body_add_break() %>%
        body_add_par("3. Plot Hasil", style = "heading 2") %>%
        body_add_img(src = temp_plot_path, width = 6, height = 3.75) %>%
        body_add_break() %>%
        body_add_par("4. Ringkasan Statistik", style = "heading 2") %>%
        body_add_table(prop_var_summary_table_reactive(), style = "Table Professional")
      
      print(doc, target = file)
    }
  )
  
  
  
  # =====================================================================
  # TAB ANOVA (VERSI FINAL BERSIH DAN LENGKAP)
  # =====================================================================
  
  # --- Objek Reaktif Utama ---
  anova_result <- reactive({
    req(input$anova_y, input$anova_x1, input$anova_type)
    if(input$anova_x1 == "none" || !input$anova_x1 %in% names(categorical_vars$data)) {
      return(list(error = "Pilih variabel faktor 1 yang valid."))
    }
    anova_data_full <- data.frame(y = sovi_data[[input$anova_y]], x1 = categorical_vars$data[[input$anova_x1]])
    if (input$anova_type == "two_way") {
      if(is.null(input$anova_x2) || input$anova_x2 == "none" || !input$anova_x2 %in% names(categorical_vars$data)) {
        return(list(error = "Pilih variabel faktor 2 yang valid."))
      }
      anova_data_full$x2 <- categorical_vars$data[[input$anova_x2]]
    }
    anova_data_clean <- na.omit(anova_data_full)
    validate(need(nrow(anova_data_clean) > 0, "Tidak ada data yang valid setelah menghapus missing values."))
    tryCatch({
      if (input$anova_type == "one_way") {
        model <- aov(y ~ x1, data = anova_data_clean)
        list(model = model, summary = summary(model), data = anova_data_clean, type = "one_way")
      } else { # two_way
        model <- aov(y ~ x1 * x2, data = anova_data_clean)
        list(model = model, summary = summary(model), data = anova_data_clean, type = "two_way")
      }
    }, error = function(e) { list(error = e$message) })
  })
  
  posthoc_result <- reactive({
    result <- anova_result()
    if(is.null(result) || !is.null(result$error)) return(NULL)
    p_value <- result$summary[[1]]$`Pr(>F)`[1]
    if(is.na(p_value) || p_value >= input$alpha_anova) return(NULL)
    TukeyHSD(result$model)
  })
  
  # --- Objek Reaktif untuk Interpretasi ---
  anova_interpretation_reactive <- reactive({
    result <- anova_result()
    validate(need(!is.null(result) && is.null(result$error), "Hasil ANOVA tidak valid."))
    p_value <- result$summary[[1]]$`Pr(>F)`[1]
    validate(need(!is.na(p_value), "Tidak dapat menghitung p-value."))
    base_interp <- create_interpretation(list(p.value = p_value), "ANOVA", alpha = input$alpha_anova)
    if (result$type == "two_way") {
      p_value_interaksi <- result$summary[[1]]$`Pr(>F)`[3]
      interaksi_interp <- if(!is.na(p_value_interaksi) && p_value_interaksi < input$alpha_anova) {
        "Efek interaksi juga signifikan, menunjukkan bahwa pengaruh satu faktor bergantung pada level faktor lainnya."
      } else { "Efek interaksi tidak signifikan." }
      base_interp <- paste(base_interp, interaksi_interp)
    }
    return(base_interp)
  })
  
  posthoc_interpretation_reactive <- reactive({
    posthoc <- posthoc_result()
    validate(need(!is.null(posthoc), "Uji lanjut tidak dijalankan."))
    posthoc_df <- as.data.frame(posthoc[[which(grepl("x", names(posthoc)))]])
    significant_pairs <- rownames(posthoc_df)[posthoc_df[, "p adj"] < input$alpha_anova]
    if (length(significant_pairs) == 0) {
      return("Tidak ditemukan perbedaan signifikan antar pasangan kelompok.")
    } else {
      paste0("Ditemukan perbedaan rata-rata yang signifikan antara pasangan berikut: ", 
             paste(significant_pairs, collapse = ", "), ".")
    }
  })
  
  # --- Objek Reaktif untuk Plot (semua pakai ggplot2) ---
  anova_plot_object <- reactive({
    result <- anova_result()
    validate(need(!is.null(result) && is.null(result$error), "Plot tidak dapat dibuat."))
    if (result$type == "one_way") {
      ggplot(result$data, aes(x = x1, y = y, fill = x1)) +
        geom_boxplot(alpha = 0.8, show.legend = FALSE) +
        geom_jitter(width = 0.1, alpha = 0.4, show.legend = FALSE) +
        labs(title = "Perbandingan Kelompok (One-Way ANOVA)", x = "Faktor", y = "Nilai Variabel Dependen") +
        theme_minimal(base_size = 14)
    } else { # two_way
      ggplot(result$data, aes(x = x1, y = y, color = x2, group = x2)) +
        stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
        stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, linewidth = 1) +
        labs(title = "Plot Interaksi (Two-Way ANOVA)", x = "Faktor 1", y = "Rata-rata Variabel Dependen", color = "Faktor 2") +
        theme_minimal(base_size = 14) + theme(legend.position = "bottom")
    }
  })
  
  # --- Render Output untuk UI ---
  output$anova_result <- renderPrint({ anova_result()$summary })
  output$posthoc_result <- renderPrint({ posthoc_result() })
  output$show_posthoc <- reactive({ !is.null(posthoc_result()) })
  outputOptions(output, "show_posthoc", suspendWhenHidden = FALSE)
  output$anova_interpretation <- renderText({ anova_interpretation_reactive() })
  output$posthoc_interpretation <- renderText({ posthoc_interpretation_reactive() })
  output$anova_plot <- renderPlot({ print(anova_plot_object()) })
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
  
  # =====================================================================
  # DOWNLOAD HANDLERS UNTUK ANOVA (VERSI FINAL)
  # =====================================================================
  
  # --- Download Laporan Teks Individual (Word) ---
  output$download_anova_report <- downloadHandler(
    filename = function() {
      paste0("laporan_teks_anova_", input$anova_y, "_", Sys.Date(), ".docx")
    },
    content = function(file) {
      shinyjs::show("loading_overlay"); on.exit(shinyjs::hide("loading_overlay"))
      
      # Ambil semua hasil dari reactives
      anova_output <- capture.output(anova_result()$summary)
      anova_interp <- anova_interpretation_reactive()
      posthoc_output <- capture.output(posthoc_result())
      posthoc_interp <- posthoc_interpretation_reactive()
      
      # Buat dokumen Word
      doc <- read_docx() %>%
        body_add_par("Laporan Teks Analisis Varian (ANOVA)", style = "heading 1") %>%
        body_add_par(paste("Variabel Dependen:", input$anova_y)) %>%
        body_add_break() %>%
        body_add_par("Hasil Tabel ANOVA", style = "heading 2")
      
      for(line in anova_output) { doc <- doc %>% body_add_par(line) }
      
      doc <- doc %>% 
        body_add_par("Interpretasi ANOVA:", style = "heading 3") %>%
        body_add_par(anova_interp)
      
      # Tambahkan bagian Post-Hoc jika ada
      if (!is.null(posthoc_result())) {
        doc <- doc %>%
          body_add_break() %>%
          body_add_par("Hasil Uji Lanjut (Post-Hoc)", style = "heading 2")
        
        for(line in posthoc_output) { doc <- doc %>% body_add_par(line) }
        
        doc <- doc %>%
          body_add_par("Interpretasi Post-Hoc:", style = "heading 3") %>%
          body_add_par(posthoc_interp)
      }
      
      print(doc, target = file)
    }
  )
  
  # --- Download Plot (PNG) ---
  output$download_anova_plot <- downloadHandler(
    filename = function() {
      paste0("plot_anova_", input$anova_y, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      # Pastikan plot object ada sebelum menyimpan
      req(anova_plot_object())
      ggsave(file, plot = anova_plot_object(), width = 8, height = 6, dpi = 150)
    }
  )
  
  # --- Download Laporan LENGKAP (Gabungan Word) ---
  output$download_anova_full <- downloadHandler(
    filename = function() {
      paste0("laporan_lengkap_anova_", input$anova_y, "_", Sys.Date(), ".docx")
    },
    content = function(file) {
      shinyjs::show("loading_overlay"); on.exit(shinyjs::hide("loading_overlay"))
      
      # 1. Siapkan semua data yang dibutuhkan
      req(anova_result(), anova_plot_object())
      anova_output <- capture.output(anova_result()$summary)
      anova_interp <- anova_interpretation_reactive()
      posthoc_output <- capture.output(posthoc_result())
      posthoc_interp <- posthoc_interpretation_reactive()
      
      # 2. Simpan plot ke file temporer
      temp_plot_path <- tempfile(fileext = ".png")
      ggsave(temp_plot_path, plot = anova_plot_object(), width = 8, height = 6, dpi = 150)
      
      # 3. Susun Dokumen Word
      doc <- read_docx() %>%
        body_add_par("Laporan Lengkap Analisis Varian (ANOVA)", style = "heading 1") %>%
        body_add_par(paste("Tanggal Analisis:", Sys.Date())) %>%
        body_add_break() %>%
        
        body_add_par("1. Hasil Uji ANOVA", style = "heading 2")
      for(line in anova_output) { doc <- doc %>% body_add_par(line) }
      
      doc <- doc %>% 
        body_add_par("Interpretasi ANOVA:", style = "heading 3") %>%
        body_add_par(anova_interp) %>%
        body_add_break()
      
      if (!is.null(posthoc_result())) {
        doc <- doc %>%
          body_add_par("2. Hasil Uji Lanjut (Post-Hoc)", style = "heading 2")
        for(line in posthoc_output) { doc <- doc %>% body_add_par(line) }
        doc <- doc %>%
          body_add_par("Interpretasi Post-Hoc:", style = "heading 3") %>%
          body_add_par(posthoc_interp) %>%
          body_add_break()
      }
      
      doc <- doc %>%
        body_add_par("3. Plot Hasil", style = "heading 2") %>%
        body_add_img(src = temp_plot_path, width = 6, height = 4.5)
      
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
  
  regression_interpretation_reactive <- reactive({
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
  linearity_test_reactive <- reactive({
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
  
  linearity_interpretation_reactive <- reactive({
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
  
  multicollinearity_test_reactive <- reactive({
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
  
  multicollinearity_interpretation_reactive <- reactive({
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
  
  heteroscedasticity_test_reactive <- reactive({
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
  
  heteroscedasticity_interpretation_reactive <- reactive({
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
  
  # --- Render Output untuk UI (Sekarang hanya memanggil reactive) ---
  output$regression_summary <- renderPrint({ summary(regression_model()$model) })
  output$regression_interpretation <- renderText({ regression_interpretation_reactive() })
  output$linearity_test <- renderPrint({ linearity_test_reactive() })
  output$linearity_interpretation <- renderText({ linearity_interpretation_reactive() })
  output$multicollinearity_test <- renderPrint({ multicollinearity_test_reactive() })
  output$multicollinearity_interpretation <- renderText({ multicollinearity_interpretation_reactive() })
  output$heteroscedasticity_test <- renderPrint({ heteroscedasticity_test_reactive() })
  output$heteroscedasticity_interpretation <- renderText({ heteroscedasticity_interpretation_reactive() })
  
  
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
  
  # --- Download Laporan LENGKAP REGRESI (Word) - VERSI FINAL ---
  output$download_regresi_full <- downloadHandler(
    filename = function() {
      paste0("laporan_lengkap_regresi_", input$reg_y, "_", Sys.Date(), ".docx")
    },
    content = function(file) {
      shinyjs::show("loading_overlay")
      on.exit(shinyjs::hide("loading_overlay"))
      
      # 1. Ambil semua hasil dari objek reaktif
      model_obj <- regression_model()
      validate(need(!is.null(model_obj) && is.null(model_obj$error), "Model regresi belum berhasil dibuat."))
      
      summary_output <- capture.output(summary(model_obj$model))
      interp_model <- regression_interpretation_reactive()
      interp_plot <- regression_plot_interpretation_reactive() # Tambahkan interpretasi plot
      
      linear_output <- capture.output(linearity_test_reactive())
      linear_interp <- linearity_interpretation_reactive()
      
      multi_output <- capture.output(multicollinearity_test_reactive())
      multi_interp <- multicollinearity_interpretation_reactive()
      
      hetero_output <- capture.output(heteroscedasticity_test_reactive())
      hetero_interp <- heteroscedasticity_interpretation_reactive()
      
      # --- BAGIAN BARU: Simpan KEDUA plot ke file temporer ---
      
      # 2a. Simpan plot Regresi (Actual vs Predicted)
      temp_scatter_path <- tempfile(fileext = ".png")
      png(temp_scatter_path, width = 800, height = 600)
      predicted <- fitted(model_obj$model)
      actual <- model_obj$data$y
      plot(actual, predicted, 
           main = paste("Actual vs Predicted:", input$reg_y),
           xlab = "Actual Values", ylab = "Predicted Values",
           pch = 19, col = "#2980b9")
      abline(0, 1, col = "#c0392b", lwd = 2, lty = 2)
      dev.off()
      
      # 2b. Simpan plot Diagnostik
      temp_diag_plot_path <- tempfile(fileext = ".png")
      png(temp_diag_plot_path, width = 800, height = 800)
      par(mfrow = c(2, 2))
      plot(model_obj$model)
      dev.off()
      
      # 3. Susun Dokumen Word yang Lebih Lengkap
      doc <- read_docx() %>%
        body_add_par("Laporan Lengkap Regresi Linear Berganda", style = "heading 1") %>%
        body_add_par(paste("Tanggal Analisis:", Sys.Date())) %>%
        body_add_break() %>%
        body_add_par("1. Spesifikasi Model", style = "heading 2") %>%
        body_add_par(paste("Variabel Dependen (Y):", input$reg_y)) %>%
        body_add_par(paste("Variabel Independen (X):", paste(input$reg_x, collapse = ", "))) %>%
        body_add_break() %>%
        
        body_add_par("2. Ringkasan Model", style = "heading 2")
      for(line in summary_output) { doc <- doc %>% body_add_par(line) }
      
      doc <- doc %>%
        body_add_par("Interpretasi Model:", style = "heading 3") %>%
        body_add_par(interp_model) %>%
        body_add_break() %>%
        
        # --- BAGIAN BARU: Masukkan Plot Regresi ---
        body_add_par("3. Plot Model Regresi", style = "heading 2") %>%
        body_add_img(src = temp_scatter_path, width = 6, height = 4.5) %>%
        body_add_par("Interpretasi Plot:", style = "heading 3") %>%
        body_add_par(interp_plot) %>%
        body_add_break() %>%
        
        body_add_par("4. Uji Asumsi Regresi", style = "heading 2") %>%
        body_add_par("Linearitas (Rainbow Test):", style = "heading 3")
      for(line in linear_output) { doc <- doc %>% body_add_par(line) }
      doc <- doc %>% body_add_par(paste("Interpretasi:", linear_interp)) %>%
        
        body_add_par("Multikolinearitas (VIF):", style = "heading 3")
      for(line in multi_output) { doc <- doc %>% body_add_par(line) }
      doc <- doc %>% body_add_par(paste("Interpretasi:", multi_interp)) %>%
        
        body_add_par("Heteroskedastisitas (Breusch-Pagan Test):", style = "heading 3")
      for(line in hetero_output) { doc <- doc %>% body_add_par(line) }
      doc <- doc %>% body_add_par(paste("Interpretasi:", hetero_interp)) %>%
        body_add_break() %>%
        
        body_add_par("5. Plot Diagnostik", style = "heading 2") %>%
        body_add_img(src = temp_diag_plot_path, width = 6, height = 6)
      
      # 4. Simpan file
      print(doc, target = file)
    }
  )
  
  # Jangan lupa pastikan Anda juga sudah membuat reactive untuk interpretasi plotnya
  # Jika belum ada, tambahkan ini di server Anda
  regression_plot_interpretation_reactive <- reactive({
    result <- regression_model()
    if(is.null(result) || "error" %in% names(result)) {
      return("Plot tidak tersedia karena error dalam model regresi.")
    }
    
    predicted <- fitted(result$model)
    actual <- result$data$y
    correlation <- cor(actual, predicted, use = "complete.obs")
    
    paste0("Plot actual vs predicted menunjukkan korelasi r = ", round(correlation, 3), 
           " antara nilai aktual dan prediksi model. ",
           if(correlation > 0.8) {
             "Korelasi yang tinggi ini menunjukkan bahwa model memiliki kemampuan prediksi yang baik."
           } else if(correlation > 0.5) {
             "Korelasi yang sedang ini menunjukkan bahwa model memiliki kemampuan prediksi yang cukup baik."
           } else {
             "Korelasi yang rendah ini menunjukkan bahwa model memiliki kemampuan prediksi yang terbatas."
           })
  })
  
  # Dan pastikan output di UI memanggilnya
  output$regression_plot_interpretation <- renderText({
    regression_plot_interpretation_reactive()
  })
  
  # =====================================================================
  # DOWNLOAD HANDLERS INDIVIDUAL UNTUK REGRESI
  # =====================================================================
  
  # --- Download Ringkasan Model (Word) ---
  output$download_summary_report <- downloadHandler(
    filename = function() {
      paste0("ringkasan_model_regresi_", input$reg_y, "_", Sys.Date(), ".docx")
    },
    content = function(file) {
      shinyjs::show("loading_overlay")
      on.exit(shinyjs::hide("loading_overlay"))
      
      model_obj <- regression_model()
      validate(need(!is.null(model_obj) && is.null(model_obj$error), "Model belum dibuat."))
      
      summary_output <- capture.output(summary(model_obj$model))
      interp_model <- regression_interpretation_reactive()
      
      doc <- read_docx() %>%
        body_add_par("Ringkasan Model Regresi", style = "heading 1") %>%
        body_add_par(paste("Model:", model_obj$formula)) %>%
        body_add_break() %>%
        body_add_par("Hasil Summary", style = "heading 2")
      
      for(line in summary_output) { doc <- doc %>% body_add_par(line) }
      
      doc <- doc %>%
        body_add_break() %>%
        body_add_par("Interpretasi", style = "heading 2") %>%
        body_add_par(interp_model)
      
      print(doc, target = file)
    }
  )
  
  # --- Download Plot Regresi (JPG) ---
  output$download_scatter_plot_jpg <- downloadHandler(
    filename = function() {
      paste0("plot_regresi_", input$reg_y, "_", Sys.Date(), ".jpg")
    },
    content = function(file) {
      result <- regression_model()
      validate(need(!is.null(result) && is.null(result$error), "Model belum dibuat."))
      
      jpeg(file, width = 800, height = 600, quality = 95)
      # Replikasi kode plotting dari output$regression_scatter_plot
      predicted <- fitted(result$model)
      actual <- result$data$y # Ambil dari data yang sudah bersih
      plot(actual, predicted, 
           main = paste("Actual vs Predicted:", input$reg_y),
           xlab = "Actual Values", ylab = "Predicted Values",
           pch = 19, col = "#2980b9", sub = "Garis Merah: Kesesuaian Sempurna (Y=X)")
      abline(0, 1, col = "#c0392b", lwd = 2, lty = 2)
      dev.off()
    }
  )
  
  # --- Download Laporan Asumsi (Word) ---
  output$download_assumption_report <- downloadHandler(
    filename = function() {
      paste0("laporan_asumsi_regresi_", input$reg_y, "_", Sys.Date(), ".docx")
    },
    content = function(file) {
      shinyjs::show("loading_overlay")
      on.exit(shinyjs::hide("loading_overlay"))
      
      # Ambil semua hasil asumsi dari reactives
      linear_output <- capture.output(linearity_test_reactive())
      linear_interp <- linearity_interpretation_reactive()
      multi_output <- capture.output(multicollinearity_test_reactive())
      multi_interp <- multicollinearity_interpretation_reactive()
      hetero_output <- capture.output(heteroscedasticity_test_reactive())
      hetero_interp <- heteroscedasticity_interpretation_reactive()
      
      doc <- read_docx() %>%
        body_add_par("Laporan Uji Asumsi Regresi", style = "heading 1") %>%
        body_add_par(paste("Model untuk variabel dependen:", input$reg_y)) %>%
        body_add_break() %>%
        
        body_add_par("Linearitas (Rainbow Test)", style = "heading 2")
      for(line in linear_output) { doc <- doc %>% body_add_par(line) }
      doc <- doc %>% body_add_par(paste("Interpretasi:", linear_interp)) %>%
        
        body_add_par("Multikolinearitas (VIF)", style = "heading 2")
      for(line in multi_output) { doc <- doc %>% body_add_par(line) }
      doc <- doc %>% body_add_par(paste("Interpretasi:", multi_interp)) %>%
        
        body_add_par("Heteroskedastisitas (Breusch-Pagan Test)", style = "heading 2")
      for(line in hetero_output) { doc <- doc %>% body_add_par(line) }
      doc <- doc %>% body_add_par(paste("Interpretasi:", hetero_interp))
      
      print(doc, target = file)
    }
  )
  
  # LETAKKAN SELURUH BLOK KODE INI DI DALAM FUNGSI server
  
  # =====================================================================
  # ANALISIS CLUSTERING (K-MEANS)
  # =====================================================================
  
  # 1. Update pilihan variabel untuk clustering secara dinamis
  # Kita ambil dari data master yang sudah bersih
  updateSelectizeInput(
    session, "cluster_vars",
    choices = names(sovi_data)[sapply(sovi_data, is.numeric)], # sovi_data adalah versi non-spasial
    selected = c("POVERTY", "LOWEDU", "NOELECTRIC", "ILLITERATE"),
    server = TRUE
  )
  
  # 2. Lakukan perhitungan clustering HANYA saat tombol ditekan
  cluster_results <- eventReactive(input$run_cluster, {
    
    # Validasi: pastikan pengguna memilih minimal 2 variabel
    validate(
      need(length(input$cluster_vars) >= 2, "Silakan pilih minimal 2 variabel untuk clustering.")
    )
    
    # Persiapan data
    data_for_clustering <- sovi_data %>%
      select(all_of(input$cluster_vars)) %>%
      na.omit() # Hapus baris dengan nilai NA
    
    # PENTING: Lakukan scaling
    data_scaled <- scale(data_for_clustering)
    
    # Jalankan algoritma K-Means
    set.seed(123) # Agar hasilnya selalu sama
    kmeans_model <- kmeans(data_scaled, centers = input$cluster_k, nstart = 25)
    
    # Buat tabel ringkasan
    summary_table <- data_for_clustering %>%
      mutate(Klaster = as.factor(kmeans_model$cluster)) %>%
      group_by(Klaster) %>%
      summarise(across(everything(), list(mean = mean, median = median))) %>%
      mutate(Jumlah_Anggota = as.integer(table(kmeans_model$cluster)))
    
    # Kembalikan hasil dalam sebuah list
    list(
      model = kmeans_model,
      summary = summary_table
    )
  })
  
  # 3. Render Peta Hasil Clustering
  output$cluster_map <- renderLeaflet({
    
    # Ambil hasil dari eventReactive
    results <- cluster_results()
    
    # Tambahkan kolom klaster ke data spasial utama
    data_to_map <- spatial_data %>%
      mutate(cluster = as.factor(results$model$cluster))
    
    # Buat palet warna untuk klaster
    pal <- colorFactor(palette = "viridis", domain = data_to_map$cluster)
    
    # Buat label popup
    popup_content <- paste0(
      "<strong>", data_to_map$WADMKK, "</strong><br>",
      "Provinsi: ", data_to_map$WADMPR, "<br>",
      "<strong>Klaster: ", data_to_map$cluster, "</strong>"
    )
    
    # Buat peta
    leaflet(data_to_map) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(cluster),
        weight = 1, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.7,
        label = lapply(popup_content, htmltools::HTML)
      ) %>%
      addLegend(pal = pal, values = ~cluster, opacity = 0.7, title = "Klaster", position = "bottomright")
  })
  
  # 4. Render Tabel Ringkasan
  output$cluster_summary_table <- DT::renderDataTable({
    
    # Ambil tabel ringkasan dari hasil
    summary_tbl <- cluster_results()$summary
    
    DT::datatable(
      summary_tbl,
      options = list(scrollX = TRUE, pageLength = 5),
      rownames = FALSE,
      caption = "Rata-rata (mean) dan Median dari setiap variabel per klaster."
    ) %>% DT::formatRound(columns = which(sapply(summary_tbl, is.numeric)), digits = 2)
    
  })
  
  
  
  # 5. Render Teks Interpretasi Dinamis (VERSI FINAL)
  output$cluster_interpretation_text <- renderText({
    cluster_interpretation_reactive()
  })
  
  # LETAKKAN DUA BLOK INI DI DALAM FUNGSI server
  
  # Reactive untuk Teks Interpretasi Moran's I
  moran_interpretation_reactive <- eventReactive(input$run_moran, {
    result <- moran_calculation()
    if (!is.null(result$error)) { return("") }
    
    p_val <- result$p.value
    moran_I <- result$estimate[1]
    
    if (p_val < 0.05) {
      if (moran_I > 0) {
        paste("Hasilnya signifikan (p < 0.05). Nilai Moran's I positif (", round(moran_I, 4), ") menunjukkan adanya pola MENGELOMPOK (clustered). Wilayah dengan nilai serupa (tinggi-tinggi atau rendah-rendah) cenderung berdekatan.")
      } else {
        paste("Hasilnya signifikan (p < 0.05). Nilai Moran's I negatif (", round(moran_I, 4), ") menunjukkan adanya pola MENYEBAR (dispersed). Wilayah dengan nilai tinggi cenderung berdekatan dengan wilayah bernilai rendah.")
      }
    } else {
      paste("Hasilnya tidak signifikan (p >= 0.05). Tidak ada cukup bukti adanya pola spasial. Distribusi nilai variabel di seluruh wilayah cenderung ACAK (random).")
    }
  })
  
  # Reactive untuk Teks Interpretasi Klaster
  cluster_interpretation_reactive <- eventReactive(input$run_cluster, {
    results <- cluster_results()
    summary_tbl <- results$summary
    
    overall_stats <- sovi_data %>%
      select(all_of(input$cluster_vars)) %>%
      summarise(across(everything(), list(mean = mean, sd = sd), na.rm = TRUE))
    
    final_interpretation <- paste0(
      "Analisis mengelompokkan ", nrow(spatial_data), " wilayah ke dalam ", input$cluster_k, 
      " klaster dengan profil kerentanan yang unik:\n\n"
    )
    
    for (i in 1:nrow(summary_tbl)) {
      cluster_num <- summary_tbl$Klaster[i]
      cluster_size <- summary_tbl$Jumlah_Anggota[i]
      
      z_scores <- c()
      var_names <- c()
      for (var in input$cluster_vars) {
        clean_var_name <- sub("\\.x$|\\.y$", "", var)
        cluster_mean <- summary_tbl[[paste0(var, "_mean")]][i]
        overall_mean <- overall_stats[[paste0(var, "_mean")]]
        overall_sd   <- overall_stats[[paste0(var, "_sd")]]
        
        z <- (cluster_mean - overall_mean) / overall_sd
        z_scores <- c(z_scores, z)
        var_names <- c(var_names, clean_var_name)
      }
      names(z_scores) <- var_names
      
      most_extreme_var_name <- names(which.max(abs(z_scores)))
      most_extreme_z_value  <- z_scores[most_extreme_var_name]
      archetype_level       <- if (most_extreme_z_value > 0.5) "Tinggi" else if (most_extreme_z_value < -0.5) "Rendah" else "Moderat"
      archetype_title       <- if (archetype_level != "Moderat") {
        paste0('"', most_extreme_var_name, ' ', archetype_level, '"')
      } else {
        '"Profil Moderat"'
      }
      
      cluster_text <- paste0("🔹 **Klaster ", cluster_num, " - ", archetype_title, "** (", cluster_size, " anggota): ")
      
      sorted_z <- sort(z_scores, decreasing = TRUE)
      top_var <- names(sorted_z)[1]
      bottom_var <- names(sorted_z)[length(sorted_z)]
      
      description <- ""
      if (archetype_level == "Moderat") {
        description <- "Profilnya cenderung seimbang dan mendekati rata-rata keseluruhan pada semua variabel."
      } else if (top_var == bottom_var) {
        description <- paste0("Secara dominan ditandai oleh nilai **", top_var, "** yang ", tolower(archetype_level), ".")
      } else {
        description <- paste0("Ciri utamanya adalah nilai **", top_var, "** yang sangat tinggi, sementara nilai **", bottom_var, "** sangat rendah dibandingkan klaster lain.")
      }
      
      final_interpretation <- paste0(final_interpretation, cluster_text, description, "\n\n")
    }
    return(final_interpretation)
  })
  
  # =====================================================================
  # [PENAMBAHAN SLM] --- ANALISIS SPATIAL LAG MODEL (SLM) - VERSI FINAL DENGAN MATRIKS JARAK
  # =====================================================================
  
  slm_results <- eventReactive(input$run_slm, {
    
    validate(
      need(!is.null(input$slm_y), "Pilih satu variabel dependen (Y)."),
      need(length(input$slm_x) > 0, "Pilih minimal satu variabel independen (X)."),
      need(!(input$slm_y %in% input$slm_x), "Variabel dependen tidak boleh sama dengan variabel independen.")
    )
    
    formula_slm <- as.formula(paste(input$slm_y, "~", paste(input$slm_x, collapse = " + ")))
    model_vars <- c(input$slm_y, input$slm_x)
    
    # --- PERBAIKAN UTAMA: KEMBALI MENGGUNAKAN MATRIKS JARAK DENGAN BENAR ---
    
    # 1. Dapatkan INDEKS NUMERIK dari baris-baris yang datanya lengkap (tanpa NA)
    complete_cases_idx <- which(complete.cases(sovi_data[, model_vars]))
    
    validate(need(length(complete_cases_idx) > (length(model_vars) + 1), "Data tidak cukup setelah menghapus NA."))
    
    # 2. Filter data tabel menggunakan indeks
    data_clean <- sovi_data[complete_cases_idx, ]
    
    # 3. Filter MATRIKS JARAK MENTAH (raw) menggunakan indeks yang sama untuk baris dan kolom
    dist_matrix_clean <- dist_matrix_raw[complete_cases_idx, complete_cases_idx]
    
    # 4. Buat bobot dari matriks jarak yang sudah bersih MENGGUNAKAN FORMULA STABIL
    #    Menggunakan (jarak + 1) untuk menghindari pembagian dengan nol
    inv_dist_clean <- 1 / (dist_matrix_clean + 1)
    diag(inv_dist_clean) <- 0
    weights_clean <- mat2listw(inv_dist_clean, style = "W", zero.policy = TRUE)
    # --- AKHIR DARI PERBAIKAN ---
    
    tryCatch({
      model <- lagsarlm(formula = formula_slm, data = data_clean, listw = weights_clean, zero.policy = TRUE)
      return(model)
    }, error = function(e) {
      # Memberikan pesan error yang lebih informatif jika terjadi
      if (grepl("different dimensions", e$message)) {
        return(paste("Gagal menjalankan model SLM: Dimensi data (", nrow(data_clean), " baris) tidak cocok dengan dimensi bobot spasial. Cek kembali proses filter data."))
      } else {
        return(paste("Gagal menjalankan model SLM:", e$message))
      }
    })
  })
  
  # 2. Tampilkan ringkasan model di UI
  output$slm_summary <- renderPrint({
    result <- slm_results()
    if (is.character(result)) { # Jika hasilnya adalah pesan error
      cat(result)
    } else { # Jika hasilnya adalah model
      summary(result)
    }
  })
  
  # 3. Buat interpretasi dinamis
  output$slm_interpretation <- renderText({
    result <- slm_results()
    validate(need(!is.character(result), "Interpretasi tidak tersedia karena model gagal dijalankan."))
    
    model_summary <- summary(result)
    rho <- model_summary$rho
    rho_pval <- model_summary$LR1$p.value # Ambil p-value dari Likelihood Ratio Test
    
    # Interpretasi Rho
    rho_interp <- if (rho_pval < 0.05) {
      if (rho > 0) {
        paste0("Koefisien lag spasial (Rho) sebesar ", round(rho, 4), " signifikan secara statistik. Ini menunjukkan adanya efek limpahan (spillover) positif yang kuat. Artinya, nilai variabel '", input$slm_y, "' di suatu wilayah dipengaruhi secara positif oleh nilai di wilayah tetangganya.")
      } else {
        paste0("Koefisien lag spasial (Rho) sebesar ", round(rho, 4), " signifikan secara statistik. Ini menunjukkan adanya efek limpahan (spillover) negatif. Artinya, nilai variabel '", input$slm_y, "' di suatu wilayah cenderung berlawanan dengan nilai di wilayah tetangganya (misal, tinggi dikelilingi rendah).")
      }
    } else {
      "Koefisien lag spasial (Rho) tidak signifikan. Ini menunjukkan bahwa efek ketergantungan spasial antar wilayah tidak cukup kuat untuk memengaruhi model setelah memperhitungkan variabel independen."
    }
    
    # Interpretasi Variabel Signifikan
    coef_table <- as.data.frame(model_summary$Coef)
    significant_vars <- rownames(coef_table)[coef_table$`Pr(>|z|)` < 0.05]
    significant_vars <- significant_vars[significant_vars != "(Intercept)"]
    
    vars_interp <- if (length(significant_vars) > 0) {
      paste0("Variabel independen yang berpengaruh signifikan adalah: ", paste(significant_vars, collapse = ", "), ".")
    } else {
      "Tidak ada variabel independen yang berpengaruh signifikan dalam model ini."
    }
    
    paste(rho_interp, vars_interp, sep = " \n")
  })
  
  # 4. Fungsi download laporan SLM
  output$download_slm_report <- downloadHandler(
    filename = function() {
      paste0("laporan_SLM_", input$slm_y, "_", Sys.Date(), ".docx")
    },
    content = function(file) {
      result <- slm_results()
      validate(need(!is.character(result), "Model gagal, laporan tidak dapat dibuat."))
      
      summary_output <- capture.output(summary(result))
      interp_output <- output$slm_interpretation()
      
      doc <- read_docx() %>%
        body_add_par("Laporan Spatial Lag Model (SLM)", style = "heading 1") %>%
        body_add_par(paste("Tanggal Analisis:", Sys.Date())) %>%
        body_add_par(paste("Variabel Dependen (Y):", input$slm_y)) %>%
        body_add_par(paste("Variabel Independen (X):", paste(input$slm_x, collapse = ", "))) %>%
        body_add_break() %>%
        body_add_par("Ringkasan Hasil Model", style = "heading 2")
      
      for(line in summary_output) { doc <- doc %>% body_add_par(line, style = "Courier New") }
      
      doc <- doc %>%
        body_add_break() %>%
        body_add_par("Interpretasi Hasil", style = "heading 2") %>%
        body_add_par(interp_output)
      
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
    moran_interpretation_reactive()
  })

  
  # =====================================================================
  # DOWNLOAD HANDLERS UNTUK ANALISIS SPASIAL (VERSI OPTIMASI)
  # =====================================================================
  
  # --- Download Laporan Moran's I (Word) ---
  # (Kode ini tidak berat, jadi tidak perlu diubah, tapi disertakan agar lengkap)
  output$download_moran_report <- downloadHandler(
    filename = function() {
      paste0("laporan_morans_i_", Sys.Date(), ".docx")
    },
    content = function(file) {
      moran_output <- capture.output(moran_calculation())
      moran_interp <- moran_interpretation_reactive()
      
      doc <- read_docx() %>%
        body_add_par("Laporan Analisis Autokorelasi Spasial (Moran's I)", style = "heading 1") %>%
        body_add_par(paste("Tanggal Analisis:", Sys.Date())) %>%
        body_add_par("Hasil Uji Moran's I", style = "heading 2")
      
      for(line in moran_output) { doc <- doc %>% body_add_par(line) }
      
      doc <- doc %>%
        body_add_par("Interpretasi", style = "heading 2") %>%
        body_add_par(moran_interp)
      
      print(doc, target = file)
    }
  )
  
  cluster_map_object <- eventReactive(input$run_cluster, {
    results <- cluster_results()
    data_to_map <- spatial_data %>%
      mutate(cluster = as.factor(results$model$cluster))
    
    pal <- colorFactor(palette = "viridis", domain = data_to_map$cluster)
    
    popup_content <- paste0(
      "<strong>", data_to_map$WADMKK, "</strong><br>",
      "Provinsi: ", data_to_map$WADMPR, "<br>",
      "<strong>Klaster: ", data_to_map$cluster, "</strong>"
    )
    
    leaflet(data_to_map) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(cluster),
        weight = 1, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.7,
        label = lapply(popup_content, htmltools::HTML)
      ) %>%
      addLegend(pal = pal, values = ~cluster, opacity = 0.7, title = "Klaster", position = "bottomright")
  })
  
  
  output$cluster_map <- renderLeaflet({ cluster_map_object() })
  
  
  # --- Download Peta Klaster (PNG) dengan NOTIFIKASI LOADING ---
  output$download_cluster_map_jpg <- downloadHandler(
    filename = function() { paste0("peta_klaster_", Sys.Date(), ".png") },
    content = function(file) {
      req(cluster_map_object())
      
      # Tampilkan notifikasi "Loading"
      withProgress(message = 'Membuat gambar peta...', value = 0.5, {
        
        # Gunakan mapshot dengan resolusi yang dioptimalkan
        mapview::mapshot(
          cluster_map_object(), 
          file = file,
          vwidth = 800, # Lebar gambar (lebih kecil, lebih cepat)
          vheight = 600  # Tinggi gambar
        )
        
        incProgress(0.5, detail = "Selesai!")
      })
    }
  )
  
  # --- Download Laporan Klaster (Word) ---
  # (Kode ini tidak berat, jadi tidak perlu diubah, tapi disertakan agar lengkap)
  output$download_cluster_report <- downloadHandler(
    filename = function() {
      paste0("laporan_klaster_", Sys.Date(), ".docx")
    },
    content = function(file) {
      summary_table <- cluster_results()$summary
      cluster_interp <- cluster_interpretation_reactive()
      
      doc <- read_docx() %>%
        body_add_par("Laporan Analisis Clustering (K-Means)", style = "heading 1") %>%
        body_add_par(paste("Tanggal Analisis:", Sys.Date())) %>%
        body_add_par("Ringkasan Karakteristik Klaster", style = "heading 2") %>%
        body_add_table(summary_table, style = "Table Professional") %>%
        body_add_par("Interpretasi", style = "heading 2") %>%
        body_add_par(cluster_interp)
      
      print(doc, target = file)
    }
  )
  
  # --- Download Laporan GABUNGAN (Word) dengan NOTIFIKASI LOADING ---
  output$download_spatial_full_report <- downloadHandler(
    filename = function() {
      paste0("laporan_lengkap_analisis_spasial_", Sys.Date(), ".docx")
    },
    content = function(file) {
      
      # Tampilkan notifikasi "Loading"
      withProgress(message = 'Menyusun laporan lengkap...', value = 0, {
        
        incProgress(0.2, detail = "Mengambil data Moran's I...")
        moran_output <- capture.output(moran_calculation())
        moran_interp <- moran_interpretation_reactive()
        
        incProgress(0.2, detail = "Mengambil data klaster...")
        summary_table <- cluster_results()$summary
        cluster_interp <- cluster_interpretation_reactive()
        
        incProgress(0.2, detail = "Membuat gambar peta (proses berat)...")
        temp_map_path <- tempfile(fileext = ".png")
        mapview::mapshot(
          cluster_map_object(), 
          file = temp_map_path,
          vwidth = 800, vheight = 600
        )
        
        incProgress(0.2, detail = "Menyusun dokumen Word...")
        doc <- read_docx() %>%
          # ... (sisa kode untuk menyusun docx sama seperti sebelumnya)
          body_add_par("Laporan Lengkap Analisis Spasial", style = "heading 1") %>%
          body_add_par(paste("Tanggal Analisis:", Sys.Date())) %>%
          body_add_par("1. Analisis Autokorelasi Spasial (Moran's I)", style = "heading 2") %>%
          body_add_par("Hasil Uji:", style = "heading 3")
        
        for(line in moran_output) { doc <- doc %>% body_add_par(line) }
        
        doc <- doc %>%
          body_add_par("Interpretasi:", style = "heading 3") %>%
          body_add_par(moran_interp) %>%
          body_add_break() %>%
          
          body_add_par("2. Analisis Clustering (K-Means)", style = "heading 2") %>%
          body_add_par("Peta Sebaran Klaster:", style = "heading 3") %>%
          body_add_img(src = temp_map_path, width = 6, height = 4.5) %>%
          body_add_par("Ringkasan Karakteristik Klaster:", style = "heading 3") %>%
          body_add_table(summary_table, style = "Table Professional") %>%
          body_add_par("Interpretasi Klaster:", style = "heading 3") %>%
          body_add_par(cluster_interp)
        
        incProgress(0.2, detail = "Menyimpan file...")
        print(doc, target = file)
      })
    }
  )
}

# ===============================================================================
# 5. MENJALANKAN APLIKASI
# ===============================================================================

# Jalankan aplikasi Shiny
shinyApp(ui = ui, server = server)
