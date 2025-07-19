# =================================================================
# APLIKASI SHINY SEDERHANA UNTUK MENAMPILKAN PETA
# =================================================================

# Langkah 1: Memuat semua library yang diperlukan
library(shiny)
library(shinydashboard)
library(sf)
library(leaflet)

# Langkah 2: Memuat data peta (dilakukan satu kali saat aplikasi dimulai)
# Pastikan file berada di dalam subfolder "data"
peta_sovi <- st_read("data/sovi_spasial_final.gpkg", quiet = TRUE)

# Dapatkan nama kolom untuk pilihan dropdown, kecuali kolom geometri
pilihan_kolom <- names(peta_sovi)[names(peta_sovi) != "geom"]


# =================================================================
# User Interface (UI) - Tampilan dashboard
# =================================================================
ui <- dashboardPage(
  skin = "purple",
  
  # Header
  dashboardHeader(title = "Peta SOVI Indonesia"),
  
  # Sidebar - Tempat meletakkan input kontrol
  dashboardSidebar(
    # Membuat input dropdown untuk memilih variabel/kolom
    selectInput(
      inputId = "pilihan_variabel",
      label = "Pilih Variabel untuk Ditampilkan:",
      choices = pilihan_kolom,
      selected = "wadmkk" # Variabel default yang ditampilkan pertama kali
    )
  ),
  
  # Body - Tempat menampilkan output utama
  dashboardBody(
    fluidRow(
      box(
        title = "Peta Interaktif",
        status = "primary",
        solidHeader = TRUE,
        width = 12, # Lebar box (maksimal 12)
        # Output untuk peta leaflet, dengan tinggi 85% dari layar
        leafletOutput("peta_interaktif", height = "85vh")
      )
    )
  )
)


# =================================================================
# Server - Logika dan pemrosesan di belakang layar
# =================================================================
server <- function(input, output, session) {
  
  # Membuat output peta interaktif
  output$peta_interaktif <- renderLeaflet({
    
    # Ambil kolom yang dipilih oleh pengguna dari input dropdown
    kolom_terpilih <- input$pilihan_variabel
    data_kolom <- peta_sovi[[kolom_terpilih]]
    
    # Membuat palet warna
    # Jika datanya numerik, gunakan palet gradasi. Jika bukan, gunakan palet kategori.
    if (is.numeric(data_kolom)) {
      pal <- colorNumeric(palette = "viridis", domain = data_kolom, na.color = "transparent")
    } else {
      pal <- colorFactor(palette = "Set3", domain = data_kolom, na.color = "transparent")
    }
    
    # Membuat teks untuk popup saat poligon diklik
    popup_info <- paste0(
      "<strong>Kab/Kota: </strong>", peta_sovi$wadmkk, "<br>",
      "<strong>", kolom_terpilih, ": </strong>", data_kolom
    )
    
    # Merender peta Leaflet
    leaflet(data = peta_sovi) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% # Peta dasar yang bersih
      addPolygons(
        fillColor = ~pal(data_kolom), # Warna poligon berdasarkan palet
        fillOpacity = 0.8,
        weight = 1,
        color = "white",
        popup = popup_info # Tambahkan popup
      ) %>%
      addLegend(
        pal = pal,
        values = data_kolom,
        opacity = 0.7,
        title = kolom_terpilih,
        position = "bottomright"
      ) %>%
      setView(lng = 118, lat = -2, zoom = 5) # Set view awal di tengah Indonesia
  })
}


# =================================================================
# Menjalankan aplikasi
# =================================================================
shinyApp(ui = ui, server = server)