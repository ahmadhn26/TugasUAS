# =================================================================
# SKRIP PERSIAPAN DATA FINAL (VERSI HEMAT MEMORI)
# =================================================================
library(sf)
library(dplyr)
library(spdep)
library(rmapshaper)

# Atur working directory
setwd("C:/UAS Komstat")

# --- LANGKAH 1: SIAPKAN DATA SECARA TERPISAH ---

# Muat data non-spasial
sovi_data <- read.csv("data/sovi_data.csv", header = TRUE)

# Muat data peta ASLI yang berat
peta_awal <- st_read("data/sovi_spasial_final.gpkg", quiet = TRUE)

# --- LANGKAH 2: SEDERHANAKAN GEOMETRI TERLEBIH DAHULU (INI KUNCINYA!) ---

# Lakukan rename, lalu SEGERA pilih HANYA kolom kunci dan geometri
peta_geom_saja <- peta_awal %>%
  rename(
    DISTRICTCODE = KODE_BERSIH,
    DISTRICT_NAME = WADMKK,
    PROVINCE_NAME = WADMPR
  ) %>%
  select(DISTRICTCODE, DISTRICT_NAME, PROVINCE_NAME, geom)

# Lakukan simplifikasi pada objek yang jauh lebih kecil ini (lebih hemat RAM)
peta_sederhana <- rmapshaper::ms_simplify(peta_geom_saja, keep = 0.05, keep_shapes = TRUE)

# Hapus objek peta besar yang sudah tidak terpakai untuk membebaskan RAM
rm(peta_awal, peta_geom_saja)


# --- LANGKAH 3: SINKRONKAN DAN GABUNGKAN DATA YANG SUDAH RINGAN ---

# Cari ID yang sama antara data csv dan peta yang sudah sederhana
sovi_data$DISTRICTCODE <- as.character(sovi_data$DISTRICTCODE)
peta_sederhana$DISTRICTCODE <- as.character(peta_sederhana$DISTRICTCODE)
common_ids <- intersect(sovi_data$DISTRICTCODE, peta_sederhana$DISTRICTCODE)

# Filter kedua dataset
sovi_data_synced <- sovi_data %>% filter(DISTRICTCODE %in% common_ids)
peta_sederhana_synced <- peta_sederhana %>% filter(DISTRICTCODE %in% common_ids)

# Gabungkan menjadi satu dataset final yang bersih
spatial_data_final <- left_join(peta_sederhana_synced, sovi_data_synced, by = "DISTRICTCODE")

# Simpan data peta final yang siap pakai
st_write(spatial_data_final, "data/peta_final_siap_pakai.gpkg", delete_layer = TRUE)


# --- LANGKAH 4: BUAT BOBOT SPASIAL YANG SUDAH SINKRON ---
distance_df <- read.csv("data/distance.csv", header = TRUE, row.names = 1)
distance_matrix <- as.matrix(distance_df)

# Filter matriks jarak agar sesuai dengan ID yang sinkron
matched_indices <- match(common_ids, rownames(distance_matrix))
# Hapus NA jika ada ID yang tidak cocok
matched_indices <- matched_indices[!is.na(matched_indices)] 
distance_matrix_synced <- distance_matrix[matched_indices, matched_indices]

# Buat dan simpan file bobot spasial yang baru
inv_dist_matrix <- 1 / distance_matrix_synced
diag(inv_dist_matrix) <- 0
spatial_weights_final <- mat2listw(inv_dist_matrix, style = "W")
saveRDS(spatial_weights_final, file = "data/spatial_weights_final.rds")

print("SEMUA DATA SUDAH SINKRON DAN SIAP PAKAI! Proses selesai tanpa crash.")
# =================================================================