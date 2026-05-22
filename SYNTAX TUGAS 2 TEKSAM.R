# ============================================
#   UJI VALIDITAS, RELIABILITAS & ANALISIS
#   DESKRIPTIF - KEPUASAN SHOPEE
#   n = 63 Mahasiswa Statistika
# ============================================

library(readxl)
library(psych)

# IMPORT DATA
data <- read_excel("D:/Kuliah/Semester 4/TEKNIK SAMPLING/TUGAS 2/DATA TUGAS 2.xlsx")

# Ambil semua 10 item
item <- data[, 1:10]

# Beri nama item
colnames(item) <- c("Kemudahan Aplikasi", "Kemudahan Fitur",
                    "Kecepatan Transaksi", "Kualitas Informasi",
                    "Pilihan Produk", "Keamanan Transaksi",
                    "Metode Pembayaran", "Promo & Diskon",
                    "Kepuasan Layanan", "Niat Pakai Ulang")

# 1. UJI VALIDITAS
cat("====== UJI VALIDITAS ======\n")
skor_total <- rowSums(item)
r_tabel    <- 0.2483

for (i in 1:ncol(item)) {
  r_hitung <- cor(item[, i], skor_total, method = "pearson")
  status   <- ifelse(r_hitung > r_tabel, "VALID", "TIDAK VALID")
  cat(sprintf("%-20s | r hitung = %.4f | %s\n", colnames(item)[i], r_hitung, status))
}

# 2. UJI RELIABILITAS
cat("\n====== UJI RELIABILITAS ======\n")
alpha_val <- alpha(item)$total$raw_alpha
status_r  <- ifelse(alpha_val >= 0.7, "RELIABEL", "TIDAK RELIABEL")
cat(sprintf("Cronbach's Alpha = %.4f | %s\n", alpha_val, status_r))

# 3. STATISTIK DESKRIPTIF
cat("\n====== STATISTIK DESKRIPTIF ======\n")
desc <- describe(item)
print(round(desc[, c("mean", "median", "sd", "min", "max")], 2))

# 4. RATA-RATA PER ASPEK
cat("\n====== RATA-RATA PER ASPEK ======\n")
rata <- colMeans(item)
rata_sorted <- sort(rata, decreasing = TRUE)
print(round(rata_sorted, 2))
cat(sprintf("\nAspek Tertinggi : %s (%.2f)\n", names(rata_sorted)[1], rata_sorted[1]))
cat(sprintf("Aspek Terendah  : %s (%.2f)\n", names(rata_sorted)[length(rata_sorted)], rata_sorted[length(rata_sorted)]))

# 5. RATA-RATA TOTAL KEPUASAN
cat("\n====== RATA-RATA TOTAL KEPUASAN ======\n")
skor_total <- rowMeans(item)
cat(sprintf("Rata-rata Keseluruhan = %.2f dari skala 5\n", mean(skor_total)))
kategori <- ifelse(mean(skor_total) >= 4, "PUAS",
                   ifelse(mean(skor_total) >= 3, "CUKUP PUAS", "TIDAK PUAS"))
cat(sprintf("Kategori Kepuasan     = %s\n", kategori))