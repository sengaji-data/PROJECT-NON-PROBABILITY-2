# NAMA : MUHAMAD FAUJAN ANJAR ALMAHJI SENGAJI
# NIM : G1F02410059
# PROGRAM STUDI : STATISTIKA
# ANALISIS KEPUASAN MAHASISWA STATISTIKA TERHADAP LAYANAN BELANJA ONLINE SHOPEE

## LATAR BELAKANG
Perkembangan teknologi informasi telah mengubah cara masyarakat berbelanja, salah satunya melalui platform e-commerce seperti Shopee yang banyak digunakan di Indonesia. Shopee menyediakan berbagai layanan unggulan seperti kemudahan penggunaan aplikasi, informasi produk yang jelas, keamanan transaksi, metode pembayaran yang beragam, serta promo dan diskon yang menarik. Kualitas layanan tersebut menjadi faktor utama yang memengaruhi kepuasan pengguna, di mana pengguna yang puas cenderung akan kembali menggunakan Shopee untuk berbelanja.
Mahasiswa Statistika merupakan kelompok pengguna yang aktif dan mampu memberikan penilaian yang objektif terhadap kualitas layanan Shopee berdasarkan latar belakang keilmuan mereka di bidang analisis data. Oleh karena itu, penelitian ini dilakukan menggunakan teknik convenience sampling dengan menyebarkan kuesioner yang mencakup aspek kemudahan penggunaan, kualitas informasi, keamanan transaksi, metode pembayaran, dan promosi. Data dianalisis secara deskriptif untuk menggambarkan tingkat kepuasan mahasiswa Statistika terhadap layanan belanja online Shopee, sehingga hasilnya dapat menjadi sumber informasi yang berguna bagi pengembangan layanan platform tersebut.

## RUMUSAN MASALAH
1. Bagaimana tingkat kepuasan mahasiswa Statistika terhadap layanan belanja online Shopee?
2. Aspek layanan apa yang paling memengaruhi kepuasan mahasiswa Statistika dalam menggunakan Shopee?

## TUJUAN PENELITIAN
1.  mengetahui tingkat kepuasan mahasiswa Statistika terhadap layanan belanja online Shopee.
2.  Untuk mengidentifikasi aspek layanan yang paling memengaruhi kepuasan mahasiswa Statistika dalam menggunakan Shopee.

## METODE PENELITIAN
Penelitian ini menggunakan teknik nonprobability sampling dengan metode convenience sampling. Populasi dalam penelitian ini adalah mahasiswa Statistika yang pernah berbelanja menggunakan Shopee sebanyak 154 orang. Penentuan jumlah sampel menggunakan rumus Slovin:

$$n = \frac{N}{1 + N \cdot e^2} = \frac{154}{1 + 154(0{,}12)^2} \approx 63 \text{ responden}$$

Sehingga diperoleh responden sebanyak 63 mahasiswa Statistika. Data dikumpulkan melalui kuesioner dengan 10 pernyataan menggunakan skala Likert 1–5 dengan keterangan sebagai berikut:

| Skor | Keterangan |
|------|------------|
| 1 | Sangat Tidak Setuju |
| 2 | Tidak Setuju |
| 3 | Netral |
| 4 | Setuju |
| 5 | Sangat Setuju |

Analisis data dilakukan secara deskriptif untuk menggambarkan penilaian responden terhadap kualitas layanan Shopee, tingkat kepuasan, dan niat untuk menggunakan kembali aplikasi Shopee.

## ANALISIS DATA
Analisis data dalam penelitian ini dilakukan menggunakan software R Studio dengan pendekatan statistik deskriptif. Sebelum melakukan analisis, terlebih dahulu dilakukan uji validitas dan uji reliabilitas untuk memastikan bahwa instrumen penelitian yang digunakan layak dan konsisten. Data yang telah dinyatakan valid dan reliabel kemudian dianalisis secara deskriptif untuk menggambarkan tingkat kepuasan mahasiswa Statistika terhadap layanan belanja online Shopee berdasarkan setiap aspek yang diukur. Tahapan analisis data adalah sebagai berikut:

### UJI VALIDITAS
Uji validitas dilakukan untuk mengetahui apakah setiap item pernyataan dalam kuesioner mampu mengukur apa yang seharusnya diukur. Pengujian menggunakan metode korelasi Pearson dengan membandingkan nilai r hitung setiap item terhadap r tabel. Dengan jumlah responden 63 mahasiswa dan taraf signifikansi 5%, diperoleh nilai r tabel sebesar 0,2483. Apabila r hitung > r tabel maka item dinyatakan valid.

skor_total <- rowSums(item)
r_tabel    <- 0.2483

```r
skor_total <- rowSums(item)
r_tabel    <- 0.2483

for (i in 1:ncol(item)) {
  r_hitung <- cor(item[, i], skor_total, method = "pearson")
  status   <- ifelse(r_hitung > r_tabel, "VALID ", "TIDAK VALID ")
  cat(sprintf("%-20s | r hitung = %.4f | %s\n", colnames(item)[i], r_hitung, status))
}
```

### UJI RELIABILITAS
Uji reliabilitas dilakukan untuk mengetahui konsistensi dan keandalan instrumen penelitian yang digunakan. Pengujian menggunakan metode Cronbach's Alpha, di mana instrumen dinyatakan reliabel apabila nilai Alpha ≥ 0,70. Semakin tinggi nilai Alpha, semakin konsisten instrumen tersebut dalam mengukur variabel yang diteliti.

```r
alpha_val <- alpha(item)$total$raw_alpha
status_r  <- ifelse(alpha_val >= 0.7, "RELIABEL ", "TIDAK RELIABEL ")
cat(sprintf("Cronbach's Alpha = %.4f | %s\n", alpha_val, status_r))
```
### STATISTIK DESKRIPTIF
Statistik deskriptif digunakan untuk menggambarkan karakteristik data secara umum. Analisis ini meliputi nilai rata-rata (mean), nilai tengah (median), dan standar deviasi (SD) dari setiap aspek layanan Shopee, sehingga dapat diketahui gambaran umum penilaian responden terhadap masing-masing aspek.

```r
desc <- describe(item)
print(round(desc[, c("mean", "median", "sd", "min", "max")], 2))
```
### RATA-RATA PER ASPEK
Rata-rata per aspek dihitung untuk mengidentifikasi aspek layanan mana yang memiliki nilai kepuasan tertinggi dan terendah menurut penilaian mahasiswa Statistika. Hasil ini kemudian diurutkan dari nilai tertinggi ke terendah untuk memudahkan interpretasi.

```r
rata <- colMeans(item)
rata_sorted <- sort(rata, decreasing = TRUE)
print(round(rata_sorted, 2))
```
### RATA-RATA TOTAL KEPUASAN
Rata-rata total kepuasan dihitung untuk menentukan kategori tingkat kepuasan mahasiswa Statistika secara keseluruhan terhadap layanan belanja online Shopee. Kategorisasi dilakukan berdasarkan nilai rata-rata, di mana nilai ≥ 4 dikategorikan puas, nilai 3–4 dikategorikan cukup puas, dan nilai < 3 dikategorikan tidak puas.

```r
skor_total <- rowMeans(item)
kategori <- ifelse(mean(skor_total) >= 4, "PUAS ",
            ifelse(mean(skor_total) >= 3, "CUKUP PUAS ", "TIDAK PUAS "))
cat(sprintf("Rata-rata Keseluruhan = %.2f | %s\n", mean(skor_total), kategori))
```
