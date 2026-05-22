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
## HASIL DAN PEMBAHASAN
### HASIL UJI VALIDITAS
Uji validitas dilakukan untuk memastikan bahwa setiap item pernyataan dalam kuesioner mampu mengukur apa yang seharusnya diukur. Pengujian dilakukan menggunakan metode korelasi Pearson dengan membandingkan nilai r hitung setiap item terhadap r tabel. Dengan jumlah responden sebanyak 63 mahasiswa dan taraf signifikansi 5%, diperoleh nilai r tabel sebesar 0,2483. Suatu item dinyatakan valid apabila nilai r hitung lebih besar dari r tabel.
Berdasarkan hasil pengujian, diperoleh hasil sebagai berikut:

| Item | r Hitung | r Tabel | Status |
|:---|:---:|:---:|:---:|
| Kemudahan Aplikasi | 0.8323 | 0.2483 | VALID  |
| Kemudahan Fitur | 0.8190 | 0.2483 | VALID |
| Kecepatan Transaksi | 0.8151 | 0.2483 | VALID  |
| Kualitas Informasi | 0.7324 | 0.2483 | VALID  |
| Pilihan Produk | 0.8034 | 0.2483 | VALID  |
| Keamanan Transaksi | 0.8602 | 0.2483 | VALID  |
| Metode Pembayaran | 0.8073 | 0.2483 | VALID  |
| Promo & Diskon | 0.5909 | 0.2483 | VALID  |
| Kepuasan Layanan | 0.8292 | 0.2483 | VALID  |
| Niat Pakai Ulang | 0.8267 | 0.2483 | VALID  |

Hasil pengujian menunjukkan bahwa seluruh 10 item pernyataan memiliki nilai r hitung yang lebih besar dari r tabel (0,2483). Dengan demikian, semua item pernyataan dinyatakan valid dan layak digunakan sebagai instrumen penelitian untuk mengukur kepuasan mahasiswa Statistika terhadap layanan Shopee.

### HASIL UJI RELIABILITAS
Uji reliabilitas dilakukan untuk mengetahui sejauh mana instrumen penelitian dapat dipercaya dan konsisten dalam menghasilkan data yang sama apabila digunakan pada waktu yang berbeda. Pengujian menggunakan metode Cronbach's Alpha dengan kriteria nilai Alpha ≥ 0,70 dinyatakan reliabel.
Hasil uji reliabilitas menunjukkan nilai Cronbach's Alpha sebesar 0,9298. Nilai tersebut jauh melampaui batas minimum 0,70 dan masuk dalam kategori excellent, yang berarti instrumen penelitian memiliki tingkat konsistensi yang sangat tinggi. Dengan demikian, instrumen yang digunakan dalam penelitian ini dinyatakan reliabel dan dapat dipercaya untuk mengukur kepuasan mahasiswa Statistika terhadap layanan belanja online Shopee.

### HASIL STATISTIK DESKRIPTIF
Analisis statistik deskriptif dilakukan untuk menggambarkan penilaian mahasiswa Statistika terhadap setiap aspek layanan Shopee. Hasil analisis meliputi nilai rata-rata (mean), nilai tengah (median), dan standar deviasi (SD) dari masing-masing aspek yang diukur. Berikut hasil statistik deskriptif yang diperoleh:

| Aspek | Mean | Median | SD |
|:---|:---:|:---:|:---:|
| Pilihan Produk | 4.60 | 5.0 | 0.71 |
| Niat Pakai Ulang | 4.52 | 5.0 | 0.67 |
| Kemudahan Aplikasi | 4.48 | 5.0 | 0.74 |
| Kemudahan Fitur | 4.39 | 4.0 | 0.66 |
| Metode Pembayaran | 4.37 | 4.0 | 0.61 |
| Kepuasan Layanan | 4.35 | 4.5 | 0.75 |
| Kualitas Informasi | 4.32 | 4.0 | 0.65 |
| Kecepatan Transaksi | 4.31 | 4.0 | 0.78 |
| Promo & Diskon | 4.31 | 5.0 | 0.90 |
| Keamanan Transaksi | 4.16 | 4.0 | 0.81 |

Berdasarkan tabel di atas, aspek Pilihan Produk memiliki nilai rata-rata tertinggi sebesar 4,60 dengan standar deviasi 0,71. Hal ini menunjukkan bahwa mahasiswa Statistika merasa sangat puas dengan banyaknya pilihan produk yang tersedia di Shopee. Sementara itu, aspek Keamanan Transaksi memiliki nilai rata-rata terendah sebesar 4,16 dengan standar deviasi 0,81. Meskipun demikian, nilai tersebut masih tergolong tinggi dan menunjukkan bahwa mahasiswa Statistika tetap merasa puas terhadap aspek keamanan transaksi di Shopee, namun aspek ini perlu mendapat perhatian lebih untuk ditingkatkan dibandingkan aspek lainnya.

### HASIL KEPUASAN KESELURUHAN
Berdasarkan hasil analisis secara keseluruhan, rata-rata total kepuasan mahasiswa Statistika terhadap layanan belanja online Shopee adalah sebesar 4,38 dari skala 5. Nilai tersebut masuk dalam kategori PUAS, yang berarti mahasiswa Statistika secara keseluruhan merasa puas terhadap layanan yang diberikan oleh Shopee. Aspek yang paling memuaskan adalah pilihan produk yang tersedia, sedangkan aspek keamanan transaksi menjadi aspek yang perlu ditingkatkan meskipun masih dinilai baik oleh responden. Secara umum, Shopee dinilai telah memberikan layanan belanja online yang baik dan memuaskan bagi mahasiswa Statistika.

## KESIMPULAN
Berdasarkan hasil analisis data yang telah dilakukan, dapat ditarik kesimpulan sebagai berikut:
1. Tingkat kepuasan mahasiswa Statistika terhadap layanan belanja online Shopee secara keseluruhan berada pada kategori PUAS dengan nilai rata-rata sebesar 4,38 dari skala 5. Hal ini menunjukkan bahwa mahasiswa Statistika merasa puas terhadap layanan yang diberikan oleh Shopee, mulai dari kemudahan penggunaan aplikasi, kualitas informasi produk, keamanan transaksi, metode pembayaran, hingga promo dan diskon yang ditawarkan.
2. Aspek layanan yang paling memengaruhi kepuasan mahasiswa Statistika dalam menggunakan Shopee adalah Pilihan Produk dengan nilai rata-rata tertinggi sebesar 4,60. Sementara itu, aspek Keamanan Transaksi memiliki nilai rata-rata terendah sebesar 4,16 dan menjadi aspek yang perlu mendapat perhatian lebih untuk ditingkatkan oleh Shopee ke depannya.

## LINK QUISIONER
:https://docs.google.com/forms/d/e/1FAIpQLSdWQCO4jW8j_Qj5H0zu2nJ_yNI_TaKH4qkgGczibIzheZj06g/viewform?usp=publish-editor
