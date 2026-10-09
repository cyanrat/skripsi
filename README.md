# StatSkripsi

Aplikasi web R Shiny untuk eksplorasi data sekunder kuantitatif dan kualitatif.

## Fitur
- Impor CSV, XLS, dan XLSX
- Audit jumlah baris/kolom, NA, duplikat persis, dan kolom konstan
- Selector multi-pilih yang dapat dicari untuk menandai kolom kualitatif; kolom lain otomatis diperlakukan sebagai kuantitatif
- Statistik deskriptif numerik dan tabel frekuensi variabel kategorik
- Korelasi Pearson/Spearman
- Regresi linear sederhana dan grafik diagnostik residual
- Ekspor tabel deskriptif/frekuensi ke CSV dan draf Bab IV ke TXT

## Kebutuhan
- R (versi modern yang masih didukung)
- RStudio Desktop (disarankan)
- Paket `shiny` dan `readxl`

## Instalasi
Buka Console R lalu jalankan:

```r
install.packages(c("shiny", "readxl"))
```

## Menjalankan
Buka folder proyek di RStudio, buka `app.R`, lalu klik **Run App**.
Atau dari Console, jalankan:

```r
shiny::runApp("PATH/KE/StatSkripsi")
```

Di VS Code, pilih **Terminal → Run Task → Run StatSkripsi locally**. Aplikasi akan tersedia di <http://127.0.0.1:3839>.
Antarmuka HTML dibuat dan disajikan oleh Shiny saat aplikasi berjalan; aplikasi ini memerlukan R dan tidak dapat digunakan sebagai file HTML statis mandiri.

## Format input
- CSV dengan pemisah koma dan titik sebagai desimal
- Excel XLS/XLSX
- Setiap baris sebaiknya merepresentasikan satu unit observasi
- Kolom numerik yang terbaca sebagai teks dapat ditetapkan sebagai kuantitatif jika nilainya bisa dikonversi ke angka
- Kolom angka berkode kategori (misalnya 1 = kontrol, 2 = perlakuan) dapat ditetapkan sebagai kualitatif sebelum analisis
- Pilih kolom kualitatif pada selector yang dapat dicari di panel kiri. Kolom yang tidak dipilih otomatis menjadi kuantitatif; pilihan deskriptif, frekuensi, korelasi, dan regresi mengikuti klasifikasi tersebut

## Batasan penting
- Regresi hanya regresi linear sederhana dengan satu prediktor.
- Nilai kuantitatif berformat teks harus dapat dibaca sebagai angka menggunakan titik desimal; teks lain akan ditampilkan sebagai kesalahan agar dapat dibersihkan.
- Pemeriksaan NA tidak mengidentifikasi semua bentuk data tidak valid.
- Aplikasi tidak otomatis menentukan metode terbaik, menghapus data, atau membuktikan kausalitas.
- Periksa asumsi, desain penelitian, definisi variabel, sumber data, dan kebijakan pembimbing sebelum memakai hasil di skripsi.
