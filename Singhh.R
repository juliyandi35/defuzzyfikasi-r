library(readxl)
library(AnalyzeTS)
library(dplyr)
library(timeSeries)
library(forecast)
library(fuzzyforest)
library(fuzzySim)
file_path <- file.choose()
data_cabai<- read_excel(file_path,range = "A1:C426")

data_cabai$harga
data=data_cabai$harga
data
# Mengambil data training
train_percentage <- 0.9
train_size <- ceiling(train_percentage * length(data))
data_training <- data[1:train_size]
data_testing <- data[train_size:length(data)]

#peramalan singh
data.ts=ts(data_training, start=c(2023,1), frequency = 365)
data.ts
singh = fuzzy.ts1(data.ts, n=10, D1=7000, D2=1000,type = "Singh", bin = NULL, trace = TRUE, plot = TRUE)
singh
chen= fuzzy.ts1(data.ts, n=10, D1=7000, D2=1000,type = "Chen", bin = NULL, trace = TRUE, plot = TRUE)
chen

library(writexl)
# Menyimpan hasil ke file Excel
#write_xlsx(singh$table2, "C:/Users/user/Documents/peramalan singh1.xlsx")
write_xlsx(singh$table2, "peramalan singh1.xlsx")

#manual

# Memilih file dan membaca data
file_path <- file.choose()
data <- read_excel(file_path)
data <- data$harga

# Mengambil data training
train_percentage <- 0.9
train_size <- ceiling(train_percentage * length(data))
data_training <- data[1:train_size]
data_testing <- data[train_size:length(data)]

# Menampilkan jumlah data training
n <- length(data_training)
n

# Plot data training
plot(data_training, xlab = "Data", ylab = "Harga Cabai Rawit", type = "l")

# Data maksimum dan minimum
minimal <- min(data_training)
minimal
maksimal <- max(data_training)
maksimal

# Batas atas dan bawah untuk himpunan semesta menggunakan D1 & D2
D1 <- 7000
D2 <- 1000
min.baru <- minimal - D1
min.baru
maks.baru <- maksimal + D2
maks.baru

# Banyak kelas dan panjang interval
k <- round(1 + (3.322 * log10(length(data_training))))
k
l <- (maks.baru - min.baru) / k
l

# Batas interval
batasinterval <- seq(min.baru, maks.baru, length.out = k + 1)
batasinterval

# Pembagian interval dan membentuk himpunan fuzzy
interval <- data.frame(matrix(NA, nrow = length(batasinterval) - 1, ncol = 3))
names(interval) <- c("bawah", "atas", "kel")

for (i in 1:(length(batasinterval) - 1)) {
  interval[i, 1] <- batasinterval[i]
  interval[i, 2] <- batasinterval[i + 1]
  interval[i, 3] <- i
}
interval

# Nilai tengah interval
n.tengah <- data.frame(tengah = (interval[, 1] + interval[, 2]) / 2, kel = interval[, 3])
n.tengah 

# Fuzzifikasi
fuzzifikasi_data <- function(data, interval) {
  fuzzifikasi <- numeric(length(data))
  
  for (i in 1:length(data)) {
    for (j in 1:nrow(interval)) {
      if (data[i] >= interval[j, 1] & data[i] < interval[j, 2]) {
        fuzzifikasi[i] <- j
        break
      }
    }
  }
  
  return(fuzzifikasi)
}

# Contoh fuzzifikasi
fuzzifikasi_hasil <- fuzzifikasi_data(data_training, interval)

# Menampilkan hasil fuzzifikasi
fuzzykasi <- data.frame(data_training, fuzzifikasi_hasil)
fuzzykasi

# Membuat FLR (Fuzzy Logic Rule)
buat_FLR <- function(fuzzifikasi) {
  FLR <- data.frame(fuzzifikasi = 0, Current_state = NA, Next_state = NA)
  
  for (i in 1:(length(fuzzifikasi) )) {
    FLR[i, 1] <- fuzzifikasi[i]
    FLR[i + 1, 2] <- fuzzifikasi[i]
    FLR[i, 3] <- fuzzifikasi[i]
  }
  
  FLR <- FLR[-nrow(FLR), ]
  FLR <- FLR[-1, ]
  
  return(FLR)
}

# Menghasilkan FLR
FLR <- buat_FLR(fuzzifikasi_hasil)
FLR

# Mengelompokkan jumlah kejadian FLRG, termasuk jika ada interval kosong
buat_FLRG <- function(FLR_hasil, interval) {
  # Mendapatkan semua kemungkinan state
  semua_state <- interval$kel
  
  # Membuat matriks kosong untuk FLRG
  FLRG_matrix <- matrix(0, nrow = length(semua_state), ncol = length(semua_state))
  rownames(FLRG_matrix) <- semua_state
  colnames(FLRG_matrix) <- semua_state
  
  # Mengisi matriks berdasarkan FLR yang ada
  for (i in 1:nrow(FLR_hasil)) {
    current_state <- FLR_hasil$Current_state[i]
    next_state <- FLR_hasil$Next_state[i]
    if (!is.na(current_state) && !is.na(next_state)) {
      FLRG_matrix[current_state, next_state] <- FLRG_matrix[current_state, next_state] + 1
    }
  }
  
  return(as.data.frame.matrix(FLRG_matrix))
}

# Membuat FLRG dengan memastikan semua kombinasi state ada
FLRG <- buat_FLRG(FLR, interval)
FLRG

#menghitng D
# Definisikan fungsi dengan input vektor E
hitung_variabel <- function(E) {
  # Hitung D_i untuk semua indeks dari 3 hingga panjang data E
  D <- abs(abs(E[3:length(E)] - E[2:(length(E)-1)]) - abs(E[2:(length(E)-1)] - E[1:(length(E)-2)]))
  
  # Tambahkan NA di awal agar panjang D sama dengan E
  D <- c(NA, NA, D)
  
  # Lakukan perhitungan variabel-variabel lainnya
  X <- E + D / 2
  XX <- E - D / 2
  Y <- E + D
  YY <- E - D
  P <- E + D / 4
  PP <- E - D / 4
  Q <- E + 2 * D
  QQ <- E - 2 * D
  G <- E + D / 6
  GG <- E - D / 6
  H <- E + 3 * D
  HH <- E - 3 * D
  
  # Kembalikan hasil dalam bentuk data frame
  hasil <- data.frame(E, D, X, XX, Y, YY, P, PP, Q, QQ, G, GG, H, HH)
  
  return(hasil)
}

#penggunaan
E <- data # masukkan data E yang diinginkan
hasil <- hitung_variabel(E)
print(hasil)