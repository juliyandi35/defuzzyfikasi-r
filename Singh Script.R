# Function to convert time index to readable format
namthang <- function(data.ts) {
  batdau <- start(data.ts)
  tanso <- frequency(data.ts)
  nam1 <- batdau[1]
  thang1 <- batdau[2]
  ketthuc <- end(data.ts)
  nam2 <- ketthuc[1]
  thang2 <- ketthuc[2]
  namkq <- 1:length(data.ts)
  thangkq <- 1:length(data.ts)
  index = 0
  for (nam in nam1:nam2) {
    for (thang in 1:tanso) {
      if (nam != nam1 || thang >= thang1) {
        index = index + 1
        namkq[index] <- nam
        thangkq[index] <- thang
        if (nam == nam2 & thang == thang2) 
          break
      }
    }
  }
  if (tanso == 4) {
    thangkq[thangkq == 1] <- "Q1"
    thangkq[thangkq == 2] <- "Q1"
    thangkq[thangkq == 3] <- "Q1"
    thangkq[thangkq == 4] <- "Q1"
    print <- paste(namkq, thangkq, sep = " ")
  } else if (tanso == 12) {
    thangkq[thangkq == 1] <- "Jan"
    thangkq[thangkq == 2] <- "Feb"
    thangkq[thangkq == 3] <- "Mar"
    thangkq[thangkq == 4] <- "Apr"
    thangkq[thangkq == 5] <- "May"
    thangkq[thangkq == 6] <- "Jun"
    thangkq[thangkq == 7] <- "Jul"
    thangkq[thangkq == 8] <- "Aug"
    thangkq[thangkq == 9] <- "Sep"
    thangkq[thangkq == 10] <- "Oct"
    thangkq[thangkq == 11] <- "Nov"
    thangkq[thangkq == 12] <- "Dec"
    print <- paste(namkq, thangkq, sep = " ")
  } else if (tanso == 7) {
    thangkq[thangkq == 1] <- "Mon"
    thangkq[thangkq == 2] <- "Tue"
    thangkq[thangkq == 3] <- "Wed"
    thangkq[thangkq == 4] <- "Thu"
    thangkq[thangkq == 5] <- "Fri"
    thangkq[thangkq == 6] <- "Sat"
    thangkq[thangkq == 7] <- "Sun"
    print <- paste(namkq, thangkq, sep = " ")
  } else if (tanso != 1) {
    print <- paste("(", namkq, ",", thangkq, ")", sep = "")
  } else {
    print <- namkq
  }
  print
}

# Function to define the relationship between consecutive values
quanhe <- function(ts, type) {
  qh <- 1:length(ts)
  if (type == "Singh") {
    for (i in 1:length(ts)) {
      if (i > 1) {
        qh[i] <- c("<--")
      }
    }
    qh[qh != "<--"] <- "-x-"
  }
  qh
}

# Load the time series data
library(readxl)
library(AnalyzeTS)
library(dplyr)
library(timeSeries)
library(forecast)
library(fuzzyforest)
library(fuzzySim)
data_cabai<- read_excel("dataa.xlsx",range = "A1:C426")

data_cabai$harga
data=data_cabai$harga
data
# Mengambil data training
train_percentage <- 0.9
train_size <- ceiling(train_percentage * length(data))
data_training <- data[1:train_size]
data_testing <- data[train_size:length(data)]
data.ts=ts(data_training, start=c(2023,1), frequency = 365)
ts <- data.ts

# Define the number of fuzzy sets
n <- 10

# Define the extension of the time series range
D1 <- 7000
D2 <- 1000

# Perform fuzzy time series analysis using Singh's model
# Calculate the minimum and maximum values of the time series
min.x <- min(ts) - D1
max.x <- max(ts) + D2

# Calculate the interval width
h <- (max.x - min.x) / n

# Create a data frame to store information about the fuzzy sets
k <- 1:(n + 1)
U <- 1:n
for (i in 1:(n + 1)) {
  if (i == 1) {
    k[i] <- min.x
  } else {
    k[i] <- min.x + (i - 1) * h
    U[i - 1] <- paste("A", i - 1, sep = "")
  }
}
D <- data.frame(U, low = k[1:n], up = k[2:(n + 1)])
D$Bw <- (1/2) * (D$low + D$up)

# Classify each data point into fuzzy sets
loai <- 1:length(ts)
for (i in 1:length(ts)) {
  for (j in 1:n) {
    if (D$low[j] <= ts[i] & ts[i] <= D$up[j]) {
      loai[i] <- paste("A", j, sep = "")
      break
    }
  }
}

# Create a data frame to store the original time series and fuzzy set memberships
loai.old <- 1:length(loai)
for (i in 1:length(loai)) {
  if (i == 1) {
    loai.old[i] <- NA
  } else {
    loai.old[i] <- loai[i - 1]
  }
}
D1 <- data.frame(ts, loai, loai.old)

# Calculate the frequency of each fuzzy set
b <- table(D1$loai)
ni <- 1:n
for (i in 1:n) {
  for (j in 1:n) {
    if (paste("A", i, sep = "") == names(b)[j] & j <= length(b)) {
      ni[i] <- b[j]
      break
    } else {
      if (j > length(b)) {
        ni[i] <- 0
        break
      }
    }
  }
}
D$ni <- ni

# Create a transition matrix
a <- 1:(n * n)
a <- matrix(a, nrow = n)
for (cot in 1:n) {
  for (i in 2:length(D1$loai.old)) {
    for (j in 1:n) {
      if (D1$loai.old[i] == paste("A", cot, sep = "") & 
          a[j, cot] != paste("A", j, sep = "") & D1$loai[i] == 
          paste("A", j, sep = "")) {
        a[j, cot] <- paste("A", j, sep = "")
        break
      }
    }
  }
}
for (cot in 1:n) {
  for (j in 1:n) {
    if (a[j, cot] != paste("A", j, sep = "")) {
      a[j, cot] <- NA
    }
  }
}

# Calculate the forecasted values using Singh's model
F <- 1:length(ts)
Ds <- 1:length(ts)
X <- 1:length(ts)
XX <- 1:length(ts)
Y <- 1:length(ts)
YY <- 1:length(ts)
P <- 1:length(ts)
PP <- 1:length(ts)
Q <- 1:length(ts)
QQ <- 1:length(ts)
G <- 1:length(ts)
GG <- 1:length(ts)
H <- 1:length(ts)
HH <- 1:length(ts)
E <- ts
tt <- 0
v <- 1:length(D$ni)
for (t0 in 1:length(D$ni)) {
  if (D$ni[t0] == 0) {
    tt <- tt + 1
    v[tt] <- t0
  }
}
if (tt == 0) {
  v <- NULL
} else {
  v <- v[1:tt]
}
if (!is.null(v)) {
  Dt <- D[-v, ]
  t1 <- 1
  while (t1 <= length(levels(Dt$U))) {
    test <- 0
    for (t2 in 1:length(Dt$U)) {
      if (levels(Dt$U)[t1] == Dt$U[t2]) {
        test <- 1
        break
      }
    }
    if (test == 0) {
      levels(Dt$U)[t1] <- NA
    } else {
      t1 <- t1 + 1
    }
  }
}
if (is.null(v)) {
  Dt <- D
}
for (i in 0:(length(ts) - 1)) {
  if (i < 3) {
    F[i + 1] <- NA
  } else {
    R <- 0
    S <- 0
    Ds[i] <- abs(abs(E[i] - E[i - 1]) - abs(E[i - 1] - E[i - 2]))
    X[i] <- E[i] + (Ds[i]/2)
    XX[i] <- E[i] - (Ds[i]/2)
    Y[i] <- E[i] + Ds[i]
    YY[i] <- E[i] - Ds[i]
    P[i] <- E[i] + (Ds[i]/4)
    PP[i] <- E[i] - (Ds[i]/4)
    Q[i] <- E[i] + 2 * Ds[i]
    QQ[i] <- E[i] - 2 * Ds[i]
    G[i] <- E[i] + (Ds[i]/6)
    GG[i] <- E[i] - (Ds[i]/6)
    H[i] <- E[i] + 3 * Ds[i]
    HH[i] <- E[i] - 3 * Ds[i]
    Dt.U <- as.character(Dt$U)
    D1.loai <- as.character(D1$loai)
    for (j in 1:length(Dt.U)) {
      if (D1.loai[i + 1] == Dt.U[j]) {
        l <- j
        break
      }
    }
    if (Dt$low[l] <= X[i] & X[i] <= Dt$up[l]) {
      R <- R + X[i]
      S <- S + 1
    }
    if (Dt$low[l] <= XX[i] & XX[i] <= Dt$up[l]) {
      R <- R + XX[i]
      S <- S + 1
    }
    if (Dt$low[l] <= Y[i] & Y[i] <= Dt$up[l]) {
      R <- R + Y[i]
      S <- S + 1
    }
    if (Dt$low[l] <= YY[i] & YY[i] <= Dt$up[l]) {
      R <- R + YY[i]
      S <- S + 1
    }
    if (Dt$low[l] <= P[i] & P[i] <= Dt$up[l]) {
      R <- R + P[i]
      S <- S + 1
    }
    if (Dt$low[l] <= PP[i] & PP[i] <= Dt$up[l]) {
      R <- R + PP[i]
      S <- S + 1
    }
    if (Dt$low[l] <= Q[i] & Q[i] <= Dt$up[l]) {
      R <- R + Q[i]
      S <- S + 1
    }
    if (Dt$low[l] <= QQ[i] & QQ[i] <= Dt$up[l]) {
      R <- R + QQ[i]
      S <- S + 1
    }
    if (Dt$low[l] <= G[i] & G[i] <= Dt$up[l]) {
      R <- R + G[i]
      S <- S + 1
    }
    if (Dt$low[l] <= GG[i] & GG[i] <= Dt$up[l]) {
      R <- R + GG[i]
      S <- S + 1
    }
    if (Dt$low[l] <= H[i] & H[i] <= Dt$up[l]) {
      R <- R + H[i]
      S <- S + 1
    }
    if (Dt$low[l] <= HH[i] & HH[i] <= Dt$up[l]) {
      R <- R + HH[i]
      S <- S + 1
    }
    F[i + 1] <- (R + Dt$Bw[l]) / (S + 1)
  }
}
F <- ts(F, start = start(ts), frequency = frequency(ts))

# Print the results
thoidiem <- namthang(ts)
quanhemo <- quanhe(ts, "Singh")
quanhemokq <- paste(D1$loai, quanhemo, D1$loai.old, sep = "")
DB2 <- data.frame(thoidiem, sl.goc = D1$ts, quanhemo = quanhemokq, sl.mo = F)
print(DB2)

# Plot the actual and forecasted time series
plot(ts, col = "blue", main = "Actual vs Forecasted Time Series (Singh's Model)", type = "l", pch = 15, ylim = c(min(c(ts, F), na.rm = TRUE), max(c(ts, F), na.rm = TRUE)), xlab = "Time", ylab = "Value")
lines(F, col = "red", type = "l", pch = 17)
legend("topleft", legend = c("Actual", "Forecasted"), col = c("blue", "red"), lty = 1)

# Calculate the forecasted values for the next 50 time steps
F_future <- rep(NA, 60)
for (i in 1:60) {
  Ds <- abs(abs(F[length(F)] - F[length(F) - 1]) - abs(F[length(F) - 1] - F[length(F) - 2]))
  X <- F[length(F)] + (Ds/2)
  XX <- F[length(F)] - (Ds/2)
  Y <- F[length(F)] + Ds
  YY <- F[length(F)] - Ds
  P <- F[length(F)] + (Ds/4)
  PP <- F[length(F)] - (Ds/4)
  Q <- F[length(F)] + 2 * Ds
  QQ <- F[length(F)] - 2 * Ds
  G <- F[length(F)] + (Ds/6)
  GG <- F[length(F)] - (Ds/6)
  H <- F[length(F)] + 3 * Ds
  HH <- F[length(F)] - 3 * Ds
  Dt.U <- as.character(Dt$U)
  D1.loai <- as.character(D1$loai)
  for (j in 1:length(Dt.U)) {
    if (D1.loai[length(D1.loai)] == Dt.U[j]) {
      l <- j
      break
    }
  }
  if (Dt$low[l] <= X & X <= Dt$up[l]) {
    R <- R + X
    S <- S + 1
  }
  if (Dt$low[l] <= XX & XX <= Dt$up[l]) {
    R <- R + XX
    S <- S + 1
  }
  if (Dt$low[l] <= Y & Y <= Dt$up[l]) {
    R <- R + Y
    S <- S + 1
  }
  if (Dt$low[l] <= YY & YY <= Dt$up[l]) {
    R <- R + YY
    S <- S + 1
  }
  if (Dt$low[l] <= P & P <= Dt$up[l]) {
    R <- R + P
    S <- S + 1
  }
  if (Dt$low[l] <= PP & PP <= Dt$up[l]) {
    R <- R + PP
    S <- S + 1
  }
  if (Dt$low[l] <= Q & Q <= Dt$up[l]) {
    R <- R + Q
    S <- S + 1
  }
  if (Dt$low[l] <= QQ & QQ <= Dt$up[l]) {
    R <- R + QQ
    S <- S + 1
  }
  if (Dt$low[l] <= G & G <= Dt$up[l]) {
    R <- R + G
    S <- S + 1
  }
  if (Dt$low[l] <= GG & GG <= Dt$up[l]) {
    R <- R + GG
    S <- S + 1
  }
  if (Dt$low[l] <= H & H <= Dt$up[l]) {
    R <- R + H
    S <- S + 1
  }
  if (Dt$low[l] <= HH & HH <= Dt$up[l]) {
    R <- R + HH
    S <- S + 1
  }
  F_future[i] <- (R + Dt$Bw[l]) / (S + 1)
}

# Print the forecasted values
print(F_future)

# Combine the original time series and the forecasted values
F_combined <- c(F, F_future)

plot_base <- rep(NA,443)
# Plot the combined time series
plot(c(1:length(plot_base)),c(data,rep(NA,18)), type = "l", col = "black", main = "Actual and Forecasted Time Series (Singh's Model)", xlab = "Time", ylab = "Value")
lines(c(1:length(plot_base)),c(F,rep(NA,60)), type = "l", col = "red")
lines(c(1:length(plot_base)),c(rep(NA,length(F)),F_future[1:43],rep(NA,length(F_future[44:60]))), col = "blue", type = "l")
lines(c(1:length(plot_base)),c(rep(NA,(length(F)+length(F_future[1:43]))),F_future[44:60]), col = "green", type = "l")
legend("topleft", legend = c("Actual", "Training Fitted", "Testing Fitted","Forecasted"), col = c("black","red","blue","green"), lty = 1)

# Accuracy with MAPE and RMSE
# Function to calculate MAPE (Mean Absolute Percentage Error)
mape <- function(actual, predicted) {
  mean(abs((actual - predicted) / actual),na.rm = TRUE) * 100
}

# Function to calculate RMSE (Root Mean Squared Error)
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2,na.rm = TRUE))
}

MAPE_train <- mape(data_training,F)
MAPE_test <- mape(data_testing,F_future[1:43])
RMSE_train <- rmse(data_training,F)
RMSE_test <- rmse(data_testing,F_future[1:43])
Accuracy.table <- data.frame(MAPE = c(MAPE_train,MAPE_test),
                             RMSE = c(RMSE_train,RMSE_test))
rownames(Accuracy.table) <- c("training","testing")
Accuracy.table
