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
  if (type == "Chen") {
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
data.ts=ts(data_training, start=c(2023,1), frequency = 365)
ts <- data.ts

# Define the number of fuzzy sets
n <- 10

# Define the extension of the time series range
D1 <- 7000
D2 <- 1000

# Perform fuzzy time series analysis using Chen's model
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

# Calculate the forecasted values using Chen's model
mo <- 1:n
for (cot in 1:n) {
  if (cot > 1) {
    a[a == 1] <- NA
  }
  s <- length(na.omit(a[, cot]))
  a[is.na(a)] <- 1
  if (s == 1) {
    for (j in 1:n) {
      if (a[j, cot] == paste("A", j, sep = "")) {
        t <- D$Bw[j]
        break
      }
    }
    mo[cot] <- t
  } else if (s > 1) {
    t <- 0
    for (j in 1:n) {
      if (a[j, cot] == paste("A", j, sep = "")) {
        t <- t + D$Bw[j]
      }
    }
    mo[cot] <- t / s
  } else if (s == 0) {
    mo[cot] <- D$Bw[cot]
  }
}

# Calculate the forecasted time series
db <- 1:length(loai.old)
for (i in 1:length(loai.old)) {
  if (i == 1) {
    db[i] <- NA
  } else {
    for (j in 1:n) {
      if (loai.old[i] == paste("A", j, sep = "")) {
        db[i] <- mo[j]
        break
      }
    }
  }
}
db <- ts(db, start = start(ts), frequency = frequency(ts))

# Print the results
thoidiem <- namthang(ts)
quanhemo <- quanhe(ts, "Chen")
quanhemokq <- paste(D1$loai, quanhemo, D1$loai.old, sep = "")
DB1 <- data.frame(thoidiem, sl.goc = D1$ts, quanhemo = quanhemokq, db)
print(DB1)

# Plot the actual and forecasted time series
plot(ts, col = "blue", main = "Actual vs Forecasted Time Series (Chen's Model)", type = "l", pch = 15, ylim = c(min(c(ts, db), na.rm = TRUE), max(c(ts, db), na.rm = TRUE)), xlab = "Time", ylab = "Value")
lines(db, col = "red", type = "l", pch = 17)
legend("topleft", legend = c("Actual", "Forecasted"), col = c("blue", "red"), lty = 1)