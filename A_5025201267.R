# Nomor 1
# Carilah Standar Deviasi dari Data Selisih Pasangan Pengamatan Tabel Diatas

# Data Sebelum melakukan aktivitas
before <- c(78, 75, 67, 77, 70, 72, 78, 74, 77)

# Data Setelah melakukan aktivitas
after <- c(100, 95, 70, 90, 90, 90, 89, 90, 100)

# Check Data
my_data <- data.frame(
  group = rep(c("before", "after"), each = 9),
  saturation = c(before, after)
)

# Print Data
print(my_data)

# Standar Devisiasi before activity
sd_before <- sd(before)
sd_before

# Standar Devisiasi after activity
sd_after <- sd(after)
sd_after

#1b
# Carilah Nilai t (p-value)

# Menggunakan t-test
t.test(before, after, alternative = "greater", var.equal = FALSE)

#1c
# tentukanlah apakah terdapat pengaruh yang signifikan secara statistika dalam hal kadar saturasi oksigen , sebelum dan sesudah melakukan aktivitas A jika diketahui tingkat signifikansi a = 5% serta H0 : "tidak ada pengaruh yang signifikan secara statistika dalam hal kadar saturasi oksigen, sebelum dan sesudah melakukan aktivitas A

var.test(before, after)

t.test(before, after, mu = 0, alternative = "two.sided", var.equal = TRUE)

#NOMOR 2
library(BSDA)

# lebih dari 20000km
# ukuran sampel (n) = 1000
# mean (x) = 23500
# sd = 3900
# H0 = miu <= 20000
# H1 = miu > 20000
zsum.test(mean.x = 23500, sigma.x = 3900, n.x = 100,
          alternative = "greater", mu = 20000,
          conf.level = 0.95)

#2a
# Setuju, karena kesimpulan dari uji z menolak H0,
# sehingga mobil dikemudikan rata-rata lebih dari
# 20000 kilometer per tahun

#2b 
# Output dari z test adalah, hipotesis alternatif
# alternative hypothesis: true mean is greater than 20000
# atau H1 diterima sehingga klaim benar. 

#2c 
# P-value dari uji tes z adalah 2.2e-16 atau mendekati 0,
# dari hasil p-value tersebut hipotesis awal dapat ditolak
# dan H1 diterima.

#nomor 3
#3a
# H0 = (miu1 = miu2)
# H1 = (miu1 != miu2) 

#3b
tsum.test(mean.x=3.64, s.x = 1.67, n.x = 19, 
          mean.y=2.79, s.y = 1.32, n.y = 27, 
          alternative = "two.sided", mu = 0, var.equal = TRUE,
          conf.level = 0.95)

#3c
plotDist(dist ='t', df = 2, col="pink")

#3d nilai kritis
qt(p = 0.05, df = 2, lower.tail = FALSE)

#3e
cat("Karena p-value < a , Hipotesis awal ditolak")

#3f
cat("Dengan tingkat keyakinan 95%, diyakini bahwa tidak terdapat perbedaan rata-rata saham pada perusahaan di Bandung dan Bali.")

# NOMOR 4
#4a Pembagian menjadi 3 subjek grup dan membuat plot kuantil normal setiap kelompok

my_data <- read.delim(file.choose())

my_data$Group <- as.factor(my_data$Group)
my_data$Group = factor(my_data$Group, labels = c("grup1", "grup1", "grup3"))


grup1 <- subset(my_data, Group == "grup1")
grup2 <- subset(my_data, Group == "grup1")
grup3 <- subset(my_data, Group == "grup3")

qqnorm(grup1$Length)

qqnorm(grup2$Length)

qqnorm(grup3$Length)

#4b
bartlett.test(Length ~ Group, data = my_data)

#4c 
model1 <- aov(Length ~ Group, data = my_data)
summary(model1)

#4d 
# nilai p adalah 0.0013, maka H0 ditolak

#4e
TukeyHSD(model1)

#4f
# Visualisasikan data dengan ggplot2

library(ggplot2)
ggplot(dataoneway, aes(x = Group, y = Length)) + geom_boxplot(fill = "grey80", colour = "black") + 
  scale_x_discrete() + xlab("Treatment Group") +  ylab("Length (cm)")

# nomor 5
library(dplyr)
library(multcompView)

gtl <- read.csv(file.choose())

#5a
qplot(x = Temp, y = Light, geom = "point", data = gtl) +
  facet_grid(.~Glass, labeller = label_both)

#5b
gtl$Glass <- as.factor(gtl$Glass)
gtl$Temp_Factor <- as.factor(gtl$Temp)
str(gtl)

gtlaov <- aov(Light ~ Glass*Temp_Factor, data = gtl)
summary(gtlaov)

#5c
data_summary <- group_by(gtl, Glass, Temp) %>%
  summarise(mean=mean(Light), sd=sd(Light)) %>%
  arrange(desc(mean))

print(data_summary)

#5d
tukey <- TukeyHSD(gtlaov)
print(tukey)

#5e
tukey.cld <- multcompLetters4(gtlaov, tukey)
print(tukey.cld)
