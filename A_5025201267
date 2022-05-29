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

#5a
# Setuju, karena kesimpulan dari uji z menolak H0,
# sehingga mobil dikemudikan rata-rata lebih dari
# 20000 kilometer per tahun

# 5b 
# Output dari z test adalah, hipotesis alternatif
# alternative hypothesis: true mean is greater than 20000
# atau H1 diterima sehingga klaim benar. 

#5c 
# P-value dari uji tes z adalah 2.2e-16 atau mendekati 0,
# dari hasil p-value tersebut hipotesis awal dapat ditolak
# dan H1 diterima.

#nomor 3
#3a
cat("H0 : mu = mu0","\n","mu !=(tidak sama dengan) mu0")

#3b
tsum.test(mean.x=3.64, s.x = 1.67, n.x = 19, 
          mean.y=2.79, s.y = 1.32, n.y = 27, 
          alternative = "two.sided", mu = 0, var.equal = TRUE,
          conf.level = 0.95)

#3c
xbar3 = 2.79
mu03 = 3.64
s3 = 1.32
n3 = 27              
t3 = (xbar3-mu03)/(s3/sqrt(n3)) 
t3  

#3d nilai kritis
alpha3 = 0.05 
t.alpha3 = qt(1-alpha3, df=2) 
t.alpha3 

#3e
cat("Keputusan : Gagal Tolak H0")

#3f
cat("Kesimpulan : Tidak ada perbedaan pada rata-rata jumlah saham perusahaan di dua kota tersebut")

# NOMOR 4
dataoneway <- read.table("onewayanova.txt",h=T)
attach(dataoneway)
names(dataoneway)

dataoneway$Group <- as.factor(dataoneway$Group)
dataoneway$Group = factor(dataoneway$Group,labels = c("Grup 1", "Grup 2", "Grup 3"))

class(dataoneway$Group)

#4a Pembagian menjadi 3 subjek grup dan membuat
# plot kuantil normal setiap kelompok

Group1 <- subset(dataoneway, Group == "Grup 1")
Group2 <- subset(dataoneway, Group == "Grup 2")
Group3 <- subset(dataoneway, Group == "Grup 3")

qqnorm(Group1$Length)
qqline(Group1$Length)

qqnorm(Group2$Length)
qqline(Group2$Length)

qqnorm(Group3$Length)
qqline(Group3$Length)

#4b
# Mencari homogenity of variances
bartlett.test(Length ~ Group, data = dataoneway)

#4c 
# Uji anova satu arah
model1 = lm(Length ~ Group, data = dataoneway)
anova(model1)

#4d 
# nilai p adalah 0.8054, maka H0 ditolak

#4e
# Post-hoc test Tukey HSD
TukeyHSD(aov(model1))

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
