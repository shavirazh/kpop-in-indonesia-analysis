# Import library yang dibutuhkan

library(tidyr)
library(readr)
library(dplyr)
library(readxl)
library(ggplot2)
library(leaflet)

# Berdasarkan artikel (https://data.tempo.co/data/1174/ada-75-miliar-twit-k-pop-pada-juli-2020-juni-2021-terbanyak-dari-indonesia)
# tertulis bahwa Indonesia menjadi negara urutan pertama yang menyumbang twit terbanyak yang membahas tentang K-Pop.
# Hal ini menunjukkan bahwa penggemar K-Pop di Indonesia sedang marak-maraknya, sehingga
# menimbulkan hipotesis bahwa apakah popularitas para penggemar K-Pop di Indonesia
# memberikan potensi sebagai peluang peningkatan bisnis?
# karena selama pandemi kita dihadapkan oleh segala sesuatunya secara daring, bahkan sampai dengan belanja keinginan dan kebutuhan, maka
# bisnis yang akan diambil adalah e-commerce.

# Akan dianalisis secara grafis bagaimana perkembangan popularitas musik K-Pop di Indonesia.

#1. Popularitas Musik Korean Pop di Indonesia tahun 2019

popularity <- read_excel("D:/DS/DQLab Tetris Program/Capstone Project/KPOP POPULARITY IN INDONESIA 2019.xlsx", sheet = "Sheet1")

str(popularity) # cek struktur data 

popularity$Percentage <- as.numeric(popularity$Percentage) # mengubah kolom menjadi numerik

# Membuat pie chart
piepercent<- round(100*popularity$Percentage/sum(popularity$Percentage), 1)
pie(popularity$Percentage, popularity$Popularity, 
    main = "K-Pop Popularity in Indonesia 2019",
    col = rainbow(length(popularity$Percentage)))

#2.Jumlah Penonton YouTube Video Musik K-Pop tahun 2019
#Jumlah penonton YouTube dianalisis untuk melihat bagaimana perkembangan K-Pop di media sosial.

views <- read_excel("D:/DS/DQLab Tetris Program/Capstone Project/KPOP YOUTUBE VIEW WORLDWIDE 2019.xlsx", sheet="Sheet1")

# Membuat bar chart
ggplot(data=views, mapping = aes(x = reorder(Country, Views), y = Views)) +
  geom_col(fill = "maroon") +
  coord_flip() +
  labs(
    x = 'Country',
    title='K-Pop Contents YouTube Views by Country (2019)'
  ) +
  theme_light()

#3. Penjualan Album Grup dari tahun 2010 - 2021
# Penjualan album dianalisis untuk melihat bagaimana perkembangan grup K-Pop dari tahun ke tahun berdasarkan penjualan album fisiknya.

kpop_sales <- read_excel("D:/DS/DQLab Tetris Program/Capstone Project/KPOP ALBUM SALES 2010-2020.xlsx") 
names(kpop_sales) <- make.names(names(kpop_sales))

str(kpop_sales)
kpop_sales$Total.sales <- as.numeric(kpop_sales$Total.sales)

sales_year <- kpop_sales %>%
  select(Total.sales, Year) %>%
  group_by(Year) %>%
  summarise(sales = sum(Total.sales))
View(sales_year)

ggplot(data=sales_year, mapping = aes(x = Year, y=sales, fill=sales)) +
  scale_fill_gradient(low="red", high="blue") +
  geom_col(show.legend = FALSE) +
  geom_line(size=1.2, show.legend = FALSE) +
  geom_point(size=3, show.legend = FALSE) +
  labs(
    title='K-pop Album Sales per Year (2010 - 2021)',
    subtitle = 'Based on Top 10 Best Selling Albums Gaon Charts') +
  theme_light()


# 4. Berapa lama penggemar K-Pop menghabiskan waktu untuk konten K-Pop?
# Ini dianalisis untuk melihat bagaimana kebiasaan penggemar K-Pop dalam melihat konten K-Pop

time_spent <- read_excel("D:/DS/DQLab Tetris Program/Capstone Project/KPOP MONTHLY TIME SPENT WORLDWIDE.xlsx", sheet = "Sheet1")
str(time_spent)  

ggplot(data = time_spent, mapping=aes(x = reorder(Country, desc(Time)), y = Time)) +
  geom_col(fill = "blue") +
  coord_flip() +
  labs(
    x = 'Country',
    title='K-Pop Fans Monthly Time Spent by Country'
  ) +
  theme_light()

# 5. Apakah dapat dibuktikan secara grafis bagaimana pengaruh mekanisme ekspor di industri musik Korea?
# Ini dianalisis untuk melihat bagaimana kontribusi jumlah ekspor di bidang industri musik Korea Selatan. Apakah ada
# peningkatan yang signifikan pula sejalur dengan popularitasnya?

export_values <- read_excel("D:/DS/DQLab Tetris Program/Capstone Project/VALUE OF MUSIC INDUSTRY EXPORTS FROM SOUTH KOREA 2005 - 2019.xlsx", sheet = "Sheet1")
str(export_values)
export_values$Value <- as.numeric(export_values$Value)

ggplot(data = export_values, mapping = aes(x = Year, y = Value, group = 1)) +
  geom_col(fill = "grey") +
  geom_line(size = 1, color = "orange") +
  labs(
    title = 'Value of Music Industry Export from South Korea (2005-2019)'
  )
theme_light()

# 6. K-POP Fans berdasarkan gender
# Sebaran penggemar K-Pop berdasarkan gender
fans_age_gender <- read_excel("D:/DS/DQLab Tetris Program/Capstone Project/KPOP FANS AGE AND GENDER 2020.xlsx", sheet = "Sheet1")

fag <- fans_age_gender %>%
  pivot_longer(-Age, names_to = "Gender", values_to = "Percentage")
fag$Percentage <- as.numeric(fag$Percentage)

ggplot() +
  geom_col(data = fag, mapping = aes(x = Age, y = Percentage, fill = Gender), 
           position = "dodge") + 
  labs(
    title='K-Pop Fans Age Percentage by Gender 2020',
    subtitle = 'Based on VLive Application'
  ) +
  theme_light()

#7. Pesebaran penggemar K-Pop di Indonesia
idn_kpop <- read_excel("D:/DS/DQLab Tetris Program/Capstone Project/E-Commerce Customer Behaviour.xlsx", sheet = 'Penyebaran Fans KPop di IDN')
str(idn_kpop)
idn_kpop$Persentase <- as.numeric(idn_kpop$Persentase)

idnkpopies <- idn_kpop %>%
  select(-Sumber)

pie(idnkpopies$Persentase, idnkpopies$Daerah, 
    main = "Sebaran Penggemar K-Pop di Indonesia")

# Setelah menganalisis bagaimana perkembangan K-Pop di Indonesia, mulai dari kebiasaan penggemar,
# persebarannya di Indonesia, sampai dengan berdasarkan total penjualan albumnya.
# Berdasarkan analisis grafis ini dapat diketahui bahwa sejak 2018-2019, K-Pop mulai mencapai puncak 
# tertinggi ketenarannya di Indonesia, sehingga penggemarnya juga seiring bertambah dari tahun ke tahun.
# Oleh karena itu, fenomena popularitas K-Pop di Indonesia yang sedang tinggi-tingginya, 
# memberikan potensi peluang terhadap bisnis. Seperti yang sudah dipaparkan sebelumnya bahwa semenjak pandemi,
# orang-orang mulai beralih kegiatan secara daring, termasuk belanja online di e-commerce.
# Maka, sebelum menyimpulkan apakah popularitas K-Pop di Indonesia benar-benar berpotensi di untuk
# bisnis, akan dianalisis terlebih dahulu bagaimana kebiasaan konsumen berbelanja online.

# Perlu diingat bahwa popularitas K-Pop di Indonesia tidak hanya membawa genre musik saja,
# tetapi juga fashion, kecantikan, hingga makanan. Maka dari itu, indikator inilah yang akan dianalisis
# penjualannya. Indikator ini seterusnya akan disebut dengan istilah Indikator Korean-Wave.

# 1. Rata-rata transaksi E-commerce per Kategori (Indikator)
avg_tr <- read_excel("D:/DS/DQLab Tetris Program/Capstone Project/E-Commerce Customer Behaviour.xlsx", sheet = 'Rata-rata Transaksi e-commerce')
str(avg_tr)

avg_trs <- avg_tr %>%
  select(-Sumber) %>%
  pivot_longer(-Kategori, names_to = "Tahun", values_to = "Rata.rata")

ggplot() +
  geom_col(data = avg_trs, mapping = aes(x = Kategori, y = Rata.rata, fill = Tahun), position='dodge') + 
  labs(
    title='Rata-rata transaksi per kategori',
    subtitle = 'Indikator Korean-Wave'
  ) +
  theme_light()

#2. Persentase transaksi yang dilakukan customer berdasarkan umur
prc_tr <- read_excel("D:/DS/DQLab Tetris Program/Capstone Project/E-Commerce Customer Behaviour.xlsx", sheet = 'Jumlah transaksi produk by umur')
str(prc_tr)
names(prc_tr) <- make.names(names(prc_tr))

prc_trs <- prc_tr %>%
  select(-Sumber) %>%
  pivot_longer(-Kelompok.Umur, names_to = "Kategori", values_to = "Persentase")

ggplot() +
  geom_col(data = prc_trs, mapping = aes(x = Kelompok.Umur, y = Persentase, fill = Kategori), position='dodge') + 
  labs(
    title='Persentase Transaksi yang dilakukan customer berdasarkan Umur',
    subtitle = 'Indikator Korean-Wave'
  ) +
  theme_light()

#3. Rata rata jumlah item yang dibeli per kategori berdasarkan gender
item_tr <- read_excel("D:/DS/DQLab Tetris Program/Capstone Project/E-Commerce Customer Behaviour.xlsx", sheet = 'Rata-rata jumlah item by gender')
View(item_tr)
names(item_tr) <- make.names(names(item_tr))
item_tr

item_trs <- item_tr %>%
  pivot_longer(-Kategori, names_to = "Gender", values_to = "Jumlah")

ggplot() +
  geom_col(data = item_trs, mapping = aes(x = Kategori, y = Jumlah, fill = Gender), position='dodge') +
  labs(
    title='Rata-rata jumlah item yang dibeli per kategori berdasarkan gender',
    subtitle = 'Indikator Korean-Wave'
  ) +
  theme_light()

# Setelah menganalisis customer behaviour, maka berdasarkan hasil grafisnya, apabila
# hasil dari analisis ini sesuai dengan perkembangan K-Pop di Indonesia, maka dapat dikatakan
# bahwa secara grafis, perkembangan K-Pop di Indonesia berpotensi untuk peluang bisnis di e-commerce.
# Contohnya,
# Penggemar K-Pop di Indonesia didominasi oleh Gen Z dan milenial, di mana Gen Z dan milenial ini 
# juga mendominasi transaksi di e-commerce. Contoh lain adalah penggemar K-Pop di Indonesia didominasi oleh perempuan,
# dan kategori Fashion adalah kategori yang paling diminati. Rata-rata transaksi di bidang ini dari tahun 2019 hingga 2020
# juga mengalami kenaikan. Sehingga, hal ini menunjukkan bahwa potensi pasar terutama di bidang fashion (korea) cukup luas.


