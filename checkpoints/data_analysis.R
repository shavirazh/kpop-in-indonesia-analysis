library(tidyr)
library(readr)
library(dplyr)
library(readxl)
library(ggplot2)
library(leaflet)

#Parameter pengukuran popularitas musik korean pop di Indonesia

#3. Penjualan Album Grup dari tahun 2010 - 2021
kpop_sales <- read_excel("D:/DS/DQLab Tetris Program/Capstone Project/KPOP ALBUM SALES 2010-2020.xlsx") 
names(kpop_sales) <- make.names(names(kpop_sales))
View(kpop_sales)

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


#1. Popularitas Musik Korean Pop di Indonesia tahun 2019

popularity <- read_excel("D:/DS/DQLab Tetris Program/Capstone Project/KPOP POPULARITY IN INDONESIA 2019.xlsx", sheet = "Sheet1")
View(popularity)
str(popularity)
popularity$Percentage <- as.numeric(popularity$Percentage)

#ggplot(data=popularity, mapping = aes(x=Popularity, y=Percentage, fill=Percentage)) +
#  scale_fill_gradient(low="yellow",high="orange") +
#  geom_bar(stat = 'identity', show.legend = FALSE) +
#  labs(
#      title='K-pop Popularity in Indonesia 2019'
#  ) +
#  theme_light()

piepercent<- round(100*popularity$Percentage/sum(popularity$Percentage), 1)

pie(popularity$Percentage, popularity$Popularity, 
    main = "K-Pop Popularity in Indonesia 2019",
    col = rainbow(length(popularity$Percentage)))

#2. Jumlah Penonton YouTube Video Musik K-Pop tahun 2019

views <- read_excel("D:/DS/DQLab Tetris Program/Capstone Project/KPOP YOUTUBE VIEW WORLDWIDE 2019.xlsx", sheet="Sheet1")
views

ggplot(data=views, mapping = aes(x = reorder(Country, Views), y = Views)) +
geom_col(fill = "maroon") +
coord_flip() +
labs(
    x = 'Country',
    title='K-Pop Contents YouTube Views by Country (2019)'
    ) +
theme_light()

# 4. Berapa lama penggemar K-Pop menghabiskan waktu untuk konten K-Pop?
time_spent <- read_excel("D:/DS/DQLab Tetris Program/Capstone Project/KPOP MONTHLY TIME SPENT WORLDWIDE.xlsx", sheet = "Sheet1")
View(time_spent)  

ggplot(data = time_spent, mapping=aes(x = reorder(Country, desc(Time)), y = Time)) +
  geom_col(fill = "blue") +
  coord_flip() +
  labs(
    x = 'Country',
    title='K-Pop Fans Monthly Time Spent by Country'
  ) +
  theme_light()

# 5. Apakah dapat dibuktikan secara grafis bagaimana pengaruh mekanisme ekspor di industri musik Korea?
export_values <- read_excel("D:/DS/DQLab Tetris Program/Capstone Project/VALUE OF MUSIC INDUSTRY EXPORTS FROM SOUTH KOREA 2005 - 2019.xlsx", sheet = "Sheet1")
View(export_values)
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
fans_age_gender <- read_excel("D:/DS/DQLab Tetris Program/Capstone Project/KPOP FANS AGE AND GENDER 2020.xlsx", sheet = "Sheet1")
View(fans_age_gender)  

View(fag)

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

# Yang termasuk ke dalam indikator pengaruh korean wave terhadap penjualan e commerce adalah:
# 1. Fashion dan aksesoris
#2. Kosmetik
#3. Film, musik, video
#4. Tiket untuk acara hiburan (olahraga, konser, dll)

#7. Pesebaran fans K-Pop di Indonesia
idn_kpop <- read_excel("D:/DS/DQLab Tetris Program/Capstone Project/E-Commerce Customer Behaviour.xlsx", sheet = 'Penyebaran Fans KPop di IDN')
View(idn_kpop)
str(idn_kpop)

idn_kpop$Persentase <- as.numeric(idn_kpop$Persentase)
idnkpopies$Persentase <- as.numeric(idnkpopies$Persentase)

idn_kpop$latitude <- c(-6.4058, -6.2088, -7.0909, -7.1510, -7.5361, NaN)
idn_kpop$longitude <- c(106.0640, 106.8456, 107.6689 ,110.1403 ,112.2384, NaN)

idn_kpop %>%
  select(-Sumber) %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    stroke = FALSE, 
    fillOpacity = 0.5, 
    radius = ~Persentase*2.9,
    color = ~pal('Persentase'), 
    popup = paste("Persentase Penggemar di ", idn_kpop$Daerah ,":", idn_kpop$Persentase)
  ) %>%
  setView(lat = -6.200000, lng = 106.816666, zoom = 3)


pie(idnkpopies$Persentase, idnkpopies$Daerah, 
    main = "Sebaran Penggemar K-Pop di Indonesia")


# ------------- #

# Rata-rata transaksi E-commerce
avg_tr <- read_excel("D:/DS/DQLab Tetris Program/Capstone Project/E-Commerce Customer Behaviour.xlsx", sheet = 'Rata-rata Transaksi e-commerce')
str(avg_tr)

avg_trs <- avg_tr %>%
  select(-Sumber) %>%
  pivot_longer(-Kategori, names_to = "Tahun", values_to = "Rata.rata")
avg_trs

ggplot() +
  geom_col(data = avg_trs, mapping = aes(x = Kategori, y = Rata.rata, fill = Tahun), position='dodge') + 
  labs(
    title='Rata-rata transaksi per kategori',
    subtitle = 'Indikator Korean-Wave'
  ) +
  theme_light()

# Persentase transaksi yang dilakukan customer berdasarkan umur
prc_tr <- read_excel("D:/DS/DQLab Tetris Program/Capstone Project/E-Commerce Customer Behaviour.xlsx", sheet = 'Jumlah transaksi produk by umur')
str(prc_tr)
names(prc_tr) <- make.names(names(prc_tr))

prc_trs <- prc_tr %>%
  select(-Sumber) %>%
  pivot_longer(-Kelompok.Umur, names_to = "Kategori", values_to = "Persentase")

View(prc_trs)
ggplot() +
  geom_col(data = prc_trs, mapping = aes(x = Kelompok.Umur, y = Persentase, fill = Kategori), position='dodge') + 
  labs(
    title='Persentase Transaksi yang dilakukan customer berdasarkan Umur',
    subtitle = 'Indikator Korean-Wave'
  ) +
  theme_light()


# Rata rata jumlah item yang dibeli per kategori berdasarkan gender
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
