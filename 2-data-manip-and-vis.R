#########################################
# 20170630
# FOSS4G北海道 ハンズオンデイ: モダンな方法で学ぶ、Rによる地理空間情報データの処理
# Rを使った地理空間情報データ操作と可視化
#########################################
## ----パッケージを呼び出そう----------------------------------------------
# library(tidyverse)
library(dplyr) # データ操作一般
library(sf) # 地理空間情報データ処理
library(leaflet) # 地図描画

## ----インタラクティブな地図操作が可能----
leaflet() %>% addTiles() %>% 
  setView(lng = 141.348, lat = 43.066, zoom = 17) %>% 
  addPopups(lng = 141.348, lat = 43.066,
            popup = "Hi!",
            options = popupOptions(closeButton = FALSE))

## ----データの用意-----------------------------
df.pops <- readr::read_rds("inst/data/pref01_population2015.rds")

class(df.pops)
glimpse(df.pops) # dplyrのデータ出力関数

## ----データの用意-----------------------------
df.pref01 <- read_sf("inst/data/ksj_n0301.shp") %>%
  magrittr::set_names(c("pref_name", "city_name_", "city_name", "city_name_full", "city_code", "geometry"))

class(df.pref01)

## ----sfオブジェクト------------------------------------------------------
head(df.pref01, 3)

plot(df.pref01)

plot(df.pref01["city_code"])

# 結果は同じ
df.pref01 %>%
  select(city_code) %>%
  plot()

plot(st_geometry(df.pref01))

## select()による変数の選択 ---------------------------------------
# 変数名を引数で指定する
df.mod <- df.pref01 %>% select(city_name_full)
names(df.mod)

plot(df.mod)

## ----filter()によるデータ抽出--------------------------------------------
# 条件に従うデータを抽出する
df.mod <- df.mod %>% 
  # 「札幌市」を含んだ行を取り出す
  filter(grepl("札幌市", city_name_full))

plot(df.mod, 
     col = "gray", 
     axes = TRUE)

## ----mutate()によるデータの加工------------------------------------------
df.pref01$city_code[1] %>% class()
# 文字列型に変換
df.pref01$city_code[1] %>% as.character() %>% 
  class()

# データフレームの列に適用
df.mod <- df.pref01 %>% 
  mutate(city_code = as.character(city_code))

df.mod$city_code[1] %>% class()

## ----人口データと結合----------------------------------------------------
df.mod <- df.mod %>% 
  left_join(df.pops, by = "city_code")

# 結合時の変数名とデータ型に注意
# df %>%
#   left_join(df.pops, by = "city_code")

plot(df.mod["value"], 
     col = colormap::colormap("viridis"))

## ----sfパッケージによるジオメトリ操作, eval = FALSE, echo = TRUE---------
st_union(df.pref01) %>%
  plot()

df.pref01["city_code"] %>%
  st_buffer(dist = 0.05) %>%
  plot()

st_centroid(df.pref01["city_code"]) %>%
  plot()

## ----leaflet, eval = TRUE, echo = TRUE-----------------------------------
base.map <- leaflet() %>% 
  addTiles()

## ----leafletにsfオブジェクトを描画させる, eval = FALSE, echo = TRUE------
base.map %>%
  addPolygons(data = df.pref01)

## ------------------------------------------------------------------------
df.mod <- df.pref01 %>% 
  filter(city_name_ == "札幌市")

base.map %>%
  addPolygons(data = df.mod) %>%
  addMarkers(data = st_centroid(df.mod),
             popup = ~city_name)


# ## ----どのデータを使う?---------------------------------------------------
# library(rvest)
# 
# x <- read_html("http://www.pref.hokkaido.lg.jp/ss/jsk/opendata/opendata.htm")
# df.opd <- x %>% html_table(fill = TRUE) %>%
#   .[[3]]
# 
# ## ----詳細な要素（リンク先のURL）を取得, eval = FALSE, echo = TRUE--------
# x %>% html_nodes(css = '#rs_contents > p:nth-child(4) > span > strong')
# # {xml_nodeset (1)}
# # [1] <strong>北海道オープンデータカタログ</strong>
# 
# x %>% html_nodes(css = '#rs_contents > p:nth-child(4) > span > strong') %>%
#   html_text()
# # 北海道オープンデータカタログ
# 
# ## ---- 森林計画関係資料のダウンロード------------------------------------------
# x %>% html_nodes(css = '#open_data > tbody > tr:nth-child(222) > td:nth-child(3) > a') %>%
#   html_text()
# # [1] "森林計画関係資料（GIS用データ）"
# (link.url <- x %>% html_nodes(css = '#open_data > tbody > tr:nth-child(222) > td:nth-child(3) > a') %>%
#     html_attr(name = "href"))
# # [1] "http://www.pref.hokkaido.lg.jp/sr/srk/OPD.htm"
# 
# x <- read_html(link.url)
# 
# x %>% html_nodes(css= '#rs_contents > div > table > tbody > tr > td:nth-child(2) > p:nth-child(1) > a:nth-child(9)') %>%
#   html_text()
# # [1] "留萌 " "留萌"
# 
# x %>% html_nodes(css= '#rs_contents > div > table > tbody > tr > td:nth-child(2) > p:nth-child(1) > a:nth-child(9)') %>%
#   html_attr("href")
# # [1] "https://www.fics.pref.hokkaido.lg.jp/FILE/2015/KMZ/09rumoi.zip"
# # [2] "https://www.fics.pref.hokkaido.lg.jp/FILE/2015/GIS/09rumoi.zip"
# 
# download.file("https://www.fics.pref.hokkaido.lg.jp/FILE/2015/GIS/09rumoi.zip",
#               destfile = "inst/09rumoi.zip")
# unzip(
#   'inst/09rumoi.zip',
#   exdir = "inst/"
# )

# やってみよう ------------------------------------------------------------------
# 小班shape
df.rumoi <- read_sf("inst/09留萌/09留萌_小班.shp")
# 森林に関するデータ
df.forest <- readr::read_csv("inst/09留萌/留萌森林簿データ（コード置換版）.csv",
                             locale = readr::locale(encoding = "cp932"))

names(df.rumoi)
# plot(df.rumoi["NO"])

df.rumoi$ID[1:5]
df.rumoi <- df.rumoi %>% 
  select(KEYCODE, 市町村コー, 林班, 計画区コー, GISAREA, NO, LUX, ID, geometry) %>% 
  filter(between(ID, 9010000001, 9010001000)) %>% 
  st_simplify()

dim(df.rumoi)

## ------------------------------------------------------------------------
df.rumoi <- df.rumoi %>% inner_join(df.forest, by = c("KEYCODE", "林班"))

df.rumoi %>%
  group_by(林種) %>%
  summarise(total_area = sum(GISAREA))

library(ggplot2)
# 時々失敗します
ggplot() +
  geom_sf(data = df.rumoi, aes(fill = GISAREA)) +
  scale_fill_gradientn(colors = sf.colors(10))

# 文字化けしないためのおまじない
# Macなら quartzFonts(ipa = quartzFont(rep("HiraMaruProN-W4", 4)))
# Windownsなら ... windowsFonts(Mei = windowsFont("Meiryo"))
quartzFonts(HiraMaru = quartzFont(rep("HiraMaruProN-W4", 4)))
theme_set(ggthemes::theme_map(base_size = 12, base_family = "HiraMaruProN-W4"))
ggplot() +
  geom_sf(data = df.rumoi, aes(fill = 林種))

## ---- eval = FALSE, echo = TRUE------------------------------------------
leaflet() %>% addTiles() %>%
  addPolygons(data = df.rumoi)
## # Warning messages:
## # 1: sf layer is not long-lat data
## # 2: sf layer has inconsistent datum (+proj=tmerc +lat_0=44 +lon_0=142.25 +k=0.9999 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs).
## # Need '+proj=longlat +datum=WGS84'

## ------------------------------------------------------------------------
# 投影法を確認する
df.rumoi %>% st_crs()

## ------------------------------------------------------------------------
df.rumoi <- df.rumoi %>% st_transform(crs = "+init=epsg:4326")

## ---- eval = FALSE, echo = FALSE, purl = TRUE----------------------------
## # 次の方法でも結果は同じ
## df.rumoi <- df.rumoi %>% st_transform(4326)

## ---- eval = require('leaflet'), out.width = '80%', fig.height = 4.6-----
leaflet() %>% addTiles() %>%
  addPolygons(data = df.rumoi)
