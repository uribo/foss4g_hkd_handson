##########################################
# 20170630
# FOSS4G北海道 ハンズオンデイ: モダンな方法で学ぶ、Rによる地理空間情報データの処理
# 補足資料
##########################################
## ------------------------------------------------------------------------
library(sf)

# ポイントとポイントの組み合わせがマルチポイント
c(st_point(1:2), st_point(5:6))

## ------------------------------------------------------------------------
library(sp)

## ---- eval = FALSE, echo = TRUE------------------------------------------
sp.rumoi <- rgdal::readOGR("inst/09留萌/09留萌_小班.shp")

class(sp.rumoi)

sf.rumoi <- sp.rumoi %>% st_as_sf()
class(sf.rumoi)

sf.rumoi %>% as("Spatial") %>% class()

## データベース(PostgreSQL)接続
## library(DBI)
## library(RPostgreSQL)
## conn = dbConnect(PostgreSQL(),
##                  dbname = "postgis",
##                  user = "username",
##                  password = "password")
## st_read_db(conn, query = "SELECT * FROM scheme.table")
## dbDisconnect(conn)

## ------------------------------------------------------------------------
# 札幌駅
point.spr.st <- st_point(c(141.3509218, 43.06866))

x <- st_sfc(
  point.spr.st,
  crs = 4326)

# ACU
y <- st_sfc(
st_point(c(141.34807, 43.06608)),
crs = 4326
)

st_distance(x, y)

## jpndistrictパッケージ
library(jpndistrict)
df.spr <- spdf_jpn_cities(1,
                          jis_code_city = c("01101", "01102", "01103", "01104", "01105",
                                            "01106", "01107", "01108", "01109", "01110"))

df.spr %>% st_as_sf() %>%
  st_union() %>%
  plot()

## jpmeshパッケージ------------------------------------------------------------------------
library(jpmesh)
meshcode_to_latlon(5133)
meshcode_to_latlon(51337783)

latlong_to_meshcode(34.583333, 133.875, order = 2)

data("jpnrect")
d <- jpnrect %>% 
  rowwise() %>% 
  mutate(st_point = st_sfc(st_point(c(longitude, latitude)))) %>% 
  ungroup() %>%
  st_as_sf()

plot(d["mesh_code"])
