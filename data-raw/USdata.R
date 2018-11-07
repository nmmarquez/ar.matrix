require("leaflet")
require("rgdal")
require("surveillance")
require("maptools")
require("devtools")
require("stringi")

# download the 2015 shape file from us census
file_zip <- tempfile()
extr_dir <- tempdir()
url_ <- "http://www2.census.gov/geo/tiger/GENZ2015/shp/cb_2015_us_county_5m.zip"
download.file(url_, file_zip)
unzip(file_zip, exdir=extr_dir)

# pull the shape file into memory
df <- readOGR(extr_dir)
unlink(file_zip)
unlink(extr_dir)

# lets only look at Louisiana(22), Texas(48), Mississippi(28), & Arkansas(05)
df <- df[df@data$STATEFP %in% c("22", "48", "28", "05"),]

# translate to desired proj4 string for leaflet
p4s <- "+title=WGS 84 (long/lat) +proj=longlat +ellps=WGS84 +datum=WGS84"
US.df <- spTransform(df, CRS(p4s))
US.df$NAME <- stri_trans_general(US.df$NAME, "latin-ascii")
US.graph <- poly2adjmat(US.df)

use_data(US.df, US.graph)
