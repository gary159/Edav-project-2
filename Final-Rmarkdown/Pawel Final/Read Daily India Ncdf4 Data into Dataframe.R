library(ncdf4)
library(reshape2)

#File at http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP-NCAR/.CDAS-1/.DAILY/.Intrinsic/.PressureLevel/.phi/P/500/VALUE/Y/%2860N%29%2810S%29RANGEEDGES/X/%28150E%29%2840E%29RANGEEDGES/datafiles.html
seAsiaNc = nc_open("C:\\Users\\14z\\Documents\\Github\\Southeast Asia GeoPotential Height Data.nc")

gph.t = ncvar_get(seAsiaNc, "T")
gph.lon = ncvar_get(seAsiaNc, "X")
gph.lat = ncvar_get(seAsiaNc, "Y")
gph.gph = ncvar_get(seAsiaNc, "phi")

nc_close(seAsiaNc)

gph.lon[gph.lon > 180] = gph.lon[gph.lon > 180] - 360

remove(seAsiaNc)

india2007.moDf = data.frame(dataDate = as.Date(character()),
                    lat = double(),
                    lon = double(),
                    geoPotHeight = double()
)

for(d in 2:length(gph.t)){
  if(as.Date.numeric(gph.t[d], origin = "1948-01-01") >= as.Date("2007-06-01") &
     as.Date.numeric(gph.t[d], origin = "1948-01-01") < as.Date("2007-11-01")){
    slice = gph.gph[,,d]
    rownames(slice) = gph.lon
    colnames(slice) = gph.lat
    sliceMelted = cbind(rep(as.Date.numeric(gph.t[d], origin = "1948-01-01")),
                        melt(slice))
    india2007.moDf = rbind(india2007.moDf, 
                           subset(sliceMelted, 
                                  Var1 >= 65 & Var1 <= 100 & Var2 >= 5 & Var2 <= 35)
    )
    
  }
}

remove(slice)
remove(sliceMelted)
remove(gph.t)
remove(gph.lat)
remove(gph.lon)
remove(gph.gph)
remove(d)

colnames(india2007.moDf) = c("dataDate", "lon", "lat", "geoPotHeight")


