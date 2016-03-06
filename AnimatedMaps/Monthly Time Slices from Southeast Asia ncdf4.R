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

seAsia.moDf = data.frame(yr = integer(),
                    mo = integer(),
                    lat = double(),
                    lon = double(),
                    geoPotHeight = double()
)

for(d in 2:length(gph.t)){
  if(months(gph.t[d]) != months(gph.t[d-1]) &
      1985 <= as.numeric(format(as.Date.numeric(gph.t[d-1], origin = "1948-01-01"), "%Y"))){
    slice = gph.gph[,,d-1]
    rownames(slice) = gph.lon
    colnames(slice) = gph.lat
    sliceMelted = cbind(rep(as.numeric(format(as.Date.numeric(gph.t[d-1], origin = "1948-01-01"), "%Y")),
                            length(gph.lat) * length(gph.lon)), 
                        rep(as.numeric(format(as.Date.numeric(gph.t[d-1], origin = "1948-01-01"), "%m")),
                            length(gph.lat) * length(gph.lon)), 
                        melt(slice))
    seAsia.moDf = rbind(seAsia.moDf, sliceMelted)
    
  }
}

remove(slice)
remove(sliceMelted)
remove(gph.t)
remove(gph.lat)
remove(gph.lon)
remove(gph.gph)
remove(d)

colnames(seAsia.moDf) = c("yr", "mo", "lon", "lat", "geoPotHeight")





