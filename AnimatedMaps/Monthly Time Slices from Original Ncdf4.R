library(ncdf4)
library(reshape2)

geoPotHeightNcAll = nc_open("C:\\Users\\14z\\Documents\\Github\\NOAA_Daily_phi_500mb.nc")

gph.t = ncvar_get(geoPotHeightNcAll, "T")
gph.lon = ncvar_get(geoPotHeightNcAll, "X")
gph.lat = ncvar_get(geoPotHeightNcAll, "Y")
gph.gph = ncvar_get(geoPotHeightNcAll, "phi")

nc_close(geoPotHeightNcAll)

gph.lon[gph.lon > 180] = gph.lon[gph.lon > 180] - 360

remove(geoPotHeightNcAll)

gph.moDf = data.frame(yr = integer(),
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
    gph.moDf = rbind(gph.moDf, sliceMelted)
    
  }
}

remove(slice)
remove(sliceMelted)
remove(gph.t)
remove(gph.lat)
remove(gph.lon)
remove(gph.gph)
remove(d)

colnames(gph.moDf) = c("yr", "mo", "lon", "lat", "geoPotHeight")





