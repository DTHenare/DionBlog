library(rgdal)
library(ggplot2)
GenMap <- readOGR(dsn = "GenMap2020", layer = "proposed-general-electorates-2020")

ggplot() + 
  geom_polygon(data = GenMap, aes(x = long, y = lat, group = group), colour = "black", fill = NA)




#AucklandZoom
AklCen.ymin <- 1760000-40000
AklCen.ymax <- 1760000+40000
AklCen.xmin <- 5905000-40000
AklCen.xmax <- 5905000+40000
ggplot() + 
  geom_polygon(data = GenMap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  coord_cartesian(ylim = c(AklCen.xmin,AklCen.xmax), xlim = c(AklCen.ymin,AklCen.ymax))
