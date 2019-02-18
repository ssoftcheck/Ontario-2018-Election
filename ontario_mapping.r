library(ggplot2)
library(data.table)
library(plotly)

library(rgdal)
library(sf)
library(rgdal)
library(stringi)
library(crosstalk)

setwd("e:/ontraio election 2018.06.07/")

voting = fread("consolidated_votes.csv",encoding = "UTF-8",stringsAsFactors = FALSE)
voting[,district_name := stri_enc_toutf8(district_name,is_unknown_8bit = TRUE)]
party_colours = data.table(party=c("NDP","PCP","GPO","LIB"),colour=c("orange","royalblue","green","red"))
party_flip_colours = data.table(party_flip=c("PCP","NON-PCP"),colour_flip=c("royalblue","gold"))
voting = merge(voting,party_colours,by="party",all.x=TRUE,all.y=FALSE)
voting = merge(voting,party_flip_colours,by="party_flip",all.x=TRUE,all.y=FALSE)

shapefile = readOGR(dsn = "./shapefiles/ELECTORAL_DISTRICT", layer = "ELECTORAL_DISTRICT" )
shapefile_sf = st_read("./shapefiles/ELECTORAL_DISTRICT/ELECTORAL_DISTRICT.shp",stringsAsFactors = FALSE)
shapefile_sf = shapefile_sf[order(shapefile_sf$ED_ID),]
shapefile_sf$ENGLISH_NA = gsub("\u0097",replacement = "\u2014",shapefile_sf$ENGLISH_NA)
shapefile_sf = merge(shapefile_sf,voting,by.y="district_name",by.x="ENGLISH_NA",all.x=TRUE)

shapefile_sf$text = apply(shapefile_sf,1,function(x) sprintf("%s<br>NDP: %s<br>LIB: %s<br>PCP: %s<br>GPO: %s",x["ENGLISH_NA"],x["NDP"],x["LIB"],x["PCP"],x["GPO"]))
map = plot_ly(shapefile_sf,split=~ENGLISH_NA, color=I("black"), fillcolor=~colour, legendgroup=~party, name=~ENGLISH_NA, type="scatter", mode="lines",
              text=~text, hoverinfo="text", hoveron = "fill") 
htmlwidgets::saveWidget(as_widget(map), "election_map.html")

shapefile_sf$text = apply(shapefile_sf,1,function(x) sprintf("%s<br>PCP Lead: %s",x["ENGLISH_NA"],x["pcp_lead"],x["non_pcp_votes"]))
map_pcp = plot_ly(shapefile_sf,split=~ENGLISH_NA, color=~pcp_lead, colors=colorRamp(c("gold","royalblue")),legendgroup=~party, 
                  showlegend = FALSE, type="scatter", mode="lines", line=list(color="black"), text=~text, hoverinfo="text", hoveron="fill") %>%  colorbar(title = "PCP Lead")
htmlwidgets::saveWidget(as_widget(map_pcp), "election_pcp.html")

shapefile_sf$text = apply(shapefile_sf,1,function(x) sprintf("%s<br>PCP: %s<br>NON-PCP: %s",x["ENGLISH_NA"],x["PCP"],x["non_pcp_votes"]))
map_flip = plot_ly(shapefile_sf,split=~ENGLISH_NA, color=I("black"), fillcolor=~colour_flip, legendgroup=~party_flip, name=~ENGLISH_NA, type="scatter", mode="lines",
                   text=~text,hoverinfo="text",hoveron="fill")
htmlwidgets::saveWidget(as_widget(map_flip), "election_flip.html")


shapefile_sf$text = apply(shapefile_sf,1,function(x) sprintf("%s<br>NDP: %s<br>LIB: %s<br>PCP: %s<br>GPO: %s",x["ENGLISH_NA"],x["NDP"],x["LIB"],x["PCP"],x["GPO"]))
ont = SharedData$new(shapefile_sf)
map = plot_mapbox(ont, split=~ENGLISH_NA, color=I("black"), fillcolor=~colour,  legendgroup=~party, text=~text, hoverinfo="text",name=~ENGLISH_NA, alpha=0.75) %>% 
layout(mapbox=list(zoom=4), hovermode="closest", hoverdistance=100)
htmlwidgets::saveWidget(as_widget(map),"election_map_interactive.html")


# non interactive plots
shape_df = as.data.table(fortify(shapefile))




map_plot1 = ggplot(shape_df) + aes(x=long,y=lat,group=group) + geom_path()
map_plot1

map_plot2 = ggplot(shapefile_sf) + geom_sf()
map_plot2


# plot_geo(shapefile_sf, split=~ENGLISH_NA)

# not responsive/no plot
# plot_mapbox(shapefile_sf)
