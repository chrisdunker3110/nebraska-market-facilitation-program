### NEBRASKA AND THE MARKET FACILITATION PROGRAM
### CREATED BY CHRIS DUNKER, SEPTEMBER 9, 2019

require(tidyverse)
require(readr)
require(tigris)
require(leaflet)
require(sf)
require(formattable)
require(scales)
require(RColorBrewer)
require(htmlwidgets)

### TURN OFF SCIENTIFIC NOTATION
options(scipen = 999)

mfp = read.csv(("https://raw.githubusercontent.com/chrisdunker3110/nebraska-market-facilitation-program/master/market-facilitation-program.csv"),
               colClasses = c(county = "character"), stringsAsFactors = FALSE) %>%
  select(1:6,8:14)

mfp$payment <- currency(mfp$payment, digits = 0L)

### READ IN COUNTY CODE DATA TO GIVE COUNTY NAMES TO FARMER LOCATION
county_codes = read.csv(("https://raw.githubusercontent.com/chrisdunker3110/nebraska-market-facilitation-program/master/county-codes.csv"), 
                        colClasses = c(Cntycd = "character"), stringsAsFactors = FALSE) %>%
  rename("county" = "Cntycd", "office" = "Office.Name", "city" = "City")

### LEFT JOIN COUNTY CODES TO MFP DATAFRAME
mfp = left_join(mfp, county_codes, by = "county") %>%
  rename("cntycode" = "county", "city" = "city.x", "zip" = "new_zip", "office_city" = "city.y") %>%

mutate(office = str_remove(office, "USDA Serv Cntr"))

mfp_counties = mfp %>%
  mutate(office = str_remove(office, "FSA Office"))

mfp_total = mfp_counties %>%
  group_by(cntycode, office) %>%
  summarize(payment = sum(payment))

### SET sf OPTION
options(tigris_class = "sf")
ne <- counties("NE", cb=T)
st_transform(ne, "+proj=longlat +ellps=clrk66 +nadgrids=@conus,@alaska,@ntv2_0.gsb,@ntv1_can.dat +no_defs")
st_crs(ne) 

### CREATE AN INTERACTIVE COUNTY MAP OF NEBRASKA
ne %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(popup =~ name)

ggplot(ne) + 
  geom_sf() +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  labs(title="Nebraska counties")

### JOIN TOGETHER COUNTY MAP, OFFICE DATA
counties_merged <- geo_join(ne, mfp_total, "COUNTYFP", "cntycode")

popup <- paste0(counties_merged$office, "County", "<br/>","<b/>",
                as.character(counties_merged$payment))

risk.bins <- c(0, 3000000, 6000000, 9000000, 14000000, 15000000, 18000000, 21000000)

pal = colorBin("YlOrRd", domain = mfp_counties$payment, bins = risk.bins)

labels <- sprintf("<strong>%s</strong>%s",
                  counties_merged$office, counties_merged$payment) %>% 
  lapply(htmltools::HTML)

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-99.9018, 41,4925, zoom = 7) %>% 
  addPolygons(data = counties_merged, 
              fillColor = ~pal(counties_merged$payment), 
              fillOpacity = 1, 
              weight = 1, 
              smoothFactor = 0.5,
              dashArray = "3",
              color = "black",
              highlight = highlightOptions(
                weight = 5,
                color = "blue",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal, 
            values = counties_merged$payment,
            position = "bottomleft", 
            title = "MFP Payment Totals By County",
            labFormat = labelFormat(prefix = "$"),
            opacity = 1)

saveWidget(widget = market_facilitation_map,
           file = "market-facilitation-program.html",
           selfcontained = TRUE)

### BUILD A MAP SHOWING AVERAGE PAYMENTS BY COUNTY
mfp_avg = mfp_counties %>%
  group_by(cntycode, office) %>%
  summarize(payment = mean(payment))

counties_avg <- geo_join(ne, mfp_avg, "COUNTYFP", "cntycode")

popup <- paste0(counties_avg$office, "County", "<br/>","<b/>",
                as.character(counties_avg$payment))

avg.bins <- c(0, 1000, 4000, 7000, 10000, 13000, 16000, 19000, 22000, 25000)

pal_avg = colorBin("YlGnBu", domain = mfp_avg$payment, bins = avg.bins)

label_avg <- sprintf("<strong>%s</strong>%s",
                     counties_avg$office, counties_avg$payment) %>% 
  lapply(htmltools::HTML)

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-99.9018, 41,4925, zoom = 7) %>% 
  addPolygons(data = counties_avg, 
              fillColor = ~pal_avg(counties_avg$payment), 
              fillOpacity = 1, 
              weight = 1, 
              smoothFactor = 0.5,
              dashArray = "3",
              color = "black",
              highlight = highlightOptions(
                weight = 5,
                color = "red",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = label_avg,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal_avg, 
            values = counties_avg$payment,
            position = "bottomleft", 
            title = "MFP Average Payment By County",
            labFormat = labelFormat(prefix = "$"),
            opacity = 0.8)

### MAP OF MEDIAN PAYMENTS TO FARMERS
options(tigris_class = "sf")
ne <- counties("NE", cb=T)
st_transform(ne, "+proj=longlat +ellps=clrk66 +nadgrids=@conus,@alaska,@ntv2_0.gsb,@ntv1_can.dat +no_defs")
st_crs(ne)

mfp_median = mfp_counties %>%
  group_by(cntycode, office) %>%
  summarize(payment = median(payment))

counties_med <- geo_join(ne, mfp_median, "COUNTYFP", "cntycode")

popup <- paste0(counties_med$office, "County", "<br/>","<b/>",
                as.character(counties_med$payment))

med.bins <- c(0, 500, 1000, 2000, 3000, 4000, 5000)

pal_med = colorBin("PuBuGn", domain = mfp_median$payment, bins = med.bins)

label_med <- sprintf("<strong>%s</strong>%s",
                     counties_med$office, counties_med$payment) %>% 
  lapply(htmltools::HTML)

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-99.9018, 41,4925, zoom = 7) %>% 
  addPolygons(data = counties_med, 
              fillColor = ~pal_med(counties_med$payment), 
              fillOpacity = 1, 
              weight = 1, 
              smoothFactor = 0.5,
              dashArray = "3",
              color = "black",
              highlight = highlightOptions(
                weight = 5,
                color = "red",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = label_avg,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal_med, 
            values = counties_med$payment,
            position = "bottomleft", 
            title = "Median Payment to Farmers",
            labFormat = labelFormat(prefix = "$"),
            opacity = 1)


### WRITING A CSV TO CREATE A SEARCHABLE DATABASE ON THE BACKEND OF LJS'S WEBSITE
mfp_database = mfp_counties %>%
  select(2,6,9:10,13,15)
mfp_database$payment = as.numeric(gsub('[$,]', '', mfp_database$payment))
mfp_database = mfp_database %>%
  rename("Farm" = "farm", "City" = "city", "Payment" = "payment", "Date" = "date", "Commodity" = "commodity", "County" = "office")

write.csv(mfp_database, file = "mfp-neb.csv")
