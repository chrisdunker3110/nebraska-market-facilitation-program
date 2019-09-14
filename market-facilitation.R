### NEBRASKA AND THE MARKET FACILITATION PROGRAM
### CREATED BY CHRIS DUNKER, SEPTEMBER 9, 2019

require(tidyverse)
require(tigris)
require(leaflet)
require(sf)
require(formattable)
require(scales)
require(RColorBrewer)
require(htmltools)

### TURN OFF SCIENTIFIC NOTATION
options(scipen = 999)

### DATA PROVIDED VIA FOIA REQUEST TO THE U.S. DEPT. OF AGRICULTURE
### IT WAS PROVIDED AS A TXT FILE, BUT I CONVERTED TO A CSV IN EXCEL
mfp = read.csv(("C:/Users/jspdunkc/Desktop/R Projects/Market-Facilitation/market-facilitation-program.csv"), 
               colClasses = c(county = "character"), stringsAsFactors = FALSE) %>%
  select(1:6,8:14)

### USING FORMATTABLE TO TURN PAYMENT COLUMN INTO CURRENCY
### https://stackoverflow.com/questions/22070777/represent-numeric-value-with-typical-dollar-amount-format
mfp$payment <- currency(mfp$payment, digits = 0L)

### READ IN COUNTY CODE DATA TO GIVE COUNTY NAMES TO FARMER LOCATION
county_codes = read.csv(("C:/Users/jspdunkc/Desktop/R Projects/Market-Facilitation/county-codes.csv"), 
                        colClasses = c(Cntycd = "character"), stringsAsFactors = FALSE) %>%
  rename("county" = "Cntycd", "office" = "Office.Name", "city" = "City")

### LEFT JOIN COUNTY CODES TO MFP DATAFRAME
mfp = left_join(mfp, county_codes, by = "county") %>%
  rename("cntycode" = "county", "city" = "city.x", "zip" = "new_zip", "office_city" = "city.y") %>%
### I FINALLY FOUND SMOETHING THAT WORKED TO REMOVE THE "COUNTY USDA SERV CNTR" FROM THE COUNTY ID.
### https://www.reddit.com/r/rstats/comments/b3s8ne/stringrstr_remove_is_there_an_easy_way_to_escape/
  mutate(office = str_remove(office, "USDA Serv Cntr"))

mfp_counties = mfp %>%
  mutate(office = str_remove(office, "FSA Office"))

### TRANSFORM DATA TO FIND OUT TOTAL RECEIVED BY COUNTY
mfp_total = mfp_counties %>%
  group_by(cntycode, office) %>%
  summarize(payment = sum(payment))

View(mfp_total)

### IN ADDITION TO TOTAL BY COUNTY, SOME QUICK-HIT STATS
sum(mfp$payment) # $693,986,917
mean(mfp$payment) # $8,827
median(mfp$payment) # $1,340

### QUICK BOXPLOT FOR ANALYSIS' SAKE
mfp %>%
  filter(payment <= 50000) %>%
  ggplot(aes(x = commodity, y = payment)) +
  geom_boxplot() +
  coord_flip()

### SET sf OPTION
options(tigris_class = "sf")
ne <- counties("NE", cb=T)
st_transform(ne, "+proj=longlat +ellps=clrk66 +nadgrids=@conus,@alaska,@ntv2_0.gsb,@ntv1_can.dat +no_defs")
st_crs(ne) 

### Coordinate Reference System:
### EPSG: 4267 
### proj4string: "+proj=longlat +ellps=clrk66 +nadgrids=@conus,@alaska,@ntv2_0.gsb,@ntv1_can.dat +no_defs"

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

### ASSIGN A COLOR PALETTE
### YlOrRd has 9 bins.
risk.bins <- c(0, 100000, 250000, 500000, 1000000, 5000000, 10000000, 15000000, 20000000, 21000000)

pal = colorBin("YlOrRd", domain = mfp_counties$payment, bins = risk.bins)

labels <- sprintf("<strong>%s</strong>%s",
                  counties_merged$office, counties_merged$payment) %>% 
  lapply(htmltools::HTML)

### ANDREW TRAN'S CODE MODIFIED WITH MY DATA
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-99.9018, 41,4925, zoom = 6) %>% 
  addPolygons(data = counties_merged, 
              fillColor = ~pal(counties_merged$payment), 
              fillOpacity = 1, 
              weight = 1, 
              smoothFactor = 0.5,
              dashArray = "3",
              color = "white",
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
            title = "USDA Market Facilitation Program",
            labFormat = labelFormat(prefix = "$"),
            opacity = 1)


### labels = c("$0 to $100k", "$100k to $250k", "$250k to $500k",
   ###        "$500k to $1M", "$1M to $5M", "$5M to $10M", 
      ###     "$10M to $15M","$15M to $20M", "$20M+"),


### WRITING A CSV TO CREATE A SEARCHABLE DATABASE ON THE BACKEND OF LJS'S WEBSITE
mfp_database = mfp_counties %>%
  select(2,6,9:10,13,15)
mfp_database$payment = as.numeric(gsub('[$,]', '', mfp_database$payment))
mfp_database = mfp_database %>%
  rename("Farm" = "farm", "City" = "city", "Payment" = "payment", "Date" = "date", "Commodity" = "commodity", "County" = "office")

write.csv(mfp_database, file = "mfp-neb.csv")