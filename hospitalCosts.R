packs = c("magrittr", "dplyr", "reshape2", "ggplot2", "ggmap", "ggthemes", "zipcode")
sapply(packs, require, character.only=TRUE)  

# https://data.cms.gov/Medicare/Inpatient-Prospective-Payment-System-IPPS-Provider/97k6-zzx3
data_csv <- RSocrata::read.socrata("https://data.cms.gov/resource/97k6-zzx3.csv")

names(data_csv)
head(data_csv)
qplot(data=data_csv, x=Provider.State) + coord_flip() + theme_fivethirtyeight()


# Geolocate by zipcode
require("zipcode")
data(zipcode)
zips = clean.zipcodes(data_csv$Provider.Zip.Code)


# Merge and format data
df <- merge(data_csv, zipcode, by.x='Provider.Zip.Code', by.y='zip') %>% 
  select(-Provider.City, -Provider.State, -Provider.Zip.Code) %>%
  mutate(Average.Covered.Charges   = as.numeric(gsub( "[$,]", "", Average.Covered.Charges)),
         Average.Total.Payments    = as.numeric(gsub( "[$,]", "", Average.Total.Payments)),
         Average.Medicare.Payments = as.numeric(gsub( "[$,]", "", Average.Medicare.Payments)),
         Total.Discharges          = as.numeric(Total.Discharges))

# require(ggvis)
# df %>%
#   ggvis(x = ~Average.Total.Payments, y = ~Total.Discharges) %>%
#   layer_bars(y=~input_slider(0,400000)) 
# unique(df$DRG.Definition)
# qplot(data=df, , y=)  + coord_flip()


state_cond <- group_by(df, DRG.Definition, state, latitude, longitude) %>% summarise(discharges = sum(Total.Discharges))
  ggplot(aes(x=state, y=DRG.Definition, size=discharges)) + geom_point()


# Make the map object
require("ggmap")
map <- get_map(location='texas', zoom=6, maptype = "terrain",
             source='google',color='bw')
atx <- get_map(location='austin, texas', zoom=8, maptype = "terrain",
               source='google',color='bw')


# Plot
ggmap(atx) + 
  geom_point(
    aes(x=longitude, y=latitude,  size=Total.Discharges, alpha=Total.Discharges, color=Average.Total.Payments, position="jitter"),
    data=filter(df, state=="TX", Average.Total.Payments < 40000) , na.rm = T)  + 
  scale_color_gradient(low="yellow", high="red")
