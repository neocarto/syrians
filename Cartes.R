        # ------------------------------------ #
        #              MIGRTAIONS              #
        #      Nicolas LAMBERT, apr 2018       #
        # ------------------------------------ #


# libraries
library("sf")
library("countrycode")
library("readxl")
library("cartography")

setwd("/home/nlambert/Dropbox/NicoFrancoise/Syriens/R")


##########################
# STEP 1 : DATA BUILDING #
##########################

# Download

mapfile <- "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip"
migfile <- "http://www.un.org/en/development/desa/population/migration/data/estimates2/data/UN_MigrantStockByOriginAndDestination_2015.xlsx"
popfile <- "http://api.worldbank.org/v2/en/indicator/SP.POP.TOTL?downloadformat=excel"

# download world basemap
# download.file(url =mapfile, destfile = "data/countries.zip")
# unzip("data/countries.zip", exdir = "data", unzip = "internal")
# file.remove("data/countries.zip")

# Downlad statistical data
# download.file(url=migfile, destfile="data/mig.xlsx")
# download.file(url=popfile, destfile="data/pop.xls")

# Import

# import world countries
countries <- st_read(dsn = "data/ne_50m_admin_0_countries.shp", stringsAsFactors = F)
countries <- countries[,c("ADM0_A3", "ISO_A2","NAME_SORT","geometry")]
colnames(countries) <- c("ISO3", "ISO2","name","geometry")

# import migration data
mig <- data.frame(read_excel("data/mig.xlsx", skip = 15, sheet = "Table 16"))
mig$ISO3 <- countrycode(sourcevar = mig$X__2, origin = "country.name", destination = "iso3c")

# import population data
pop <-  data.frame(read_excel("data/pop.xls", skip = 3, sheet = "Data"))
pop <- pop[,c("Country.Code","Country.Name","X2015")]
colnames(pop) <- c("ISO3","name","pop2015")

# merge countries geometries and mig data
countries <- merge(countries[,c("ISO3", "ISO2","name")], mig[,-(1:5)], 
                   by.x="ISO3", by.y="ISO3", all.x=TRUE)

colnames(pop)
# merge countries geometries and pop data
countries <- merge(countries, pop[,c("ISO3","pop2015")],by.x="ISO3", by.y="ISO3", all.x=TRUE)

# modify col names with ISO3 codes
coln <- countrycode(sourcevar = names(countries)[7:238], origin = "country.name",destination = "iso3c")
names(countries)[7:238] <- coln
names(countries)[48] <- "GGY"

# Antarctica
countries <- countries[countries$ISO3 != "ATA",]

####################
# STEP 2 :  FONCTION
####################


drawmap <- function(x){

origin <- x
fixmax <- 1000000
crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "


# projection

ctr <- st_transform(x = countries, crs = crs)

# output file

if(is.null(origin)){i <- "world"} else {i <- origin}
file <- paste0("outputs/from_",i,"_to_",j,"_fixmax_",fixmax,".png")
title <- paste0("from ",i," to ",j)

# Selection

if (origin %in% colnames(ctr)){

ctr <- ctr[,c("ISO3","name",origin,"pop2015","geometry")]

myctr <- ctr[ctr$ISO3== origin,]

myctr2 <- st_centroid(st_geometry(myctr), of_largest_polygon = TRUE)

buff1 <- st_buffer(x = myctr2, dist=500000 , nQuadSegs = 30)
buff2 <- st_buffer(x = myctr2, dist=1000000 , nQuadSegs = 30)
buff3 <- st_buffer(x = myctr2, dist=1500000 , nQuadSegs = 30)
buff4 <- st_buffer(x = myctr2, dist=2000000 , nQuadSegs = 30)
buff5 <- st_buffer(x = myctr2, dist=2500000 , nQuadSegs = 30)
buff6 <- st_buffer(x = myctr2, dist=3000000 , nQuadSegs = 30)

top <- sort(data.frame(ctr)[,origin], decreasing = TRUE)

# plot the layers
exp_dim <- getFigDim(x = ctr, width = 1500, mar = c(0.2,0.2,1.4,0.2), res = 100)
png(filename = file,  width = exp_dim[1], height = exp_dim[2], res = 100)
par(mar=c(0.2,0.2,1.4,0.2), bg="grey95")

plot(st_geometry(ctr), col = "lightblue", border ="ivory", lwd = 0.5)
plot(st_geometry(ctr[is.na(ctr[origin]),]), col = "#e5edf9", border ="ivory", lwd = 0.5, add=T)

plot(st_geometry(buff6), col="#ff8c8c25", border=NA, lty=2, lwd=1, add=T)
plot(st_geometry(buff5), col="#ff8c8c25", border=NA, lty=2, lwd=1, add=T)
plot(st_geometry(buff4), col="#ff8c8c25", border=NA, lty=2, lwd=1, add=T)
plot(st_geometry(buff3), col="#ff8c8c25", border=NA, lty=2, lwd=1, add=T)
plot(st_geometry(buff2), col="#ff8c8c25", border=NA, lty=2, lwd=1, add=T)
plot(st_geometry(buff1), col="#ff8c8c25", border=NA, lty=2, lwd=1, add=T)
#plot(st_geometry(myctr), col="black", add= T)
propSymbolsLayer(x = ctr, var = origin,
                 inches=0.2, col="red",legend.frame = T, 
                 legend.pos = "topright", fixmax=fixmax)

ctr <- ctr[!is.na(ctr[origin]),]
labelLayer(x = ctr[data.frame(ctr)[,origin] %in% top[1:5],], txt = "ISO3", halo=TRUE, cex = 0.5, col= "#000000", bg = "#FFFFFF50", overlap = FALSE) 

labelLayer(x = myctr, txt = "name", halo=TRUE, cex = 0.8, col= "red", bg = "#FFFFFF50") 


layoutLayer(title = title, 
            sources = "Trends in International Migrant Stock: Migrants by Destination and Origin, 2015", author = "Nicolas Lambert, 2018", 
            col = "darkblue", coltitle = "white", 
            frame = TRUE, scale = NULL, north = FALSE)
dev.off()

}
}


########################################
# STEP 3 :  ON BOUCLE SIUR TOUS LES PAYS
########################################

for (i in countries$ISO3) {drawmap(i)}


