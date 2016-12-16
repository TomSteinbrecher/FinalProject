options(stringsAsFactors = FALSE)
cdc <- read.csv(file = "cdc.csv")

library(tidyr)

names(cdc) <- c("year", "name", "source", "topic", "measure", "var_name",
                "var_unit", "var_value")

#remove all unneded varaibles
keep <- setdiff(names(cdc), c("source", "topic", "measure", "var_unit"))
keep

cdc <- cdc[, keep]

#rearrange
cdc <- spread(cdc, key = var_name, value = var_value)

#Rename
names(cdc) <- c("year", "name", "acpp", "cc", "fstprp", "fstpp", "gctr", "stpp")

#BEA
bea <- read.csv(file = "bea.csv", check.names = FALSE)

keep <- setdiff(names(bea), c("GeoFips", "LineCode"))
#Get rid of unneded data
bea <- bea[, keep]

bea <- gather(bea, key = year, value = var_value, -GeoName, -Description)

names(bea) <- c("name", "var_name", "year", "var_value")
#Fix messed up state names
bea$name <- gsub(x = bea$name, pattern = "*", replacement = "", fixed = TRUE)

bea$year <- as.numeric(bea$year)
bea  <- subset(bea, subset = (year >= 1970))
bea$var_value <- as.numeric(bea$var_value)
bea <- spread(bea, key = var_name, value = var_value)

names(bea) <- c("name", "year", "pcpi", "pi", "pop")

cdcbea <- merge(cdc, bea, by = c("name", "year"))

cpi <- read.csv(file = "cpi.csv")
names(cpi) <- c("year", "cpi")
base_cpi <- cpi[cpi$year == 2014, "cpi"]

# Inflation factor = base cpi / current cpi
cpi$inflation_factor <- base_cpi / cpi$cpi

cdcbeacpi <- merge(cdcbea, cpi, by = "year")

gst <- read.csv("gst.csv")
gst$gstpp <- gst$gstpp/100 #Convert to dollars
cdcbeacpi <- merge(cdcbeacpi, gst, by= c("name", "year"))

# Make all real adjustments
cdcbeacpi$rpcpi <- cdcbeacpi$pcpi * cdcbeacpi$inflation_factor
cdcbeacpi$racpp <- cdcbeacpi$acpp * cdcbeacpi$inflation_factor
cdcbeacpi$rfstpp <- cdcbeacpi$fstpp * cdcbeacpi$inflation_factor
cdcbeacpi$rstpp <- cdcbeacpi$stpp * cdcbeacpi$inflation_factor
cdcbeacpi$rgctr <- cdcbeacpi$gctr * cdcbeacpi$inflation_factor
cdcbeacpi$rpi <- cdcbeacpi$pi * cdcbeacpi$inflation_factor
cdcbeacpi$rgstpp <- cdcbeacpi$gstpp * cdcbeacpi$inflation_factor
cdcbeacpi$rpcpi <- cdcbeacpi$pcpi * cdcbeacpi$inflation_factor


write.csv(cdcbeacpi,file="cigdata.csv", row.names = F)

