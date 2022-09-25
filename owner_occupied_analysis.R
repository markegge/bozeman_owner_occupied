library(sf)
library(data.table)
library(mapview)
library(stringr)

# downloaded from https://ftpgeoinfo.msl.mt.gov/Data/Spatial/MSDI/Cadastral/
# based on Sept. 7 2022 file
shp <- st_read("shp/MontanaCadastral_GDB/Montana_Cadastral.gdb", 
               layer = "OwnerParcel", 
               options = c("METHOD=SKIP"))

# bozeman city limits
# https://public-bozeman.opendata.arcgis.com/datasets/bozeman::city-limits-3/
bzn <- st_read("shp/City_limits/City_Limits.shp")
bzn <- st_transform(bzn, st_crs(shp))

# zoning <- st_read("shp/Zoning/Zoning.shp")
# zoning <- st_transform(zoning, st_crs(shp))
# zoning <- zoning[zoning$ZONING %in% c( "R-O", "R-3", "R-4", "R-2", "R-1", "R-S",
#                                       "B-2", "R-MH", "NEHMU", "B-2M", "UMU", "R-5", "REMU", 
#                                       "B-3", NA)]
# bozeman future land use
# https://public-bozeman.opendata.arcgis.com/datasets/bozeman::future-land-use-1/
flu <- st_read("shp/Future_Land_Use/Future_Land_Use.shp")
flu <- st_transform(flu, st_crs(shp))
flu <- flu[flu$MPMAP %in% c("RDMU", "RD", "MSMU"), ]

# remove ag land, exempt land, etc.
shp <- shp[!shp$PropType %in% 
             c(NA, "VAC_R - Vacant Land - Rural", "EP - Exempt Property", 
               "FARM_R - Farmstead - Rural", "VAC_U - Vacant Land - Urban", 
               "NV - Non-Valued Property", "GRAVEL - Gravel Pit",
               "RV_MOB PARK - COML", "CN - Centrally Assessed Non-Valued Property", 
               "IR - Industrial Rural", "IU - Industrial Urban", 
               "GOLF - Golf Course", "EP_PART - Partial Exempt Property", 
               "CA - Centrally Assessed", "MINE - Mining Claim", "FARM_U - Farmstead - Urban", 
               "NVS - Non-Valued with Specials", "TP - Tribal Property", 
               "LEASE_U - On Leased Land - Urban", "OIL_IMP - Oilfield Improvements", 
               "LEASE_R - On Leased Land - Rural", "MC - Mining Claim", "VU - Vacant Land Urban", 
               "LA - Locally Assessed Utility"), ]
# shp <- shp[shp$PropType %in% 
#              c("IMP_U - Improved Property - Urban", "TU - Townhouse Urban", 
#                "IMP_R - Improved Property - Rural", 
#                "KR - Condominium Rural", "CONDO_U - Condo - Urban", "CONDO_R - Condo - Rural", 
#                "TR - Townhouse Rural", "APT_U - Apartment Urban", 
#                "IR - Industrial Rural", "APT_R - Apartment Rural", 
#                "PARK_U - Manufactured Home Park -  Urban", 
#                "PARK_R - Manufactured Home  Park -  Rural"), ]


shp <- shp[shp$CountyName == "Gallatin", ]
shp <- st_cast(shp, "MULTIPOLYGON")

# filter to parcels within Bozeman city limits
shp <- st_filter(shp, bzn)
# keep where the Future Land Use is residential, residential mixed use, or maker space mixed use
shp <- st_filter(shp, flu)


shp <- shp[!is.na(shp$PARCELID), ]
shp <- shp[shp$TotalBuildingValue > 0, ]
shp <- shp[shp$TotalAcres < 5, ] # this may remove a few apartment parcels

DT <- as.data.table(st_drop_geometry(shp))

DT[, `:=`(address_line_1 = clean_strings(AddressLine1),
          owner_address_1 = clean_strings(OwnerAddress1))]

DT[OwnerCity != "BOZEMAN", owner_occupied := FALSE]
DT[address_line_1 == owner_address_1, owner_occupied := TRUE]

house_no <- DT[, str_split(address_line_1, "\\s", simplify = TRUE)]
hn <- house_no[, 1]
DT[, addr_no := hn]

owner_no <- DT[, str_split(owner_address_1, "\\s", simplify = TRUE)]
on <- owner_no[, 1]
DT[, ownr_no := on]

DT[is.na(owner_occupied), owner_occupied := addr_no == ownr_no]

table(DT$owner_occupied, useNA = "ifany")

DT[, sum(owner_occupied) / .N]

shp <- merge(shp, DT[, .(PARCELID, owner_occupied)])
mapview(shp, zcol = "owner_occupied")
