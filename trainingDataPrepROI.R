library(sf)

library(dplyr)

library(lwgeom)

library(ggplot2)

"1) Define a region of interest bounding box
2) Separate SCDB into felled, young, mature con, mature blv
3) Sample X polygons per type and per region
4) Export samples as one shapefile"


#wd = "C:/Users/tom/Documents/Data Science MSc/Project/SupportingData/"

wd = "C:/Users/tom/Documents/Data Science MSc/Project/SupportingData"

#######CREATE REGION OF INTEREST POLYGON FROM BNG BOUNDING COORDINATES#####

xmin = 338091
ymin = 565106
xmax = 386911
ymax = 614409

f = rbind(c(xmin,ymin), c(xmin,ymax), c(xmax,ymax), c(xmax,ymin), c(xmin,ymin))
p = st_polygon(list(f))
extent = st_sfc(p, crs =27700)

########IMPORT DATA#####################################

#1 Get sub-compartments (already clipped by woodland)
scdb <- st_read(file.path(wd,'GB_SCDB_Oct_2018_flyr_BNG_FIXED_REGIONS_SP.shp'), stringsAsFactors = FALSE)

#1a Set to correct CRS then clip by the region of interest
scdb <- st_set_crs(scdb, value=27700)
scdb <- st_intersection(scdb, extent)

#2 Get the species data
species <- read.csv(file.path(wd,'Species.csv'),stringsAsFactors=FALSE)


########MAKE BROADLEAVED AND CONIFER SPECIES LISTS########################
blvs = species %>% filter(SP_TYPE == 'B') %>% select(SPECIES)
blvs = as.vector(blvs$SPECIES)

cons = species %>% filter(SP_TYPE == 'C') %>% select(SPECIES)
cons = as.vector(cons$SPECIES)

spis = as.vector(species$SPECIES)

#######ADD AN AREA FIELD AND REMOVE SMALL POLYS##################
scdb$area <- as.numeric(st_area(scdb))

#Filter out sample polygons < 100m2 (Sentinel 1 Resolution)
scdb = scdb %>% filter(area >= 100)

#######CLASSIFY POLYGONS BLV, CON, FELLED, YOUNG#################

scdb = scdb %>% mutate(TYPE = case_when(
  PRI_PLYEAR > 2003 & (is.na(SEC_PLYEAR)| SEC_PLYEAR > 2003) & (is.na(TER_PLYEAR) | TER_PLYEAR > 2003)  ~ 'young',
  PRI_SPCODE %in% blvs & (is.na(SEC_SPCODE) | SEC_SPCODE %in% blvs) & (is.na(TER_SPCODE) | TER_SPCODE %in% blvs) ~ 'broadleaved',
  PRI_SPCODE %in% cons & (is.na(SEC_SPCODE) | SEC_SPCODE %in% cons) & (is.na(TER_SPCODE) | TER_SPCODE %in% cons) ~ 'conifer',
  PRI_LUCODE== 'PFE' & (is.na(SEC_LUCODE) | SEC_LUCODE =='PFE') & (is.na(TER_LUCODE) | TER_LUCODE =='PFE') ~ 'felled'))

#2nd Sweep for mixed and open not already set by the above
scdb = scdb %>% mutate(TYPE = case_when(       
  ! is.na(TYPE) ~ TYPE,
  PRI_SPCODE %in% spis & (SEC_SPCODE %in% spis) & (is.na(TER_SPCODE) | TER_SPCODE %in% spis) & (is.na(TYPE)) ~ 'mixed',
  is.na(PRI_SPCODE) & is.na(SEC_SPCODE) & is.na(TER_SPCODE) & (is.na(TYPE)) ~ 'open'
))

#Anything else goes to other:
scdb = scdb %>% mutate(TYPE = ifelse(is.na(TYPE),'other',TYPE))


######DETERMINE THE AMOUNT OF AREA PER CLASS PER REGION#########

scdb_table = scdb %>% st_set_geometry(NULL)

scdb_type_sum = scdb_table %>% group_by(TYPE) %>%
  summarise(total_area = sum(area), count_all = n()) %>%
  mutate(prop_by_area = total_area/sum(total_area))

tooLow = (scdb_type_sum %>% filter(prop_by_area < 0.01) %>% select(TYPE))

tooLow = as.vector(tooLow$TYPE)

#Remove from SCDB layer
scdb = scdb %>% filter(! TYPE %in% tooLow)

#Update table for use later
scdb_table = scdb %>% st_set_geometry(NULL)


######GET SAMPLE POINTS WITHIN POLYGONS#####################

distinct_types = pull(scdb_table %>% distinct(TYPE))

target_sample_per_class <- 1000

rm(outSample)

#create n sample points in each polygon class
for (t in distinct_types){
  subset = scdb %>% filter(TYPE == t)
  sample = st_sample(subset, target_sample_per_class)
  #Force it to get to n samples (it uses bounding box, not polygons)
  while (length(sample) < target_sample_per_class) {
  sampleextra = st_sample(subset, target_sample_per_class)
  sample = c(sample, sampleextra)
  }
  sample = sample[1:target_sample_per_class]
  sample = st_set_crs(sample, 27700)
  typeval = data.frame(TYPE = rep(t, target_sample_per_class))
  s = st_sf(typeval, geometry = sample)
  outSample = rbind(s, if(exists("outSample")) outSample)
}


#######PREPARE DATA FOR OUTPUT########################

#Convert types to codes for models
outSample = outSample %>% mutate(TYPE_CODE = case_when(TYPE =='broadleaved'~0,
                                                       TYPE =='conifer'~1,
                                                       TYPE=='mixed'~2,
                                                       TYPE=='felled'~3,
                                                       TYPE=='young'~4,
                                                       TYPE== 'open'~5,
                                                       TYPE=='other'~6
                                                       ))


#Write out the shapefile

st_write(outSample,file.path(wd,'samplePointsTypes.shp'))

#######PLOT THE OUTCOME##############################
##sf_geom plot not included with regular ggplot2 install. 
# Requires special version of ggplot2 from github Run:
#devtools::install_github("tidyverse/ggplot2")


#outputTest = st_sample(outputAll, 5000)



ggplot() +
  geom_sf(data=outSample, aes(colour=TYPE), size=0.7, fill=NA, show.legend='point') +
  labs(color='Type', title ='Sample Locations')+
  theme_bw()+
  theme(legend.title=element_blank(), legend.text=element_text(size=5))+
  guides(color = guide_legend(override.aes = list(size=1.5)))




