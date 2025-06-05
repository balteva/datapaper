# ---------------------------------------------------------------------------------
# Title       : Summary statistics of 2019-2012 Culicoides surveillance campaign
# Description : Generates plots for seasonal and inter-annual population dynamics,
#               histograms and species distribution maps 
# Author      : BALTUSYTE Ieva (ievbalt@gmail.com)
# Date        : 2025-05-27
# Project     : VECTOCLIM
# ---------------------------------------------------------------------------------
setwd("C:/Users/ibalt/OneDrive/Desktop/uni/M2 stage/Cullicoides_data/CIRAD/culicoides_ml/DP_old")

## Loading packages 

library(tidyverse)  ## Version '2.0.0'
library(sf)         ## Version '1.0.19'
library(scatterpie) ## Version '0.2.4'
library(scales)     ## Version '1.3.0'

setwd("./datapaper")

## Loading measurement data
measurement_data <- read.delim("./data/measurementorfacts.txt", header = TRUE)%>%
  filter(measurementID %in% c("BETAIL", "BOV", "CAP", "EQU", "OVI", "ALT", "SURF_CANT",
                              "EVI", "NDVI", "PP", "FG_ERA5", "HU_mean_ERA5", "QQ_ERA5", "RR_ERA5", 
                              "TP_mean_ERA5", "TP_min_ERA5", "TP_max_ERA5", "TPsoil_mean", "swvl1_mean")) %>%
select(c(id,measurementID,measurementValue))%>%
  mutate(measurementValue=as.numeric(measurementValue))%>%
  mutate(measurementValue = case_when(measurementID=="NDVI" & measurementValue <= -1 ~NA,
                                      measurementID=="EVI" & measurementValue <= -1  ~NA, TRUE ~measurementValue)) 
# EVI and NDVI measurements include erroneous values obtained from the Copernicus Climate Data store and require pre-processing
  


## Loading entomological data
data <- read.delim("./data/occurrence.txt", header = TRUE) %>%
  select(eventID,individualCount, organismQuantityType, occurrenceStatus, organismQuantity, scientificName)


## Loading sampling location information
event <- read.delim("./data/event.txt", header = TRUE) %>%
  select(eventID, eventDate,startDayOfYear,month,year, habitat, locationID, stateProvince)%>%
  mutate(date = as.Date(startDayOfYear - 1, origin = paste0(year, "-01-01")))

## Joining the two dataframes
df_indiv <- data %>%
  left_join(event, by="eventID") %>%
  select(-c(eventDate, startDayOfYear)) %>%
  relocate(stateProvince, locationID,date, .after= eventID) %>%
  filter(organismQuantityType=="individuals")#select organism type of interest (i.e. all specimens/only females/parous females.. )



###################################
## Figure 1. Distribution of values
###################################
measurement_data %>%
  group_by(measurementID)%>%
  ggplot(aes(x=measurementValue))+
  geom_histogram(fill="grey75", color="grey30")+
  facet_wrap(~measurementID, scales="free")+
  theme_bw()+
  theme(title=element_text(face="bold"))+
  labs(x="measurement value",
       y="measurement occurence",
       title="Fig. 1 Distribution of values of quantitative variables among observations/events")

################################
## Figure 3. Trapping schedule
################################

## Sumarising data
trap_info <- df_indiv %>%
  select(year, date, locationID)%>%
  distinct()%>%
  mutate(month=month(date), week=week(date))%>%
  group_by(year, month, week)%>%
  summarise(trap_count=n(), .groups="drop")


my_colors<- c("#31a354", "#7fcdbb", "deepskyblue3","#253494" )

## Creating plot
ggplot(trap_info, aes(x=week, y=trap_count, fill=as.factor(year)))+
  geom_bar(stat = "identity", position = "stack", width = 0.9) +
  scale_fill_manual(values =  my_colors)+
  facet_wrap(~year, scales="fixed", ncol=1) +
  scale_x_continuous(breaks = seq(1, 52, 2)) +
  labs(title = "Fig. 3 Trap collection schedule for the 2009 - 2012 study period",
       x = "Week of the year",
       y = "Number of traps at national scale",
       fill= "Surveillance period")+
  theme_minimal() +
  theme(axis.title.x = element_text(size = 11, face="bold"),
        axis.title.y = element_text(size = 11, face="bold"),
        plot.title = element_text(hjust = 0.5, size= 14, face="bold"),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        strip.text=element_text(size=12,face="bold"),
        legend.position="bottom",
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 10))


############################################
## Figure 5.  Population composition (maps)
############################################


## Summarizing data
df_region <- df_indiv %>%
  filter(!occurrenceStatus=="absent")%>%
  mutate(scientificName = case_when( !(scientificName %in% species_of_interest) ~ "other species", TRUE ~ scientificName))%>%
  group_by(scientificName ,stateProvince ) %>% 
  summarise(sum_trap = sum(individualCount), .groups = "drop")

df_region_wide <- pivot_wider(df_region, names_from = scientificName, values_from = sum_trap, values_fill = 0)

## Loading France's shape file
france <- st_read("./france/france_regions_modified.shp")
france_centroids <- st_centroid(france)

france_species <- left_join(france_centroids, df_region_wide, by = c("region" = "stateProvince")) #joining culicoides df with geometry

coords <- st_coordinates(france_species)
france_species$lon <- coords[,1]
france_species$lat <- coords[,2]

## Selecting the columns to project
species_cols <- c("obsoletus/scoticus", "pulicaris","punctatus", "newsteadi", "dewulfi", "imicola", "chiopterus", "other species")

### Plotting
ggplot() +
  geom_sf(data = france, fill = "grey95", color = "black") +
  geom_scatterpie(
    aes(x = lon, y = lat, group = region),
    data = st_drop_geometry(france_species), 
    cols = species_cols, alpha=0.88, pie_scale = 3.5) +
  coord_sf() +
  theme_minimal()+
  scale_fill_brewer(palette = "Set2", name="Species")+
  theme(legend.position="bottom",
        plot.title = element_text(size=13, hjust=0.5, face="bold"),
        axis.title.x=element_blank(),
        axis.title.y = element_blank()) + 
  ggtitle("Fig. 5 Spatial Culicoides population structure with seven most abundant Culicoides species\nacross different administrative regions in France")+
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))




#######################
## ANALYSIS
#######################

##########################
## Types of farms/habitats
##########################
farm_counts <- df_indiv %>% 
  select(c(locationID, habitat))%>%
  group_by(locationID, habitat)%>%
  unique() %>%
  group_by(habitat)%>%
  summarise(sites_per_habitat=n())%>%
  mutate(total_sites=sum(sites_per_habitat))%>%
  mutate(percent=sites_per_habitat/total_sites*100)

##########################################################################################
# habitat                                         sites_per_habitat   total_sites percent
# 1 cattle farm                                                 137         210  65.2  
# 2 educational farm - cattle farm                                1         210   0.476
# 3 goat farm                                                     7         210   3.33 
# 4 horse farm                                                    1         210   0.476
# 5 mixed farming                                                44         210  21.0  
# 6 sheep farm                                                   19         210   9.05 
# 7 vocational agricultural college - cattle farm                 1         210   0.476
##########################################################################################

####################
## Unique sites per year
####################
unique_trap_counts <- df_indiv %>%
  select(c(locationID, year))%>%
  group_by(locationID, year)%>%
  unique()%>%
  group_by(year)%>%
  summarise(total_sites=n())
#################################
#    year       total_sites
# 1  2009         168
# 2  2010         171
# 3  2011         166
# 4  2012         162
#################################


## Investigating the most abundant species
most_abundant_species <- df_indiv %>%
  group_by(scientificName)%>%
  summarise(sum_per_species=sum(individualCount))%>%
  arrange(desc(sum_per_species))%>%
  mutate(total_culicoides=sum(sum_per_species))

######################################
# 1 obsoletus/scoticus  4186089
# 2 dewulfi             597361
# 3 imicola             521319
# 4 chiopterus          177757
# 5 newsteadi           163853
# 6 punctatus           139213
# 7 pulicaris           103284
# 8 achrayi             51401
# 9 brunnicans          49644
# 10 deltus             49536
######################################


## Counts of the seven most prevalent species
main_species_metrics <-  most_abundant_species %>%
  slice_head(n = 7) %>%  
  mutate(percentage=sum_per_species/total_culicoides*100)

#####################################################################
# scientificName         sum_per_species total_culicoides percentage
# 1 obsoletus/scoticus         4186089          6340177       66.0 
# 2 dewulfi                     597361          6340177       9.42
# 3 imicola                     521319          6340177       8.22
# 4 chiopterus                  177757          6340177       2.80
# 5 newsteadi                   163853          6340177       2.58
# 6 punctatus                   139213          6340177       2.20
# 7 pulicaris                   103284          6340177       1.63
#####################################################################




############################################
## Plotting only the seven species of interest
############################################

species_of_interest <- c("obsoletus/scoticus", "dewulfi", "imicola", "chiopterus","newsteadi", "punctatus", "pulicaris" ) 


df_main_species <- df_indiv %>%
  filter(!occurrenceStatus == "absent") %>%
  mutate(scientificName = case_when( !(scientificName %in% species_of_interest) ~ "other species", TRUE ~ scientificName))%>%
  mutate(month=month(date))%>%
  filter(!scientificName=="other species") 




##################################
## Figure 6. Interannual dynamics
##################################

df_main_species %>%
  group_by(month,year, scientificName)%>%
  summarise(
    mean_abundance = mean(individualCount),
    median_abundance = median(individualCount),
    q1 = quantile(individualCount, 0.25),
    q2 = quantile(individualCount, 0.75), .groups = "drop") %>%
  mutate(date = ymd(paste(year, month, "01", sep = "-")))%>%#sumarising data
  #creating plot
  ggplot(aes(x = date, y = median_abundance)) +
  geom_ribbon(aes(ymin = q1, ymax = q2, fill = "Central Range (25–75%)"), alpha = 0.6) +
  geom_point(aes(y = median_abundance), shape = 21, fill = "white", size = 1, stroke = 1, alpha=0.8) +
  geom_line(aes(y = median_abundance, color = "Median"), linewidth = 0.8, alpha = 0.9) +
  labs(title = paste0("Fig. 6 Intra-annual variability of seven most abundant Culicoides species found in France for the 2009 - 2012 period"),
       y = "Number of individuals per trap",
       x = "Year") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(), 
        axis.title.x = element_text(size = 10, face="bold"),
        axis.title.y = element_text(size = 10, face="bold"),
        plot.title = element_text(size= 12, face="bold"),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        strip.text=element_text(size=10,face="bold"))+
  scale_fill_manual(values = c("Central Range (25–75%)" = "deepskyblue2")) +
  scale_color_manual(values = c("Median" = "deepskyblue4")) +
  facet_wrap(~scientificName, scales="free", ncol=3)



#########################################
## Figure 7. Seasonal dynamics (boxplots)
#########################################

df_main_species %>%
  ggplot(aes(x=as.factor(month), y=individualCount, fill= scientificName)) +
  geom_boxplot(outliers= F, fill="deepskyblue3")  + #removing outliers
  theme_minimal() +
  xlab("month") +
  ylab("Number of individuals per trap") +
  labs(fill="species", title="Fig. 7 Seasonal dynamics of the most abundant Culicoides spp. observed during the 2009 - 2012 period")+
  scale_fill_brewer(palette = "Dark2")+
  theme(plot.title = element_text(face="bold", size=13),
        axis.text.x = element_text(size=10, face="bold"),
        axis.text.y = element_text(size=10, face="bold"),
        axis.title.x = element_text(size=12, face="bold"),
        axis.title.y = element_text(size=12, face="bold"),
        strip.text=element_text(size=12,face="bold"),
        legend.position="none") +
  facet_wrap(~scientificName,scales="free_y", ncol=3)


###############################################
## Figure 8. Summary of trap count frequencies 
###############################################

df_indiv%>%
  filter(!occurrenceStatus == "absent") %>%
  group_by(eventID)%>%
  summarise(individualCount=sum(individualCount))%>%
  ggplot(aes(individualCount)) +
  geom_histogram(col = "black", alpha= 0.7, fill="deepskyblue")+
  geom_vline(aes(xintercept = mean(individualCount), color = "mean"), linetype = "solid", size = 1)+ #adding descriptive stats
  geom_vline(aes(xintercept = median(individualCount), color = "median"), linetype = "solid", size = 1)+
  scale_x_log10(labels = label_number(), breaks=breaks_log(n=6, base=10))+
  theme_minimal()+
  ggtitle(expression("Fig. 8 Distribution of positive" *italic(" Culicoides spp. ")* "counts per trap from 2009 to 2012 in France."))+
  theme(plot.title = element_text(hjust = 0.5, face="bold"),
        axis.text.x = element_text(size=10, face="bold"),
        axis.text.y = element_text(size=10, face="bold"),
        axis.title.x = element_text(size=12, face="bold"),
        axis.title.y = element_text(size=12, face="bold"),
        strip.text=element_text(size=12,face="bold"),
        legend.position = "bottom")+
  labs(x="Individuals caught in one trapping session (log scale)",
       y="Frequency", color = "Statistics")+
  scale_color_manual(values=c("mean"="darkred", "median"="darkblue"))

