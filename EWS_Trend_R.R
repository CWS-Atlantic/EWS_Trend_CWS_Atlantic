##############################################################
##   Code to prepare EWS data for use in EWSTrend package   ##
##############################################################

require(dplyr)
#require(EWStrend) #open the .R files individually: "Misc_functions.R", "create_data_objects.R", "Describe_trend.R"
require(ggmap)
require(ggplot2)
require(ggpubr)
require(leaflet)
require(leaflet.extras)
require(leafpop)
require(mapedit)
require(miniUI)
require(RColorBrewer)
require(readr)
require(sf)
require(shiny)
require(shinyWidgets)
require(stringr)
require(tidyr)

# library("devtools")
# install_github("pachevalier/tricky")
require(tricky)


#########################
##  make fig of plots  ##
#########################

#Read in EWS NL plots shapefile
plots.sf <- st_read("C:/Users/englishm/Documents/EWS/NL/Shiny App/app/data/EWS_NL_Plots.gdb")


## #Append the .dbf file for the plots
# plot.info <- read_csv("data/nfplotloc_poly_.csv")
# 
# plots@data$YEAR_ADDED <- plot.info$YEAR_ADDED
# plots@data$DESC <- plot.info$DESC
# 
# st_write(plots.sf, "nfplotloc_poly.shp")

# plots.sf <- st_as_sf(plots.sf)
# 
# plots.sf <- st_transform(plots.sf, 4326)
# 
# names(plots.sf) <- c("plot", "plot_name", "utm", "year_added", "description", "geometry")
# 
# plots.centroids <- st_centroid(plots.sf)
# 
# p <- as.data.frame(str_split_fixed(plots.centroids$geometry , ",",2))
# p$plot <- plots.centroids$plot
# names(p) <- c("lon_cent", "lat_cent", "plot")
# p$lon_cent <- gsub("c\\(", "", p$lon_cent)
# p$lat_cent <- gsub("\\)", "", p$lat_cent)
# 
# 
# plots.centroids$lon_cent <- p$lon_cent
# plots.centroids$lat_cent <- p$lat_cent
# 
# plots.centroids$lon_cent <- as.numeric(plots.centroids$lon_cent)
# plots.centroids$lat_cent <- as.numeric(plots.centroids$lat_cent)
# 
# plots.centroids$year_added <- as.factor(plots.centroids$year_added)

# require(celstial)
# #get centroids in deg min sec (required for journal)
# dms <- data.frame(deg2dms(plots.centroids$lat_cent))
# 
# dms$dms <- paste(dms$DEG, dms$MIN, dms$SEC, sep = ":")
# 
# write.csv(dms, "EWS_NL_Plot_Centroids.csv")


#################################
##   species obs proportions   ##
#################################


#subset out species of interest.  
waterfowl <- c("ABDU", 
               "AGWT",
               "AMWI",
               #"BAGO",
               "BLSC",
               "BUFF",
               #"BWTE",
               "CAGO", 
               "COGO",
               "COSN", #not waterfowl
               "COLO", #not waterfowl
               "COME",
               "GRSC",
               "HARD",
               "HERG", #not waterfowl
               "HOME",
               "LESC",
               "LTDU",
               "MALL",
               "NOPI",
               #"NSHO",
               "RBME", 
               "RNDU",
               "RTLO",
               "SPSA", #not waterfowl
               "SUSC",
               "UNYE", #not waterfowl
               "USCA",
               #"WODU",
               "WWSC")


#create vector of labrador plots
lab.plots <- c(20:25, 35:48)

#Read in most recent EWS data file
ews.sf <- read_csv(file = "C:/Users/englishm/Documents/EWS/NL/Processed Data/EWS_NL_Observations_1990-2025.csv")

#rename headers
ews.sf <- set_standard_names(ews.sf)

names(ews.sf)[names(ews.sf) == 'species_e'] <- 'species'
names(ews.sf)[names(ews.sf) == 'males'] <- 'male'
names(ews.sf)[names(ews.sf) == 'females'] <- 'female'


ews.sf <-  ews.sf[order(ews.sf$year),] 

ews.sf$total <- ews.sf$male + ews.sf$female + ews.sf$unknown

#strip whitespace from species names
ews.sf$species <- gsub(" ", "", ews.sf$species)

#create a region variable
ews.sf$region <- "NL"

#assign plots to appropriate region
ews.sf$region <- ifelse(ews.sf$plot %in% lab.plots, "Labrador", "Newfoundland")
ews.sf$region

#deal with terns
ews.sf$species <- gsub("TERN", "UNTE", ews.sf$species)
ews.sf$species <- gsub("UTER", "UNTE", ews.sf$species)
ews.sf$species <- gsub("COTE", "UNTE", ews.sf$species)
ews.sf$species <- gsub("ARTE", "UNTE", ews.sf$species)

#deal with harlequins
ews.sf$species <- gsub("HADU", "HARD", ews.sf$species)

#deal with snipe
ews.sf$species <- gsub("WISN", "COSN", ews.sf$species)


#subset data by waterfowl
ews.wf <- ews.sf[ews.sf$species %in% waterfowl,]

ews.wf.sum <- ews.wf %>%
  group_by(region, species) %>%
  dplyr::summarise(prop = length(species)/length(ews.wf$species))

ews.wf.sum <-  ews.wf.sum[order(ews.wf.sum$prop),] 

sum(ews.wf.sum$prop) #should always equal 1

#create variable for Scaup

scaup <- c("USCA",
           "GRSC",
           "LESC")

common <- c("CAGO",
            "ABDU",
            "RNDU",
            "COGO",
            "AGWT",
            "SUSC")

#create the summary for all scaup
ews.scaup <- ews.wf.sum[ews.wf.sum$species %in% scaup,]

ews.scaup <- ews.scaup %>%
  group_by(region) %>%
  dplyr::summarise(prop = sum(prop))
  
ews.scaup$species <- "Scaup"
ews.scaup <- ews.scaup[,c(1,3,2)]

ews.wf.sum <- bind_rows(ews.wf.sum, ews.scaup)

ews.wf.sum <- ews.wf.sum[!ews.wf.sum$species %in% scaup,]

ews.common <- ews.wf.sum[ews.wf.sum$species %in% common,]

sum(ews.common$prop)

ggplot(data = ews.wf.sum, aes(x = reorder(species, -prop), y = prop, fill = region)) +
  geom_bar(position = position_dodge2(width = 0.9, 
                                      preserve = "single"), 
           stat = "identity",
           colour = "black") +
  theme( 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = c(0.855,0.875),
    legend.box.background = element_rect(colour = "black"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.background = element_blank(), 
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    text = element_text(size = 13)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 0.18)) +
  scale_fill_manual(name = "Region", values = c("Labrador" = "black", "Newfoundland" = "white")) +
  xlab("Species") +
  ylab("Proportion")

#ggsave("EWS_NL_SpeciesProportions_2025.tiff", width = 6, height = 5)

##########################
##   CAGO clutch size   ##
##########################

cago <- ews.wf[ews.wf$species == "CAGO",]

unique(cago$breedtype)
unique(cago$breedcnt)

##i think the breed_type column is messed up, so just use all of it for now
cago <- cago[cago$breedtype %in% c("E", "O"),] ##doesnt include anything from 2010-2019, so theres an error

#deal with the 9999s
cago$breedcnt <- ifelse(cago$breedcnt == "9999", NA, cago$breedcnt)

#deal with the 0s
cago$breedcnt <- ifelse(cago$breedcnt == "0", NA, cago$breedcnt)

#create summary DF for plotting
cago <- cago %>%
  group_by(year) %>%
  dplyr::summarise(mean_clutch = mean(breedcnt, na.rm = T),
                   sd_clutch = sd(breedcnt, na.rm = T),
                   n_nest = length(na.omit(breedcnt)))

ggplot(cago, aes(x = year, y = mean_clutch)) +
  geom_point() +
  geom_errorbar(ymin = cago$mean_clutch - cago$sd_clutch,
                ymax = cago$mean_clutch + cago$sd_clutch,
                stat = "identity",
                width = 0.25) +
  theme( 
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.background = element_blank(), 
    #panel.border = element_rect(colour = "black", fill = NA, size = 1),
    text = element_text(size = 13)) +
  scale_y_continuous(limits = c(0, 8), expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1990,2025, by = 2)) +
  xlab("Year") +
  ylab("Mean CAGO clutch size ± SD") +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 4.2, ymax = 5.2, fill = "black",
           alpha = .20)

mean(cago$mean_clutch, na.rm =T)
sd(cago$mean_clutch, na.rm =T)
range(cago$mean_clutch, na.rm =T)

#ggsave("EWS_NL_CAGO_Clutch_2024.tiff", width = 8, height = 4)


##################
##   TIP calc   ##
##################

#read in tip
tip.code.df = read.csv(file = "C:/Users/englishm/Documents/EWS/Phenology/Indicateur_couples_newABDU.csv", sep=",", header=T)
group.code.df = read.csv(file = "C:/Users/englishm/Documents/EWS/Phenology/GroupsID.csv",  stringsAsFactors= FALSE)


tip.obs <- ews.wf

tip.obs$obscode <- paste(tip.obs$male, tip.obs$female, tip.obs$unknown, tip.obs$total, sep="-")

tip.obs$species.id <- sapply(1:nrow(tip.obs), function(k){as.character(group.code.df$group[group.code.df$species==tip.obs$species[k]])})

tip.obs$TIP <- sapply(1:nrow(tip.obs), 
                      function(k){tip.code.df[match(tip.obs$obscode[k],tip.code.df$obscode),tip.obs$species.id[k]]})

################################################################################
#                         make columns for groups
################################################################################

tip.obs$TIP[is.na(tip.obs$TIP)] <- 0
tip.obs$groups <- ifelse (tip.obs$TIP == 0, tip.obs$total, 0)


#########################
##   phenology index   ##
#########################

require(tidyr)
require(binom)

##???????????????##
##   ABDU only   ##
##???????????????##

#extract ABDU from TIP df
abdu <- filter(tip.obs, species =="ABDU")

#extract the pairs (i.e. 2 unknowns)
pair_abdu <- abdu %>%
  group_by(year) %>%
  filter(male == 0 & female == 0 & unknown == 2) %>%
  dplyr::summarise(pair_m=sum(TIP)) %>%
  complete(year = seq(1990, 2025, by = 1))

pair_abdu$pair_m <- ifelse(is.na(pair_abdu$pair_m), 0, pair_abdu$pair_m)

#extract the truepairs (although we really dont record those)
truepair_abdu <- abdu %>%
  group_by(year) %>%
  filter(male == 1 & female == 1 & unknown == 0) %>%
  dplyr::summarise(pair_m=sum(TIP)) %>%
  complete(year = seq(1990, 2025, by = 1))

truepair_abdu$pair_m <- ifelse(is.na(truepair_abdu$pair_m), 0, truepair_abdu$pair_m)

#bind the two together
pair_abdu <- cbind(pair_abdu, truepair_abdu)
names(pair_abdu) <- c("year", "pair_abdu", "year_dup", "truepair_abdu")

#get a sum of the paired males for the Bordage PI
pair_abdu$pair_m <- (pair_abdu$pair_abdu + pair_abdu$truepair_abdu)


#extract the flocks of males
flock_abdu <-  abdu %>%
  group_by(year) %>%
  filter(male == 0 & female == 0 & unknown > 2) %>%
  dplyr::summarise(unk_flock=sum(TIP)) %>%
  complete(year = seq(1990, 2025, by = 1))

flock_abdu$unk_flock <- ifelse(is.na(flock_abdu$unk_flock), 0, flock_abdu$unk_flock)

#extract the flocks of sex-specific flocks (less obs because we dont really record them)
flock_m_abdu <- abdu %>%
  group_by(year) %>%
  filter(male > 0 & female == 0 & unknown == 0) %>%
  dplyr::summarise(males_flock=sum(TIP)) %>%
  complete(year = seq(1990, 2025, by = 1))

flock_m_abdu$males_flock <- ifelse(is.na(flock_m_abdu$males_flock), 0, flock_m_abdu$males_flock)

#bind the two together
flock_abdu <- cbind(flock_abdu, flock_m_abdu)
#names(flock_abdu) <- c("year", "unk_flock", "year_dup", "males_flock")

#get a sum of the flocked males for the Bordage PI
flock_abdu$flock_m <- (flock_abdu$unk_flock + flock_abdu$males_flock)

#create a DF for phenology
abdu.phen <- data.frame(pair_abdu$year, pair_abdu$pair_m, flock_abdu$flock_m)
names(abdu.phen) <- c("year", "pair_m", "flock_m")

#calculate the Bordage PI
abdu.phen$bordage_pi <- abdu.phen$pair_m/abdu.phen$flock_m


#calculate the new PI (PI = PM / (PM + FM))

x <- binom.confint(abdu.phen$pair_m, abdu.phen$flock_m + abdu.phen$pair_m, method="exact")

phen <- cbind(abdu.phen,x)

abdu.phen <- phen
abdu.phen$species <- "ABDU"

#names(abdu.phen) <- names(agwt.phen)
#names(cago.phen) <- names(agwt.phen)

#make a graph
p <- ggplot(abdu.phen, aes(x = year, y = bordage_pi)) + 
            geom_abline(intercept = 1, slope = 0, colour = "grey30", size = 2, alpha = 0.6) + 
            geom_point(aes(x = year, y = bordage_pi), size = 2) +
            #ylab("Paired Males / (Lone Males + Flocked Males)") +
            ylab(NULL) +
            #xlab("Year") +
            xlab(NULL) +
            theme_bw() +
            theme(plot.title = element_text(size = 10, face = "bold")) +
            ggtitle("ABDU") +
            scale_y_continuous(limits = c(0,2.5),
                               expand = c(0,0)) +
            annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.75, ymax = 1.25, fill = "black",
                     alpha = .50)

abdu.p <- p            

mean(abdu.phen$mean, na.rm = T)
sd(abdu.phen$mean, na.rm = T)
range(abdu.phen$mean, na.rm = T)


######################################
##   Phenology Index for non-ABDU   ##
######################################

bird = scaup  # scaup == this selects the list of scaup

#pick a species
phen.sp <- filter(tip.obs, species %in% bird)


#extract the truepairs (for sexually dimorphic species)
truepair <- phen.sp %>%
  group_by(year) %>%
  filter(male == 1 & female == 1 & unknown == 0) %>%
  dplyr::summarise(pair_m=sum(TIP)) %>%
  complete(year = seq(1990, 2025, by = 1))

truepair$pair_m <- ifelse(is.na(truepair$pair_m), 0, truepair$pair_m)


#extract the flocks of sex-specific flocks (for sexually dimorphic species)
flock.m <- phen.sp %>%
  group_by(year) %>%
  filter(male > 0 & female == 0 & unknown == 0) %>%
  dplyr::summarise(males_flock=sum(TIP)) %>%
  complete(year = seq(1990, 2025, by = 1))

flock.m$males_flock <- ifelse(is.na(flock.m$males_flock), 0, flock.m$males_flock)


phen.sp <- data.frame(truepair$year, truepair$pair_m, flock.m$males_flock)

names(phen.sp) <- c("year", "pair_m", "males_flock")

#calculate Bordage et al PI
phen.sp$bordage_pi <- phen.sp$pair_m/(phen.sp$males_flock)

#calculate new PI
x <- binom.confint(phen.sp$pair_m, phen.sp$males_flock + phen.sp$pair_m, method="exact")

# susc.phen <- cbind(phen.sp, x)
# susc.phen$species <- "SUSC"


scaup.phen <- cbind(phen.sp, x)
scaup.phen$species <- "Scaup"

scaup.phen$upper <- ifelse(scaup.phen$year ==  c(1992, 2001), NA, scaup.phen$upper)
scaup.phen$lower <- ifelse(scaup.phen$year ==  c(1992, 2001), NA, scaup.phen$lower)


all.phen <- rbind(abdu.phen,
                  agwt.phen,
                  #cago.phen,  #there are almost no flocks seen 
                  cogo.phen,
                  come.phen,
                  rbme.phen,
                  rndu.phen,
                  scaup.phen,  #may not use this
                  susc.phen)


#deal with COVID years
all.phen$lower <- ifelse(all.phen$year %in% c(2020,2021), NA, all.phen$lower)
all.phen$upper <- ifelse(all.phen$year %in% c(2020,2021), NA, all.phen$upper)


#make a graph

pd = position_dodge(width = .5)

p <- ggplot(all.phen, aes(x = year, y = mean)) + 
            geom_abline(intercept = 0.5, slope = 0, colour = "grey30", size = 2, alpha = 0.5) + 
            geom_point(size = 2, position = pd, shape = 20) +
            geom_errorbar(aes(ymin = lower,
                              ymax = upper,
                              width=0.20),
                          position = pd) +
            ylab("Paired Males / (Paired Males + Flocked Males)") +
            #ylab(NULL) +
            xlab("Year") +
            #xlab(NULL) +
            theme_bw() +
            #theme(plot.title = element_text(size = 10, face = "bold")) +
            #ggtitle(bird) +
            scale_y_continuous(expand = c(0,0),
                               limits = c(0, 1.1),
                               breaks = c(0, 0.5, 1)) +
            annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.375, ymax = 0.625, fill = "black",
                     alpha = .25) +
            facet_grid(species~.)

p

# ggsave("Phenology_Index_2024.tiff", width = 5.25, height = 7)


####################################
##   phenology index of tot/ibp   ##
####################################

#this doesnt seem to work well for ABDU since 1 unknown = 1 TIP; I think stick with the PI = PM/MF for this paper

bird <- "COME"

phen.tot <- filter(tip.obs, species == bird)

p.tot <- phen.tot %>%
  group_by(year, plot) %>%
  dplyr::summarise(tot = sum(total),
                   tip.tot = sum(TIP),
                   tib = sum(TIP)*2) #%>%
  #complete(year = seq(1990, 2019, by = 1))

p.tot$pi <- p.tot$tot/p.tot$tib

ggplot(p.tot, aes(x = year, y = pi, group = year)) + 
  geom_violin() + 
  # geom_point(aes(x = year, y = pi), size = 5) +
  ylab("Total Birds / Total Breeding Population") + 
  xlab("Year") +
  theme_bw() +
  ggtitle(bird) +
  #scale_y_continuous(limits = c(0,max(p.tot$pi + 1))) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.75, ymax = 1.25, fill = "black",
           alpha = .20)

p.tot <- p.tot %>%
  group_by(year) %>%
  dplyr::mutate(pi = binom.confint(tot, tip.tot + tot, method="exact"))


                                                                        ###################
                                                                        ##   EWS Trend   ##
                                                                        ###################

# Doing this from a github repository right now
# Make sure the appropriate files are loaded in

#require(EWStrend)
require(rjags)
require(loo)
require(jagsUI)
require(plyr)

#set the JAGS directory
jags_dir_path <- "C:/Users/englishm/Documents/EWS/BDJV Tech report/EWStrend-master/inst/jags_files"


str(tip.obs)

tip.obs <- as.data.frame(tip.obs)

ews.trend <- select(tip.obs,
                    #X1,
                    year,
                    plot,
                    species,
                    male,
                    female,
                    unknown,
                    total,
                    TIP,
                    region)

ews.trend$strata <- ifelse(ews.trend$region == "Newfoundland", 1, 2)

str(ews.trend)

ews.trend <- ews.trend %>%
  complete(species, plot = 1:52, year = 1990:2025, fill = list(total = NA, TIP = NA))


################################################################################
flown.df <- read.csv(file = "C:/Users/englishm/Documents/EWS/BDJV Tech report/Plots.csv") 
flown.df$plot_ID <- paste(flown.df$year, flown.df$Plot, sep="_")
flown.df$plot_ID <- as.factor(flown.df$plot_ID)

#At the end of this we need a column for year, plot, region,  species, and the
#summed TIP to feed into the "select_sp" function
sum.tip.obs <- tip.obs %>%
  group_by(species) %>%
  complete(plot = 1:52, year = 1990:2025, fill = list(total = NA, TIP = NA)) %>% 
  mutate(plot_ID = paste(year, plot, sep="_")) 

sum.tip.obs$plot_ID <- as.factor(sum.tip.obs$plot_ID)


sum.tip.obs$flown <- ifelse(sum.tip.obs$plot_ID %in% flown.df$plot_ID, T, F)


sum.tip.obs <- sum.tip.obs %>% 
  group_by(plot_ID, species, flown) %>%
  dplyr::summarise(TIP = sum(TIP, na.rm = T)) %>% 
  separate(plot_ID, into = c("year", "plot")) %>% 
  filter(species %in% waterfowl) %>%
  mutate(region = ifelse(plot %in% lab.plots, 1, 2), 
         year = as.numeric(year), 
         plot = as.numeric(plot))


#account for COVID
sum.tip.obs$TIP <- ifelse(sum.tip.obs$year %in% c(2020,2021), NA, sum.tip.obs$TIP)


#account for COVID and years not flown
sum.tip.obs$TIP <- ifelse(sum.tip.obs$flown == FALSE, NA, sum.tip.obs$TIP)

## set the region if you only want to use one stratum

#sum.tip.obs$region <- 1

ews.scaup <- sum.tip.obs[sum.tip.obs$species %in% scaup,]

ews.scaup <- ews.scaup %>%
  group_by(year, plot, flown, region) %>%
  dplyr::summarise(TIP = sum(TIP, na.rm = T))
                   #TIB = sum(TIB, na.rm = T))

ews.scaup$species <- "Scaup"
  
#account for COVID and years not flown
ews.scaup$TIP <- ifelse(ews.scaup$flown == FALSE, NA, ews.scaup$TIP)


#deal with non-TIP species
notcountedin1992 <- c("SPSA", "HERG", "UNYE", "COSN", "UNTE")

non.waterfowl.df <- ews.sf %>%
  group_by(species) %>%
  complete(plot = 1:52, year = 1990:2025, fill = list(total = NA, TIP = NA)) %>% 
  mutate(plot_ID = paste(year, plot, sep="_")) 

non.waterfowl.df$plot_ID <- as.factor(non.waterfowl.df$plot_ID)


non.waterfowl.df$flown <- ifelse(non.waterfowl.df$plot_ID %in% flown.df$plot_ID, T, F)


non.waterfowl.df <- non.waterfowl.df %>% 
  group_by(plot_ID, species, flown) %>%
  dplyr::summarise(total = sum(total, na.rm = T)) %>% 
  separate(plot_ID, into = c("year", "plot")) %>% 
  filter(species %in% notcountedin1992) %>%
  mutate(region = ifelse(plot %in% lab.plots, 1, 2), 
         year = as.numeric(year), 
         plot = as.numeric(plot))


#account for 1992
non.waterfowl.df$total <- ifelse(non.waterfowl.df$year ==  "1992", NA, non.waterfowl.df$total)


#account for COVID and years not flown
non.waterfowl.df$total <- ifelse(non.waterfowl.df$flown == FALSE, NA, non.waterfowl.df$total)

##Create dataframes for JAGS analysis

###############################
##  Waterfowl JAGS analysis  ##
###############################

#Created DF based on species name
abdu.df <- select_sp(x = sum.tip.obs, 
                     sp = "ABDU" ,
                     year = seq(1990,2025),
                     plots = c(1:52),
                     strata = c(1,2),
                     speciesname = "species", 
                     plotname = "plot", 
                     strataname = "region",
                     yearname ="year",
                     responsename = "total")


#-------------------------#
# start the JAGS analysis #
#-------------------------#

abdu.jags <- dataForJAGS(abdu.df)


start.time <- Sys.time()


abdu.pois.model.results <- trend(abdu.jags, model="poisson", wdrive=jags_dir_path)

abdu.nb.model.results <- trend(abdu.jags, model="NB", wdrive=jags_dir_path)
 
abdu.zip.model.results <- trend(abdu.jags, model="ZIP", wdrive=jags_dir_path)

abdu.zinb.model.results <- trend(abdu.jags, model="ZINB", wdrive=jags_dir_path)


end.time <- Sys.time()

end.time - start.time

#model diagnostics

abdu.elpd <- loo::loo_compare(loo_fn(abdu.pois.model.results), #model 1 - poisson
                              loo_fn(abdu.nb.model.results),   #model 2 - negative binomial
                              loo_fn(abdu.zip.model.results),  #model 3 - zero-inflated poisson
                              loo_fn(abdu.zinb.model.results)) #model 4 - zero-inflated negative binomial

abdu.elpd

converge_fn(abdu.zinb.model.results)

#jags.summary(agwt.nb.model.results)


#combine both strata in to one
#non.waterfowl.df$region <- 1

#############################
##  Non Waterfowl Species  ##
#############################


#HERG
herg.df <- select_sp(x = non.waterfowl.df, 
                     sp = "HERG", 
                     year = seq(1990, 2025),
                     plots = 1:52, 
                     strata = c(1,2), 
                     speciesname = "species", 
                     plotname = "plot", 
                     strataname = "region",
                     yearname = "year", 
                     responsename = "total")

str(herg.df)


herg.jags <- dataForJAGS(herg.df)


start.time <- Sys.time()


herg.pois.model.results <- trend(herg.jags, model="poisson", wdrive=jags_dir_path)

herg.nb.model.results <- trend(herg.jags, model="NB", wdrive=jags_dir_path)

herg.zip.model.results <- trend(herg.jags, model="ZIP", wdrive=jags_dir_path)

herg.zinb.model.results <- trend(herg.jags, model="ZINB", wdrive=jags_dir_path)


end.time <- Sys.time()

end.time - start.time

#model diagnostics

herg.elpd <- loo::loo_compare(loo_fn(herg.pois.model.results), #model 1 - poisson
                              loo_fn(herg.nb.model.results),   #model 2 - negative binomial
                              loo_fn(herg.zip.model.results),  #model 3 - zero-inflated poisson
                              loo_fn(herg.zinb.model.results)) #model 4 - zero-inflated negative binomial

herg.elpd


converge_fn(herg.nb.model.results)


#SPSA
spsa.df <- select_sp(x = non.waterfowl.df, 
                     sp = "SPSA", 
                     year = seq(1990, 2024),
                     plots = 1:52, 
                     strata = c(1,2), 
                     speciesname = "species", 
                     plotname = "plot", 
                     strataname = "region",
                     yearname = "year", 
                     responsename = "total")

str(spsa.df)


spsa.jags <- dataForJAGS(spsa.df)


start.time <- Sys.time()


spsa.pois.model.results <- trend(spsa.jags, model="poisson", wdrive=jags_dir_path)

spsa.nb.model.results <- trend(spsa.jags, model="NB", wdrive=jags_dir_path)

spsa.zip.model.results <- trend(spsa.jags, model="ZIP", wdrive=jags_dir_path)

spsa.zinb.model.results <- trend(spsa.jags, model="ZINB", wdrive=jags_dir_path)


end.time <- Sys.time()

end.time - start.time

#model diagnostics

spsa.elpd <- loo::loo_compare(loo_fn(spsa.pois.model.results), #model 1 - poisson
                              loo_fn(spsa.nb.model.results),   #model 2 - negative binomial
                              loo_fn(spsa.zip.model.results),  #model 3 - zero-inflated poisson
                              loo_fn(spsa.zinb.model.results)) #model 4 - zero-inflated negative binomial

spsa.elpd


converge_fn(spsa.nb.model.results)


#UNYE
unye.df <- select_sp(x = non.waterfowl.df, 
                     sp = "UNYE", 
                     year = seq(1990, 2024),
                     plots = 1:52, 
                     strata = c(1,2), 
                     speciesname = "species", 
                     plotname = "plot", 
                     strataname = "region",
                     yearname = "year", 
                     responsename = "total")

str(unye.df)


unye.jags <- dataForJAGS(unye.df)


start.time <- Sys.time()


unye.pois.model.results <- trend(unye.jags, model="poisson", wdrive=jags_dir_path)

unye.nb.model.results <- trend(unye.jags, model="NB", wdrive=jags_dir_path)

unye.zip.model.results <- trend(unye.jags, model="ZIP", wdrive=jags_dir_path)

unye.zinb.model.results <- trend(unye.jags, model="ZINB", wdrive=jags_dir_path)


end.time <- Sys.time()

end.time - start.time

#model diagnostics

unye.elpd <- loo::loo_compare(loo_fn(unye.pois.model.results), #model 1 - poisson
                              loo_fn(unye.nb.model.results),   #model 2 - negative binomial
                              loo_fn(unye.zip.model.results),  #model 3 - zero-inflated poisson
                              loo_fn(unye.zinb.model.results)) #model 4 - zero-inflated negative binomial

unye.elpd


converge_fn(unye.zinb.model.results)


#cosn
cosn.df <- select_sp(x = non.waterfowl.df, 
                     sp = "COSN", 
                     year = seq(1990, 2024),
                     plots = 1:52, 
                     strata = c(1,2), 
                     speciesname = "species", 
                     plotname = "plot", 
                     strataname = "region",
                     yearname = "year", 
                     responsename = "total")

str(cosn.df)


cosn.jags <- dataForJAGS(cosn.df)


start.time <- Sys.time()


cosn.pois.model.results <- trend(cosn.jags, model="poisson", wdrive=jags_dir_path)

cosn.nb.model.results <- trend(cosn.jags, model="NB", wdrive=jags_dir_path)

cosn.zip.model.results <- trend(cosn.jags, model="ZIP", wdrive=jags_dir_path)

cosn.zinb.model.results <- trend(cosn.jags, model="ZINB", wdrive=jags_dir_path)


end.time <- Sys.time()

end.time - start.time

#model diagnostics

cosn.elpd <- loo::loo_compare(loo_fn(cosn.pois.model.results), #model 1 - poisson
                              loo_fn(cosn.nb.model.results),   #model 2 - negative binomial
                              loo_fn(cosn.zip.model.results),  #model 3 - zero-inflated poisson
                              loo_fn(cosn.zinb.model.results)) #model 4 - zero-inflated negative binomial

cosn.elpd

converge_fn(cosn.nb.model.results)


# #UNTE - doesnt really matter
# unte.df <- select_sp(x = non.waterfowl.df, 
#                      sp = "UNTE", 
#                      year = seq(1990, 2024),
#                      plots = lab.plots, 
#                      strata = c(1), 
#                      speciesname = "species", 
#                      plotname = "plot", 
#                      strataname = "region",
#                      yearname = "year", 
#                      responsename = "total")
# 
# str(unte.df)
# 
# 
# unte.jags <- dataForJAGS(unte.df)
# 
# 
# start.time <- Sys.time()
# 
# 
# unte.pois.model.results <- trend(unte.jags, model="poisson", wdrive=jags_dir_path)
# 
# unte.nb.model.results <- trend(unte.jags, model="NB", wdrive=jags_dir_path)
# 
# unte.zip.model.results <- trend(unte.jags, model="ZIP", wdrive=jags_dir_path)
# 
# unte.zinb.model.results <- trend(unte.jags, model="ZINB", wdrive=jags_dir_path)
# 
# 
# end.time <- Sys.time()
# 
# end.time - start.time
# 
# #model diagnostics
# 
# unte.elpd <- loo::loo_compare(loo_fn(unte.pois.model.results), #model 1 - poisson
#                               loo_fn(unte.nb.model.results),   #model 2 - negative binomial
#                               loo_fn(unte.zip.model.results),  #model 3 - zero-inflated poisson
#                               loo_fn(unte.zinb.model.results)) #model 4 - zero-inflated negative binomial
# 
# unte.elpd
# 
# converge_fn(unte.pois.model.results)

###############################################
##   Waterfowl results to be bound together  ##
###############################################

#waterfowl results

abdu.elpd <- data.frame(abdu.elpd)
abdu.elpd$species <- "ABDU"

agwt.elpd <- data.frame(agwt.elpd)
agwt.elpd$species <- "AGWT"

cago.elpd <- data.frame(cago.elpd)
cago.elpd$species <- "CAGO"

come.elpd <- data.frame(come.elpd)
come.elpd$species <- "COME"

cogo.elpd <- data.frame(cogo.elpd)
cogo.elpd$species <- "COGO"

colo.elpd <- data.frame(colo.elpd)
colo.elpd$species <- "COLO"

rbme.elpd <- data.frame(rbme.elpd)
rbme.elpd$species <- "RBME"

rndu.elpd <- data.frame(rndu.elpd)
rndu.elpd$species <- "RNDU"

scau.elpd <- data.frame(scau.elpd)
scau.elpd$species <- "Scaup"

susc.elpd <- data.frame(susc.elpd)
susc.elpd$species <- "SUSC"


##bind all the elpd results together
all.elpd <- rbind(abdu.elpd, #ZINB
                  agwt.elpd, #ZINB
                  cago.elpd, #ZIP
                  come.elpd, #NB
                  cogo.elpd, #ZINB
                  colo.elpd, #ZINB
                  rbme.elpd, #ZINB
                  rndu.elpd, #ZINB
                  scau.elpd, #ZINB
                  susc.elpd) #ZINB

# write.csv(all.elpd, file = "EWS_NL_2024_JAGS_ELPD_2Strata.csv")


##################################################
##  Non-waterfowl results to be bound together  ##
##################################################

##non-waterfowl results
cosn.elpd <- data.frame(cosn.elpd)
cosn.elpd$species <- "COSN"

herg.elpd <- data.frame(herg.elpd)
herg.elpd$species <- "HERG"

spsa.elpd <- data.frame(spsa.elpd)
spsa.elpd$species <- "SPSA"

unye.elpd <- data.frame(unye.elpd)
unye.elpd$species <- "UNYE"

# unte.elpd <- data.frame(unte.elpd)
# unte.elpd$species <- "UNTE"

non.wf.elpd <- rbind(cosn.elpd,
                     herg.elpd,
                     spsa.elpd,
                     unye.elpd)#,
                     #unte.elpd)

# write out the results
#write.csv(non.wf.elpd, file = "EWS_NL_2024_JAGS_ELPD_Nonwaterfowl_2Strata.csv")

#American Black Duck
abdu.stats <- plot_trends(abdu.jags, abdu.zinb.model.results, plot.type = "quantile", response="density", return.df = T)
abdu.stats$species <- 'ABDU'
abdu.stats$model <- "ZINB"

abdu.coef <- as.data.frame(model_coef(abdu.jags, abdu.zinb.model.results))

abdu.coef$species <- "ABDU"


#American Green-winged Teal
agwt.stats <- plot_trends(agwt.jags, agwt.zinb.model.results, plot.type = "quantile", response="density", return.df = T)
agwt.stats$species <- 'AGWT'
agwt.stats$model <- "ZINB"

agwt.coef <- as.data.frame(model_coef(agwt.jags, agwt.zinb.model.results))
agwt.coef$species <- "AGWT"

#global_plot(agwt.jags, agwt.zinb.model.results)

#Canada Goose
cago.stats <- plot_trends(cago.jags, cago.zinb.model.results, plot.type = "quantile", response="density", return.df = T)
cago.stats$species <- 'CAGO'
cago.stats$model <- "ZINB"

cago.coef <- as.data.frame(model_coef(cago.jags, cago.zinb.model.results))
cago.coef$species <- "CAGO"

global_plot(cago.jags, cago.zinb.model.results)

#Common Merganser
come.stats <- plot_trends(come.jags, come.nb.model.results, plot.type = "quantile", response="density", return.df = T)
come.stats$species <- 'COME'
come.stats$model <- "NB"

come.coef <- as.data.frame(model_coef(come.jags, come.nb.model.results))
come.coef$species <- "COME"

#global_plot(come.jags, come.nb.model.results)

#Common Goldeneye
cogo.stats <- plot_trends(cogo.jags, cogo.zinb.model.results, plot.type = "quantile", response="density", return.df = T)
cogo.stats$species <- 'COGO'
cogo.stats$model <- "ZINB"

cogo.coef <- as.data.frame(model_coef(cogo.jags, cogo.zinb.model.results))

cogo.coef$species <- "COGO"

#global_plot(cogo.jags, cogo.zinb.model.results)

#Common Loon
colo.stats <- plot_trends(colo.jags, colo.nb.model.results, plot.type = "quantile", response="density", return.df = T)
colo.stats$species <- 'COLO'
colo.stats$model <- "NB"

colo.coef <- as.data.frame(model_coef(colo.jags, colo.nb.model.results))

colo.coef$species <- "COLO"

global_plot(colo.jags, colo.nb.model.results)


#Red-breasted Merganser
rbme.stats <- plot_trends(rbme.jags, rbme.zinb.model.results, plot.type = "quantile", response="density", return.df = T)
rbme.stats$species <- 'RBME'
rbme.stats$model <- "ZINB"

rbme.coef <- as.data.frame(model_coef(rbme.jags, rbme.zinb.model.results))

rbme.coef$species <- "RBME"

global_plot(rbme.jags, rbme.nb.model.results)


#Ring-necked Duck
rndu.stats <- plot_trends(rndu.jags, rndu.zinb.model.results, plot.type = "quantile", response="density", return.df = T)
rndu.stats$species <- 'RNDU'
rndu.stats$model <- "ZINB"

rndu.coef <- as.data.frame(model_coef(rndu.jags, rndu.zinb.model.results))

rndu.coef$species <- "RNDU"

global_plot(rndu.jags, rndu.zinb.model.results)


#Scaup species (UNSC, LESC, GRSC)
scau.stats <- plot_trends(scau.jags, scau.zinb.model.results, plot.type = "quantile", response="density", return.df = T)
scau.stats$species <- 'Scaup'
scau.stats$model <- "NB"

scau.coef <- as.data.frame(model_coef(scau.jags, scau.nb.model.results))

scau.coef$species <- "SCAU"

global_plot(scau.jags, scau.zinb.model.results)


#Surf Scoter - Labrador only
susc.stats <- plot_trends(susc.jags, susc.zinb.model.results, plot.type = "quantile", response="density", return.df = T)
susc.stats$species <- 'SUSC'
susc.stats$model <- "ZINB"

susc.coef <- as.data.frame(model_coef(susc.jags, susc.zinb.model.results))
susc.coef$species <- "SUSC"

global_plot(susc.jags, susc.zinb.model.results)


all.stats <- rbind(abdu.stats,
                   agwt.stats,
                   cago.stats,
                   come.stats,
                   cogo.stats,
                   colo.stats,
                   rbme.stats,
                   rndu.stats,
                   scau.stats,
                   susc.stats)  

all.coef <- rbind(abdu.coef,
                   agwt.coef,
                   cago.coef,
                   come.coef,
                   cogo.coef,
                   colo.coef,
                   rbme.coef,
                   rndu.coef,
                   scau.coef,
                   susc.coef)  


#write.csv(all.stats, "EWS_NL_2024_JAGS_Outputs_2Strata.csv")
#write.csv(all.coef, "EWS_NL_2024_JAGS_Coefs_2Strata.csv")


## non-waterfowl stats
cosn.stats <- plot_trends(cosn.jags, cosn.nb.model.results, plot.type = "quantile", response="density", return.df = T)
cosn.stats$species <- 'COSN'
cosn.stats$model <- "NB"

cosn.coef <- as.data.frame(model_coef(cosn.jags, cosn.nb.model.results))

cosn.coef$species <- "COSN"

#HERG
herg.stats <- plot_trends(herg.jags, herg.nb.model.results, plot.type = "quantile", response="density", return.df = T)
herg.stats$species <- 'HERG'
herg.stats$model <- "NB"

herg.coef <- as.data.frame(model_coef(herg.jags, herg.nb.model.results))

herg.coef$species <- "HERG"

#SPSA
spsa.stats <- plot_trends(spsa.jags, spsa.zinb.model.results, plot.type = "quantile", response="density", return.df = T)
spsa.stats$species <- 'SPSA'
spsa.stats$model <- "ZINB"

spsa.coef <- as.data.frame(model_coef(spsa.jags, spsa.nb.model.results))

spsa.coef$species <- "SPSA"

#UNYE
unye.stats <- plot_trends(unye.jags, unye.zinb.model.results, plot.type = "quantile", response="density", return.df = T)
unye.stats$species <- 'UNYE'
unye.stats$model <- "ZINB"

unye.coef <- as.data.frame(model_coef(unye.jags, unye.zinb.model.results))

unye.coef$species <- "UNYE"


# #UNTE
# unte.stats <- plot_trends(unte.jags, unte.pois.model.results, plot.type = "quantile", response="density", return.df = T)
# unte.stats$species <- 'UNTE'
# unte.stats$model <- "POIS"
# 
# unte.coef <- as.data.frame(model_coef(unte.jags, unte.pois.model.results))
# 
# unte.coef$species <- "UNTE"


non.wf.stats <- rbind(cosn.stats,
                      herg.stats,
                      spsa.stats,
                      unye.stats)#,
                      #unte.stats)

non.wf.coef <-rbind(cosn.coef,
                    herg.coef,
                    spsa.coef,
                    unye.coef)#,
                    #unte.coef)

#write.csv(non.wf.stats, "EWS_NL_2024_JAGS_Outputs_Nonwaterfowl_2Strata.csv")
#write.csv(non.wf.coef, "EWS_NL_2024_JAGS_Coefs_Nonwaterfowl_2Strata.csv")


median(all.stats$TIP_q50)

quantile(all.stats$TIP_q50, c(0.5))

##################
##  ggplotting  ##
##################

non.wf.stats <- read.csv("C:/Users/englishm/Documents/EWS/BDJV Tech report/Data/Outputs/By strata/EWS_NL_2024_JAGS_Outputs_Nonwaterfowl_2Strata.csv")

all.stats <- read.csv("C:/Users/englishm/Documents/EWS/BDJV Tech report/Data/Outputs/By strata/EWS_NL_2024_JAGS_Outputs_2Strata.csv")

ggplot(all.stats, aes(x = Year, y = RawTIP_mean)) +
  geom_line() +
  # geom_errorbar(ymin = all.stats$TIP_LCL,
  #               ymax = all.stats$TIP_UCL,
  #               width = 0.125) +
  #geom_smooth() +
  facet_wrap(species~., scales = "free_y") +
  theme_bw()+
  ylab("Mean TIP per plot") +
  #scale_y_continuous(limits = c(0,7.5))


ggplot(non.wf.stats, aes(x = Year, y = TIP_mean)) +
  geom_line() +
  # geom_errorbar(ymin = non.wf.stats$TIP_LCL,
  #               ymax = non.wf.stats$TIP_UCL,
  #               width = 0.125,
  #               na.rm = T) +
  geom_smooth() +
  facet_grid(species~., scales = "free_y") +
  theme_bw() +
  ylab("50th Quantile")



p<- ggarrange(p.abdu, p.agwt, p.cago, p.cogo, p.colo, p.come, p.rbme, p.rndu, p.scau, p.susc,
          ncol = 3,
          nrow = 3)



bird <- "CAGO"

# #for TIP species
# df <- all.stats[all.stats$species %in% bird,]

# for non-TIP species
df  <- non.wf.stats[non.wf.stats$species %in% bird,]

#df <- abdu.stats

tip.min <- df$RawTIP_mean - df$RawTIP_se
tip.max <- df$RawTIP_mean + df$RawTIP_se

df$tip.pred.min <- df$TIP_mean - df$TIP_se
df$tip.pred.max <- df$TIP_mean + df$TIP_se

lims <- c(0, round(max(df$TIP_mean, na.rm = T) + max(df$TIP_se, na.rm = T) + 1, digits = 1) )


m1 <- lm(TIP_mean ~ (Year), data = df[df$Strat == 1,])

pp1 <- data.frame(predict(m1, interval = 'confidence'))

pp1$Year <- seq(1990,2025)

m2 <- lm(TIP_mean ~ (Year), data = df[df$Strat == 2,])

pp2 <- data.frame(predict(m2, interval = 'confidence'))

pp2$Year <- seq(1990,2025)



#start ggplot
p.cago <- ggplot() +
  
  # geom_point(data = df,
  #            aes(x = Year, y = RawTIP_mean, colour = "Raw Data")) +
  # 
  # geom_line(data = df,
  #           aes(x = Year, y = RawTIP_mean, colour = "Raw Data")) +
  #   
  # geom_errorbar(data = df,
  #               aes(x = Year, colour = "Raw Data"),
  #               stat = "identity",
  #               ymin = tip.min,
  #               ymax = tip.max,
  #               width = 0.2) +
  
  
  geom_point(data = df[df$Strata == 2,],
             aes(x = Year, y = TIP_mean, colour = "JAGS-estimated Data - NF")) +

  geom_point(data = df[df$Strata == 1,],
             aes(x = Year, y = TIP_mean, colour = "JAGS-estimated Data - LB")) +
  
  geom_line(data = df[df$Strata == 2,],
            aes(x = Year, y = TIP_mean, colour = "JAGS-estimated Data - NF")) +

  geom_line(data = df[df$Strata == 1,],
            aes(x = Year, y = TIP_mean, colour = "JAGS-estimated Data - LB")) +
  
  
  # stat_smooth(data = df, #global
  #             aes(x = Year, y = TIP_mean),
  #             method = "lm", 
  #             formula = y~log(x),
  #             colour = "red",
  #             se = F) +
  
  stat_smooth(data = df[df$Strata == 2,], #NF
              aes(x = Year, y = TIP_mean, colour = "JAGS-estimated Data - NF"),
              method = "lm",
              formula = y~log(x),
              #colour = "blue",
              se = F) +

  stat_smooth(data = df[df$Strata == 1,], #LB
              aes(x = Year, y = TIP_mean, colour = "JAGS-estimated Data - LB"),
              method = "lm", 
              formula = y~log(x),
              #colour = "orange",
              se = F) +
  
  # geom_smooth(data = pp,
  #             aes(x = x, y = lwr), color = 'red', lty = 2, linewidth = 0.5) +
  # 
  # geom_smooth(data = pp,
  #             aes(x = x, y = upr), color = 'red', lty = 2, linewidth = 0.5) +

  
  geom_smooth(data = pp2,
              aes(x = Year, y = lwr, color = 'JAGS-estimated Data - NF'),
              lty = 2,
              linewidth = 0.5) +

  geom_smooth(data = pp2,
              aes(x = Year, y = upr, color = 'JAGS-estimated Data - NF'),
              lty = 2,
              linewidth = 0.5) +

  geom_smooth(data = pp1,
              aes(x = Year, y = lwr, color = 'JAGS-estimated Data - LB'),
              lty = 2, 
              linewidth = 0.5) +
  
  geom_smooth(data = pp1,
              aes(x = Year, y = upr, color = 'JAGS-estimated Data - LB'), 
              lty = 2, 
              linewidth = 0.5) +

 
  geom_errorbar(data = df[df$Strata == 2,],
                aes(x = Year,
                    colour = "grey",
                    ymin = tip.pred.min,
                    ymax = tip.pred.max),
                stat = "identity",
                width = 0.2) +

  geom_errorbar(data = df[df$Strata == 1,],
                aes(x = Year, 
                    colour = "grey",
                    ymin = tip.pred.min,
                    ymax = tip.pred.max),
                stat = "identity",
                width = 0.2) +
  
  scale_color_manual(values = c(#"Raw Data" = "black",
                     "JAGS-estimated Data - NF" = "black",
                     "JAGS-estimated Data - LB" = "grey")) +
  # 
  #facet_grid(species~., scales = "free") +
  theme( 
    #axis.line = element_line(colour = "black"),
    #axis.text.x = element_text(angle = 45, hjust = 1),
    #legend.position = c(0.775, 0.90),
    legend.position ='none',
    legend.title = element_blank(),
    legend.box.background = element_rect(colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.background = element_blank(), 
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    text = element_text(size = 13)) +
  
  scale_y_continuous(limits = lims) +
  ggtitle(bird) +
  xlab("Year") +
  # xlab(NULL) +
  # ylab(NULL)
  ylab("TIP per 25km^2")

p.cago

#list of plots per species
#p.abdu <- recordPlot()
#p.agwt <- recordPlot()
#p.cago <- recordPlot()

#p.cogo <- recordPlot()
#p.colo <- recordPlot()
#p.come <- recordPlot()

#p.rbme <- recordPlot()
#p.rndu <- recordPlot()
#p.scau <- recordPlot()
#p.susc <- recordPlot()

p.cosn
p.herg
p.unye
p.spsa

# p.unye + geom_smooth(data = df,
#                      aes(x = year, y = TIPpred_mean))


ggarrange(p.cosn, p.herg, p.unye,
          ncol = 1,
          nrow = 3)

ggsave(paste(bird, "2024_Trends.tiff", sep = "_"), width = 8, height = 6)



## end
