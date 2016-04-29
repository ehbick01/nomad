# ----------------------------------------
# Re-creating Spectra demographic database
# ----------------------------------------

## Load packages
library(rgdal)
library(maptools)
library(ggplot2)
library(plyr)
library(dplyr)
library(parallel)
library(doParallel)
library(foreach)
library(geosphere)
library(rattle)
library(rpart.plot)
library(randomForest)
library(reshape2)
library(scales)
library(ggthemes)
library(msm)
library(ggmap)
library(extrafont)
library(stringr)
library(tigris)


# ------------------------------
# ANALYZING EXISTING LOCATIONS
# ------------------------------
## Load functions

# Gets Euclidean distance between two points.
# param point.1 First point.
# param point.2 Second point.
# return Euclidean distance.
get.euc.distance = function(point.1, point.2)
{
  return (sqrt(sum((point.1 - point.2) ^ 2)))
}

# Gets the minimum distance from all points in one collection to all points in another.
# param points.1 An n-by-2 matrix of points.
# param points.2 An n-by-2 matrix of points.
# return Vector of distances from each point in \code{points.1} to points in \code{points.2}.
get.min.distances = function(points.1, points.2)
{
  return (apply(points.1,
                1,
                function(point.1)
                {
                  distances = apply(points.2,
                                    1,
                                    function(point.2, point.1)
                                    {
                                      return (get.euc.distance(point.1, point.2))
                                    },
                                    point.1)
                  
                  return (min(distances))
                }))
}

# Rebuild to pull relevant tract
get.min.tract = function(points.1, points.2)
{
  return (apply(points.1,
                1,
                function(point.1)
                {
                  distances = apply(points.2,
                                    1,
                                    function(point.2, point.1)
                                    {
                                      return (get.euc.distance(point.1, point.2))
                                    },
                                    point.1)
                  
                  return (tracts$NAMELSAD10[which(distances == min(distances))])
                }))
}

get.min.geoid = function(points.1, points.2)
{
  return (apply(points.1,
                1,
                function(point.1)
                {
                  distances = apply(points.2,
                                    1,
                                    function(point.2, point.1)
                                    {
                                      return (get.euc.distance(point.1, point.2))
                                    },
                                    point.1)
                  
                  return (tracts$GEOID10[which(distances == min(distances))])
                }))
}


## Read in Existing Location Data
existing <- read.csv('existing_locations.csv', 
                     header = TRUE,
                     stringsAsFactors = FALSE)

## Manipulate Data

# Geocode existing addresses
existing_geocoded <- geocode(existing$addy)
existing_geocoded <- cbind(existing, existing_geocoded)

# Read in US shapefiles and find appropriate tracts for each location
usZip <-
  readOGR(dsn = "US Shapefiles", layer = "Tract_2010Census_DP1")

# Convert shapefile to dataframe
tracts <- data.frame(usZip)

# tracts <- usZip.df[grep('21111', usZip.df$GEOID10), ] # Pull relevant states (?)

tracts$NAMELSAD10 <-
  tolower(tracts$NAMELSAD10) # Move census tract names to lowercase

names(tracts)[c(5,6)] <- c('avgLat', 'avgLon')

tracts <-
  data.frame(tracts %>% distinct(GEOID10)) # Get rid of duplicated tract info

tracts$avgLat <- as.numeric(as.character(tracts$avgLat))
tracts$avgLon <- as.numeric(as.character(tracts$avgLon))

tracts$ALAND10 <- tracts$ALAND10*3.86102e-7 # Convert square meters to square miles

## Pull in economic information for every state

# Read in table for fips codes to iterate through
fips <- read.csv('fipscodes.csv',
                 header = TRUE, 
                 stringsAsFactors = FALSE)

fips <- fips[!duplicated(fips$state), ] # Get rid of multiples

fips$fips <- lapply(fips$fips, FUN = function(x) {ifelse(nchar(x) == 1, paste('0', x, sep = ''), x)}) # Convert FIPS code to two characters

# Define the variables to pull - taken from http://api.census.gov/data/2014/acs5/profile/variables.html
varsDF <- read.csv('vars.csv', header = TRUE)
vars <- as.character(varsDF[,1])

# Split variables into groups of <= 50 (thanks a lot, limitations)
vars1 <- vars[1:50]
vars2 <- vars[51:100]
vars3 <- vars[101:length(vars)]
vars1 <- paste(vars1, collapse = ',', sep = '')
vars2 <- paste(vars2, collapse = ',', sep = '')
vars3 <- paste(vars3, collapse = ',', sep = '')

# usEcon <- list()
# 
# for (i in 1:nrow(fips)) { 
#   
#   url <- 'http://api.census.gov/data/2014/acs5/profile?get='
#   region <- paste('&for=tract:*&in=state:', as.character(fips$fips[i]), 'key=', sep = '')
#   key <- '0d49769f4920428f1240cd67d86ee8d61ebaa4bc'
#   
#   # Build URLs to feed into the API call
#   url_rebuilt1 <- paste(url, vars1, region, key, sep = '')
#   url_rebuilt2 <- paste(url, vars2, region, key, sep = '')
#   url_rebuilt3 <- paste(url, vars3, region, key, sep = '')
#   
#   # Read HTML doc from URL
#   pull1 <- readLines(url_rebuilt1, warn = 'F')
#   pull2 <- readLines(url_rebuilt2, warn = 'F')
#   pull3 <- readLines(url_rebuilt3, warn = 'F')
#   
#   # Clean the HTML doc up
#   pull1 <- strsplit(gsub('[^[:alnum:], _]', '', pull1), '\\,')
#   pull2 <- strsplit(gsub('[^[:alnum:], _]', '', pull2), '\\,')
#   pull3 <- strsplit(gsub('[^[:alnum:], _]', '', pull3), '\\,')
#   
#   # Build into dataframes
#   pull1DF <- do.call(rbind, pull1)
#   colnames(pull1DF) <- pull1DF[1,]
#   pull1DF <- pull1DF[-1,]
#   pull1DF <- data.frame(pull1DF)
#   pull1DF$geoid <-
#     paste(pull1DF$state, pull1DF$county, pull1DF$tract, sep = '')
#   
#   pull2DF <- do.call(rbind, pull2)
#   colnames(pull2DF) <- pull2DF[1,]
#   pull2DF <- pull2DF[-1,]
#   pull2DF <- data.frame(pull2DF)
#   pull2DF$geoid <-
#     paste(pull2DF$state, pull2DF$county, pull2DF$tract, sep = '')
#   
#   pull3DF <- do.call(rbind, pull3)
#   colnames(pull3DF) <- pull3DF[1,]
#   pull3DF <- pull3DF[-1,]
#   pull3DF <- data.frame(pull3DF)
#   pull3DF$geoid <-
#     paste(pull3DF$state, pull3DF$county, pull3DF$tract, sep = '')
#   
#   # Merge all together
#   pullTot <- merge(pull1DF, pull2DF, by = 'geoid')
#   pullTot <- merge(pullTot, pull3DF, by = 'geoid')
#   pullTot <- pullTot[,-grep('state|county|tract|\\.', names(pullTot))]
#   
#   usEcon[[i]] <- pullTot
#   
#   }
# 
# # Convert list of dataframes into one dataframe
# usEcon <- do.call('rbind', usEcon)
# 
# # Rename economic variables
# names(usEcon)[2:ncol(usEcon)] <- as.character(varsDF[,2])
# names(usEcon) <- tolower(names(usEcon))
# 
# usEcon[, c(2:ncol(usEcon))] <- sapply(usEcon[, c(2:ncol(usEcon))], as.character) # Convert vars to character
# usEcon[, c(2:ncol(usEcon))] <- sapply(usEcon[, c(2:ncol(usEcon))], as.numeric) # Convert vars to numeric

# write.csv(usEcon, 'us-tract-econ-characteristics.csv')

usEcon <- read.csv('us-tract-econ-characteristics.csv',
                   header = TRUE,
                   stringsAsFactors = FALSE)

# Define points for functions
points.1 <- matrix(NA, nrow = nrow(existing_geocoded), ncol = 2)
points.1[,1] <- existing_geocoded$lon
points.1[,2] <- existing_geocoded$lat

points.2 <- matrix(NA, nrow = nrow(tracts), ncol = 2)
points.2[,1] <- tracts$avgLon
points.2[,2] <- tracts$avgLat

existing_geocoded$minDist <- get.min.distances(points.1, points.2)
existing_geocoded$geoid <- get.min.geoid(points.1, points.2)

# write.csv(existing_geocoded, 'existing_tracts.csv')

## Bucket demographics and economic characteristics to corresponding acct based on tract w/ minimum distance
existing_geocoded_merged <- merge(existing_geocoded, tracts, by.x = 'geoid', by.y = 'GEOID10')
existing_geocoded_merged <- merge(existing_geocoded_merged, usEcon, by = 'geoid', all.x = TRUE)

# write.csv(existing_geocoded_merged, 'existing_geocoded_merged.csv')

# Clean up the names a bit
existing_geocoded_merged <- read.csv('existing_geocoded_merged.csv',
                                     header = TRUE,
                                     stringsAsFactors = FALSE)

names(existing_geocoded_merged) <- gsub('\\!!', '\\.', names(existing_geocoded_merged))
names(existing_geocoded_merged) <- gsub(':', '', names(existing_geocoded_merged))
names(existing_geocoded_merged) <- tolower(names(existing_geocoded_merged))

## Plot Data

# Plot total population
ggplot(existing_geocoded_merged, aes(x = reorder(addy, -total.population/aland10), y = total.population/aland10, group = addy)) + 
  geom_bar(stat = 'identity', fill = '#4C4C4C', alpha = 0.25) + 
  scale_y_continuous(labels = comma) + 
  theme_bw(base_family = "Trebuchet MS", base_size = 12) +
  ggtitle(expression(atop(bold('Total Population by Location                                                                               '), 
                          atop('At nearly 13,000 people, the Houston, TX location has the largest surrounding population of all existing locations')))) + 
  geom_hline(aes(yintercept = mean(existing_geocoded_merged$total.population/existing_geocoded_merged$aland10)), colour = '#fd7d47', size = 1.25) + 
  theme(
    plot.title = element_text(hjust = 0),
    text = element_text(colour = '#4C4C4C'),
    panel.background = element_rect('white'),
    plot.background = element_rect('white'),
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = '#F3F3F3', linetype = 'dashed', size = 0.25),
    panel.grid.minor.y = element_blank(),
    legend.background = element_rect('white'),
    legend.title = element_blank(),
    legend.position = 'none',
    legend.direction = 'vertical',
    legend.key = element_rect('white'),
    axis.text = element_text(face = 'bold'),
    axis.text.x = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  )
#ggsave('totalpop_within1mile_existinglocations.png', type = 'cairo-png')

# Plot kids under 14 as share of total population
for (i in 1:nrow(existing_geocoded_merged)) { 
  
  existing_geocoded_merged$TotalUnder14[i] <- sum(existing_geocoded_merged[i, c(13:15)])
  
}

ggplot(existing_geocoded_merged, aes(x = reorder(addy, -TotalUnder14/total.population), y = 100*TotalUnder14/total.population, group = addy)) + 
  geom_bar(stat = 'identity', fill = '#4C4C4C', alpha = 0.25) + 
  geom_hline(aes(yintercept = 100*mean(existing_geocoded_merged$TotalUnder14/existing_geocoded_merged$total.population)), colour = '#fd7d47', size = 1.25) + 
  scale_y_continuous(labels = comma) + 
  theme_bw(base_family = "Trebuchet MS", base_size = 12) +
  ggtitle(expression(atop(bold('Concentration of Children Under 14 Years Old, in Percentages           '), 
                          atop('At nearly 50%, the San Diego location has the highest concentration of pre-teen children within 1 mile')))) + 
  theme(
    plot.title = element_text(hjust = 0),
    text = element_text(colour = '#4C4C4C'),
    panel.background = element_rect('white'),
    plot.background = element_rect('white'),
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = '#F3F3F3', linetype = 'dashed', size = 0.25),
    panel.grid.minor.y = element_blank(),
    legend.background = element_rect('white'),
    legend.title = element_blank(),
    legend.position = 'none',
    legend.direction = 'vertical',
    legend.key = element_rect('white'),
    axis.text = element_text(face = 'bold'),
    axis.text.x = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  )
#ggsave('totalpreteens_within1mile_existinglocations.png', type = 'cairo-png')

# ------------------------------
# ANALYZING LOUISVILLE LOCATIONS
# ------------------------------

## Load functions

# Gets Euclidean distance between two points.
# param point.1 First point.
# param point.2 Second point.
# return Euclidean distance.
get.euc.distance = function(point.1, point.2)
{
  return (sqrt(sum((point.1 - point.2) ^ 2)))
}

# Gets the minimum distance from all points in one collection to all points in another.
# param points.1 An n-by-2 matrix of points.
# param points.2 An n-by-2 matrix of points.
# return Vector of distances from each point in \code{points.1} to points in \code{points.2}.
get.min.distances = function(points.1, points.2)
{
  return (apply(points.1,
                1,
                function(point.1)
                {
                  distances = apply(points.2,
                                    1,
                                    function(point.2, point.1)
                                    {
                                      return (get.euc.distance(point.1, point.2))
                                    },
                                    point.1)
                  
                  return (min(distances))
                }))
}

# Rebuild to pull relevant tract
get.min.tract = function(points.1, points.2)
{
  return (apply(points.1,
                1,
                function(point.1)
                {
                  distances = apply(points.2,
                                    1,
                                    function(point.2, point.1)
                                    {
                                      return (get.euc.distance(point.1, point.2))
                                    },
                                    point.1)
                  
                  return (tractCoords$NAMELSAD10[which(distances == min(distances))])
                }))
}

get.min.geoid = function(points.1, points.2)
{
  return (apply(points.1,
                1,
                function(point.1)
                {
                  distances = apply(points.2,
                                    1,
                                    function(point.2, point.1)
                                    {
                                      return (get.euc.distance(point.1, point.2))
                                    },
                                    point.1)
                  
                  return (tractCoords$GEOID10[which(distances == min(distances))])
                }))
}

### Data Collection

## Pull in demographic information

# # Read in US shapefiles
# usZip <-
#   readOGR(dsn = "US Shapefiles", layer = "Tract_2010Census_DP1")
# 
# # Convert shapefile to dataframe
# usZip.df <- data.frame(usZip)
# 
# louTracts <- usZip.df[grep('21111', usZip.df$GEOID10), ]
# 
# louTracts$NAMELSAD10 <-
#   tolower(louTracts$NAMELSAD10) # Move census tract names to lowercase
# 
# names(louTracts)[c(5,6)] <- c('avgLat', 'avgLon')
# 
# # Write louTracts to csv to not have to do the above anymore
# write.csv(louTracts, 'louisville_tracts.csv')

#######################################################################################################################

## Now that all of the above is done, just read in the louTracts info
louTracts <- read.csv('louisville_tracts.csv', 
                      header = TRUE,
                      stringsAsFactors = FALSE)

# ## Pull in economic information
# 
# # Pull all for state code 21 (KY)
# url <- 'http://api.census.gov/data/2014/acs5/profile?get='
# region <- '&for=tract:*&in=state:21key='
# key <- '0d49769f4920428f1240cd67d86ee8d61ebaa4bc'
# 
# # Split variables into groups of <= 50 (thanks a lot, limitations)
# vars1 <- vars[1:50]
# vars2 <- vars[51:100]
# vars3 <- vars[101:length(vars)]
# vars1 <- paste(vars1, collapse = ',', sep = '')
# vars2 <- paste(vars2, collapse = ',', sep = '')
# vars3 <- paste(vars3, collapse = ',', sep = '')
# 
# # Build URLs to feed into the API call
# url_rebuilt1 <- paste(url, vars1, region, key, sep = '')
# url_rebuilt2 <- paste(url, vars2, region, key, sep = '')
# url_rebuilt3 <- paste(url, vars3, region, key, sep = '')
# 
# # Read HTML doc from URL
# pull1 <- readLines(url_rebuilt1, warn = 'F')
# pull2 <- readLines(url_rebuilt2, warn = 'F')
# pull3 <- readLines(url_rebuilt3, warn = 'F')
# 
# # Clean the HTML doc up
# pull1 <- strsplit(gsub('[^[:alnum:], _]', '', pull1), '\\,')
# pull2 <- strsplit(gsub('[^[:alnum:], _]', '', pull2), '\\,')
# pull3 <- strsplit(gsub('[^[:alnum:], _]', '', pull3), '\\,')
# 
# # Build into dataframes
# pull1DF <- do.call(rbind, pull1)
# colnames(pull1DF) <- pull1DF[1,]
# pull1DF <- pull1DF[-1,]
# pull1DF <- data.frame(pull1DF)
# pull1DF$geoid <-
#   paste(pull1DF$state, pull1DF$county, pull1DF$tract, sep = '')
# 
# pull2DF <- do.call(rbind, pull2)
# colnames(pull2DF) <- pull2DF[1,]
# pull2DF <- pull2DF[-1,]
# pull2DF <- data.frame(pull2DF)
# pull2DF$geoid <-
#   paste(pull2DF$state, pull2DF$county, pull2DF$tract, sep = '')
# 
# pull3DF <- do.call(rbind, pull3)
# colnames(pull3DF) <- pull3DF[1,]
# pull3DF <- pull3DF[-1,]
# pull3DF <- data.frame(pull3DF)
# pull3DF$geoid <-
#   paste(pull3DF$state, pull3DF$county, pull3DF$tract, sep = '')
# 
# # Merge all together
# pullTot <- merge(pull1DF, pull2DF, by = 'geoid')
# pullTot <- merge(pullTot, pull3DF, by = 'geoid')
# pullTot <- pullTot[,-grep('state|county|tract|\\.', names(pullTot))]
# 
# # Rename economic variables
# names(pullTot)[2:ncol(pullTot)] <- as.character(varsDF[,2])
# names(pullTot) <- tolower(names(pullTot))

# Read in Proposed Location Data
locations <- read.csv('initial_locations.csv', 
                      header = TRUE, 
                      stringsAsFactors = FALSE)

# Geocode locations
locationsGeocoded <- geocode(locations$address, output = 'latlon')
dat <- cbind(locations, locationsGeocoded)

# Select needed info from louTracts and merge with the dataframe of min/max values
tractCoords <-
  louTracts[,which(names(louTracts) %in% c('NAMELSAD10', 'GEOID10', 'avgLat', 'avgLon'))]

tractCoords <-
  data.frame(tractCoords %>% distinct(GEOID10)) # Get rid of duplicated tract info

tractCoords$avgLat <- as.numeric(as.character(tractCoords$avgLat))
tractCoords$avgLon <- as.numeric(as.character(tractCoords$avgLon))

## Assign tract to dat based on minimum difference in lat/lon

# Define points for functions
points.1 <- matrix(NA, nrow = nrow(dat), ncol = 2)
points.1[,1] <- dat$lon
points.1[,2] <- dat$lat

points.2 <- matrix(NA, nrow = nrow(tractCoords), ncol = 2)
points.2[,1] <- tractCoords$avgLon
points.2[,2] <- tractCoords$avgLat

dat$minDist <- get.min.distances(points.1, points.2)
dat$geoid <- get.min.geoid(points.1, points.2)
dat$minTract <- get.min.tract(points.1, points.2)

### Plotting

## Bucket demographics to corresponding acct based on tract w/ minimum distance
datMerged <- merge(dat, louTracts, by.x = 'geoid', by.y = 'GEOID10')
datMerged <- merge(datMerged, usEcon, by = 'geoid')
names(datMerged) <- tolower(names(datMerged))

# # Write to csv
# write.csv(datMerged, 'louisville_tract_merged.csv')

# Read csv
datMerged <- read.csv('louisville_tract_merged.csv',
                      header = TRUE,
                      stringsAsFactors = FALSE)

# Clean up the names a bit
names(datMerged) <- gsub('\\!!', '\\.', names(datMerged))
names(datMerged) <- gsub(':', '', names(datMerged))

# Plot total population
ggplot(datMerged, aes(x = reorder(location, -total.population/aland10), y = total.population/aland10, group = location)) + 
  geom_bar(stat = 'identity', aes(fill = location, alpha = location)) + 
  scale_fill_manual(values = c(rep('#4C4C4C', 5), '#FD7E47')) + 
  scale_alpha_manual(values = c(rep(0.25, 5), 1)) + 
  scale_y_continuous(labels = comma) + 
  theme_bw(base_family = "Trebuchet MS", base_size = 12) +
  ggtitle(expression(atop(bold('Total Population by Location per Square Mile                                        '), 
                          atop(' Based on population, Westport Village boasts the largest within 1 mile at 6,200 people per square mile')))) + 
  theme(
    plot.title = element_text(hjust = 0),
    text = element_text(colour = '#4C4C4C'),
    panel.background = element_rect('white'),
    plot.background = element_rect('white'),
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = '#F3F3F3', linetype = 'dashed', size = 0.25),
    panel.grid.minor.y = element_blank(),
    legend.background = element_rect('white'),
    legend.title = element_blank(),
    legend.position = 'none',
    legend.direction = 'vertical',
    legend.key = element_rect('white'),
    axis.text = element_text(face = 'bold'),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  )
#ggsave('totalpop_within1mile_persqmi.png', type = 'cairo-png')

# Plot kids under 14 as share of total population
for (i in 1:nrow(datMerged)) { 
  
  datMerged$TotalUnder14[i] <- sum(datMerged[i, c(14:16)])
  
  }

ggplot(datMerged, aes(x = reorder(location, -TotalUnder14/total.population), y = 100*TotalUnder14/total.population, group = location)) + 
  geom_bar(stat = 'identity', aes(fill = location, alpha = location)) + 
  scale_fill_manual(values = c(rep('#4C4C4C', 4), '#FD7E47', '#4C4C4C')) + 
  scale_alpha_manual(values = c(rep(0.25, 4), 1, 0.25)) + 
  scale_y_continuous(labels = comma) + 
  theme_bw(base_family = "Trebuchet MS", base_size = 12) +
  ggtitle(expression(atop(bold(' Concentration of Children Under 14 Years Old, in Percentages'), 
                          atop('At nearly 25%, Tyler Village has the highest concentration of pre-teen children within 1 mile')))) + 
  theme(
    plot.title = element_text(hjust = 0),
    text = element_text(colour = '#4C4C4C'),
    panel.background = element_rect('white'),
    plot.background = element_rect('white'),
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = '#F3F3F3', linetype = 'dashed', size = 0.25),
    panel.grid.minor.y = element_blank(),
    legend.background = element_rect('white'),
    legend.title = element_blank(),
    legend.position = 'none',
    legend.direction = 'vertical',
    legend.key = element_rect('white'),
    axis.text = element_text(face = 'bold'),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  )
#ggsave('preteenpop_within1mile.png', type = 'cairo-png')

# Plot median family income
ggplot(datMerged, aes(x = reorder(location, -`income.and.benefits..in.2014.inflation.adjusted.dollars...families..median.family.income..dollars.`), y = `income.and.benefits..in.2014.inflation.adjusted.dollars...families..median.family.income..dollars.`, group = location)) + 
  geom_bar(stat = 'identity', aes(fill = location, alpha = location)) + 
  scale_fill_manual(values = c(rep('#4C4C4C', 4), '#FD7E47', '#4C4C4C')) + 
  scale_alpha_manual(values = c(rep(0.25, 4), 1, 0.25)) + 
  scale_y_continuous(labels = dollar) + 
  theme_bw(base_family = "Trebuchet MS", base_size = 12) +
  ggtitle(expression(atop(bold('Median Family Income (in 2014 Inflation-Adjusted Dolalrs)                             '), 
                          atop(' Both Tyler Village and Springhurst Shopping Center have much higher median incomes than their counterparts')))) + 
  theme(
    plot.title = element_text(hjust = 0),
    text = element_text(colour = '#4C4C4C'),
    panel.background = element_rect('white'),
    plot.background = element_rect('white'),
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = '#F3F3F3', linetype = 'dashed', size = 0.25),
    panel.grid.minor.y = element_blank(),
    legend.background = element_rect('white'),
    legend.title = element_blank(),
    legend.position = 'none',
    legend.direction = 'vertical',
    legend.key = element_rect('white'),
    axis.text = element_text(face = 'bold'),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  )

ggsave('medianfamilyincome.png', type = 'cairo-png')

# Plotting Louisville locations within existing for comparison-sake
locations1 <- data.frame('address' = existing_geocoded_merged$addy,
                         'pop.dens' = existing_geocoded_merged$total.population/existing_geocoded_merged$aland10,
                         'total.pop' = existing_geocoded_merged$total.population,
                         'preteens.per' = existing_geocoded_merged$TotalUnder14/existing_geocoded_merged$total.population)
locations2 <- data.frame('address' = datMerged$location,
                         'pop.dens' = datMerged$total.population/datMerged$aland10,
                         'total.pop' = datMerged$total.population,
                         'preteens.per' = datMerged$TotalUnder14/datMerged$total.population)
locations <- rbind(locations1, locations2)
locations$louisville <- c(rep('Existing Locations', nrow(existing_geocoded_merged)),
                         rep('Proposed Louisville', nrow(datMerged)))

# Plot total population
ggplot(locations, aes(x = reorder(address, -pop.dens), y = pop.dens, group = louisville)) + 
  geom_bar(stat = 'identity', aes(fill = louisville, alpha = louisville)) + 
  scale_fill_manual(values = c('#4C4C4C', '#30ac7a')) + 
  scale_alpha_manual(values = c(0.25, 1)) + 
  scale_y_continuous(labels = comma) + 
  theme_bw(base_family = "Trebuchet MS", base_size = 12) +
  ggtitle(expression(atop(bold('Total Population by Location (Existing and Proposed)                                                    '), 
                          atop('Both Wesport Villange and Shelbyville Plaza have a surrounding population density that is above average for existing locations')))) + 
  geom_hline(aes(yintercept = mean(existing_geocoded_merged$total.population/existing_geocoded_merged$aland10)), colour = '#fd7d47', size = 1.25) + 
  theme(
    plot.title = element_text(hjust = 0),
    text = element_text(colour = '#4C4C4C'),
    panel.background = element_rect('white'),
    plot.background = element_rect('white'),
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = '#F3F3F3', linetype = 'dashed', size = 0.25),
    panel.grid.minor.y = element_blank(),
    legend.background = element_rect('white'),
    legend.title = element_blank(),
    legend.position = 'right',
    legend.direction = 'vertical',
    legend.key = element_blank(),
    axis.text = element_text(face = 'bold'),
    axis.text.x = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  )
#ggsave('totalpop_within1mile_persqmi_comparison.png', type = 'cairo-png')

# Density of population..uh...density
ggplot(locations, aes(x = pop.dens, group = louisville)) + 
  geom_density(aes(fill = louisville), colour = '#ffffff', alpha = 0.25, size = 0) + 
  scale_fill_manual(values = c(c('#4E5C65', '#30ac7a'))) +
  scale_x_continuous(labels = comma) + 
  theme_bw(base_family = "Trebuchet MS", base_size = 12) +
  ggtitle(expression(atop(bold('Distributions of Population Densities (Existing and Proposed)                                       '), 
                          atop('The distributions of density within the surrounding geographies of both existing and proposed locations overlap significantly')))) + 
  theme(
    plot.title = element_text(hjust = 0),
    text = element_text(colour = '#4C4C4C'),
    panel.background = element_rect('white'),
    plot.background = element_rect('white'),
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = '#F3F3F3', linetype = 'dashed', size = 0.25),
    panel.grid.minor.y = element_blank(),
    legend.background = element_rect('white'),
    legend.title = element_blank(),
    legend.position = 'right',
    legend.direction = 'vertical',
    legend.key = element_blank(),
    axis.text = element_text(face = 'bold'),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  )
# ggsave('totalpop_densitywithin1mile_persqmi_comparison.png', type = 'cairo-png')

# Plot kids under 14 as share of total population
ggplot(locations, aes(x = reorder(address, -preteens.per), y = 100*preteens.per, group = louisville)) + 
  geom_bar(stat = 'identity', aes(fill = louisville, alpha = louisville)) + 
  scale_fill_manual(values = c('#4C4C4C', '#30ac7a')) + 
  scale_alpha_manual(values = c(0.25, 1)) + 
  scale_y_continuous(labels = comma) + 
  theme_bw(base_family = "Trebuchet MS", base_size = 12) +
  ggtitle(expression(atop(bold('Concentration of Children Under 14 Years Old (Existing and Proposed), in Percentages'), 
                          atop('Both Tyler Village and Springhurst are above the average concentration within 1 mile for existing locations')))) + 
  geom_hline(aes(yintercept = 100*mean(subset(locations, louisville == 'Existing Locations')$preteens.per)), colour = '#fd7d47', size = 1.25) +
  theme(
    plot.title = element_text(hjust = 0),
    text = element_text(colour = '#4C4C4C'),
    panel.background = element_rect('white'),
    plot.background = element_rect('white'),
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = '#F3F3F3', linetype = 'dashed', size = 0.25),
    panel.grid.minor.y = element_blank(),
    legend.background = element_rect('white'),
    legend.title = element_blank(),
    legend.position = 'right',
    legend.direction = 'vertical',
    legend.key = element_blank(),
    axis.text = element_text(face = 'bold'),
    axis.text.x = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  )
#ggsave('totalpreteens_within1mile_comparison.png', type = 'cairo-png')


# ---------------------------------------------------
# FINDING ELQs ('EXPERT LOCATION QUOTIENTS') FOR EACH 
# EXISTING LOCATION & COMPARING TO LOUISVILLE TRACTS
# ---------------------------------------------------

# Pull only population numbers for now, run household numbers later
existing_population_percentages <- merge(existing_geocoded, tracts, by.x = 'geoid', by.y = 'GEOID10')
existing_population_percentages <- merge(existing_population_percentages, usEcon, by = 'geoid', all.x = TRUE)
existing_population_percentages <- existing_population_percentages[, -c(2:5)]

# # Write to csv for later
# write.csv(existing_population_percentages, 'existing_population_percentages.csv')

# # Read from previously saved file
# existing_population_percentages <- read.csv('existing_population_percentages.csv',
#                                             header = TRUE, 
#                                             stringsAsFactors = FALSE)

# # Calculate % of total population for each demo
# existing_population_percentages[, c(7:ncol(existing_population_percentages))] <- existing_population_percentages[, c(11:ncol(existing_population_percentages))]/existing_population_percentages$DP0010001
# 
# # Create county geoid
# existing_population_percentages$county <- substr(as.character(existing_population_percentages$geoid), 1, 5)
# 
# ## Create US-Wide County-Level Demos
# 
# # Merge demographic and economic characteristics for all tracts together
# totalDemos <- merge(tracts, usEcon, by.x = 'GEOID10', by.y = 'geoid') 
# totalDemos$county <- substr(totalDemos$GEOID10, 1, 5) # Create county geoid
# 
# # Calculate county-level totals
# counties <- unique(totalDemos$county)
# countyDemos <- aggregate(totalDemos[, c(7:(ncol(totalDemos)-1))], by = list(totalDemos$county), 'sum') # Calculate county totals
# names(countyDemos)[1] <- 'geoid' # Rename first column to geoid
# 
# # Calculate % of total population for each demo
# countyDemos_percentages <- cbind(countyDemos$geoid, countyDemos[, c(2:ncol(countyDemos))]/countyDemos$DP0010001)
# names(countyDemos_percentages)[1] <- 'geoid'
# 
# ## Calculate Location Quotients for Existing Locations
# 
# # Uh, calculate them
# location_quotients <- existing_population_percentages[, c(7:(ncol(existing_population_percentages)-1))] / countyDemos_percentages[match(existing_population_percentages$county, countyDemos_percentages$geoid), c(2:ncol(countyDemos_percentages))]
# location_quotients <- cbind(existing_population_percentages$county, location_quotients)
# names(location_quotients)[1] <- 'geoid'
# 
# lq_melt <- melt(location_quotients) # Convert to long-form vs wide
# 
# # Built dataframe of 'prevalence' of existing locations to be 'experts' in the factor (# of locations that are ELQ for each variable)
# elq_prevalence <- as.data.frame(table(lq_melt$variable[which(lq_melt$value > 1)]))

# write.csv(elq_prevalence, 'elq.csv') # Save for renaming dumb guys

elq_prevalence <- read.csv('elq.csv', 
                           header = TRUE,
                           stringsAsFactors = FALSE)

elq_prevalence$Coded <- tolower(elq_prevalence$Coded)

# Remove duplicates, and re-plot with full names
elq_prevalence <- elq_prevalence[!duplicated(elq_prevalence$Coded), ]

ggplot(subset(elq_prevalence, Freq > quantile(elq_prevalence$Freq, 0.95)), aes(x = reorder(Coded, Freq), y = Freq)) + 
  geom_bar(stat = 'identity', fill = '#4e5c65', alpha = 0.25) + 
  coord_flip() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 50)) + 
  theme_bw(base_family = "Trebuchet MS", base_size = 12) +
  ggtitle(expression(atop(bold('Concentration of Expert Location Quotients ("ELQs") Within Existing Locations       '), 
                          atop('Existing locations appear to be centered in areas of largely white homeowners with high levels of employment and salary')))) +
  theme(
    plot.title = element_text(hjust = 0),
    text = element_text(colour = '#4e5c65'),
    panel.background = element_rect('white'),
    plot.background = element_rect('white'),
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = '#F3F3F3', linetype = 'dashed', size = 0.25),
    panel.grid.minor.y = element_blank(),
    legend.background = element_rect('white'),
    legend.title = element_blank(),
    legend.position = 'right',
    legend.direction = 'vertical',
    legend.key = element_blank(),
    axis.text = element_text(face = 'bold'),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  )
# ggsave('existing-elqs.png', type = 'cairo-png')

# Plot ELQs for age groups
elq_prevalence$under14 <- ifelse(grepl('under 5|5 to 9|10 to 14', elq_prevalence$Coded), 'Under14', 'Over14')

ggplot(subset(elq_prevalence, grepl('total population', Coded) & grepl('years', Coded) & !grepl('households|male|female', Coded)), aes(x = reorder(Coded, Freq), y = Freq, group = under14)) + 
  geom_bar(stat = 'identity', aes(fill = under14, alpha = under14)) + 
  coord_flip() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 50)) + 
  scale_fill_manual(values = c('#4e5c65', '#fd7d47')) + 
  scale_alpha_manual(values = c(0.25, 1)) +
  theme_bw(base_family = "Trebuchet MS", base_size = 12) +
  ggtitle(expression(atop(bold('Concentration of Age Attribute ELQs Within Existing Locations                  '), 
                          atop('Existing locations appear to be less focused around ares with less concentration of kids under the age of 14')))) +
  theme(
    plot.title = element_text(hjust = 0),
    text = element_text(colour = '#4e5c65'),
    panel.background = element_rect('white'),
    plot.background = element_rect('white'),
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = '#F3F3F3', linetype = 'dashed', size = 0.25),
    panel.grid.minor.y = element_blank(),
    legend.background = element_rect('white'),
    legend.title = element_blank(),
    legend.position = 'right',
    legend.direction = 'vertical',
    legend.key = element_blank(),
    axis.text = element_text(face = 'bold'),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  )
# ggsave('age-elqs.png', type = 'cairo-png')

# Calculate Louisville LQs
# Pull only population numbers for now, run household numbers later
lou_population_percentages <- subset(tracts, grepl('21111', tracts$GEOID10))
lou_population_percentages <- merge(lou_population_percentages, usEcon, by.x = 'GEOID10', by.y = 'geoid', all.x = TRUE)
lou_population_percentages <- lou_population_percentages[, -c(2:6)]

# # Write to csv for later
# write.csv(lou_population_percentages, 'louisville_population_percentages.csv')

# # Read from previously saved file
# lou_population_percentages <- read.csv('louisville_population_percentages.csv',
#                                             header = TRUE, 
#                                             stringsAsFactors = FALSE)

# Calculate % of total population for each demo
lou_population_percentages[, c(3:ncol(lou_population_percentages))] <- lou_population_percentages[, c(3:ncol(lou_population_percentages))]/lou_population_percentages$Total.Population

# # Write to csv
# write.csv(lou_population_percentages, 'lou_concentration.csv') # Save for renaming columns to top 15 ELQ variables

lou_concentration <- read.csv('lou_concentration.csv', 
                           header = TRUE,
                           stringsAsFactors = FALSE)

names(lou_concentration) <- tolower(names(lou_concentration))
lou_concentration[is.na(lou_concentration)] <- 0

lou_concentration_index <- data.frame(lou_concentration$geoid10)

for (i in 2:ncol(lou_concentration)) { 
  for (j in 1:nrow(lou_concentration)) { 
    
    lou_concentration_index[j, i] <- lou_concentration[j, i]/mean(lou_concentration[, i])
    
    }
  }
names(lou_concentration_index) <- names(lou_concentration)

# # Write to csv
# write.csv(lou_concentration_index, 'lou_concentration_index.csv')

# Building tract-level chloropleth (for total households)
names(lou_concentration_index)[1] <- 'GEOID10'

# Import shape file
tract <- readOGR(dsn = "tl_2010_21111_tract10", layer = "tl_2010_21111_tract10")
tract@data$id <- rownames(tract@data)

# Convert polygons in tract to a data frame for plotting
tract.df <- fortify(tract)

# Join columns
tract.df <- join(tract.df, tract@data, by="id")

# Join columns from df to tract
lou_concentration_index$GEOID10 <- as.factor(lou_concentration_index$GEOID10)

tract.df <- join(tract.df, lou_concentration_index, by = "GEOID10")

# Plot where complaints occur the most (subset where Frequency is not NA and greater than 2 to account for fat-finger)
ggplot(data = tract.df, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill = tract.df[,34]), colour = "#f0f2f3", alpha = 1, size = 0.0001) +
  scale_fill_gradient(low = "#f0f2f3",
                      high = "#30ac7a") +
  geom_path(data = tract.df, color="#f0f2f3", alpha = 0.25, size = 0.001) +
  geom_point(data = dat, aes(x = lon, y = lat, group = location), colour = '#fd7d47', fill = '#fd7d47', size = 4) +
  geom_text(data = dat, aes(x = lon, y = lat, group = location, label = location), colour = '#4e5c65', vjust = 1.25, fontface = 'bold') +
  ggtitle(str_wrap(paste('Index of ', gsub('  ', ' ', gsub('\\.', ' ',names(tract.df)[34])), sep = ''), width = 100)) +
  coord_equal() + 
  theme_bw(base_family = "Trebuchet MS", base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0),
    text = element_text(colour = '#4e5c65', face = 'bold'),
    panel.background = element_rect('white'),
    plot.background = element_rect('white'),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.background = element_rect('white'),
    legend.title = element_blank(),
    legend.position = 'right',
    legend.direction = 'vertical',
    legend.key = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  )

# Iterate through the 15 main guys and plot

for (i in 20:ncol(tract.df)) { 
  
  ggplot(data = tract.df, aes(x=long, y=lat, group=group)) +
    geom_polygon(aes(fill = tract.df[,i]), colour = "#f0f2f3", alpha = 1, size = 0.0001) +
    scale_fill_gradient(low = "#f0f2f3",
                        high = "#30ac7a") +
    geom_path(data = tract.df, color="#f0f2f3", alpha = 0.25, size = 0.001) +
    geom_point(data = dat, aes(x = lon, y = lat, group = location), colour = '#fd7d47', fill = '#fd7d47', size = 4) +
    geom_text(data = dat, aes(x = lon, y = lat, group = location, label = location), colour = '#4e5c65', vjust = 1.25, fontface = 'bold') +
    ggtitle(str_wrap(paste('Index of ', gsub('  ', ' ', gsub('\\.', ' ',names(tract.df)[i])), sep = ''), width = 100)) +
    coord_equal() + 
    theme_bw(base_family = "Trebuchet MS", base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0),
      text = element_text(colour = '#4e5c65', face = 'bold'),
      panel.background = element_rect('white'),
      plot.background = element_rect('white'),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.background = element_rect('white'),
      legend.title = element_blank(),
      legend.position = 'right',
      legend.direction = 'vertical',
      legend.key = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank()
    )
  ggsave(paste('Index of ', gsub('  ', ' ', gsub('\\.', ' ',names(tract.df)[i])), '.png', sep = ''), type = 'cairo-png')
  
  }


ggplot(lou_concentration, aes(x = reorder(geoid10, -total.population.in.households), y = total.population.in.households)) + 
  geom_bar(stat = 'identity', fill = '#4e5c65', alpha = 0.25) + 
  coord_flip() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 50)) + 
  theme_bw(base_family = "Trebuchet MS", base_size = 12) +
  ggtitle(expression(atop(bold('Concentration of Expert Location Quotients ("ELQs") Within lou Locations       '), 
                          atop('lou locations appear to be centered in areas of largely white homeowners with high levels of employment and salary')))) +
  theme(
    plot.title = element_text(hjust = 0),
    text = element_text(colour = '#4e5c65'),
    panel.background = element_rect('white'),
    plot.background = element_rect('white'),
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = '#F3F3F3', linetype = 'dashed', size = 0.25),
    panel.grid.minor.y = element_blank(),
    legend.background = element_rect('white'),
    legend.title = element_blank(),
    legend.position = 'right',
    legend.direction = 'vertical',
    legend.key = element_blank(),
    axis.text = element_text(face = 'bold'),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  )
# ggsave('lou-elqs.png', type = 'cairo-png')


