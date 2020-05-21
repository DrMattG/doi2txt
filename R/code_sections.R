# Geocoding study sites ####


# get_countries_from_section ----------------------------------------------

# get dataframe of countries mentioned in article section

get_countries_fr_section<-function(section){
library(tidyverse)
library(maps)
## Loading country data from package maps
data(world.cities)
all_countries <- str_c(unique(world.cities$country.etc), collapse = "|")
###Remove punctuation
raw <- gsub("[[:punct:]\n]","",section)

CountryList_raw1<-sapply(str_extract_all(raw, all_countries), toString)
CountryList_raw<-gsub('\\b\\w{1,4}\\b','',CountryList_raw1)

as_tibble(CountryList_raw)
}


# get_countries_from_section ----------------------------------------------


#get dataframe of cities mentioned in article section

get_cities_fr_section<-function(section){
  library(tidyverse)
  library(maps)
  ## Loading country data from package maps
  data(world.cities)
  all_cities <- str_c(unique(world.cities$name), collapse = "|")
  ###Remove punctuation
  raw <- gsub("[[:punct:]\n]","",section)

  CityList_raw1<-sapply(str_extract_all(raw, all_cities), toString)
  CityList_raw<-gsub('\\b\\w{1,3}\\b','',CityList_raw1)
  as_tibble(CityList_raw)
}


# get_country_location ----------------------------------------------------

#username="matt_grainger"

get_country_location<-function(x, username){
  if(missing(username)) stop('Please input your geonames users name - make sure you have enabled webservices at https://www.geonames.org')

dat=x
dat %>%
  rowid_to_column() %>%
  mutate(Country = strsplit(value, ",")) %>%
  unnest(Country) %>%
  select(c(rowid,Country)) %>%
  group_by(rowid) %>%
  mutate(Country=str_trim(Country, side ="both")) %>%
  distinct()->dat


library(countrycode)
library(geonames)


options(geonamesUsername=username)

dat %>%
  mutate(CountryCode=countrycode(Country, origin = 'country.name', destination = 'iso2c')) %>%
  group_by(Country) %>%
  mutate(north=geonames::GNcountryInfo(CountryCode)$north) %>%
  mutate(east=geonames::GNcountryInfo(CountryCode)$east)

}

# Coding features from an ontology ####

