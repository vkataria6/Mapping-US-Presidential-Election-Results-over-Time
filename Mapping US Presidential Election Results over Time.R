```{r}
par(cex = 1.5)
library(maps)
library(choroplethr)
library(choroplethrMaps)
library(dplyr)
library(ggplot2)

## I have downloaded fips codes from www.nrcs.usda.gov and created fips.csv so thtat I can add fips codes to the original dataset.
election <- read.csv("elections.csv") %>% data.frame()
fips <- read.csv("fips.csv") %>% data.frame()
## Checking if there is any NA
any(is.na(election))
## create a character vector of `state,county' inputs
## 2008 election
election <- election %>% 
  filter(year == 2008) %>%
  left_join(fips, by = "county")

## coloring based on two-party vote share
### If a value is greater than 0 in a county, the Republican candidate received more votes in the county.
### If a value is smaller than 0 in a county, the Democratic candidate received more votes in the county.
### Red indicates the Republican inclination, blue indicates the Democratic inclination, and the lighter the color is, the less the difference between the two parties was observed.
election <- data.frame(election) %>%
  mutate("total" = rep + dem + other) %>%
  mutate("rep_share" = rep/total) %>%
  mutate("dem_share" = dem/total) %>%
  mutate("value" = rep_share - dem_share) 

## initialize a map for Arizona
electionAZ <- election %>%
  filter(state == "arizona") %>%
  rename(region = fips)


AZ <- county_choropleth(electionAZ,
                        title      = "Outcome of the 2008 US Presidential Election: Arizona",
                        legend     = "value",
                        num_colors = 1,
                        state_zoom = "arizona")

AZ + scale_fill_gradient2(
  low = "#0033FF",
  mid = "white",
  high = "#CC0000",
  midpoint = 0,
  na.value = "grey50",
) +
  coord_map("ortho", orientation = c(40, -100, 0))+
  theme_minimal()

## initialize a map for Massachusetts
electionMA <- data.frame(election) %>%
  filter(state == "massachusetts") %>%
  rename(region = fips)


MA <- county_choropleth(electionMA,
                        title      = "Outcome of the 2008 US Presidential Election: Massachusetts",
                        legend     = "value",
                        num_colors = 1,
                        state_zoom = "massachusetts")

MA + scale_fill_gradient2(
  low = "#0033FF",
  mid = "white",
  high = "#CC0000",
  midpoint = 0,
  na.value = "grey50",
) +
  coord_map("ortho", orientation = c(40, -100, 0))+
  theme_minimal()

```

## My comment
All of Massachusetts countries voted overwhelmingly in favor of Barack Obama in 2008. However, the political tendencies of Arizonas counties ranged widely. Only two countries were McCains strongholds when compared to Obamas in the general election. Obama won four counties in Arizona against McCain, which is surprising condisering that McCain was known as the "home of turf" of Arizona. 