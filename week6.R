library(tidyverse)
library(dplyr)
library(ggplot2)


path <- "McDaniel/ANA_515/StormEvents_details-ftp_v1.0_d1995_c20220425.csv"
data <- read_csv(path)

myvars <- c("BEGIN_YEARMONTH","BEGIN_DATE_TIME","END_DATE_TIME","EPISODE_ID","EVENT_ID","STATE","STATE_FIPS","CZ_NAME","CZ_TYPE","CZ_FIPS","SOURCE","EVENT_TYPE","BEGIN_LAT","BEGIN_LON","END_LAT","END_LON")
newdata <- data[myvars]

arranged <- arrange(newdata, BEGIN_YEARMONTH)

arranged[c("STATE")] <- lapply(arranged[c("STATE")], str_to_title)
arranged[c("CZ_NAME")] <- lapply(arranged[c("CZ_NAME")], str_to_title)

ctype <- filter(arranged, CZ_TYPE=="C")
ctype <- select(ctype, -CZ_TYPE)

ctype$STATE_FIPS <- str_pad(ctype$STATE_FIPS, width=3, side="left", pad="0")
ctype$CZ_FIPS <- str_pad(ctype$CZ_FIPS, width=3, side="left", pad="0")

merged = unite(ctype, fips, c("STATE_FIPS", "CZ_FIPS"), sep="")

renamed <- merged %>% rename_all(tolower)

data("state")
us_state_df <- data.frame(state=state.name, region=state.region, area=state.area)

new_freq_df <- data.frame(table(renamed$state))
new_freq_df <- rename(new_freq_df, c("state"="Var1"))
final <- merge(x=new_freq_df, y=us_state_df, by.x="state", by.y="state")

storm_plot <- ggplot(final, aes(x=area, y=Freq)) +
  geom_point(aes(color=region)) +
  labs(x="Land area (square miles)",
       y="# of storm events in 1995")
storm_plot