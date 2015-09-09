# Init --------------------------------------------------------------------

library(hmdget) # download human mortality database data (https://github.com/jschoeley/hmdget)
library(dplyr)  # data verbs, operations on subsets of data
library(tidyr)  # tidy data, convert between long and wide
library(readr)  # fast csv read

# Download HMD data -------------------------------------------------------

# HMD country codes
cntry <- hmdcbook$Code

# period and cohort death counts across countries
death <- HMDget(.country = cntry, .timeframe = "p+c", .measure = "Dx",
                username, password)

# period and cohort exposures across countries
exp <- HMDget(.country = cntry, .timeframe = "p+c", .measure = "Nx",
              username, password)

# join Dx and Nx
hmdDxNx <- full_join(death, exp)

# write mortality data to csv
write_csv(hmdDxNx, path = "./priv/misc/hmdDxNx.csv")

# Transform ---------------------------------------------------------------

# read downloaded data
read.csv("./priv/hmdDxNx.csv",
         stringsAsFactors = FALSE,
         na.strings = "NA") -> hmd

hmd %>%
  # variable names to lowercase
  rename(country = Country, timebase = Timeframe, sex = Sex,
         year = Year, age = Age, dx = Dx, nx = Nx) %>%
  # rename variable levels
  mutate(
    timebase = tolower(timebase),
    sex = ifelse(sex == "Female", "f", ifelse(sex == "Male", "m", "fm"))
  ) %>%
  # remove rows with 0 exposure
  filter(nx != 0) %>%
  mutate(
    # compute rounded mortality rates
    mx = round(dx / nx, digits = 5),
    # replace 0 mortality rate with NA
    mx = ifelse(mx == 0, NA, mx),
    # project cohort-age data onto period-age grid
    year = ifelse(timebase == "cohort", year + age, year)
  ) %>%
  # exclude raw death counts and exposures
  select(-dx, -nx) %>%
  # remove NA's
  na.omit() %>%
  # convert to dplyr data frame
  as_data_frame() -> hmd_mx

save(hmd_mx, file = "./priv/data/hmd_mx.Rdata")

# derive data set for mortality rate sex differences
hmd_mx %>%
  # compute sex difference in mortality
  filter(sex != "fm") %>%
  spread(key = sex, value = mx) %>%
  mutate(mx_sex_diff = f - m) %>%
  # strip sex specific mortality rates
  select(-f, -m) -> hmd_mx_sex_diff

save(hmd_mx_sex_diff, file = "./priv/data/hmd_mx_sex_diff.Rdata")
