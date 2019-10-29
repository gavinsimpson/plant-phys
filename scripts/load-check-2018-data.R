## Load & check 2018 Biol 266 Plant Phys Lab Plant Growth Data

## Packages
library('readxl')  # read from Excel sheets
library('tidyr')   # data processing
library('dplyr')   # mo data processing
library('forcats') # mo mo data processing
library('purrr')
library('here')
library('tibble')
library('ggplot2')

theme_set(theme_bw())

## wrapper to read_excel
read_sheets <- function(i, fname, col_types) {
    read_excel(fname, sheet = i, col_types = col_types)
}

## 2018 data
fname <- here('f18ph.xls')
col_types <- rep(c('text','numeric'), times = c(3, 9))
plant18 <- map_df(1:18, read_sheets, fname = fname, col_types = col_types)
plant18 <- add_column(plant18, groupid = rep(1:18, each = 24), .before = 1L)

## Column name contain data;
##   i) the variable measured 'height', 'internodes', etc
##  ii) the day on which a measurement was taken,
## with these separated by a colon `:`.
## Also, the first 4 columns need repeating for each measurement:day pair,
## so we *exclude* them from the pivot `-(1:4)`
## Finally, we want a column with the variable measured and column with the day
plant18 <- pivot_longer(plant18, -(1:4), names_sep = ':',
                        names_to = c('variable','day'),
                        names_ptypes = list(day = integer()))

## look at what this has done
plant18

## The new columns are:
##   * variable: what was measured?
##   * day: when was it measured?
##   * value: what value did you write down?

## Now we want to spread the `variable` column out to multiple new columns, one
## for each variable 'height', 'internodes', 'freshwt'. So we take the column
## names from the 'variable' column and the values (data) for these columns
## come from the `value` column.
plant18 <- pivot_wider(plant18, names_from = variable, values_from = value)

## Look at the result
plant18

## Note that `plantid` == 1 in `control` treatment is not the same as
## `plantid` == 1 in the `ga10` treatment. We need unique identifiers
plant18 <- mutate(plant18, id = paste0(groupid, "_", cultivar, "_", treatment, "_", plantid))

## Get the ordering of `treatment` ready for plotting
plant18 <- mutate(plant18, treatment = fct_relevel(treatment, 'control'))

## plot
ggplot(plant18, aes(x = day, y = height, group = id, colour = treatment)) +
    geom_line() +
    geom_point() +
    facet_wrap(~ cultivar) +
    labs(y = "Height (mm)", x = "Day", colour = "Treatment")

ggplot(plant18, aes(x = day, y = freshwt, colour = treatment)) +
    geom_boxplot() +
    facet_wrap(~ cultivar) +
    labs(y = "Fresh weight (g)", x = "Day", colour = "Treatment")
