## Analyse Maria's Plant Phys Lab Plant Growth Data

## Packages
library('readxl')  # read from Excel sheets
library('tidyr')   # data processing
library('dplyr')   # mo data processing
library('forcats') # mo mo data processing
library('ggplot2') # plottinf
library('lme4')    # for the proper model
library('emmeans') # for post hoc comparisons

## Set plot theme
theme_set(theme_bw())

## Load Data
plant <- read_xls('f18ph.xls')

## Look at the data
plant

## Column name contain data;
##   i) the variable measured 'height', 'internodes', etc
##  ii) the day on which a measurement was taken,
## with these separated by a colon `:`.
## Also, the first 3 columns need repeating for each measurement:day pair,
## so we *exclude* them from the pivot `-(1:3)`
## Finally, we want a column with the variable measured and column with the day
plant <- pivot_longer(plant, -(1:3), names_sep = ':',
                      names_to = c('variable','day'),
                      names_ptypes = list(day = integer()))

## look at what this has done
plant

## The new columns are:
##   * variable: what was measured?
##   * day: when was it measured?
##   * value: what value did you write down?

## Now we want to spread the `variable` column out to multiple new columns, one
## for each variable 'height', 'internodes', 'freshwt'. So we take the column
## names from the 'variable' column and the values (data) for these columns
## come from the `value` column.
plant <- pivot_wider(plant, names_from = variable, values_from = value)

## Look at the result
plant

## We now have columns of data for `height`, `internodes`, & `freshwt`
## The `NA` values in `freshwt` are indicators of *missing* *data*, because
## you didn't measure the fresh weight of the plants on an day other than 21.

## repeat for dwarf
dwarf <- read_xls('f18ph.xls', sheet = 4)
dwarf <- pivot_longer(dwarf, -(1:3), names_sep = ':',
                      names_to = c('variable','day'),
                      names_ptypes = list(day = integer()))
dwarf <- pivot_wider(dwarf, names_from = variable, values_from = value)

## bind dwarf on to plant
plant <- bind_rows(plant, dwarf)

## Note that `plantid` == 1 in `control` treatment is not the same as
## `plantid` == 1 in the `ga10` treatment. We need unique identifiers
plant <- mutate(plant, id = paste0(cultivar, "_", treatment, "_", plantid))

## Get the ordering of `treatment` ready for plotting
plant <- mutate(plant, treatment = fct_relevel(treatment, 'control'))

## Filter out the NAs for the height model
##plant <- drop_na(plant, c(day, treatment, cultivar, height))

## Analysis

## Plot `height` over time
ggplot(plant, aes(x = day, y = height, group = id, colour = treatment)) +
    geom_point() +
    geom_line() +
    facet_wrap(~ cultivar) +
    labs(y = 'Height (cm)', x = 'Day', colour = 'Treatment')

### Height
h1 <- lm(height ~ day + treatment + cultivar +
             day:treatment + cultivar:treatment + day:cultivar +
             day:treatment:cultivar,
         data = plant)
summary(h1)

h1mm <- emtrends(h1, trt.vs.ctrl ~ treatment | cultivar, var = "day")
h1mm
plot(h1mm, comparisons = TRUE)

emmip(h1, treatment ~ day | cultivar, cov.reduce = FALSE, CIs = TRUE)

## ggplot(plant, aes(x = day, y = height, colour = treatment)) +
##     geom_jitter(width = 0.3, height = 0) +
##     geom_smooth(method = 'lm', se = FALSE) +
##     facet_wrap(~ cultivar) +
##     labs(y = 'Height (cm)', x = 'Day', colour = 'Treatment')

emtrends(h1, pairwise ~ treatment | cultivar, var = "day")

emtrends(h1, pairwise ~ cultivar | treatment, var = "day")

emmip(h1, cultivar ~ day | treatment, cov.reduce = FALSE, CIs = TRUE)

### Internodes

## Plot `internodes` over time
ggplot(plant, aes(x = day, y = internodes, group = id, colour = treatment)) +
    geom_point() +
    geom_line() +
    facet_wrap(~ cultivar) +
    labs(y = 'Number of Internodes', x = 'Day', colour = 'Treatment')

i1 <- lm(internodes ~ day + treatment + cultivar +
             day:treatment + cultivar:treatment + day:cultivar +
             day:treatment:cultivar,
         data = plant)
summary(i1)

i1mm <- emtrends(i1, trt.vs.ctrl ~ treatment | cultivar, var = "day")
i1mm
plot(i1mm, comparisons = TRUE)

emmip(i1, treatment ~ day | cultivar, cov.reduce = FALSE, CIs = TRUE)

emtrends(i1, pairwise ~ treatment | cultivar, var = "day")

emtrends(i1, pairwise ~ cultivar | treatment, var = "day")

emmip(i1, cultivar ~ day | treatment, cov.reduce = FALSE, CIs = TRUE)

### Fresh weight

## plot `freshwt` on day 21
ggplot(plant, aes(x = treatment, y = freshwt, colour = cultivar)) +
    geom_boxplot() +
    labs(y = 'Fresh weight (g)', x = 'Treatment')

fw1 <- lm(freshwt ~ cultivar + treatment + cultivar:treatment,
         data = plant)
summary(fw1)

fw1mm <- emmeans(fw1, trt.vs.ctrl ~ treatment | cultivar)
fw1mm
plot(fw1mm, comparisons = TRUE)

emmip(fw1, treatment ~ cultivar, CIs = TRUE)

emmeans(fw1, pairwise ~ treatment | cultivar)

emmeans(fw1, pairwise ~ cultivar | treatment)

emmip(fw1, cultivar ~ treatment, CIs = TRUE)
