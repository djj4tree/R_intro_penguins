####------------------------------------------------------------------------####
## EXAMPLE SCRIPT FOR NATURAL RESOURCE SAMPLING
# November 2024
# Brought to you in 4-D
# Dan Johnson
# Denver Cayetano
# Diego Rocha
# D. Alex Bowers
####------------------------------------------------------------------------####

####---- OBJECTIVES ------------------------------------------------------------

# You will get an introduction into how to:
 # Create objects
 # View, explore and subset data
 # Create scatter and box plots with base R and ggplot
 # Conduct T-test, ANOVA and 
 # Linear regression and view results

#### ####

####---- TIPS AND TRICKS -------------------------------------------------------  

# Hash tags allow you to comment. 
# this way R doesn't mistake it for code or a function

#### Multiple hash tags allow creation 
# of collapsible sections in the code ####

# CTRL + ENTER allow you to run a line of code

# this symbol <- allows you to assign an object

# functions end with () just like Excel 

# We can use the dollar sign $ to select 
# a particular column from data e.g. pen$species

# Shorter object names saves time typing and reduces typing errors
# Only use an object name once


####---- LOAD PACKAGES/LIBRARIES -----------------------------------------------

# Libraries/package have pre-made tools/functions
# that help us to work with data and make figures
# Some functions are built into R and require no libraries/packages

library(ggplot2)  # for making figures
library(palmerpenguins) # Gives us the data


####---- LOAD DATA -------------------------------------------------------------

# Let's load some data
# We will use data about penguins 
# from the palmerpenguins package above

 # import penguins data & shorten to "pen" 
 # to avoid misspelling pequins

pen <- penguins


####---- EXPLORE THE DATA ------------------------------------------------------

# We will look at the object 'pen' with the 'View' function

View(pen)

head(pen) # gives you the first six rows of the dataset
tail(pen) # gives you the last six rows of the dataset
names(pen) # gives you the names of the columns in your dataset

# Let's look at the number of individuals per species on each island
table(pen$species, pen$island) # which species occur on which islands?

# Let's summarize all the columns with the summary function
summary(pen)

# Calculate max, min, mean, and median body mass 
max(pen$body_mass_g, na.rm = TRUE)
min(pen$body_mass_g, na.rm = TRUE)
mean(pen$body_mass_g, na.rm = TRUE)
median(pen$body_mass_g, na.rm = TRUE)

# na.rm = TRUE makes the function ignore all the NA values

####--- VISUALIZE THE DATA: SCATTER PLOTS --------------------------------------
# the plot function create scatter plots of two variables

# scatter plot of body mass vs bill length
plot(pen$body_mass_g, pen$bill_length_mm) 

# scatter plot of body mass vs flipper length
plot(pen$body_mass_g, pen$flipper_length_mm) 

# scatter plot of body mass vs bill depth
plot(pen$body_mass_g, pen$bill_depth_mm) 

# Remember the ggplot2 package? This gives prettier figures
ggplot(data = pen, aes(x = body_mass_g, y = bill_length_mm)) +
  geom_point() 




ggplot(data = pen, aes(x = body_mass_g, y = bill_length_mm)) +
  geom_point() + # make the points
  labs(x = "Body mass (g)", y = "Bill length (mm)")




ggplot(data = pen, aes(x = body_mass_g, y = bill_length_mm)) +
  geom_point() + # make the points
  labs(x = "Body mass (g)", y = "Bill length (mm)")+ # labels
  theme_bw()  # the theme is the style of the plot
              
  


####--- VISUALIZE THE DATA: BOX PLOTS ----####

# The 'boxplot' function creates box plots
# of a continuous variable by groups

# box plot of body mass by sex
boxplot(pen$body_mass_g ~ pen$sex) 

# box plot of body mass by species and interactions
boxplot(pen$body_mass_g ~ pen$species) 
# multiple categorical variables are possible
boxplot(pen$body_mass_g ~ pen$sex*pen$species)
boxplot(pen$body_mass_g ~ pen$sex*pen$species*pen$island)

# A nicer plot with ggplot 
ggplot(data = pen, aes(x = species, y = body_mass_g)) +
  geom_boxplot() +
  labs(x = "Species", y = "Body mass (g)")+ # label axes
  theme_classic()

# you can make complicated figures quickly with ggplot
ggplot(pen, aes(x = sex, y = body_mass_g))+
  geom_boxplot()+
  labs(x = "Sex", y = "Body mass (g)")+ # label axes
  facet_grid(species~island)+
  theme_light()



####---- DATA ANALYSIS: T-TEST ----####

# Adelie penguins occur on all islands
table(pen$species, pen$island)

# Question
# Do Adelie penguins differ in weight by sex?
# We can answer this with a T-test

# Let's subset the data only for Adelie penguins
# and make it a different object
# this is like making a new worksheet in Excel

adel <- subset(pen, pen$species == 'Adelie' & !is.na(pen$sex))
# look in the Environment tab


# for the t-test we need the males and females seperated
# subset data for males
male_adel <- subset(adel, adel$sex=='male') 

# subset data for females
female_adel <- subset(adel, adel$sex=='female') 

# Test for differences with T-test
t.test(x = male_adel$body_mass_g, y = female_adel$body_mass_g) 

# is there a significant difference?

# let's visualize this with boxplots
ggplot(adel, aes(y = body_mass_g, x = sex))+
  geom_boxplot()+
  theme_classic()

####---- DATA ANALYSIS: ANOVA ----####

# Does the weight of male Adelie penguins differ by island?

ggplot(male_adel, aes(x = island, y = body_mass_g))+
  geom_boxplot() +
  labs(x = 'Island', y = "Body mass (g)") +
  theme_bw()


# Let's test with an ANOVA -- can be written two ways
anova_mod <- aov(male_adel$body_mass_g ~ male_adel$island) 

anova_mod <- aov(body_mass_g ~ island, data = male_adel)

summary(anova_mod) # Is there a significant difference?


# Remember that complicated Tukey post-hoc test in excel?
TukeyHSD(anova_mod)


#What if we looked at the difference among species?
anova_mod2 <- aov(pen$body_mass_g ~ pen$species) 
summary(anova_mod2) # Is there a significant difference?

# Which ones are different?
TukeyHSD(anova_mod2)


####---- DATA ANALYSIS: LINEAR REGRESSION ----####

# Is there a relationship between body mass and 
# flipper length in female Adelie penguins?

# scatter plot of body mass vs bill length
plot(female_adel$body_mass_g,
     female_adel$bill_length_mm) 

# Let's test the relationship with a regression

regression_mod <- lm(body_mass_g ~ flipper_length_mm,
                     data = female_adel)

summary(regression_mod) # Is there a significant relationship?

ggplot(female_adel, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point() +
  geom_smooth(method = 'lm') + # this adds a trendline like in Excel
  labs(x = "Flipper length (mm)", y = "Body mass (g)") +
  theme_bw()

#### Regression where groups matter  ####

ggplot(pen, aes( x = bill_length_mm, y = bill_depth_mm))+
  geom_point() +
  geom_smooth(method = 'lm') + 
  labs(x = "Bill length (mm)", y = "Bill depth (mm)") +
  theme_bw()

ggplot(pen, aes( x = bill_length_mm, y = bill_depth_mm, color = species))+
  geom_point() +
  geom_smooth(method = 'lm') + 
  scale_color_viridis_d(option = 'magma', end = 0.8)+
  labs(x = "Bill length (mm)", y = "Bill depth (mm)") +
  theme_bw()



####---- ONLINE RESOURCES TO LEARN R -------------------------------------------

# https://ourcodingclub.github.io/tutorials
# https://swirlstats.com/

