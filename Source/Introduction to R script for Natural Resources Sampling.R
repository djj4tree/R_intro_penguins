## Example script for Natural Resource Sampling
# November 2024
# Brought to you in 4-D
# Dan Johnson
# Denver Cayetano
# Diego Rocha
# D. Alex Bowers

# first we need some toolkits called packages or libraries
library(ggplot2)  # for making figures
library(palmerpenguins) # for the data

# First thing to do is load the data into the session
# the library already loads the data in but I am going to
# shorten the name to avoid misspelling penquin

pen <- penguins

# explore the data

# We will look at the object 'pen'
View(pen)

# Let's summarize all the columns
summary(pen)

# which species occur on which islands?

table(pen$species, pen$island)

# visualize the data

plot(pen$body_mass_g, pen$bill_length_mm)
plot(pen$body_mass_g, pen$flipper_length_mm)
plot(pen$body_mass_g, pen$bill_depth_mm)

boxplot(pen$body_mass_g ~ pen$sex)
boxplot(pen$body_mass_g ~ pen$species)
boxplot(pen$body_mass_g ~ pen$sex*pen$species)
boxplot(pen$body_mass_g ~ pen$sex*pen$species*pen$island)

ggplot(pen, aes(x = sex, y = body_mass_g))+
  geom_boxplot()+
  facet_grid(species~island)

# since Adelie penguins occur on all islands lets make an object
# with just that species

adel <- subset(pen, pen$species == 'Adelie')

# t-test to see if Adelie penguins differ in weight by sex

madel <- subset(adel, adel$sex=='male')
fadel <- subset(adel, adel$sex=='female')

t.test(x = madel$body_mass_g, y = fadel$body_mass_g)



# regression model to see the relation between 
# body mass and flipper length in Adelie females

mod <- lm(fadel$body_mass_g ~ fadel$flipper_length_mm)

summary(mod)

ggplot(fadel, aes(x = flipper_length_mm, y = body_mass_g))+
  geom_point()+
  geom_smooth(method = 'lm')+
  labs(x = "Flipper length (mm)", y = "Body mass (g)")+
  theme_bw()
# ANOVA to see if Adelie male penguins differ in weight by island

mod <- lm(madel$body_mass_g~madel$island)
anova(mod)

ggplot(madel, aes(x = island, y = body_mass_g))+
  geom_boxplot()+
  labs(x = 'Island', y = "Body mass (g)")+
  theme_bw()

