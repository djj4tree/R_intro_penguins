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
 # Conduct T-test, ANOVA and Linear regression and view results



####---- TIPS AND TRICKS -------------------------------------------------------  

# Hash tags allow you to comment..this way R doesn't mistake it for code or a function
# Multiple hash tags allow creation of collapsible sections in the code
# CTRL + ENTER allow you to run a line of code
# this symbol <- allow you to assign an object
# Just as in Excel functions end with ()
# We can use the dollar sign ($) to select a particular column from data e.g. pen$species
# Shorter object names saves time typing and reduces typing errors
# Only use an object name once



####---- ONLINE RESOURCES TO LEARN R -------------------------------------------

# https://ourcodingclub.github.io/tutorials
# https://swirlstats.com/



####---- LOAD PACKAGES/LIBRARIES -----------------------------------------------

# Libraries/package have pre-made tools/functions that help us to work with data and make figures
# Some functions are built into R and require no libraries/packages

library(ggplot2)  # for making figures
library(palmerpenguins) # Gives us some data to work with



####---- LOAD DATA -------------------------------------------------------------

# Let's load some data to play with. 
# We will use data about penguins from the palmerpenguins package above

pen <- penguins # import penguins data & shorten to "pen" to avoid mispelling



####---- EXPLORE THE DATA ------------------------------------------------------

# We will look at the object 'pen' with the view function...look at the columns
View(pen)

head(pen) # gives you the first six rows of the dataset
tail(pen) # gives you the last six rows of the dataset
names(pen) # gives you the names of the columns in your dataset

# Let's look at the number of individuals per species on each island
table(pen$species, pen$island) # which species occur on which islands?

# Let's summarize all the columns with the summary function
summary(pen)

# Calculate max, min and mean body mass 
max(pen$body_mass_g, na.rm = T)
min(pen$body_mass_g, na.rm = T)
mean(pen$body_mass_g, na.rm = T)


####--- VISUALIZE THE DATA: SCATTER PLOTS --------------------------------------
# the plot function create scatter plots of two variables

plot(pen$body_mass_g, pen$bill_length_mm) # scatter plot of body mass vs bill length

plot(pen$body_mass_g, pen$flipper_length_mm) # scatter plot of body mass vs flipper length

plot(pen$body_mass_g, pen$bill_depth_mm) # scatter plot of body mass vs bill depth

# Remember the ggplot2 package we loaded above? This gives prettier figures
ggplot(data = pen, aes(x = body_mass_g, y = bill_length_mm)) +
  geom_point() +
  theme_bw() + # the theme is the style of the plot...try replacing this with theme_classic()
  labs(x = "Body mass (g)", y = "Bill length (mm)") # Rename the axis



####--- VISUALIZE THE DATA: BOX PLOTS ------------------------------------------

# The boxplot function creates box plots of a continuous variable by groups

boxplot(pen$body_mass_g ~ pen$sex) # box plot of body mass vs sex

boxplot(pen$body_mass_g ~ pen$species) # box plot of body mass vs sex

# A fancier plot with ggplot -- try adding a theme here
ggplot(data = pen, aes(x = species, y = body_mass_g)) +
  geom_boxplot() +
  labs(x = "Sex", y = "Body mass (g)") # Rename the axis

### FROM DENVER--- I THINK BELOW MIGHT BE TOO MUCH TOO SOON
boxplot(pen$body_mass_g ~ pen$sex*pen$species)
boxplot(pen$body_mass_g ~ pen$sex*pen$species*pen$island)

ggplot(pen, aes(x = sex, y = body_mass_g))+
  geom_boxplot()+
  facet_grid(species~island)



####---- DATA ANALYSIS: T-TEST -------------------------------------------------

# Adelie penguins occur on all islands, right?
table(pen$species, pen$island)

# Let's subset data only for Adelie penguins and save as a different object
adel <- subset(pen, pen$species == 'Adelie')

# Do Adelie penguins differ in weight by sex? We can answer this with a T-test
male_adel <- subset(adel, adel$sex=='male') # subset data for males
female_adel <- subset(adel, adel$sex=='female') # subset data for females

# view the subsetted data
View(male_adel)
View(female_adel)

# Test for differences with T-test
t.test(x = male_adel$body_mass_g, y = female_adel$body_mass_g) # is there a significant difference?



####---- DATA ANALYSIS: ANOVA --------------------------------------------------

# Does the weight of male Adelie penguins differ by island?
View(male_adel)

ggplot(male_adel, aes(x = island, y = body_mass_g))+
  geom_boxplot() +
  labs(x = 'Island', y = "Body mass (g)") +
  theme_bw()


# Let's test with an ANOVA -- can be written two ways
anova_mod <- aov(male_adel$body_mass_g ~ male_adel$island) 

anova_mod <- aov(body_mass_g ~ island, data = male_adel)

summary(anova_mod) # Is there a signicant difference?


# Remember that complicated Tukey post-hoc test in excel?
TukeyHSD(anova_mod)


#### FROM DENVER --- ISNT ABOVE THE COMMON APPROACH? LEAVE LM FOR REGRESSION SECTION?
#mod <- lm(male_adel$body_mass_g ~ male_adel$island)
#anova(mod)



####---- DATA ANALYSIS: LINEAR REGRESSION --------------------------------------

# Is there a relationship between body mass and flipper length in female Adelie penguins?
View(female_adel)

plot(female_adel$body_mass_g, female_adel$bill_length_mm) # scatter plot of body mass vs bill length

# Let's test the relationship with a regression-- can be written two ways
regression_mod <- lm(female_adel$body_mass_g ~ female_adel$flipper_length_mm)

regression_mod <- lm(body_mass_g ~ flipper_length_mm, data = female_adel)

summary(regression_mod) # Is there a significant relationship?

ggplot(female_adel, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point() +
  geom_smooth(method = 'lm') + # this adds a trendline like in Excel
  labs(x = "Flipper length (mm)", y = "Body mass (g)") +
  theme_bw()




####---- COOL CUSTOM GRAPHS WITH GGPLOT -----------------------------------------------

# ggplot gives you an array of possibilities when plotting, you can change the points 
# size, shape, colors, the plot theme. Let's try something different. 

ggplot(data = pen, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(color = "darkgreen", 
             size = 3) +  # scatter plot of points
  geom_smooth(method = "lm", 
              se = FALSE, 
              color = "red", 
              size = 1.5) +  # add regression line
  labs(title = "Flipper Length vs Body Mass",
       x = "Flipper Length (mm)", 
       y = "Body Mass (g)") + 
  theme_minimal()  # minimal theme for a cleaner look


# Create the second plot (Flipper Length vs Body Mass) with different customizations
ggplot(data = pen, aes(x = flipper_length_mm, y = body_mass_g)) +
  # Change the point shape to a triangle and set the color to blue
  geom_point(shape = 17, color = "steelblue", size = 4) +  
  # Add the regression line with a thicker, dashed orange line
  geom_smooth(method = "lm", se = FALSE, color = "orange", size = 1.5, linetype = "dashed") +  
  # Customize the labels and title with a more bold font
  labs(title = "Flipper Length vs Body Mass", 
       x = "Flipper Length (mm)", 
       y = "Body Mass (g)") + 
  theme_classic() +  # Change to classic theme (there's other possibilities as well)
  # Customize the font and size of the axis labels and title
  theme(axis.title = element_text(size = 14, face = "bold"), 
        axis.text = element_text(size = 12), 
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5))






#### FROM DENVER -- I think below should be removed. I think subsetting model coefs, 
# pasting and adding annotations adds a lot more function and arguments that will create confusion


# VIEWING THE DATA
# There is a couple of different ways to explore this data
head(pen) # gives you the first six rows of the dataset
tail(pen) # gives you the last six rows of the dataset
names(pen) # gives you the names of the variables in your dataset

# RUNNING THE REGRESSION
# For this example, we are going to run try to estimate body mass from bill length 
regression1 <- lm(body_mass_g ~ bill_length_mm, data = pen) # lm is the built-in 
# function for linear model, the first argument defines 
# the response variable, the second argument defines 
# the explanatory variable, and the data argument
# tells what data is going to be used 

# SUMMARY OF THE REGRESSION
# This will give you your intercept, slope, p-value, along with some additional stats
summary(regression1) 

# PLOTTING
# This will plot a scatterplot on the plots pane
plot(pen$bill_length_mm, pen$body_mass_g,
     main = "Bill Length vs Body Mass",
     xlab = "Bill Length (mm)", ylab = "Body Mass (g)",
     pch = 19, col = "black")

# Add the regression line to the plot
abline(regression1, col = "red", lwd = 2) # lwd defines the line width

# Add the regression equation and R-squared to the plot
# We will extract the slope and intercept from the regression using the functions below
intercept1 <- coef(regression1)[1]
slope1 <- coef(regression1)[2]
r_squared1 <- summary(regression1)$r.squared

# Create a label with the equation and R-squared (this is the label that is going on the plot)
equation1 <- paste("y = ", round(intercept1, 2), " + ", round(slope1, 2), "x", sep = "")
r2_label1 <- paste("R² = ", round(r_squared1, 2), sep = "")

# Now we can plot this on the scattterplot using x and y coordinates
text(x = 35, y = 6000, labels = equation1, col = "black", cex = 0.8) # x and y can be adjusted
text(x = 35, y = 5800, labels = r2_label1, col = "black", cex = 0.8)


# Now let's try another regression model using flipper length to predict body mass
regression2 <- lm(body_mass_g ~ flipper_length_mm, data = pen)

# Summary of the second model
summary(regression2)

# Extract the equation and R-squared value for the second model
intercept2 <- coef(regression2)[1]
slope2 <- coef(regression2)[2]
r_squared2 <- summary(regression2)$r.squared

# Create a label with the equation and R-squared (this is the label that is going on the plot)
equation2 <- paste("y = ", round(intercept2, 2), " + ", round(slope2, 2), "x", sep = "")
r2_label2 <- paste("R² = ", round(r_squared2, 2), sep = "")

# For this plot, we are going to use ggplot instead of base R
ggplot(pen, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(color = "darkgreen", size = 3) +  # scatter plot of points
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 1.5) +  # add regression line
  labs(title = "Flipper Length vs Body Mass",
       x = "Flipper Length (mm)", y = "Body Mass (g)") + 
  theme_minimal() +  # minimal theme for a cleaner look
  # Add the regression equation and R-squared to the plot
  annotate("text", x = 190, y = 6000, label = equation2, color = "black", size = 4) + 
  annotate("text", x = 190, y = 5800, label = r2_label2, color = "black", size = 4)

# ggplot gives you an array of possibilities when plotting, you can change the points 
# size, shape, colors, the plot theme. Let's try something different. 


# Create the second plot (Flipper Length vs Body Mass) with different customizations
ggplot(pen, aes(x = flipper_length_mm, y = body_mass_g)) +
  # Change the point shape to a triangle and set the color to blue
  geom_point(shape = 17, color = "steelblue", size = 4) +  
  # Add the regression line with a thicker, dashed orange line
  geom_smooth(method = "lm", se = FALSE, color = "orange", size = 1.5, linetype = "dashed") +  
  # Customize the labels and title with a more bold font
  labs(title = "Flipper Length vs Body Mass", 
       x = "Flipper Length (mm)", y = "Body Mass (g)") + 
  theme_classic() +  # Change to classic theme (there's other possibilities as well)
  # Customize the font and size of the axis labels and title
  theme(axis.title = element_text(size = 14, face = "bold"), 
        axis.text = element_text(size = 12), 
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) + 
  # Add the regression equation and R-squared value
  annotate("text", x = 190, y = 6000, label = equation2, color = "black", size = 4) + 
  annotate("text", x = 190, y = 5800, label = r2_label2, color = "black", size = 4)



