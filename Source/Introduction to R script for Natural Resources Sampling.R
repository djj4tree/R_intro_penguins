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


# ANOVA to see if Adelie male penguins differ in weight by island

mod <- lm(madel$body_mass_g~madel$island)
anova(mod)

ggplot(madel, aes(x = island, y = body_mass_g))+
  geom_boxplot()+
  labs(x = 'Island', y = "Body mass (g)")+
  theme_bw()

