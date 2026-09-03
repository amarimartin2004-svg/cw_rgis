library(tidyverse)
iris


# point figure ------------------------------------------------------------
## shortcut for pipe Shift+ M for pipe

iris %>% 
  ggplot(
    aes(x = Sepal.Length,
             y = Sepal.Width))+
  geom_point()

 ## aes(..., color, Collumname to color data)
iris %>% 
  ggplot(
    aes(x = Sepal.Length,
       y = Sepal.Width,
       color = Species)
  ) + 
  geom_point ()


iris %>% 
  ggplot(
    aes( x= Sepal.Length,
         y = Sepal.Width)
  ) +
  geom_point( color =" light pink")
    

# line figure ----------------------------------------------------------
df_x <- tibble( x = 1:50,
                y = 2 * x)

df_x <- tibble(x = 1:50,
               y = 2 * x)
df_x %>% 
  ggplot(
    aes(x = x,
        y = y)
  ) +
  geom_line()


# histogram ---------------------------------------------------------------


iris %>% 
  ggplot(
    aes(x = Sepal.Length)
  ) + 
  geom_histogram ()


# boxplot -----------------------------------------------------------------

iris %>% 
  ggplot(
    aes( x = Species,
         y = Sepal.Length)
  ) +
  geom_boxplot()

## change color
iris %>% 
  ggplot(
    aes( x = Species,
         y = Sepal.Length,
         color = Species)
  ) +
  geom_boxplot ()
  
##change inside box
iris %>% 
  ggplot(
    aes( x = Species,
         y = Sepal.Length,
         fill = Species)
  ) +
  geom_boxplot()



# exercise ----------------------------------------------------------------

#Q1 using "iris" data, identify the longest Sepal.Length uusing arrange() function 

iris %>% 
  arrange(desc(Sepal.Length))

#Q2 Using iris data, filter individuals with Sepal width greater than 3.0
# - use filter
iris %>% 
  filter(Sepal.Width > 3.0)        

#Q3 using iris data, select the column "petal.Length" and "Petal.Width",
# and arrange the order of rows by "Petal.Length" (desxending)
#Assign the result to object "df_petal"

df_petal <- iris %>% 
  select(Petal.Length, Petal.Width) %>% 
  arrange (desc(Petal.Length))
df_petal

#Q4 Calculate mean Sepal.Width by Species: assign the result to "df_mean"
# - Hint group_by () and summarize ()
df_mean <- iris %>% 
  group_by(Species) %>% 
  summarize (mean = mean(Sepal.Width))
df_mean

#Q5 Create a point figure of Petal.Width (y - axis ) and Sepal.Width ( x- axis)
# with color distinguishing spcies 

iris %>% 
  ggplot(aes(x = Sepal.Width,
             y = Petal.Width,
             color = Species)) +
  geom_point()