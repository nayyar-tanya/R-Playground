library(dplyr)
library(ggplot2)
library(moderndive)
library(tidyr)
library(broom)
setwd("C:/Users/TEMP/Documents/")
#load real estate data into a dataframe
taiwan_re_data <- read.csv(file="RealEstateData.csv")
# view the top rows of the dataframe
head(taiwan_re_data)
#copy data to sample df to add bucket column
taiwan_real_estate <- taiwan_re_data
#Divide and assign the age of the house, in years, into 3 groups.buckets
taiwan_real_estate$house_age_years_grp <- case_when(taiwan_re_data$house_age_years>=0 & taiwan_re_data$house_age_years < 15 ~"0 to 15"
                                 ,taiwan_re_data$house_age_years>=15 & taiwan_re_data$house_age_years < 30 ~"15 to 30"
                                 ,taiwan_re_data$house_age_years>=30 & taiwan_re_data$house_age_years <45 ~"30 to 45")
#renaming the columns
taiwan_real_estate <- taiwan_real_estate %>% 
  rename(
    orig_house_age_years = house_age_years,
    house_age_years = house_age_years_grp
  )

# Fit a linear regr'n of price_twd_msq vs. n_convenience
mdl_price_vs_conv <- lm(price_twd_msq~n_convenience,data=taiwan_real_estate)

# See the result
mdl_price_vs_conv

# Fit a linear regr'n of price_twd_msq vs. house_age_years, no intercept
mdl_price_vs_age <- lm(price_twd_msq~house_age_years+0,data=taiwan_real_estate)

# See the result
mdl_price_vs_age

# Fit a linear regr'n of price_twd_msq vs. n_convenience 
# plus house_age_years, no intercept
mdl_price_vs_both <- lm(price_twd_msq~house_age_years+n_convenience+0,data=taiwan_real_estate)

# See the result
mdl_price_vs_both

#From the results we can see that n_convenence coefficient specifies that for each additional 
#nearby convenience store, the expected house price, in TWD per square meter, increases by 2.616.

#From the results we can see that "0 to 15 years" coefficient specifies that 
#for a house aged 0 to 15 years with zero nearby convenience stores, 
#the expected house price is 31.111 TWD per square meter.


# Using taiwan_real_estate, plot price_twd_msq vs. n_convenience
ggplot(taiwan_real_estate,aes(y=price_twd_msq,x=n_convenience)) +
  # Add a point layer
  geom_point() +
  # Add a smooth trend line using linear regr'n, no ribbon
  geom_smooth(method="lm",se=FALSE)


# Using taiwan_real_estate, plot price_twd_msq vs. house_age_years
ggplot(taiwan_real_estate,aes(y=price_twd_msq,x=house_age_years)) +
  # Add a box plot layer
  geom_boxplot()

#using geom_parallel_slopes to plot to show predictions with numeric and explanatory variables
# Using taiwan_real_estate, plot price_twd_msq vs. n_convenience
# colored by house_age_years
ggplot(taiwan_real_estate,aes(color=house_age_years,y=price_twd_msq,x=n_convenience)) +
  # Add a point layer
  geom_point() +
  # Add parallel slopes, no ribbon
  geom_parallel_slopes(se=FALSE)

# Make a grid of explanatory data
explanatory_data <- expand_grid(
  # Set n_convenience to zero to ten
  n_convenience = seq(0,10,1),
  # Set house_age_years to the unique values of that variable
  house_age_years = unique(taiwan_real_estate$house_age_years)
)

# See the result
explanatory_data

# From previous step
explanatory_data <- expand_grid(
  n_convenience = 0:10,
  house_age_years = unique(taiwan_real_estate$house_age_years)
)

# Add predictions to the data frame
prediction_data <- explanatory_data %>% 
  mutate(price_twd_msq = predict(mdl_price_vs_both,explanatory_data))

# See the result
prediction_data

# From previous steps
explanatory_data <- expand_grid(
  n_convenience = 0:10,
  house_age_years = unique(taiwan_real_estate$house_age_years)
)
prediction_data <- explanatory_data %>% 
  mutate(
    price_twd_msq = predict(mdl_price_vs_both, explanatory_data)
  )

taiwan_real_estate %>% 
  ggplot(aes(n_convenience, price_twd_msq, color = house_age_years)) +
  geom_point() +
  geom_parallel_slopes(se = FALSE) +
  # Add points using prediction_data, with size 5 and shape 15
  geom_point(
    data = prediction_data,size=5,shape=15
  )


# Get the coefficients from mdl_price_vs_both
coeffs <- coefficients(mdl_price_vs_both)

# Extract the slope coefficient
slope <- coeffs[1]

# Extract the intercept coefficient for 0 to 15
intercept_0_15 <- coeffs[2]

# Extract the intercept coefficient for 15 to 30
intercept_15_30 <- coeffs[3]

# Extract the intercept coefficient for 30 to 45
intercept_30_45 <- coeffs[4]



# From previous step
coeffs <- coefficients(mdl_price_vs_both)
slope <- coeffs[1]
intercept_0_15 <- coeffs[2]
intercept_15_30 <- coeffs[3]
intercept_30_45 <- coeffs[4]

prediction_data <- explanatory_data %>% 
  mutate(
    # Consider the 3 cases to choose the intercept
    intercept = case_when(
      house_age_years == "0 to 15"~
        intercept_0_15,
      house_age_years == "15 to 30"~
        intercept_15_30, 
      house_age_years == "30 to 45"~
        intercept_30_45),
    
    # Manually calculate the predictions
    price_twd_msq = intercept +slope*n_convenience
  )

# See the results
prediction_data

##Comparing the coefficients of determination for all the models
mdl_price_vs_conv %>% 
  # Get the model-level coefficients
  glance() %>% 
  # Select the coeffs of determination
  select(r.squared,adj.r.squared)

# Get the coeffs of determination for mdl_price_vs_age
mdl_price_vs_age %>% 
  glance() %>% 
  # Select the coeffs of determination
  select(r.squared,adj.r.squared)

# Get the coeffs of determination for mdl_price_vs_both
mdl_price_vs_both %>% 
  glance() %>% 
  # Select the coeffs of determination
  select(r.squared,adj.r.squared)

#comparing residual standard errors for all three models
mdl_price_vs_conv %>% 
  # Get the model-level coefficients
  glance() %>% 
  # Pull out the RSE
  pull(sigma)

# Get the RSE for mdl_price_vs_age
mdl_price_vs_age %>% 
  glance() %>% 
  # Pull out the RSE
  pull(sigma)


# Get the RSE for mdl_price_vs_both
mdl_price_vs_both %>% 
  glance() %>% 
  # Pull out the RSE
  pull(sigma)


# Filter for rows where house age is 0 to 15 years
taiwan_0_to_15 <- taiwan_real_estate%>%filter(house_age_years=="0 to 15")


# Filter for rows where house age is 15 to 30 years
taiwan_15_to_30 <- taiwan_real_estate%>%filter(house_age_years=="15 to 30")


# Filter for rows where house age is 30 to 45 years
taiwan_30_to_45 <- taiwan_real_estate%>%filter(house_age_years=="30 to 45")



# From previous step
taiwan_0_to_15 <- taiwan_real_estate %>%
  filter(house_age_years == "0 to 15")
taiwan_15_to_30 <- taiwan_real_estate %>%
  filter(house_age_years == "15 to 30")
taiwan_30_to_45 <- taiwan_real_estate %>%
  filter(house_age_years == "30 to 45")

# Model price vs. no. convenience stores using 0 to 15 data
mdl_0_to_15 <- lm(price_twd_msq~n_convenience,data=taiwan_0_to_15)

# Model price vs. no. convenience stores using 15 to 30 data
mdl_15_to_30 <- lm(price_twd_msq~n_convenience,data=taiwan_15_to_30)

# Model price vs. no. convenience stores using 30 to 45 data
mdl_30_to_45 <- lm(price_twd_msq~n_convenience,data=taiwan_30_to_45)

# See the results
mdl_0_to_15
mdl_15_to_30
mdl_30_to_45

#predicting from multiple models
# Create a tibble of explanatory data, setting
# no. of conv stores to 0 to 10
explanatory_data <- tibble(
  n_convenience = seq(0,10,1)
)

# From previous step
explanatory_data <- tibble(
  n_convenience = 0:10
)

# Add column of predictions using "0 to 15" model and explanatory data 
prediction_data_0_to_15 <- explanatory_data %>% 
  mutate(price_twd_msq = predict(mdl_0_to_15,explanatory_data))

# Same again, with "15 to 30"
prediction_data_15_to_30 <- explanatory_data %>% 
  mutate(price_twd_msq = predict(mdl_15_to_30,explanatory_data))

# Same again, with "30 to 45"
prediction_data_30_to_45 <- explanatory_data %>% 
  mutate(price_twd_msq = predict(mdl_30_to_45,explanatory_data))


##visulaizing multiple models
# Using taiwan_real_estate, plot price vs. no. of conv. stores
# colored by house age
ggplot(taiwan_real_estate,aes(y=price_twd_msq,x=n_convenience,color=house_age_years)) +
  # Make it a scatter plot
  geom_point() +
  # Add smooth linear regression trend lines, no ribbon
  geom_smooth(method="lm",se=FALSE)

taiwan_real_estate$house_age_years <- factor(taiwan_real_estate$house_age_years)
levels(taiwan_real_estate$house_age_years)

# Extend the plot to include prediction points
ggplot(taiwan_real_estate, aes(n_convenience, price_twd_msq,color=house_age_years),inherit.aes = FALSE )+
  geom_point(aes(n_convenience, price_twd_msq)) +
  geom_smooth(method = "lm", se = FALSE) +
  # Add points using prediction_data_0_to_15, colored red, size 3, shape 15
  geom_point(data = prediction_data_0_to_15,aes(n_convenience, price_twd_msq), size = 3, shape = 15) +
  # Add points using prediction_data_15_to_30, colored green, size 3, shape 15
  geom_point(data = prediction_data_15_to_30,aes(n_convenience, price_twd_msq), size = 3, shape = 15) +
  # Add points using prediction_data_30_to_45, colored blue, size 3, shape 15
  geom_point(data = prediction_data_30_to_45,aes(n_convenience, price_twd_msq), size = 3, shape = 15)


#Assesing model performance
# Get the coeff. of determination for mdl_all_ages
mdl_all_ages%>%glance()%>%pull(r.squared)
mdl_0_to_15%>%glance()%>%pull(r.squared)
mdl_15_to_30%>%glance()%>%pull(r.squared)
mdl_30_to_45%>%glance()%>%pull(r.squared)


# Get the RSE for mdl_all
mdl_all_ages%>%glance()%>%pull(sigma)

# Get the RSE for mdl_0_to_15
mdl_0_to_15%>%glance()%>%pull(sigma)

# Get the RSE for mdl_15_to_30
mdl_15_to_30%>%glance()%>%pull(sigma)

# Get the RSE for mdl_30_to_45
mdl_30_to_45%>%glance()%>%pull(sigma)
