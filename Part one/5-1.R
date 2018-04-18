library(tidyverse)
library(stringr)

#Step 1: Import the data of 30 years from website and combine to one set

a <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46035h"
b <- ".txt.gz&dir=data/historical/stdmet/"
years <- c(1988:2017)

# no data for 2013, so we delete 2013 from the years list
years <- years[-26]
urls <- str_c(a, years, b, sep = "")
filenames <- str_c("mr", years, sep = "")

N <- length(urls)


for (i in 1:N){
  suppressMessages(assign(filenames[i], read_table(urls[i], col_names = TRUE)))
  file <- get(filenames[i])
  colnames(file)[1] <-"YYYY"
  if (i < 18) {
    file <- file %>% mutate(mm = "00")
  }
  file <- file %>% select(YYYY, MM, DD, hh, mm, ATMP, WTMP)
  if (i >= 20) {
    file <- file[-1,]
  }
  if (i >= 26) {
    file[1] <- i + 1988
  }
  else{
    file[1] <- i + 1987
  }
  if(i == 1){
    total <- file
  }
  else{
    total <- rbind.data.frame(total, file)
  }
}

 

# Step 2: Clean the data and select the data recorded at noon

noon <- total %>% filter((hh == "11" & mm == "50") | (hh == "12" & mm == "00"))


noon$ATMP <- as.numeric(noon$ATMP)
noon$WTMP <- as.numeric(noon$WTMP)


noon$ATMP <- ifelse(noon$ATMP > 90, NA, noon$ATMP)
noon$WTMP <- ifelse(noon$WTMP > 90, NA, noon$WTMP)


noon <- unite(noon, Date, YYYY, MM, DD, sep = "-")
noon$Date <-as.Date(noon$Date)

# Step 3: Data Visualization and Answering to Questions

# Time Series for Air Temperature
noon %>% ggplot(aes(Date, ATMP)) +
  geom_line(na.rm = TRUE, col = "red") +
  labs(title = "Time Series of Air Temperature (noon)",
       subtitle = "Data obtained from the National Data Buoy Center",
       y = "Temperature (Celcius)",
       x = "Year")



# Time Series for Sea Temperature
noon %>% ggplot(aes(Date, WTMP)) +
  geom_line(na.rm = TRUE, col = "blue") +
  labs(title = "Time Series of Sea Temperature (noon)",
       subtitle = "Data obtained from the National Data Buoy Center",
       y = "Temperature (Celcius)",
       x = "Year")

# Put them together
ggplot(noon, aes(Date)) + 
  geom_line(aes(y = ATMP, col = "ATMP")) + 
  geom_line(aes(y = WTMP, col = "WTMP")) +
  scale_colour_manual(values=c("red", "blue")) +
  labs(x = "Year", y = "Temperature (Celcius Degree)",
       title = "Time Series of Air & Sea Temperature (Noon Data)",
       subtitle = "Data obtained from the National Data Buoy Center")

# we think air temp and sea temp are correlated according to the plot of them

# confirm the assumption that ATMP and WTMP have a strong correlation
ggplot(noon) + 
  geom_point(mapping = aes(x = ATMP, y = WTMP)) +
  labs(x = "Noon Air Temp (Celcius)", 
       y = "Noon Sea Temp (Celcius)",
       title = "Scatter Plot to See the Correlation between ATMP and WTMP")

ggplot(noon) + 
  geom_smooth(mapping = aes(x = ATMP, y = WTMP)) +
  labs(x = "Noon Air Temp (Celcius)", 
       y = "Noon Sea Temp (Celcius)",
       title = "Smooth Line to See the Correlation between ATMP and WTMP")


# Pearson Correlation Test
cor.test(noon$ATMP, noon$WTMP, method = "pearson")
# we find ATMP and WTMP have coefficient of correlation of 0.8774953
# Therefore, we prove that ATMP and WTMP do have strong correlation


# do the t-test between year 1988 and 2017 to see the mean air temp changed over 30 years
x <- noon %>% separate(Date, into = c("YYYY", "MM", "DD"), sep = "-") %>% 
  filter(YYYY == 1988)
y <- noon %>% separate(Date, into = c("YYYY", "MM", "DD"), sep = "-") %>% 
  filter(YYYY == 2017)

t.test(x$ATMP, y$ATMP)
# Because the p-value = 5.153e-13 < 0.01, we reject the null hypothesis that the means of 
# ATMP in 1988 and in 2017 are equal, so the air temp changed over 30 years.
# means in two years:2.330748  4.645179 

# To see whether the mean sea temp changed over 30 years, we need to do t-test
# Creating datasets which are only for 1988 and 2017
t.test(x$WTMP, y$WTMP)
# Because the p-value = 9.035e-13 < 0.01, we reject the null hypothesis that the means of 
# WTMP in 1988 and in 2017 are equal,so the sea temp changed over 30 years.
# means in two years: 4.572981  6.141525 

## By using the total dataset for each year, we do t-test for the mean of ATMP again
mr1988 <- mr1988 %>% mutate(ATMP = ifelse(ATMP > 90, NA, ATMP)) %>% 
  mutate(ATMP = as.numeric(ATMP))
mr2017 <- mr2017 %>% mutate(ATMP = ifelse(ATMP > 90, NA, ATMP)) %>% 
  mutate(ATMP = as.numeric(ATMP))

x1 <- mr1988$ATMP
y1 <- mr2017$ATMP

t.test(x1, y1, na.rm=TRUE)
# Because the p-value < 2.2e-16 < 0.01, we reject the null hypothesis that the means of 
# ATMP in 1988 and in 2017 are equal,so air temp changed over 30 years.
# means in two years:  2.438602  4.714333

## By using the total dataset for each year, we do t-test for the mean of WTMP again
mr1988 <- mr1988 %>% mutate(WTMP = ifelse(WTMP > 90, NA, WTMP)) %>% 
  mutate(WTMP = as.numeric(WTMP))
mr2017 <- mr2017 %>% mutate(WTMP = ifelse(WTMP > 90, NA, WTMP)) %>% 
  mutate(WTMP = as.numeric(WTMP))

x2 <- mr1988$WTMP
y2 <- mr2017$WTMP

t.test(x2, y2, na.rm=TRUE)
# Because the p-value < 2.2e-16 < 0.01, we reject the null hypothesis that the means of 
# WTMP in 1988 and in 2017 are equal,so sea temp changed over 30 years.
# means in two years:  4.608660  6.156853

