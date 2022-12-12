library(tidyverse)
library(ggplot2) 

# data import and cleaning
df <- read.csv('data/world_population.csv')
colnames(df) <- c('Rank', 'CCA3', 'Country','Capital','Continent','Population_2022',
                  'Population_2020','Population_2015','Population_2010',
                  'Population_2000','Population_1990','Population_1980',
                  'Population_1970','Area', 'Density', 'Growth_Rate',
                  'World_Population_Percentage')

# df1 and plot1
df1 <- df
colnames(df1) <- c('Rank', 'CCA3', 'Country','Capital','Continent','2022',
                   '2020','2015','2010',
                   '2000','1990','1980',
                   '1970','Area', 'Density', 'Growth_Rate',
                   'World_Population_Percentage')

df1 <- df1 %>% select(c('Country','2022','2020',
                        '2015','2010',
                        '2000','1990',
                        '1980','1970')) %>% pivot_longer(cols=c('2022','2020',
                                                                '2015','2010',
                                                                '2000','1990',
                                                                '1980','1970'),
                                                         names_to = 'Year', values_to = 'Population') %>%
  arrange(desc(Population)) %>%
  group_by(Year) %>%
  slice(1:5)

ggplot(df1, aes(fill=Country, y=Population, x=Year)) + 
  geom_bar(position='dodge', stat='identity') +
  ggtitle ('Top 5 Populous Countries from 1970 to 2022')

# df2 & plot2
df2 <- df
colnames(df2) <- c('Rank', 'CCA3', 'Country','Capital','Continent','2022',
                   '2020','2015','2010',
                   '2000','1990','1980',
                   '1970','Area', 'Density', 'Growth_Rate',
                   'World_Population_Percentage')

df2 <- df2 %>% select(c('Country','2022','2020',
                        '2015','2010',
                        '2000','1990',
                        '1980','1970')) %>% pivot_longer(cols=c('2022','2020',
                                                                '2015','2010',
                                                                '2000','1990',
                                                                '1980','1970'),
                                                         names_to = 'Year', values_to = 'Population') %>%
  arrange(Population) %>%
  group_by(Year) %>%
  slice(1:5)

ggplot(df2, aes(fill=Country, y=Population, x=Year)) + 
  geom_bar(position='dodge', stat='identity') +
  ggtitle ('Top 5 Least Populous Countries from 1970 to 2022')

# df3 & plot3
df3 <- df1 %>% filter(Year == 2022)
df_tmp <- df %>% select(c('Country','Growth_Rate'))
df3 <- merge(df3, df_tmp, by='Country', all.x=TRUE) %>% 
  select(c('Country','Population','Growth_Rate')) %>% arrange(desc(Population))

ggplot(df3, aes(x=reorder(Country, +desc(Population))))  + 
  geom_bar(aes(y=Population*0.000000001, group=1),stat="identity", fill="cyan") +
  geom_line(aes(y=(Growth_Rate-1)*100, group=1),stat="identity",color="black",size=1)+
  scale_y_continuous(name = 'Population in Billion', sec.axis=sec_axis(trans = ~.*1, name="Increase Growth Rate in percent")) +
  labs(title= "Population vs. Growth Rate Top 5 Populous Countries", x = 'Country') +
  geom_point(aes(y=(Growth_Rate-1)*100, group=1), col='red') +
  geom_text(x=c(1,2,3,4,5), y = c(0.1, 0.8, 0.5, 0.8, 1.7), label=c(0, 0.68, 0.38, 0.64, 1.91))

# df4 & plot4
df4 <- df %>% arrange(Density) %>% slice(1:5) %>% 
  select(c("Country","Population_2022", "Density"))

ggplot(df4, aes(x=reorder(Country, +Population_2022)))  + 
  geom_bar(aes(y=Population_2022*0.000001, group=1),stat="identity", fill="blue") +
  geom_line(aes(y=Density, group=1),stat="identity",color="black",size=1)+
  scale_y_continuous(name = 'Population in Million', sec.axis=sec_axis(trans = ~.*1, name="Density (per km^2)")) +
  labs(title= "5 Least Density Countries", x= 'Country') +
  geom_point(aes(y=Density, group=1), col='red') +
  geom_text(x=c(1,2,3,4,5), y = c(0.51,0.23,2.36,3.31,2.37), label=c(0.31,0.03,2.16,3.11,2.17))

# df5 & plot5
df5 <- df %>% arrange(Density) %>% slice(1:5) %>% 
  select(c("Country", "Density", "Growth_Rate"))

ggplot(df5, aes(x=reorder(Country, +Density)))  + 
  geom_bar(aes(y=Density, group=1),stat="identity", fill="orange") +
  geom_line(aes(y=(Growth_Rate-1)*100, group=1),stat="identity",color="black",size=1)+
  scale_y_continuous(name = 'Density (per km^2)', sec.axis=sec_axis(trans = ~.*1, name="Increase Growth Rate in percent")) +
  labs(title= "5 Least Density Countries's Growth Rate", x = "Country") +
  geom_point(aes(y=(Growth_Rate-1)*100, group=1), col='red') +
  geom_text(x=c(1,2,3,4,5), y = c(0.6,0.6,2,1.7,1.7), label=c(0.4,0.43,1.81,1.51,1.46))

# df6 & plot6
df6 <- df

df6 <- df6 %>% select(c('Continent','Population_2022','Population_2020',
                        'Population_2015','Population_2010',
                        'Population_2000','Population_1990',
                        'Population_1980','Population_1970')) %>% group_by(Continent) %>%
  summarise(sum_2022 = sum(Population_2022),
            sum_2020 = sum(Population_2020),
            sum_2015 = sum(Population_2015),
            sum_2010 = sum(Population_2010),
            sum_2000 = sum(Population_2000),
            sum_1990 = sum(Population_1990),
            sum_1980 = sum(Population_1980),
            sum_1970 = sum(Population_1970))

colnames(df6) <- c('Continent', '2022',
                   '2020','2015','2010',
                   '2000','1990','1980',
                   '1970')

df6 <- df6 %>% pivot_longer(cols=c('2022','2020',
                                   '2015','2010',
                                   '2000','1990',
                                   '1980','1970'),
                            names_to = 'Year', values_to = 'Population') %>%
  arrange(Continent, Year)
df6[['Year']] <- as.numeric(df6[['Year']])

ggplot(data = df6, aes(x=Year)) + 
  geom_line(aes(y=Population / 1000000000, colour=Continent)) + 
  geom_point(aes(y=Population / 1000000000, colour=Continent)) +
  labs(title= "Population distribution by Continent", y = "Population in Billion")

# df7 & plot7
df7 <- df

df7 <- df7 %>% select(c('Continent','Population_2022','Population_2020',
                        'Population_2015','Population_2010',
                        'Population_2000','Population_1990',
                        'Population_1980','Population_1970', 'Area')) %>% group_by(Continent) %>%
  summarise(density_2022 = sum(Population_2022) / sum(Area),
            density_2020 = sum(Population_2020) / sum(Area),
            density_2015 = sum(Population_2015) / sum(Area),
            density_2010 = sum(Population_2010) / sum(Area),
            density_2000 = sum(Population_2000) / sum(Area),
            density_1990 = sum(Population_1990) / sum(Area),
            density_1980 = sum(Population_1980) / sum(Area),
            density_1970 = sum(Population_1970) / sum(Area))

colnames(df7) <- c('Continent', '2022',
                   '2020','2015','2010',
                   '2000','1990','1980',
                   '1970')

df7 <- df7 %>% pivot_longer(cols=c('2022','2020',
                                   '2015','2010',
                                   '2000','1990',
                                   '1980','1970'),
                            names_to = 'Year', values_to = 'Density') %>%
  arrange(Continent, Year)
df7[['Year']] <- as.numeric(df7[['Year']])

ggplot(data = df7, aes(x=Year)) + 
  geom_line(aes(y=Density, colour=Continent)) + 
  geom_point(aes(y=Density, colour=Continent)) +
  labs(title= "Population Density by Continent", y = "Density per km^2")

# df8 & plot8

df8 <- df %>% arrange(desc(Area)) %>% slice(1:5) %>% select(c("Country", "Area"))

ggplot(df8, aes(x=reorder(Country, +desc(Area))))  + 
  geom_bar(aes(y=Area/1000000, group=1),stat="identity", fill="purple") +
  geom_text(x=c(1,2,3,4,5), y = c(16.098,10.985,10.707,10.373,9.516), label=c(17.098,9.985,9.707,9.373,8.516)) +
  labs(title= "Five Largest Countries", y = "Area in Million km^2", 
       x = 'Country')

# df9 & plot9
df9 <- df %>% arrange(Area) %>% slice(1:5) %>% select(c("Country", "Area"))

ggplot(df9, aes(x=reorder(Country, +Area)))  + 
  geom_bar(aes(y=Area, group=1),stat="identity", fill="brown") +
  geom_text(x=c(1,2,3,4,5), y = c(2,3,7,13,20), label=c(1,2,6,12,21)) +
  labs(title= "Five Smallest Countries", y = "Area in Million km^2", 
       x = 'Country')



