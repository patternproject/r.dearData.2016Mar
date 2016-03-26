## loading libraries ####
library(dplyr)
library(ggplot2)
library(stringr)

## reading input files ####
df = read.csv(file="/mnt/r.dearData.2016Mar/data/EarningsVsRatings.csv", stringsAsFactors = FALSE)
df$Year = as.factor(df$Year)
df$Claim = as.factor(df$Claim)
names(df)[4] = "Earnings"
names(df)[5] = "Ratings"

## fixing the earnings data ####
df$Earnings %>% 
  str_replace_all("[[:punct:]]|\\$","") %>% # removing commas and dollar sign 
  as.numeric() -> # convert to numeric
  df$Earnings # assign to Earnings

df$Earnings = round(df$Earnings/1000000)

## scatterplot with Earnings and Ratings ####
p1 <- df %>%
ggplot(aes(x=Ratings, y=Earnings)) +
  #geom_point(shape=3) +    # Use hollow circles
  geom_point(aes(shape=Year)) + 
  geom_smooth(method=lm)   # Add linear regression line 

p1

## scatter plot conditioned on movie category ####
# Extend the regression lines beyond the domain of the data
p2 <- df %>%
ggplot(aes(x=Ratings, y=Earnings, color=Claim)) + 
  geom_point(shape=1) +
  #scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              #se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE, # Extend regression lines
              linetype="dashed",
              #color="darkred"
              aes(fill=Claim),alpha=0.1) 

p2

## Making p2 pretty ####
p3 <- p2 +
  ggtitle(expression(paste("Linear Regression (95% CI) ", 
                           italic("by Movie Type")))) 
  
p4 <- p3 +   
  theme_bw() + 
  theme(                              
    axis.title.x = element_text(face="bold", color="black", size=12),
    axis.title.y = element_text(face="bold", color="black", size=12),
    plot.title = element_text(face="bold", color = "black", size=14),
    # Position legend in graph, where x,y is 0,0 (bottom left) to 1,1 (top right)
    legend.position=c(0.95,0.25),
    legend.justification=c(1,1)) +
  labs(x="Meta Critic Ratings", 
       y = "World Wide Box Office Earnings") +
  theme(legend.title=element_blank()) +
  theme(legend.justification=c(1,1)) + 
  theme(axis.ticks.x=element_blank()) +
  theme(axis.ticks.y=element_blank()) 
  

p4


# Use brewer color palettes ####
# does not seem to make a difference
#p4 = p3+scale_color_brewer(palette="Dark2")

# adding geometric rug
p5 = p4 + geom_rug()

# fixing the colors ####
# https://color.adobe.com/create/color-wheel/
p6 =  p5 + 
  #scale_color_manual(values = c("aquamarine3", "midnightblue")) +
  #scale_fill_manual(values = c("aquamarine3", "midnightblue")) 
  scale_color_manual(values = c("#7E6E00", "#14007E")) +
  scale_fill_manual(values = c("#7E6E00", "#14007E"))
  

#+
#  scale_shape_manual(values=c(19, 22))

p6

