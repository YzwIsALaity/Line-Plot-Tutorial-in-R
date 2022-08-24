# packages
library(ggplot2)
library(tidyr)
library(dplyr)
library(knitr)
library(gridExtra)

# dataset
Dt <- read.csv('Line Plot.csv')
kable(head(Dt))

# Version 0.0
ggplot(Dt, aes(x = Ct, y = Biomarker, col = Virus)) + 
  geom_line() + 
  theme_bw() +                                                      # dark-on-light theme
  theme(panel.border = element_blank(),                             # these four are for the background and grid
        panel.background = element_blank(),                    
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line.x = element_line(),                               # these two are for the axis line
        axis.line.y = element_line(),
        axis.text.x = element_text(colour = "black", size = 11),    # there two are for texts in axes
        axis.text.y = element_text(colour = "black", size = 11),
        axis.ticks.x = element_line(),                              # these two are for ticks in axes
        axis.ticks.y = element_line(),
        axis.title.x = element_text(colour = "black", size = 11, face = 'bold', vjust = -1),                              
        axis.title.y = element_text(colour = "black", size = 11, face = 'bold'),
        legend.title = element_text(colour = "black", size = 11, face = 'bold')) 

# Version 1.0
ggplot(Dt, aes(x = Ct, y = Biomarker)) + 
  geom_line() + 
  facet_grid(cols = vars(Virus), scales = 'fixed') +
  theme_bw() +                                                      # dark-on-light theme
  theme(panel.border = element_blank(),                             # these four are for the background and grid
        panel.background = element_blank(),                    
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line.x = element_line(),                               # these two are for the axis line
        axis.line.y = element_line(),
        axis.text.x = element_text(colour = "black", size = 11),    # there two are for texts in axes
        axis.text.y = element_text(colour = "black", size = 11),
        axis.ticks.x = element_line(),                              # these two are for ticks in axes
        axis.ticks.y = element_line(),
        axis.title.x = element_text(colour = "black", size = 11, face = 'bold', vjust = -1),                              
        axis.title.y = element_text(colour = "black", size = 11, face = 'bold'),
        legend.title = element_text(colour = "black", size = 11, face = 'bold'),
        strip.text = element_text(size = 15),                       # set up size of title for each figure with 15
        strip.background = element_blank())                         # remove the background of titles for figures

# Version 2.0
ggplot(Dt, aes(x = Ct, y = Biomarker)) + 
  geom_line() + 
  facet_grid(cols = vars(Virus), rows = vars(Severity),             # stratified by Virus and Severity
             scales = 'fixed') +
  theme_bw() +                                                      # dark-on-light theme
  theme(panel.border = element_blank(),                             # these four are for the background and grid
        panel.background = element_blank(),                    
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line.x = element_line(),                               # these two are for the axis line
        axis.line.y = element_line(),
        axis.text.x = element_text(colour = "black", size = 11),    # there two are for texts in axes
        axis.text.y = element_text(colour = "black", size = 11),
        axis.ticks.x = element_line(),                              # these two are for ticks in axes
        axis.ticks.y = element_line(),
        axis.title.x = element_text(colour = "black", size = 11, face = 'bold', vjust = -1),                              
        axis.title.y = element_text(colour = "black", size = 11, face = 'bold'),
        legend.title = element_text(colour = "black", size = 11, face = 'bold'),
        strip.text = element_text(size = 13, face = 'bold'),        # set up size of title for each figure with 15
        strip.background = element_blank())                         # remove the background of titles for figures

# Version 3.0
ggplot(Dt, aes(x = Ct, y = Biomarker, col = Severity, fill = Severity)) + # set up colors/fill for Severity
  geom_smooth(method = 'loess',                             # use local polynomial regression
              alpha = 0.3,                                  # size = 0.5 --> line width
              size = 0.5) +                                 # alpha = 0.3 --> color transparency
  facet_grid(cols = vars(Virus), rows = vars(Location),     # stratified by Virus and Location
             scales = 'fixed') +
  theme_bw() +                                                      # dark-on-light theme
  theme(panel.background = element_blank(),                    
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line.x = element_line(),                               # these two are for the axis line
        axis.line.y = element_line(),
        axis.text.x = element_text(colour = "black", size = 11),    # there two are for texts in axes
        axis.text.y = element_text(colour = "black", size = 11),
        axis.ticks.x = element_line(),                              # these two are for ticks in axes
        axis.ticks.y = element_line(),
        axis.title.x = element_text(colour = "black", size = 11, face = 'bold', vjust = -1),                              
        axis.title.y = element_text(colour = "black", size = 11, face = 'bold'),
        legend.title = element_text(colour = "black", size = 11, face = 'bold'),
        legend.text = element_text(colour = "black", size = 11),
        strip.text = element_text(size = 13, face = 'bold'))      # set up size of title for each figure with 13

# Version 4.0
p1 <- 
  ggplot(Dt, 
         aes(x = Ct, col = Location, fill = Location)) +            
  geom_density(alpha = 0.3) + 
  xlab('Ct') + ylab('Density') +
  theme_bw() +                                                      # dark-on-light theme
  theme(panel.border = element_blank(),
        panel.background = element_blank(),                    
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line.x = element_line(),                               # these two are for the axis line
        axis.line.y = element_line(),
        axis.text.x = element_text(colour = "black", size = 11),    # there two are for texts in axes
        axis.text.y = element_text(colour = "black", size = 11),
        axis.ticks.x = element_line(),                              # these two are for ticks in axes
        axis.ticks.y = element_line(),
        axis.title.x = element_text(colour = "black", size = 11, face = 'bold', vjust = -1),                              
        axis.title.y = element_text(colour = "black", size = 11, face = 'bold'),
        legend.title = element_text(colour = "black", size = 11, face = 'bold'),
        legend.text = element_text(colour = "black", size = 11))   

# Histogram
p2 <- 
  ggplot(Dt, 
         aes(x = Ct, col = Location, fill = Location)) +            
  geom_histogram() + 
  xlab('Ct') + ylab('Count') +
  theme_bw() +                                                      # dark-on-light theme
  theme(panel.border = element_blank(),
        panel.background = element_blank(),                    
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line.x = element_line(),                               # these two are for the axis line
        axis.line.y = element_line(),
        axis.text.x = element_text(colour = "black", size = 11),    # there two are for texts in axes
        axis.text.y = element_text(colour = "black", size = 11),
        axis.ticks.x = element_line(),                              # these two are for ticks in axes
        axis.ticks.y = element_line(),
        axis.title.x = element_text(colour = "black", size = 11, face = 'bold', vjust = -1),                              
        axis.title.y = element_text(colour = "black", size = 11, face = 'bold'),
        legend.title = element_text(colour = "black", size = 11, face = 'bold'),
        legend.text = element_text(colour = "black", size = 11))  
# Arrange plots
grid.arrange(p1, p2, nrow = 1)

