library(tidyverse)
library(forcats)
library(readxl)
library(plotly)
library(scales)

# data
raw <- read_xlsx("cccd_financials_2018.xlsx")

#Noncurrent labilities per ftes ordered ####
ncl_ftes_rank <- raw %>% 
  select(ccd_short, `NCL/FTES`) %>% 
  rename(`NCL per FTES` = `NCL/FTES`,
         CCD = ccd_short) %>% 
  mutate(CCD = factor(CCD)) %>% # converts ccd names to factor
  mutate(CCD = fct_reorder(CCD, `NCL per FTES`)) # reorders ccd names for ggplot desc 

p2 <- ggplot(ncl_ftes_rank) +
  # this allows for custom plotly tooltips
  aes_string("CCD" ,
             "`NCL per FTES`") +
  geom_col() +
  scale_y_continuous(labels = label_dollar(),
                     breaks = seq(0, 109999, by = 50000),
                     expand = c(0,0)
  ) +
  coord_flip() +
  labs(title = "<b style='color: #646464'>Noncurrent Labilities per FTES for California Community College Districts in 2018</b>",
       subtitle = "The chart below shows the community colleges that had the highest noncurrent liabilities per ftes from highest to lowest.",
       x = "", 
       y = "") +
  theme_minimal() +
  theme(plot.title = element_text(face="bold", size=18,hjust = 0.5),
        panel.grid.major.y = element_blank())

p2_interactive <- ggplotly(p2) %>%
  layout(
    font = list(family = 'Lato'),
    hoverlabel = list(bgcolor = 'white', 
                      color = 'DarkGray')
  )

htmlwidgets::saveWidget(p2_animated, file="ncl_per_ftes.html")
