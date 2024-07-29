# Load packages
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(colorspace)

# Extract the data
panda <- read_excel("data/panda.xlsx")

# View the data
View(panda)

# Tile
tile <- panda %>%
  group_by(intervention, outcome) %>%
  summarise(count_author_year_tile = n(), .groups = 'drop') %>%
  complete(intervention, outcome, fill = list(count_author_year_tile = 0))

# Bubble
bubble <- panda %>%
  group_by(intervention, outcome, study_design) %>%
  summarise(count_author_year_bubble = n(), .groups = 'drop') %>%
  filter(!is.na(study_design))

# Evidence gap map
egm <- ggplot(tile, aes(x = outcome, y = intervention)) +
  geom_tile(aes(fill = count_author_year_tile), color = "black") +
  geom_point(data = bubble, aes(size = count_author_year_bubble, color = study_design), 
             alpha = 0.6, position = position_jitterdodge(jitter.width = 0.2, 
                                                          dodge.width = 0.5, seed = 6554)) +
  scale_size_continuous(range = c(3, 10),
                        breaks = c(1, 3, 5, 10, 20), 
                        labels = function(x) round(x, 0)) + 
  scale_colour_discrete_qualitative() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Evidence gap map",
       x = "Health-related outcomes",
       y = "Diet intake intervention",
       fill = "Total studies",
       size = "Number of studies per study design",
       color = "Study design") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),) +
  guides(size = guide_legend(nrow = 2,
                             title = "Number of studies\nper study design"),
         colour = guide_legend(nrow = 3, override.aes = list(size = 5)))

egm

# Save as a picture file
ggsave("visuals/egm.jpg", plot = egm, 
       width = 15, height = 10, dpi = 300)

# Finish with writing session info.
writeLines(capture.output(sessionInfo()), "session_info.txt")

# Or taking a renv snapshot (not run).
# renv::snapshot()
