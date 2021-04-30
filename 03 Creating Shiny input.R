library(data.table)
library(dplyr)

### Create properties table

# Open connection with .db file
conn <- dbConnect(RSQLite::SQLite(), "inhibitiontasks.db")

# Create df to create property plots
plottable <- dbGetQuery(conn, "SELECT * FROM dataset") %>%
  mutate(ID = row_number(),
         # BF and eta values are obtained in "02 BF analyses.R"
         BF = c(11.89953, 12.62352, 0.2732385, 18.32572, (1/20001)/0.047145, 16.75317, 
                18.09561, 0.2665678, 1.095436, 0.006331117, (1/20001)/0.046475, 
                13.11331, 15.52096, 11.49876, 12.07571, 3.501511, 0.4207574, 16.20275, 
                0.5764995, 3.209925, 14.56348, 5.584778, 7.01924, 6.987421, 10.53446, 
                5.531469, 1.373188, 0.001237662, 6.722741, 0.5045454, 10.40664, 
                6.076131, 4.130163, 12.81157),
         eta = c(0.08214838, 0.10711851, 0.05867943, 0.07229003, 0.26880019, 0.10375159,
                 0.14986896, 0.06622908, 0.09250021, 0.06247480, 0.27554772, 0.04912932,
                 0.12073383, 0.08344965, 0.07087007, 0.10866095, 0.13179700, 0.06695967, 
                 0.16133709, 0.09849532, 0.07341471, 0.11292914, 0.07043394, 0.08766538,
                 0.08327879, 0.11558858, 0.11782265, 0.13169191, 0.12272071, 0.13763810, 
                 0.07912546, 0.10864835, 0.11111033, 0.07427830),
         preferred_model = ifelse(BF < 1, "Unconstrained Model", "Positive Model")) %>%
  left_join(dbGetQuery(conn, "SELECT `studyid`, `authors`, `conducted` FROM study"), by = "studyid") %>% # add study info 
  left_join(dbGetQuery(conn, "SELECT `taskid`, `task` FROM task"), by = "taskid") %>% # add task info
  select(ID, task, authors, conducted, mean_effect, sd_effect, mean_age, 
         percentage_female, percentage_male, n_participants, n_blocks, n_trials, 
         neutral_trials, percentage_incongruent,feedback, fixation_cross, 
         time_limit, BF, preferred_model, eta)

# Save table
setwd("~/Desktop/shiny")
save(plottable, file = "plottable.rda")

### Create plots as .png files
load("results.rda")

# for loop that makes a plot for each dataset
plots <- list()
for(i in 1:34){
  p <- quid:::plotEffects(res[[i]]) +
    theme(text = element_text(size = 20))
  ggsave(paste("plot", i, ".png", sep=""), plot = p, device = "png", width = 10, height = 7)
}

# plot input for when no dataset is selected in shiny app
plot0 <- ggplot() + 
  annotate("text", x = -10, y = 10, size = 5, hjust = 0,
           label = "Please select a dataset to view its properties") + 
  scale_x_continuous(expand = expand_scale(),
                     limits = c(-10,10)) +
  ylim(-10,10) +
  theme_void()
ggsave("plot0.png", plot = plot0, device = "png", width = 10, height = 7)