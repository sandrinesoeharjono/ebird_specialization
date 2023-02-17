### 4 plots: change in CSI, # observations, # municipalities and # species in each window ###

# load packages and files into environment
library(ggplot2)
library(ggthemes)
library(grid)
library(gridExtra)
changes_w <- read.csv("changes_w.csv")
CSI <- read.csv("CSI_changes_2m.csv")
beta <- read.csv("beta.csv")

# 1) Changes in CSI by average window date
CSI_plot <- ggplot(data = CSI, aes(x = as.Date(Average_Date), y = CSI)) + 
  theme_economist_white(gray_bg = FALSE) +
  geom_line(colour = "goldenrod4") + 
  ylab("CSI") + xlab("Date") + 
  theme(plot.title = element_blank(),
        axis.text.x = element_text(size = 10, hjust = 0.5, vjust = 1.1),
        axis.ticks.x = element_blank(),
        axis.title.x = element_text(face = "bold", size = 13, vjust = 0),
        axis.text.y = element_text(size = 10, hjust = 1),
        axis.title.y = element_text(face = "bold", size = 13, vjust = 3),
        axis.line = element_blank()) +
  scale_y_continuous(limits = c(1,4)) +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y") 
CSI_plot

# 2) Changes in number of observations by average window date
observations_plot <- ggplot(data = changes_w, aes(x = as.Date(Avg_Date), y = Number_observations)) + 
  theme_economist_white(gray_bg = FALSE) +
  geom_line(colour = "#009E73") + 
  ggtitle("A") + 
  ylab("Number of observations") + xlab("Date") + 
  theme(plot.title = element_text(face = "bold", hjust = 0, size = 20),
        axis.text.x = element_text(size = 10, hjust = 0.5, vjust = 1.1),
        axis.ticks.x = element_blank(),
        axis.title.x = element_text(face = "bold", size = 13, vjust = 0),
        axis.text.y = element_text(size = 10, hjust = 1),
        axis.title.y = element_text(face = "bold", size = 13, vjust = 3),
        axis.line = element_blank()) +  
  scale_x_date(date_breaks = "4 years", date_labels = "%Y") 
observations_plot

# 3) # municipalities sampled by average window date
municipalities_plot <- ggplot(data = changes_w, aes(x = as.Date(Avg_Date), y = Number_municipalities)) + 
  theme_economist_white(gray_bg = FALSE) +
  geom_line(colour = "#0072B2") + 
  ggtitle("B") + 
  ylab("Number of municipalities sampled") + xlab("Date") + 
  theme(plot.title = element_text(face = "bold", hjust = 0, size = 20),
        axis.text.x = element_text(size = 10, hjust = 0.5, vjust = 1.1),
        axis.ticks.x = element_blank(),
        axis.title.x = element_text(face = "bold", size = 13, vjust = 0),
        axis.text.y = element_text(size = 10, hjust = 1),
        axis.title.y = element_text(face = "bold", size = 13, vjust = 3),
        axis.line = element_blank()) +  
  scale_y_continuous(limits = c(0,80)) +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y") 
municipalities_plot

# 4) community richness by average window date
richness_plot <- ggplot(data = changes_w, aes(x = as.Date(Avg_Date), y = Number_species)) + 
  theme_economist_white(gray_bg = FALSE) +
  geom_line(colour = "#D55E00") + 
  ggtitle("A") + 
  ylab("Number of species") + xlab("Date") + 
  theme(plot.title = element_text(face = "bold", hjust = 0, size = 20),
        axis.text.x = element_text(size = 10, hjust = 0.5, vjust = 1.1),
        axis.ticks.x = element_blank(),
        axis.title.x = element_text(face = "bold", size = 13, vjust = 0),
        axis.text.y = element_text(size = 10, hjust = 1),
        axis.title.y = element_text(face = "bold", size = 13, vjust = 3),
        axis.line = element_blank()) +
  scale_y_continuous(limits = c(50,250)) +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y") 
richness_plot

# 5) beta plot
beta_plot <- ggplot(data = beta, aes(x = as.Date(Avg_Date), y = Beta_values)) + 
  theme_economist_white(gray_bg = FALSE) +
  geom_line(colour = "purple") + 
  ggtitle("B") + 
  ylab("β diversity") + xlab("Date") + 
  theme(plot.title = element_text(face = "bold", hjust = 0, size = 20),
        axis.text.x = element_text(size = 10, hjust = 0.5, vjust = 1.1),
        axis.ticks.x = element_blank(),
        axis.title.x = element_text(face = "bold", size = 13, vjust = 0),
        axis.text.y = element_text(size = 10, hjust = 1),
        axis.title.y = element_text(face = "bold", size = 13, vjust = 3),
        axis.line = element_blank()) +
  scale_y_continuous(limits = c(0.8,1)) +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y") 
beta_plot

############### arrange plots for multi-panel figures ###################
# figure 1
grid.arrange(observations_plot, municipalities_plot, nrow = 1, widths = c(20,20))

# figure 2
grid.arrange(richness_plot, beta_plot, nrow = 1, widths = c(20,20))

# figure 3
CSI_plot
