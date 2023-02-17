### Perform wavelet analysis on richness, beta-diversity and CSI temporal changes ###

# load package and files into environment
library(WaveletComp)
changes_w <- read.csv("changes_w.csv")
beta <- read.csv("beta.csv")
CSI <- read.csv("CSI_changes_2m.csv")

# periodicity analysis for richness (Figure 4a & 4d)
fig4_richness <- analyze.wavelet(changes_w, "Number_species", dt = 1/(36.5))
wt.image(fig4_richness, show.date = TRUE, periodlab = "Period (years)", label.time.axis = TRUE)
reconstruct(fig4_richness, show.date = TRUE, lwd = c(2,1))

# periodicity analysis for beta-diversity values (Figure 4b & 4e)
fig4_beta <- analyze.wavelet(beta, "Beta_values", dt = 1/(36.5))
wt.image(fig4_beta, show.date = TRUE, periodlab = "Period (years)", label.time.axis = TRUE)
reconstruct(fig4_beta, show.date = TRUE, lwd = c(2,1))

# periodicity analysis for CSI values (Figure 4c & 4f)
fig4_CSI <- analyze.wavelet(CSI, "CSI", dt = 1/(36.5))
wt.image(fig4_CSI, show.date = TRUE, periodlab = "Period (years)", label.time.axis = TRUE)
reconstruct(fig4_CSI, show.date = TRUE, lwd = c(2,1))
