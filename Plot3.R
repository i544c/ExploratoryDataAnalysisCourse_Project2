# Data Science Specialization - Johns Hopkins University - Coursera
# Exploratory Data Analysis - Peer-graded Assignment: Course Project 2
# Name: Isaac G Veras
# Date: 05 de outubro de 2023

R.version.string   # R 4.3.1
getwd(); cat("\n") # Current working directory
setwd("C:/Johns Hopkins - Data Science/Exploratory Data Analysis/National_Emissions_Inventory")

# Package installation: ----------------------------------------------------------------------
	if (!require("pacman")) install.packages("pacman")
	pacman::p_load(pacman,     # Package Manager
								 data.table, # Manipulate, process and analyze large data sets
								 tidyverse   # organização dos dados
	)

# Loading the data -------------------------------------------------------------

	# read national emissions data
	NEI <- readRDS("summarySCC_PM25.rds")
	
	#read source code classification data
	SCC <- readRDS("Source_Classification_Code.rds")
	
	number.add.width  <- 800   # width length to make the changes faster
	number.add.height <- 800   # height length to make the changes faster

	#====================================== PLOT 3 ==================================================
	clrs <- c("black", "darkblue", "blue", "lightblue")

	png("Plot3.png",
			width  = number.add.width,
			height = number.add.height)
	
	# Group total NEI emissions per year:
	baltcitymary.emissions.byyear <- NEI %>%
		filter(fips == "24510") %>%
		group_by(year, type) %>%
		summarise(Emissions = sum(Emissions))

	ggplot(baltcitymary.emissions.byyear, aes(x     = factor(year),
																						y     = Emissions,
																						fill  = type,
																						label = round(Emissions, 2))
	) +
		geom_bar(stat = "identity") +
		facet_grid(. ~ type) +
		xlab("year") +
		ylab(expression("total PM"[2.5] * " emission in tons")) +
		ggtitle(expression("PM"[2.5] * paste0(" emissions in Baltimore ", "City by various source types"))
		) +
		geom_label(aes(fill = type),
							 colour   = "white",
							 fontface = "bold"
		) +
		scale_fill_manual(values = clrs)
	
	dev.off()
