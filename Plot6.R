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

	#====================================== PLOT 6 ==================================================
	clrs_baltcitymary <- "black"
	clrs_losangelscal <- "blue"

	png("Plot6.png",
			width  = number.add.width,
			height = number.add.height
	)

	baltcitymary.emissions <- NEI %>%
		filter(fips == "24510" & type == 'ON-ROAD') %>%
		group_by(year) %>%
		summarise(Emissions = sum(Emissions))
	
	losangelscal.emissions <- NEI %>%
		filter(fips == "06037" & type == 'ON-ROAD') %>%
		group_by(year) %>%
		summarise(Emissions = sum(Emissions))
	
	baltcitymary.emissions$County <- "Baltimore City, MD"
	losangelscal.emissions$County <- "Los Angeles County, CA"
	both.emissions                <- rbind(baltcitymary.emissions, losangelscal.emissions)
	
	
	ggplot(both.emissions, aes(x     = factor(year),
														 y     = Emissions,
														 fill  = County,
														 label = round(Emissions, 2))) +
		geom_bar(stat = "identity") +
		facet_grid(County ~ ., scales = "free") +
		ylab(expression("total PM"[2.5] * " emissions in tons")) +
		xlab("year") +
		ggtitle(expression("Motor vehicle emission variation in Baltimore and Los Angeles in tons")) +
		geom_label(aes(fill = County),
							 colour   = "white",
							 fontface = "bold"
		) +
		scale_fill_manual(values = c("Baltimore City, MD"     = clrs_baltcitymary,
																 "Los Angeles County, CA" = clrs_losangelscal)
		)
	
	dev.off()