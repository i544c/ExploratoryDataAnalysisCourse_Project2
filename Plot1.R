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

	#====================================== PLOT 1 ==================================================
	png("Plot1.png",
			width  = number.add.width,
			height = number.add.height
	)
	
	# Group total NEI emissions per year:
	total.emissions <- NEI %>%
		group_by(year) %>%
		summarise(Emissions = sum(Emissions))
	
	clrs <- c("black", "darkblue", "blue", "lightblue")
	
	x1 <- barplot(height    = total.emissions$Emissions / 1000,
								names.arg = total.emissions$year,
								xlab      = "years",
								ylab      = expression('total PM'[2.5] * ' emission in kilotons'),
								ylim      = c(0, 8000),
								main      = expression('Total PM'[2.5] * ' emissions at various years in kilotons'),
								col       = clrs
	)
	
	## Add text at top of bars
	text(x     = x1,
			 y     = round(total.emissions$Emissions / 1000, 2),
			 label = round(total.emissions$Emissions / 1000, 2),
			 pos   = 3,
			 cex   = 0.8,
			 col   = "black"
	)
	
	dev.off()