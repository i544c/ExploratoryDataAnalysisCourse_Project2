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

	#====================================== PLOT 4 ==================================================
	png("Plot4.png",
			width  = number.add.width,
			height = number.add.height
	)
	
	combustion.coal         <- grepl("Fuel Comb.*Coal", SCC$EI.Sector)
	combustion.coal.sources <- SCC[combustion.coal,]
	
	# Find emissions from coal combustion-related sources
	emissions.coal.combustion <- NEI[(NEI$SCC %in% combustion.coal.sources$SCC),]
	
	emissions.coal.related <- emissions.coal.combustion %>%
		group_by(year) %>%
		summarise(Emissions = sum(Emissions)
		)
	
	ggplot(emissions.coal.related, aes(x     = factor(year),
																		 y     = Emissions / 1000,
																		 fill  = year,
																		 label = round(Emissions / 1000, 2))
	) +
		geom_bar(stat = "identity") +
		xlab("year") +
		ylab(expression("total PM"[2.5] * " emissions in kilotons")) +
		ggtitle("Emissions from coal combustion-related sources in kilotons") +
		geom_label(aes(fill = year),
							 colour   = "white",
							 fontface = "bold"
		)

	dev.off()
