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

	#====================================== PLOT 2 ==================================================
	baltcitymary.emissions <- NEI %>%
		filter(fips == "24510") %>%
		group_by(year) %>%
		summarise(Emissions = sum(Emissions))
	clrs <- c("black", "darkblue", "blue", "lightblue")
	
	png("Plot2.png",
			width  = number.add.width,
			height = number.add.height
	)
	
	x2 <- barplot(height    = baltcitymary.emissions$Emissions / 1000,
								names.arg = baltcitymary.emissions$year,
								xlab      = "years",
								ylab      = expression('total PM'[2.5] * ' emission in kilotons'),
								ylim      = c(0, 4),
								main      = expression('Total PM'[2.5] * ' emissions in Baltimore City-MD in kilotons'),
								col       = clrs
	)
	
	text(x     = x2,
			 y     = round(baltcitymary.emissions$Emissions / 1000, 2),
			 label = round(baltcitymary.emissions$Emissions / 1000, 2),
			 pos   = 3,
			 cex   = 0.8,
			 col   = "black"
	)
	
	dev.off()

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

	#====================================== PLOT 5 ==================================================
	png("Plot5.png",
			width  = number.add.width,
			height = number.add.height)
	
	baltcitymary.emissions <- NEI[(NEI$fips == "24510") & (NEI$type == "ON-ROAD"),]
	
	baltcitymary.emissions.byyear <- baltcitymary.emissions %>%
		group_by(year) %>%
		summarise(Emissions = sum(Emissions))
	
	ggplot(baltcitymary.emissions.byyear, aes(x     = factor(year),
																						y     = Emissions,
																						fill  = year,
																						label = round(Emissions, 2))) +
		geom_bar(stat = "identity") +
		xlab("year") +
		ylab(expression("total PM"[2.5] * " emissions in tons")) +
		ggtitle("Emissions from motor vehicle sources in Baltimore City") +
		geom_label(aes(fill = year),
							 colour   = "white",
							 fontface = "bold"
		)
	
	dev.off()

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