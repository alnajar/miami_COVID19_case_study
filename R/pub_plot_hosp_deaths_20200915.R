# Hospitalisations and Deaths
# Gabriel Odom
# 2020-08-19

# We would like a figure that shows hospitalisations and deaths together on the
#   same figure.

library(tidyverse)
library(lubridate)


######  Import Data  ##########################################################
deathsbyday_df <- read_csv(
	file = "../../../data/deaths/FLDH_COVID19_deathsbyday_bycounty_20210207.csv"
) %>% 
	# Deaths are behind. As of 11-20, 75% of daily-added FL deaths are on or after
	#   10-03; see details in plot_deaths_20200914.R
	# At revision and resubmission, we have deaths data until at least November 18
	filter(Date <= "2020-11-19") %>% 
	filter(County == "Dade") %>% 
	select(-County)

miamidadeHospitalized_df <- read_csv(
	file = "data/ESS_southFL_summary_20201119.csv"
) %>% 
	filter(County == "MIAMI-DADE") %>% 
	select(Date, Hospitalized, ICU, Ventilated)



######  Rolling Means  ########################################################
RollUp <- function(.x, .w, .f = mean, ...){
	# Find "Rolling" values of a specified function
	# Inputs:
	#   .x: a vector (usually numeric) over which to "roll" the function .f
	#   .w: the window size
	#   .f: the fuction to "roll" over the values of .x
	#   ...: additional arguments to .f
	# Output: a vector the length of .x with the first (.w - 1) values set to NA
	#   and the remaining values equal to .f evaluated over the previous window.
	# Details: for a five-day moving average, set .w = 5. Then the moving average
	#   of .x at index 5 will be mean(.x[1], .x[2], ..., .x[5]).
	# Examples:
	#   # Rolling mean of first five integers
	#   RollUp(.x = 1:10, .w = 5)
	
	n <- length(.x)
	out <- rep(NA, times = n)
	class(out) <- class(.x)
	
	for(i in .w:n) {
		out[i] <- .f(.x[(i - .w + 1):i])
	}
	
	out
	
}


######  Tidy and Wrangle  #####################################################
miamiDadeOutcomes_df <-
	left_join(
		miamidadeHospitalized_df,
		deathsbyday_df,
		by = "Date"
	)

startEnd_date <- 
	miamiDadeOutcomes_df %>%
	slice(1, n()) %>%
	pull(Date) 

miamiDadeOutcomes_df[
	miamiDadeOutcomes_df$Date == "2020-06-04",
	"Ventilated"
] <- NA_real_
# There is an error in the original data for June 4th. Rather than deleting the
#   whole row of the raw data, we are skipping this ventilation value

miamiDadeOutcomesMA_df <- 
	miamiDadeOutcomes_df %>% 
	mutate(Hospitalized = RollUp(.x = Hospitalized, .w = 5, na.rm = TRUE)) %>% 
	mutate(ICU = RollUp(.x = ICU, .w = 5, na.rm = TRUE)) %>% 
	mutate(Ventilated = RollUp(.x = Ventilated, .w = 5, na.rm = TRUE)) %>% 
	mutate(Dead = RollUp(.x = Count, .w = 5, na.rm = TRUE)) %>% 
	select(-Count)

mdCOVID_df <-
	miamiDadeOutcomesMA_df %>%
	pivot_longer(
		Hospitalized:Dead,
		names_to = "Type",
		values_to = "Count"
	)


######  Plot Trajectories  ####################################################
hosp2dead_gg <- 
	ggplot(data = mdCOVID_df) +
	
	theme_bw() +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	theme(legend.position = "bottom") + 
	aes(x = Date, y = Count, group = Type, colour = Type, shape = Type) +
	scale_x_date(
		date_breaks = "2 weeks",
		date_minor_breaks = "1 week",
		labels = scales::date_format("%d-%b")
	) +
	scale_color_manual(
		values = c(
			"Hospitalized" = "#ffc100",
			"ICU" = "#ff7400",
			"Ventilated" = "#ff0000",
			"Dead" = "black"
		)
	) +
	scale_y_continuous(
		trans = "log2",
		breaks = c(10, 50, 250, 500, 1000, 2000)
	) +
	labs(
		# title = "Overall Miami-Dade County Hospital COVID-19 Census",
		# x = paste(
		# 	"Date: ",
		# 	format(startEnd_date[1], "%d %B"),
		# 	"to",
		# 	format(startEnd_date[2], "%d %B"),
		# 	"2020"
		# ),
		x = "Date",
		y = "Miami-Dade Counts (5-Day MA; Log2 Scale)"
	) +
	
	geom_point(size = 1.5) +
	geom_point(
		data = tibble(
			Date = as_date("2020-05-18"),
			y = 2.5
		),
		aes(x = Date, y = y),
		inherit.aes = FALSE,
		pch = 24, size = 2, fill = "red"
	)

hosp2dead_gg


###  Save Hi-Res Plots  ###
tiff(
	"../figures/FIG02_hospitalizations_deaths_20210211.tiff",
	units = "in", width = 7, height = 5,
	res = 300
)

hosp2dead_gg

dev.off()
