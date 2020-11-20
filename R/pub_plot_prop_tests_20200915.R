# Proportion of Positive Tests over Pandemic Course
# Gabriel Odom
# 2020-09-15

# We would like a figure that shows the proportion of positive COVID-19 tests
#   and the total tests together on the same figure.

library(tidyverse)
library(lubridate)


###  Utility Function  ###
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



######  Import and Wrangle Data  ##############################################
sflCases_df <- read_csv(
	file = "data/FLDH_COVID19_cases_miamidade_20201118.csv"
) %>% 
	mutate(Total = Negative + Positive) %>% 
	mutate(PropPositive = 100 * Positive / Total) %>% 
	# Move proportion to last column
	select(-PropPositive, PropPositive)

sflCasesMA_df <- 
	sflCases_df %>% 
	mutate(Positive = RollUp(.x = Positive, .w = 5, na.rm = TRUE)) %>% 
	mutate(Negative = RollUp(.x = Negative, .w = 5, na.rm = TRUE)) %>% 
	mutate(Total = RollUp(.x = Total, .w = 5, na.rm = TRUE)) %>%
	mutate(PropPositive = 100 * Positive / Total)

sflCases2_df <- 
	sflCasesMA_df %>% 
	mutate(Date = as.POSIXct(strptime(Date, format = "%d-%b"))) %>% 
	mutate(Date = as_date(Date)) 

startEnd_date <- 
	sflCases2_df %>%
	slice(1, n()) %>%
	pull(Date) %>%
	format("%d %B")



######  Plot Data  ############################################################
cases_gg <- 
	ggplot(data = sflCases2_df) +
	
	theme_bw() +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	theme(legend.position = "bottom") + 
	
	aes(x = Date, y = PropPositive) +
	scale_x_date(
		date_breaks = "2 weeks",
		date_minor_breaks = "1 week",
		labels = scales::date_format("%d-%b")
	) +
	labs(
		# title = "Positive COVID-19 Test Results: Proportion out of All Tests",
		caption = paste(
			"Miami-Dade County,",
			startEnd_date[1], "to", startEnd_date[2], "2020.",
			"Red Triangle: Phase 1 Reopening"
		),
		y = "Proportion (%) of Positive Cases (5-Day MA)"
	) +
	
	geom_point(aes(alpha = Total, colour = Total), size = 3) +
	scale_color_gradient(
		low = "#FFC30B", high = "#03AC13",
		name = "Total Tests",
		breaks = c(7500, 15000),
		guide = FALSE
	) +
	# scale_alpha_continuous() +
	
	geom_point(
		data = tibble(
			Date = as_date("2020-05-18"),
			y = 0
		),
		aes(x = Date, y = y),
		pch = 24, size = 2, fill = "red"
	)

cases_gg


###  Save Hi-Res Plots  ###
tiff(
	"../figures/FIG01_proportion_positive_tests_20201120.tiff",
	units = "in", width = 7, height = 5,
	res = 300
)

cases_gg

dev.off()
