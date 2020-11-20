# Plot Deaths Over Time by Day Reported
# Gabriel Odom
# 2020-10-06

library(tidyverse)
library(lubridate)
library(readxl)


######  Import All Deaths  ####################################################

# ###  Import All Raw Deaths Files  ###
# end_Date <- ymd("20200830")
# # From my exploration, dates at that slug/nugget start at July 1st
# dates  <- format(seq(ymd("20200701"), end_Date, by = "days"), "%Y%m%d")
# 
# main_dir <- "~/OneDrive - Florida International University/covid19_sfl_website/"
# fileTo <- paste0(main_dir, "data/deaths/florida_linelist_", dates, ".xlsx")
# 
# # slug   <- "http://ww11.doh.state.fl.us/comm/_partners/"
# # nugget <- "covid19_report_archive/state_linelist_"
# # urls   <- paste0(slug, nugget, dates, ".xlsx")
# 
# # library(httr)
# # # This downloads the data at the website urls[1] into the file fileTo[1]; the
# # #   write_disk() function tells GET() to store the data on the hard disk, not
# # #   in RAM
# # map(
# # 	.x = seq_along(dates),
# # 	.f = ~{ httr::GET(urls[.x], write_disk(fileTo[.x])) }
# # )
# 
# 
# ###  Deaths July 1st and After  ###
# deaths_ls <- 
# 	map(
# 		.x = fileTo,
# 		.f = ~{
# 			# browser()
# 			
# 			df <- read_excel(
# 				path = .x,
# 				sheet = "Deaths",
# 				skip = 4
# 			) %>% 
# 				# Clean up the dates
# 				mutate(Date = as_date(`Date case counted`)) %>% 
# 				select(-`Date case counted`)
# 			
# 			colName <- str_extract(.x, pattern = "[0-9]{8}")
# 			
# 			df %>% 
# 				arrange(County, Date) %>%
# 				# Group deaths by day+county and count how many
# 				group_by(County, Date) %>%
# 				add_tally(name = colName) %>%
# 				# Remove duplicate rows
# 				select(County, Date, one_of(colName)) %>%
# 				distinct() %>%
# 				ungroup()
# 			
# 		}
# 	)
# 
# newDeaths_df <- 
# 	reduce(
# 		.x = deaths_ls,
# 		.f = full_join,
# 		by = c("County", "Date")
# 	)
# 
# rm(deaths_ls)
# 
# 
# ###  Baseline Deaths (prior to July 1st)  ###
# baselineDeaths_df <-
# 	read_excel(
# 		path = fileTo[1],
# 		sheet = "Deaths",
# 		skip = 4
# 	) %>% 
# 	# Clean up the dates
# 	mutate(Date = as_date(`Date case counted`)) %>% 
# 	select(-`Date case counted`) %>% 
# 	# Remove the newly-added dates on the first day to yield baseline deaths
# 	filter(is.na(`Newly identified deaths`)) %>% 
# 	arrange(County, Date) %>%
# 	# Group deaths by day+county and count how many
# 	group_by(County, Date) %>%
# 	add_tally(name = "Baseline") %>%
# 	# Remove duplicate rows
# 	select(County, Date, Baseline) %>%
# 	distinct() %>%
# 	ungroup() 
# 
# 
# ###  Deaths Reported October 1st  ###
# recentDeaths_df <- 
# 	read_csv(
# 	  file = paste0(
# 	  	main_dir, "data/deaths/FLDH_COVID19_deathsbyday_bycounty_20201001.csv"
# 	  )
#   ) %>% 
# 	filter(Date <= end_Date) %>% 
# 	rename(`20201001` = Count)
# 
# 
# ###  County x Date Grid  ###
# start_Date <- min(
# 	min(newDeaths_df$Date), min(baselineDeaths_df$Date)
# )
# min(recentDeaths_df$Date)
# 
# counties_char <- union(
# 	newDeaths_df$County, baselineDeaths_df$County
# )
# 
# dateGrid_df <-
# 	full_join(
# 		tibble(County = counties_char),
# 		tibble(Date = seq(start_Date, end_Date, by = "days")),
# 		by = character()
# 	)
# 
# 
# ###  Full Data  ###
# fullDeathByDay_df <-
# 	full_join(
# 		dateGrid_df,
# 		baselineDeaths_df,
# 		by = c("County", "Date")
# 	) %>% 
# 	full_join(
# 		newDeaths_df,
# 		by = c("County", "Date")
# 	) %>% 
# 	full_join(
# 		recentDeaths_df,
# 		by = c("County", "Date")
# 	)
# 
# # write_csv(
# # 	fullDeathByDay_df,
# # 	path = paste0(
# # 		main_dir,
# # 		"data/deaths/reported_deathsXday_for_",
# # 		format(end_Date, "%Y%m%d"),
# # 		"_on_20201001",
# # 		".csv"
# # 	)
# # )
# 
# # END IMPORT



######  Plot Deaths by Day Reported  ##########################################
fullDeathByDay_df <-
	read_csv(
		"data/reported_deathsXday_for_20200830_on_20201001.csv"
	)

splineDegree_int <- 5L

# State of Florida
ggplot(
	data = fullDeathByDay_df %>% 
		group_by(Date) %>% 
		summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>% 
		filter(Date <= "2020-07-01")
) +
	theme_bw() +
	aes(x = Date) +
	labs(
		title = "Actual vs Reported Deaths by Date",
		subtitle = "State of Florida; March 1st through July 1st",
		y = "Recorded Deaths",
		caption =
			"Column Chart shows deaths as reported by the FL DoH on July 1st;\nEstimate Legend: Black/Grey = day of reporting; Red = reported two weeks later;\nOrange = reported one month later; Yellow = reported two months later; Green = reported three months later"
	) +
	
	geom_col(aes(y = `20201001`), colour = "black", fill = "darkgreen") +
	geom_col(aes(y = `20200830`), colour = "black", fill = "gold") +
	geom_col(aes(y = `20200730`), colour = "black", fill = "darkorange") +
	geom_col(aes(y = `20200715`), colour = "black", fill = "red") +
	geom_col(aes(y = `20200701`), colour = "black", fill = "darkgray") +
	
	stat_smooth(
		aes(y = `20200701`),
		size = 2,
		method = "glm",
		method.args = list(family = "quasipoisson"),
		formula = y ~ splines::ns(x, splineDegree_int),
		colour = "black",
		se = FALSE
	) +
	# stat_smooth(
	# 	aes(y = `20200715`),
	# 	method = "glm",
	# 	method.args = list(family = "quasipoisson"),
	# 	formula = y ~ splines::ns(x, splineDegree_int),
	# 	colour = "red",
	# 	se = FALSE
	# ) +
	# stat_smooth(
	# 	aes(y = `20200730`),
	# 	method = "glm",
	# 	method.args = list(family = "quasipoisson"),
	# 	formula = y ~ splines::ns(x, splineDegree_int),
	# 	colour = "darkorange",
	# 	se = FALSE
	# ) +
	# # stat_smooth(
	# # 	aes(y = `20200815`),
	# # 	method = "glm",
	# # 	method.args = list(family = "quasipoisson"),
	# # 	formula = y ~ splines::ns(x, splineDegree_int),
	# # 	colour = "yellow",
	# # 	se = FALSE
	# # ) +
	# stat_smooth(
	# 	aes(y = `20200830`),
	# 	method = "glm",
	# 	method.args = list(family = "quasipoisson"),
	# 	formula = y ~ splines::ns(x, splineDegree_int),
	# 	colour = "gold",
	# 	se = FALSE
	# ) +
	stat_smooth(
		aes(y = `20201001`),
		size = 2,
		method = "glm",
		method.args = list(family = "quasipoisson"),
		formula = y ~ splines::ns(x, splineDegree_int),
		colour = "darkgreen",
		se = FALSE
	)


# Miami-Dade County
MD_gg <- 
  ggplot(
  	data = fullDeathByDay_df %>% 
  		filter(County == "Dade") %>% 
  		filter(Date <= "2020-07-01")
  ) +
	theme_bw() +
	aes(x = Date) +
	labs(
		# title = "Deaths by Their Date Reported",
		# subtitle = "Miami-Dade County; March 15th through July 1st",
		y = "Recorded COVID-19 Deaths",
		caption =
			"Column Chart shows Miami-Dade County deaths from March 15th - July 1st as reported by the FL DoH on July 1st;\nEstimate Legend: Black/Grey = day of reporting; Red = reported two weeks later;\nOrange = reported one month later; Yellow = reported two months later; Green = reported three months later"
	) +
	
	geom_col(aes(y = `20201001`), colour = "black", fill = "darkgreen") +
	geom_col(aes(y = `20200830`), colour = "black", fill = "gold") +
	geom_col(aes(y = `20200730`), colour = "black", fill = "darkorange") +
	geom_col(aes(y = `20200715`), colour = "black", fill = "red") +
	geom_col(aes(y = `20200701`), colour = "black", fill = "darkgray") +
	
	stat_smooth(
		aes(y = `20200701`),
		size = 2,
		method = "glm",
		method.args = list(family = "quasipoisson"),
		formula = y ~ splines::ns(x, splineDegree_int),
		colour = "black",
		se = FALSE
	) +
  stat_smooth(
  	aes(y = `20200715`),
  	size = 2,
  	method = "glm",
  	method.args = list(family = "quasipoisson"),
  	formula = y ~ splines::ns(x, splineDegree_int),
  	colour = "red",
  	se = FALSE
  ) +
  stat_smooth(
  	aes(y = `20200730`),
  	size = 2,
  	method = "glm",
  	method.args = list(family = "quasipoisson"),
  	formula = y ~ splines::ns(x, splineDegree_int),
  	colour = "darkorange",
  	se = FALSE
  ) +
  # # stat_smooth(
  # # 	aes(y = `20200815`),
  # # 	method = "glm",
  # # 	method.args = list(family = "quasipoisson"),
  # # 	formula = y ~ splines::ns(x, splineDegree_int),
  # # 	colour = "yellow",
  # # 	se = FALSE
  # # ) +
  stat_smooth(
  	aes(y = `20200830`),
  	size = 2,
  	method = "glm",
  	method.args = list(family = "quasipoisson"),
  	formula = y ~ splines::ns(x, splineDegree_int),
  	colour = "gold",
  	se = FALSE
  ) +
  stat_smooth(
  	aes(y = `20201001`),
  	size = 2,
  	method = "glm",
  	method.args = list(family = "quasipoisson"),
  	formula = y ~ splines::ns(x, splineDegree_int),
  	colour = "darkgreen",
  	se = FALSE
  )

MD_gg


###  Save Hi-Res Plots  ###
tiff(
	"../figures/FIG03_back2theFuture_20201120.tiff",
	units = "in", width = 7, height = 5,
	res = 300
)

MD_gg

dev.off()

