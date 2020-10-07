# MRG Survey Analysis

# packages
library(data.table)
library(ggplot2)
library(openxlsx)
library(sf)

# helpers
# colors
rsgcolordf <- data.frame(red=c(246,0,99,186,117,255,82),
                         green=c(139,111,175,18,190,194,77),
                         blue=c(31,161,94,34,233,14,133),
                         colornames=c("orange","marine","leaf","cherry","sky","sunshine","violet"))

mrgcolordf <- data.frame(red=c(246,0,2,186,117,255,82),
                          green=c(139,64,128,18,190,194,77),
                          blue=c(31,128,64,34,233,14,133),
                          colornames=c("orange","mrgblue","mrggreen","cherry","sky","sunshine","violet"))

### Read in the data 
# spreadsheet is MRG_CrossTabs_analysis.xlsx
# field names and text explanation of field content
mrg.meta <- read.xlsx("MRG CrossTabs_analysis.xlsx", 
                      sheet = "Cleaned Data",
                      startRow = 2,
                      rows = 2:3)

# data, no headers
mrg <- read.xlsx("MRG CrossTabs_analysis.xlsx", 
                 sheet = "Cleaned Data",
                 startRow = 4,
                 colNames = FALSE)

# Name fields
names(mrg) <- names(mrg.meta)
names(mrg)[7] <- "Municipality"
names(mrg)[8] <- "State"
mrg <- data.table(mrg)
setnames(mrg, "[user.added]", "OnlyOneBarrier")

# Data descriptor
descr <- read.xlsx("MRG CrossTabs_analysis.xlsx", 
                   sheet = "Question Key",
                   cols = 2:5)
names(descr) <- c("QuestionNum", "Question", "QuestionResponseOptionID", "ResponseOption")
descr <- data.table(descr)

# Qualitative coding
codes_greenway_exp <- read.xlsx("MRG CrossTabs_analysis.xlsx",
                                sheet = "S2_P2_T0_Q2_Greenway_Experience",
                                cols = 1:2,
                                startRow = 2)
codes_greenway_exp <- data.table(codes_greenway_exp)

codes_destinations <- read.xlsx("MRG CrossTabs_analysis.xlsx",
                                sheet = "S2_P4_T0_Q2_Trail_Destinations_",
                                cols = 1:2,
                                startRow = 2)
codes_destinations <- data.table(codes_destinations)

codes_comm_value <- read.xlsx("MRG CrossTabs_analysis.xlsx",
                                sheet = "S2_P5_T0_Q1_Community_Value_1",
                                cols = 1:2,
                                startRow = 2)
codes_comm_value <- data.table(codes_comm_value)

# Counts data are in ./counts/MRG Counts.xlsx
# counts need to be factored to account for most users doing an out and back
# also counts need to be adjusted based on calibration count 
# this adjusts for missed users, e.g., two people walking alongside each other being counted as one person
count_east_89 <- read.xlsx("./counts/MRG Counts.xlsx", 
                               sheet = "East of 89 Analysis",
                               cols = 1:6)

count_price_chopper <- read.xlsx("./counts/MRG Counts.xlsx", 
                           sheet = "Price Chopper Analysis",
                           cols = 5:9)

count_factor <- 0.55
count_calibration <- 1 + (95-75)/75

count_weather <- read.xlsx("./counts/MRG Counts.xlsx", 
                           sheet = "Weather",
                           cols = 1:11,
                           startRow = 2)

### Definition of complete

# Require that survey completed:
# check if municipality, age, income questions answered (included prefer not to answer income option)
mrg_all <- copy(mrg)
mrg_all[, Complete_Age := ifelse(!is.na(Q_StandardAge),1,0)]
mrg_all[, .N, by = Complete_Age]
mrg_all[, Complete_Municipality := ifelse(!is.na(Municipality),1,0)]
mrg_all[, .N, by = Complete_Municipality]
mrg_all[, Complete_Income := ifelse(!is.na(XIT_Custom3),1,0)]
mrg_all[, .N, by = Complete_Income]

# require age, municipality for completeness, 
# for not-answered income, recode as prefer not to answer
mrg_all[, Complete := ifelse(!is.na(Q_StandardAge) & !is.na(Municipality),1,0)]
mrg <- mrg_all[Complete == 1]

### Convert fields to factors where appropriate
# Age
mrg[, Q_StandardAge := factor(Q_StandardAge, 
                              levels = c("25 and under","26 to 40","41 to 60","61 to 80","81 and over"))]

# Income
mrg[is.na(XIT_Custom3), XIT_Custom3 := "Prefer not to say"]
mrg[, XIT_Custom3 := factor(XIT_Custom3, 
                            levels = c("Less than 20000","20000 34999","35000 49999",
                                       "50000 74999","75000 99999", "Over 100000", "Prefer not to say"))]
mrg[, XIT_Custom3 := factor(XIT_Custom3, 
                            labels = c("Less than $20,000","$20,000-$34,999","$35,000-$49,999",
                                       "$50,000-$74,999","$75,000-$99,999", "Over 100,000", "Prefer not to say"))]

# Warmer Months Frequency
mrg[, S2_P1_T0_Q1_Seasonal_Use_1 := factor(S2_P1_T0_Q1_Seasonal_Use_1, 
                                           levels = c("Almost every day","A few times a week",
                                                      "About once a week","About once a month",
                                                      "Once every few months","Never"))]

# Winter Frequency
mrg[, S2_P1_T0_Q2_Seasonal_Use_2 := factor(S2_P1_T0_Q2_Seasonal_Use_2, 
                                           levels = c("Almost every day","A few times a week",
                                                      "About once a week","About once a month",
                                                      "Once every few months","Never"))]

# Mode getting to the MRG
mrg[, S2_P3_T0_Q1_Getting_to_the_Greenway_1 := factor(S2_P3_T0_Q1_Getting_to_the_Greenway_1, 
                                                      levels = c("Walk","Bicycle","Public Transportation",
                                                                 "Drive my car","Other"))]

# straight tabs to check data
# recode NAs where appropriate
# convert to (ordered) factors where appropriate

# 1. Age
mrg[,.N, keyby = Q_StandardAge]

# 2. Municipality, State
# group non local/smaller numbers into other within NH, VT and other state, with those ordered at the end
mrg[, Num_Muncipality := .N, by = .(Municipality, State)]
mrg[, Muni_State := paste(Municipality, State, sep = ", ")]
mrg[Num_Muncipality < 10 & State %in% c("NH", "VT"), Muni_State := "Other Town in NH or VT"]
mrg[Num_Muncipality < 10 & !State %in% c("NH", "VT"), Muni_State := "Other State"]
town_levels <- mrg[, .N, by = Muni_State][order(-N)][!Muni_State %in% c("Other Town in NH or VT", "Other State")]$Muni_State
mrg[, Muni_State := factor(Muni_State, levels = c(town_levels, "Other Town in NH or VT", "Other State"))]
mrg[, .N, keyby = Muni_State]
mrg[, LebRes := ifelse(Muni_State %in% c("WEST LEBANON, NH", "LEBANON, NH"), "Leb Resident", "Not a Leb Res.")]


# 3. Income
mrg[, .N, keyby = XIT_Custom3]

# 4. Which of the following recreational activities do you use the Greenway for? Check all that apply.
# Multiple select, fields S2_P0_T0_Q1
cols <- names(mrg)[grep("S2_P0_T0_Q1", names(mrg))]
mrg_rec_use <- melt(mrg[,c("VisitId", cols), with=FALSE],
                    id.vars = "VisitId",
                    variable.name = "QuestionResponseOptionID",
                    value.name = "Yes")
mrg_rec_use[descr[QuestionNum==4, .(QuestionResponseOptionID, ResponseOption)],
            ResponseOption := i.ResponseOption,
            on = c("QuestionResponseOptionID")]

# order repsonses with other, not for rec and not at all at end
rec_levels <- mrg_rec_use[,.(Yes = sum(Yes, na.rm = TRUE)), keyby = ResponseOption][order(-Yes)][!ResponseOption %in% c("Other", "I don't use the Greenway for recreation", "I don't use the Greenway at all")]$ResponseOption
mrg_rec_use[, ResponseOption := factor(ResponseOption, levels = c(rec_levels, "Other", "I don't use the Greenway for recreation", "I don't use the Greenway at all"))]
mrg_rec_use[,.(Yes = sum(Yes, na.rm = TRUE)), keyby = ResponseOption]
mrg_rec_use[, UserType := ifelse(ResponseOption %in% c("I don't use the Greenway for recreation", "I don't use the Greenway at all"),"Not a Rec. User", "Recreational User")]

# 5. Which of the following non-recreational activities do you use the Greenway for? Check all that apply.
# Multiple select, fields S2_P0_T0_Q2
cols <- names(mrg)[grep("S2_P0_T0_Q2", names(mrg))]
mrg_nonrec_use <- melt(mrg[,c("VisitId", cols), with=FALSE],
                    id.vars = "VisitId",
                    variable.name = "QuestionResponseOptionID",
                    value.name = "Yes")
mrg_nonrec_use[descr[QuestionNum==5, .(QuestionResponseOptionID, ResponseOption)],
            ResponseOption := i.ResponseOption,
            on = c("QuestionResponseOptionID")]

# order repsonses with other, not for rec and not at all at end
nonrec_levels <- mrg_nonrec_use[,.(Yes = sum(Yes, na.rm = TRUE)), keyby = ResponseOption][order(-Yes)][!ResponseOption %in% c("I only use the Greenway for recreation", "I don't use the Greenway at all")]$ResponseOption
mrg_nonrec_use[, ResponseOption := factor(ResponseOption, levels = c(nonrec_levels, "I only use the Greenway for recreation", "I don't use the Greenway at all"))]
mrg_nonrec_use[,.(Yes = sum(Yes, na.rm = TRUE)), keyby = ResponseOption]
mrg_nonrec_use[, UserType := ifelse(ResponseOption %in% c("I only use the Greenway for recreation", "I don't use the Greenway at all"),"Not a Non-Rec. User", "Non-Recreational User")]

# 6. In the warmer months, about how often do you use the Greenway?
mrg[,.N, keyby = S2_P1_T0_Q1_Seasonal_Use_1]

# 7.  In the winter, about how often do you use the Greenway?
mrg[,.N, keyby = S2_P1_T0_Q2_Seasonal_Use_2]

# 8. Please select the options below that describe your experience using the Greenway 
# Multiple select, fields S2_P2_T0_Q1
cols <- names(mrg)[grep("S2_P2_T0_Q1", names(mrg))]
mrg_experience <- melt(mrg[,c("VisitId", cols), with=FALSE],
                       id.vars = "VisitId",
                       variable.name = "QuestionResponseOptionID",
                       value.name = "Yes")
mrg_experience[descr[QuestionNum==8, .(QuestionResponseOptionID, ResponseOption)],
               ResponseOption := i.ResponseOption,
               on = c("QuestionResponseOptionID")]

mrg_experience[,.(Yes = sum(Yes, na.rm = TRUE)), keyby = ResponseOption][order(-Yes)][]

# Add short labels
experience_short <- data.table(descr[QuestionNum==8, .(QuestionResponseOptionID, ResponseOption)],
                               ResponseOptionShort = c("Feel Safe", "Experience Conflicts", "Have Fun",
                                                       "Collision/Near Miss with Bike", "Enjoy Seeing Others",
                                                       "Not Enough Rules/Safety Signage", "Most Users Respectful",
                                                       "Too Crowded Busy Times"),
                               ExperienceType = c("Positive", "Negative", "Positive",
                                                  "Negative", "Positive",
                                                  "Negative", "Positive",
                                                  "Negative"))
mrg_experience[experience_short, c("ResponseOptionShort","ExperienceType") := .(i.ResponseOptionShort,i.ExperienceType), on = "ResponseOption"]
mrg_experience[,.(Yes = sum(Yes, na.rm = TRUE)), keyby = .(ResponseOptionShort, ExperienceType)][order(-Yes)][]

# 9. Please describe any specific experiences or thoughts about the Greenway that you wish to share.
# Qualitative question, codes in codes_greenway_exp
cols <- names(mrg)[grep("S2_P2_T0_Q2_Greenway_Experience_3", names(mrg))]
mrg_exp_qual <- melt(mrg[,c("VisitId", cols), with=FALSE],
                       id.vars = "VisitId",
                       variable.name = "QuestionResponseOptionID",
                       value.name = "Code")
mrg_exp_qual[codes_greenway_exp[,.(Code, Description)],
               CodedResponse := i.Description,
               on = c("Code")]

mrg_exp_qual[!is.na(Code),.(Yes = .N), keyby = CodedResponse][order(-Yes)][]

# 10. How do you usually travel to the Greenway from your home?
# reorder factor, with Other and then NA labeled as Not Applicable
# Are NAs those who don't use the greenway? Need a user/non-user flag
mrg[is.na(S2_P3_T0_Q1_Getting_to_the_Greenway_1), S2_P3_T0_Q1_Getting_to_the_Greenway_1 := "Not Answered"]
mode_ordered_labels <- c(as.character(mrg[!S2_P3_T0_Q1_Getting_to_the_Greenway_1 %in% c("Other", "Not Answered"),.N, by = S2_P3_T0_Q1_Getting_to_the_Greenway_1][order(-N)]$S2_P3_T0_Q1_Getting_to_the_Greenway_1),
                         "Other", "Not Answered")
mrg[, S2_P3_T0_Q1_Getting_to_the_Greenway_1 := factor(S2_P3_T0_Q1_Getting_to_the_Greenway_1, levels = mode_ordered_labels)]

mrg[,.N, keyby = S2_P3_T0_Q1_Getting_to_the_Greenway_1]

# 11. What are the top TWO barriers that might discourage you from walking or bicycling to the Greenway from your home?
cols <- names(mrg)[grep("S2_P3_T0_Q2", names(mrg))]
mrg_barriers <- melt(mrg[,c("VisitId", cols), with=FALSE],
                       id.vars = "VisitId",
                       variable.name = "QuestionResponseOptionID",
                       value.name = "Yes")
mrg_barriers[descr[QuestionNum==11, .(QuestionResponseOptionID, ResponseOption)],
               ResponseOption := i.ResponseOption,
               on = c("QuestionResponseOptionID")]

mrg_barriers[,.(Yes = sum(Yes, na.rm = TRUE)), keyby = ResponseOption][order(-Yes)][]

# order repsonses with not barrier at end
barriers_levels <- mrg_barriers[,.(Yes = sum(Yes, na.rm = TRUE)), by = ResponseOptionShort][order(-Yes)][!ResponseOptionShort %in% c("Other barrier", "None, can walk or bike")]$ResponseOptionShort
mrg_barriers[, ResponseOptionShort := factor(ResponseOptionShort, levels = c(barriers_levels, "Other barrier", "None, can walk or bike"))]
mrg_barriers[,.(Yes = sum(Yes, na.rm = TRUE)), keyby = .(ResponseOptionShort, BarrierType)]

# Add short labels
barriers_short <- data.table(descr[QuestionNum==11, .(QuestionResponseOptionID, ResponseOption)],
                               ResponseOptionShort = c("Too far", "No close access", "Roads not safe",
                                                       "Mobility limitation", "Prefer to drive",
                                                       "Transit not convenient", "No bicycle",
                                                       "Other barrier", "None, can walk or bike", "[Only selected one barrier]"),
                               BarrierType = c("Barrier", "Barrier", "Barrier",
                                                  "Barrier", "Barrier",
                                                  "Barrier", "Barrier",
                                                  "Barrier", "Can Walk/Bike", "Exclude"))
mrg_barriers[barriers_short, c("ResponseOptionShort","BarrierType") := .(i.ResponseOptionShort,BarrierType), on = "ResponseOption"]
mrg_barriers[,.(Yes = sum(Yes, na.rm = TRUE)), keyby = .(ResponseOptionShort, BarrierType)][order(-Yes)][]

# 12. Do you currently use the Greenway to help you get to any of these destinations by foot or bike (check ALL that apply)?
cols <- names(mrg)[grep("S2_P4_T0_Q1", names(mrg))]
mrg_dest <- melt(mrg[,c("VisitId", cols), with=FALSE],
                     id.vars = "VisitId",
                     variable.name = "QuestionResponseOptionID",
                     value.name = "Yes")
mrg_dest[descr[QuestionNum==12, .(QuestionResponseOptionID, ResponseOption)],
             ResponseOption := i.ResponseOption,
             on = c("QuestionResponseOptionID")]

# order repsonses with other and just stay of greenways at end
mrg_dest[ResponseOption == "Not applicable - I just stay on the Greenway", ResponseOption := "Stay on Greenway"]
dest_levels <- mrg_dest[,.(Yes = sum(Yes, na.rm = TRUE)), by = ResponseOption][order(-Yes)][!ResponseOption %in% c("Other destination", "Stay on Greenway")]$ResponseOption
mrg_dest[, ResponseOption := factor(ResponseOption, levels = c(dest_levels, "Other destination", "Stay on Greenway"))]
mrg_dest[, DestType := ifelse(ResponseOption == "Stay on Greenway", "Stay on Greenway", "Destination")]
mrg_dest[,.(Yes = sum(Yes, na.rm = TRUE)), keyby = .(ResponseOption, DestType)]

# 13. Are there any barriers or safety issues that discourage you from using the Greenway to get to local destinations by foot or bike? Please describe below.
cols <- names(mrg)[grep("S2_P4_T0_Q2_Trail_Destinations_CODED", names(mrg))]
mrg_dest_qual <- melt(mrg[,c("VisitId", cols), with=FALSE],
                     id.vars = "VisitId",
                     variable.name = "QuestionResponseOptionID",
                     value.name = "Code")
mrg_dest_qual[codes_destinations[,.(Code, Description)],
             CodedResponse := i.Description,
             on = c("Code")]

mrg_dest_qual[!is.na(Code),.(Yes = .N), keyby = CodedResponse][order(-Yes)][]

# 14. Do you think the Mascoma River Greenway is a valuable community resource? Why or why not? Please explain below.
cols <- names(mrg)[grep("S2_P5_T0_Q1_Community_Value_1_CODED", names(mrg))]
mrg_comm_val <- melt(mrg[,c("VisitId", cols), with=FALSE],
                      id.vars = c("VisitId", "S2_P5_T0_Q1_Community_Value_1_CODED_YES/NO/MIXED"),
                      variable.name = "QuestionResponseOptionID",
                      value.name = "Code")
mrg_comm_val[codes_comm_value[,.(Code, Description)],
              CodedResponse := i.Description,
              on = c("Code")]

mrg_comm_val[!is.na(Code),.(Yes = .N), keyby = CodedResponse][order(-Yes)][]

# 18. Greenway Budget
cols <- names(mrg)[grep("S4_", names(mrg))]
mrg_budget <- melt(mrg[,c("VisitId", cols), with=FALSE],
                 id.vars = "VisitId",
                 variable.name = "QuestionResponseOptionID",
                 value.name = "Amount")
mrg_budget[descr[QuestionNum==18, .(QuestionResponseOptionID, ResponseOption)],
         ResponseOption := i.ResponseOption,
         on = c("QuestionResponseOptionID")]


# order repsonses with other and just stay of greenways at end
mrg_budget[ResponseOption == "Add more signage along the Greenway", ResponseOption := "Add more signage"]
budget_levels <- mrg_budget[,.(Amount = sum(Amount, na.rm = TRUE)), by = ResponseOption][order(-Amount)][!ResponseOption %in% c("Unallocated budget")]$ResponseOption
mrg_budget[, ResponseOption := factor(ResponseOption, levels = c(budget_levels, "Unallocated budget"))]

mrg_budget[,.(Amount = sum(Amount, na.rm = TRUE)), keyby = ResponseOption][order(-Amount)][]

# 19. How did you hear about the Greenway ?
mrg[,.N, keyby = XIT_Custom4][order(-N)]

# 20. Volunteering
mrg[,.N, keyby = XIT_Custom5][order(-N)]

### Analysis of map responses

access_shp <- st_read(file.path("mapping", "current access symbols.shp"))
access_shp$Respondents <- as.integer(as.character(access_shp$size))
access_shp$AccessType <-  c("Public", "Private", "Public (Pending)")[match(access_shp$Public, c("Y", "N", "P"))]
mrg_access <- as.data.table(access_shp)

desired_shp <- st_read(file.path("mapping", "desired access symbols.shp"))
desired_shp$Respondents <- desired_shp$size
desired_shp$DesiredOrBarrier <- "Desired Access"
  
mrg_desired <- as.data.table(desired_shp)

barriers_shp <- st_read(file.path("mapping", "access barrier symbols.shp"))
barriers_shp$Barrier <- barriers_shp$name
barriers_shp$Respondents <- barriers_shp$size
barriers_shp$DesiredOrBarrier <- "Access Barrier"

desired_barriers_shp <- rbind(desired_shp[!is.na(desired_shp$Respondents),c("id", "name", "Respondents", "DesiredOrBarrier")],
                          barriers_shp[!is.na(barriers_shp$Respondents),c("id", "name", "Respondents", "DesiredOrBarrier")])

### Count data analysis
count_mrg <- rbind(data.table(count_east_89)[,.(Count = sum(Count)), by = .(Date, Day, Weekend, Hour)][, Location := "East of I-89"],
                   data.table(count_price_chopper)[,.(Date, Day, Count, Weekend, Hour)][, Location := "Price Chopper"])
count_mrg[, Date := as.Date(paste0("20",Date))]

# correct the weekend field
count_mrg[, Weekday := weekdays(Date)]
count_mrg[, Weekend := ifelse(Weekday %in% c("Saturday", "Sunday"),"Weekend","Monday-Friday")]

# Remove any records after August 12
count_mrg <- count_mrg[Date < "2020-08-13"]

# add any missing hourly records
count_mrg <- melt(dcast.data.table(count_mrg, 
                                   Date + Day + Weekday + Weekend + Hour ~ Location, 
                                   value.var = "Count", 
                                   fill = 0),
                  id.vars = c("Date", "Day", "Weekday", "Weekend", "Hour"),
                  variable.name = "Location",
                  value.name = "Count")

count_mrg[, Users := Count * count_factor]                   
count_mrg[, UsersEst := Users * count_calibration]

# add weather conditions for the hour
count_weather  <- data.table(count_weather)
count_weather[, Date := as.Date(Date, origin = "1899-12-30")]
count_weather[, Hour := round(Time * 24) - 1]
count_weather[`Precip.`==unique(count_weather$`Precip.`)[1]]
count_weather[, Precip := as.numeric(lapply(strsplit(x = `Precip.`, "", fixed = TRUE),"[[",3))/10]
count_weather[, Temperature := as.numeric(substr(Temperature,1,2))]
# Avererage temp and take max precip for each hour
count_weather_hour <- count_weather[,.(Temperature = round(mean(Temperature)), Precip = max(Precip)), keyby = .(Date, Hour)]

count_mrg[count_weather_hour, c("Temperature", "Precip") := .(i.Temperature, i.Precip), on = c("Date", "Hour")]

# Totals by day and location
count_mrg[,.(Count = sum(UsersEst), 
             Temp = round(mean(Temperature)), 
             MaxTemp = round(max(Temperature)), 
             Precip = sum(Precip), 
             HourlyRecords = .N), 
          by = .(Date, Weekday, Weekend, Location)]

count_mrg[,.(Count = sum(UsersEst)),
          by = .(Date, Location, Weekend)][,.(sum(Count)/.N), by = Location][]
count_mrg[,.(Count = sum(UsersEst)),
          by = .(Date, Location, Weekend)][,.(sum(Count)/.N), by = .(Location, Weekend)][]

# Calculate hourly averages by location and weekday/weekend
count_mrg[,.(Count = sum(UsersEst), 
             HourlyRecords = .N), 
          keyby = .(Hour, Weekend, Location)][, AvCount := Count/HourlyRecords][]