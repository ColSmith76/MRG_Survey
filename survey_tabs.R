# MRG Survey Analysis

# packages
library(data.table)
library(ggplot2)
library(openxlsx)

# helpers
# colors
rsgcolordf <- data.frame(red=c(246,0,99,186,117,255,82),
                         green=c(139,111,175,18,190,194,77),
                         blue=c(31,161,94,34,233,14,133),
                         colornames=c("orange","marine","leaf","cherry","sky","sunshine","violet"))

# Read in the data 
# spreadsheet is MRG_CrossTabs_analysis.xlsx
mrg.meta <- read.xlsx("MRG CrossTabs_analysis.xlsx", 
                      sheet = "Cleaned Data",
                      startRow = 2,
                      rows = 2:3)

mrg <- read.xlsx("MRG CrossTabs_analysis.xlsx", 
                 sheet = "Cleaned Data",
                 startRow = 4,
                 colNames = FALSE)

names(mrg) <- names(mrg.meta)
names(mrg)[7] <- "Municipality"
names(mrg)[8] <- "State"
mrg <- data.table(mrg)
setnames(mrg, "[user.added]", "OnlyOneBarrier")

descr <- read.xlsx("MRG CrossTabs_analysis.xlsx", 
                   sheet = "Question Key",
                   cols = 2:5)
names(descr) <- c("QuestionNum", "Question", "QuestionResponseOptionID", "ResponseOption")
descr <- data.table(descr)

# straight tabs to check data
# 1. Age
mrg[, Q_StandardAge := factor(Q_StandardAge, 
                              levels = c("25 and under","26 to 40","41 to 60","61 to 80","81 and over"))]
mrg[,.N, keyby = Q_StandardAge]

# 2. Municipality, State
mrg[,.N, keyby = .(Municipality, State)][order(-N)][]

# 3. Income
mrg[, XIT_Custom3 := factor(XIT_Custom3, 
                              levels = c("Less than 20000","20000 34999","35000 49999","50000 74999","75000 99999", "Over 100000", "Prefer not to say"))]
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

mrg_rec_use[,.(Yes = sum(Yes, na.rm = TRUE)), keyby = ResponseOption][order(-Yes)][]

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

mrg_nonrec_use[,.(Yes = sum(Yes, na.rm = TRUE)), keyby = ResponseOption][order(-Yes)][]

# 6. In the warmer months, about how often do you use the Greenway?
mrg[, S2_P1_T0_Q1_Seasonal_Use_1 := factor(S2_P1_T0_Q1_Seasonal_Use_1, 
                              levels = c("Almost every day","A few times a week","About once a week","About once a month","Once every few months","Never"))]
mrg[,.N, keyby = S2_P1_T0_Q1_Seasonal_Use_1]

# 7.  In the winter, about how often do you use the Greenway?
mrg[, S2_P1_T0_Q2_Seasonal_Use_2 := factor(S2_P1_T0_Q2_Seasonal_Use_2, 
                                           levels = c("Almost every day","A few times a week","About once a week","About once a month","Once every few months","Never"))]
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

# 10. How do you usually travel to the Greenway from your home?
mrg[, S2_P3_T0_Q1_Getting_to_the_Greenway_1 := factor(S2_P3_T0_Q1_Getting_to_the_Greenway_1, 
                                         levels = c("Walk","Bicycle","Public Transportation","Drive my car","Other"))]
mrg[,.N, keyby = S2_P3_T0_Q1_Getting_to_the_Greenway_1][order(-N)]

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

# 12. Do you currently use the Greenway to help you get to any of these destinations by foot or bike (check ALL that apply)?
cols <- names(mrg)[grep("S2_P4_T0_Q1", names(mrg))]
mrg_dest <- melt(mrg[,c("VisitId", cols), with=FALSE],
                     id.vars = "VisitId",
                     variable.name = "QuestionResponseOptionID",
                     value.name = "Yes")
mrg_dest[descr[QuestionNum==12, .(QuestionResponseOptionID, ResponseOption)],
             ResponseOption := i.ResponseOption,
             on = c("QuestionResponseOptionID")]

mrg_dest[,.(Yes = sum(Yes, na.rm = TRUE)), keyby = ResponseOption][order(-Yes)][]

# 18. Greenway Budget
cols <- names(mrg)[grep("S4_", names(mrg))]
mrg_budget <- melt(mrg[,c("VisitId", cols), with=FALSE],
                 id.vars = "VisitId",
                 variable.name = "QuestionResponseOptionID",
                 value.name = "Amount")
mrg_budget[descr[QuestionNum==18, .(QuestionResponseOptionID, ResponseOption)],
         ResponseOption := i.ResponseOption,
         on = c("QuestionResponseOptionID")]

mrg_budget[,.(Amount = sum(Amount, na.rm = TRUE)), keyby = ResponseOption][order(-Amount)][]

# 19. How did you hear about the Greenway ?
mrg[,.N, keyby = XIT_Custom4][order(-N)]

# 20. Volunteering
mrg[,.N, keyby = XIT_Custom5][order(-N)]
