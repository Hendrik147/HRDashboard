

source("MySummaryFunction.R")

HRDashboarddataset <- read_csv("HRDashboarddataset.csv", 
                               col_types = cols(Year = col_integer(), `Base Salary` = col_number(), 
                                                Bonus = col_number(), Commission = col_number(), 
                                                `Hire Date` = col_date(format = "%d/%m/%Y"), 
                                                Overtime = col_number(), `Termination Date` = col_date(format = "%d/%m/%Y"), 
                                                `Total Compensation` = col_number(), 
                                                X17 = col_skip(), X18 = col_skip()))

GroupbyFields <- HRDashboarddataset %>% select(Location,State, "Employment Type", Year, Department, `PTO Days`, `Sick Days`, `Performance Score`) %>% names()
NumericFields <- HRDashboarddataset %>% select_if(is.numeric) %>% names()
StaffMembers <- unique(HRDashboarddataset$`Full Name`)

HRDashboardsumyear <- HRDashboarddataset %>% group_by(Year) %>% select_if(is.numeric) %>% summarise_all(funs(if(is.numeric(.)) sum(., na.rm = TRUE) else first(.)))

HRDashboardsumyear2 <- HRDashboardsumyear[1:9,1:8]
colnames(HRDashboardsumyear2)[2] <- "Total Salaries"
colnames(HRDashboardsumyear2)[3] <- "Total Bonuses"
colnames(HRDashboardsumyear2)[4] <- "Total Overtimes"
colnames(HRDashboardsumyear2)[5] <- "Total Commissions"
colnames(HRDashboardsumyear2)[6] <- "Total Compensations"
colnames(HRDashboardsumyear2)[7] <- "Total PTO Days"
colnames(HRDashboardsumyear2)[8] <- "Total Total Sick Days"

HRDashboardDistinctEmployeesbyyear <- HRDashboarddataset %>% group_by(Year) %>% mutate(count = n_distinct(`Full Name`))

HRDashboardaverageyear <- HRDashboardDistinctEmployeesbyyear %>% group_by(Year) %>% select_if(is.numeric) %>% summarise_all(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.)))

colnames(HRDashboardaverageyear)[10] <- "Total Employees"
HRDashboardaverageyear2 <- HRDashboardaverageyear[1:9,c(1,2,8,10)]

colnames(HRDashboardaverageyear2)[2] <- "Average Salary"
HRDashboardaverageyear2$`Average Salary`  <- format(HRDashboardaverageyear2$`Average Salary` , round=2)
colnames(HRDashboardaverageyear2)[3] <- "Average Sick Days per Emp."
HRDashboardaverageyear2$`Average Sick Days per Emp.`  <- format(round(as.numeric(HRDashboardaverageyear2$`Average Sick Days per Emp.`),2), nsmall =2)

HRDashboarddatafinal <- left_join(HRDashboardsumyear2,HRDashboardaverageyear2,by="Year")

HRDashboarddatafinaltransposed <- as.data.frame(t(HRDashboarddatafinal))

select(HRDashboarddatafinaltransposed,V1)
