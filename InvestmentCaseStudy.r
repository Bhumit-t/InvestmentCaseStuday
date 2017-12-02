companies <- read.delim("companies.txt", header = TRUE, na.strings=c("","NA") )
rounds2 <- read.csv("rounds2.csv", stringsAsFactors = FALSE, na.strings=c("","NA"))

#Table 1.1: Understand the Data Set
#1. How many unique companies are present in rounds2?
#Method1: Using Dplyr: 
count(distinct(rounds2, company_permalink))

#Method2: Without using any external packages: 
length(unique(rounds2$company_permalink))


#2. How many unique companies are present in the companies file?
# Understanding is permalink link coulmn in companes is the primary key. 
# Therefore number of rows equals the unique companies
#Method1: Using Dplyr: 
count(distinct(companies, name))

#Method2: Without using any external packages: 
length(unique(companies$name))

#5.Merge the two data frames so that all  variables (columns) 
#in the companies frame are added to the rounds2 data frame.
#Name the merged frame master_frame.
#How many observations are present in master_frame ?


names(companies)[names(companies) == "permalink"] <- "company_permalink"
companies$company_permalink <- tolower(companies$company_permalink)
rounds2$company_permalink <- tolower(rounds2$company_permalink)

master_frame <- merge(rounds2, companies, by="company_permalink")

#Table 2.1: Average Values of Investments for Each of these Funding Types
funding_type_groups <- group_by(master_frame, funding_round_type) 
summarise(funding_type_groups, mean(raised_amount_usd, na.rm = T))

#Table 3.1: 

venture <- subset(master_frame, funding_round_type  == "venture")

venture_country_groups <- group_by(venture, country_code) 
vcg_total_funding <- summarise(venture_country_groups, sum(raised_amount_usd, na.rm = T))

names(vcg_total_funding) <- c("country_code","total_funding_amt")
vcg_total_funding <- na.omit(vcg_total_funding)
arrange(top_n(vcg_total_funding, 9, total_funding_amt), desc(total_funding_amt))
