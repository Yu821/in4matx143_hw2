143\_hw2\_HH
================
Hien Huynh
2/13/2021

``` r
#install.packages("reshape")
#install.packages("maps")
#install.packages("ggmap")
#install.packages("ddply")
```

``` r
library(ggplot2)
library(dplyr)
library(tidyverse)
library("gapminder")
library(reshape2)
library(maps)
library(ggmap)
library(reshape)
library(maps)
library(lubridate)
library(plyr)
```

``` r
data <- read.csv(file = 'C:/Users/HOA PHAT/Downloads/uci/in4matx 143/Assignments/hw2/us_politics_EDA-main/data/fec_2008-2022.csv')

data <- data %>% 
  select(-c(X))

# Column with only numerical and int data type
num_data <- data %>% 
  select(where(is.numeric)) %>% 
  select(-c(Cand_Office_Dist, Cand_Zip))

# Convert party affiliation to number -1 to 1 to use later for visualization
data$Cand_State <- tolower(state.name[match(data$Cand_State, state.abb)])

data <- data %>% mutate(
  Cand_Party_Affiliation_num = case_when(Cand_Party_Affiliation=="DEM" ~ 1,
                                            Cand_Party_Affiliation=="REP" ~ -1,
            Cand_Party_Affiliation!="REP" | Cand_Party_Affiliation!="DEM" ~ 0))


# Check data
str(data)
```

    ## 'data.frame':    26828 obs. of  51 variables:
    ##  $ Link_Image                          : chr  "https://www.fec.gov/data/candidate/P80003338/?cycle=2008" "https://www.fec.gov/data/candidate/H8FL05116/?cycle=2008" "https://www.fec.gov/data/candidate/H8FL06056/?cycle=2008" "https://www.fec.gov/data/candidate/H8FL06080/?cycle=2008" ...
    ##  $ Cand_Name                           : chr  "OBAMA, BARACK" "CASTAGNERO, CAROL" "STEARNS, CLIFFORD B" "CUNHA, TIMOTHY M" ...
    ##  $ Cand_Id                             : chr  "P80003338" "H8FL05116" "H8FL06056" "H8FL06080" ...
    ##  $ Cand_Office                         : chr  "P" "H" "H" "H" ...
    ##  $ Cand_Office_St                      : chr  "US" "FL" "FL" "FL" ...
    ##  $ Cand_Office_Dist                    : int  0 5 6 6 7 7 8 8 8 8 ...
    ##  $ Cand_Party_Affiliation              : chr  "DEM" "DEM" "REP" "DEM" ...
    ##  $ Cand_Incumbent_Challenger_Open_Seat : chr  "OPEN" "CHALLENGER" "INCUMBENT" "CHALLENGER" ...
    ##  $ Total_Receipt                       : num  7.79e+08 0.00 8.06e+05 2.50e+05 3.06e+04 ...
    ##  $ Total_Disbursement                  : num  7.60e+08 2.76e+02 7.90e+05 2.50e+05 2.97e+04 ...
    ##  $ Cash_On_Hand_COP                    : num  18272367 0 2272965 0 0 ...
    ##  $ Debt_Owed_By_Committee              : num  434954 0 0 0 16245 ...
    ##  $ Coverage_End_Date                   : chr  "12/31/2008" "09/30/2008" "12/31/2008" "12/31/2008" ...
    ##  $ Cand_Street_1                       : chr  "PO Box 8102" "4119 GLISSON DR" "2515 SE Ashley Court" "2433 SE 20TH CIRCLE" ...
    ##  $ Cand_Street_2                       : chr  "" "" "" "PO BOX 6546" ...
    ##  $ Cand_City                           : chr  "CHICAGO" "LAKELAND" "Ocala" "OCALA" ...
    ##  $ Cand_State                          : chr  "illinois" "florida" "florida" "florida" ...
    ##  $ Cand_Zip                            : int  60680 33810 34471 34478 32174 32260 32839 34787 32773 32806 ...
    ##  $ Individual_Itemized_Contribution    : num  4.30e+08 0.00 1.50e+05 8.77e+04 8.94e+03 ...
    ##  $ Individual_Unitemized_Contribution  : num  0 0 62366 13973 1115 ...
    ##  $ Individual_Contribution             : num  6.65e+08 0.00 2.12e+05 1.02e+05 1.16e+04 ...
    ##  $ Other_Committee_Contribution        : num  12925 0 372989 21350 0 ...
    ##  $ Party_Committee_Contribution        : num  1150 0 0 0 0 0 0 0 0 0 ...
    ##  $ Cand_Contribution                   : num  0 0 0 5701 1810 ...
    ##  $ Total_Contribution                  : num  6.65e+08 0.00 5.85e+05 1.29e+05 1.34e+04 ...
    ##  $ Transfer_From_Other_Auth_Committee  : num  8.7e+07 0.0 0.0 0.0 0.0 ...
    ##  $ Cand_Loan                           : num  0 0 0 121000 15445 ...
    ##  $ Other_Loan                          : num  0 0 0 0 1700 0 0 0 0 0 ...
    ##  $ Total_Loan                          : num  0 0 0 121000 17145 ...
    ##  $ Offsets_To_Operating_Expenditure    : num  25131132 276 11616 0 9 ...
    ##  $ Offsets_To_Fundraising              : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Offsets_To_Leagal_Accounting        : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Other_Receipts                      : num  1675373 0 209959 0 0 ...
    ##  $ Operating_Expenditure               : num  7.07e+08 1.22e+04 5.22e+05 1.50e+05 2.87e+04 ...
    ##  $ Exempt_Legal_Accounting_Disbursement: num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Fundraising_Disbursement            : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Transfer_To_Other_Auth_Committee    : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Cand_Loan_Repayment                 : num  0 276 0 100016 0 ...
    ##  $ Other_Loan_Repayment                : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Total_Loan_Repayment                : num  0 276 0 100016 0 ...
    ##  $ Individual_Refund                   : num  5744310 0 300 25 0 ...
    ##  $ Party_Committee_Refund              : num  300 0 0 0 0 0 0 0 0 0 ...
    ##  $ Other_Committee_Refund              : num  11345 0 0 0 0 ...
    ##  $ Total_Contribution_Refund           : num  5755955 0 300 25 0 ...
    ##  $ Other_Disbursements                 : num  47945663 0 267040 0 1013 ...
    ##  $ Net_Contribution                    : num  4.31e+09 0.00 5.85e+05 1.29e+05 1.34e+04 ...
    ##  $ Net_Operating_Expenditure           : num  3.72e+09 0.00 5.11e+05 1.50e+05 2.86e+04 ...
    ##  $ Cash_On_Hand_BOP                    : num  0 0 2258098 0 0 ...
    ##  $ Debt_Owe_To_Committee               : num  0 0 0 0 0 0 0 0 NA 0 ...
    ##  $ Coverage_Start_Date                 : chr  "01/01/2007" "07/01/2008" "01/01/2007" "04/01/2008" ...
    ##  $ Cand_Party_Affiliation_num          : num  1 1 -1 1 1 1 1 1 1 1 ...

## \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*

## Information 5: The total receipt of final President candidates of DEM and REP in 2008, 2012, 2016 and 2020

## \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*

``` r
# create new dataset with 4 columns from data
df = data[data$Cand_Office %in% c("P"),]
df = df[,c('Cand_Name', 'Total_Receipt', 'Coverage_End_Date', 'Cand_Party_Affiliation')]
df = df[df$Cand_Name %in% c("OBAMA, BARACK", "MCCAIN, JOHN S.", "ROMNEY, MITT / RYAN, PAUL D. ", "TRUMP, DONALD J.", "TRUMP, DONALD J. / MICHAEL R. PENCE ", "CLINTON, HILLARY RODHAM / TIMOTHY MICHAEL KAINE", "BIDEN, JOSEPH R JR"),]

# remove 1 Biden 2008 row, which is not related to the President Running Year
df = df[df$Total_Receipt > 15000000,]

# Extract Year from date
df$Coverage_End_Date = year(as.Date(df$Coverage_End_Date, format = "%m/%d/%Y"))

## Add column Winner Loser to df
df5 = data.frame(append(df, c(result='z')))

# change values in result column 
df5$result = ifelse (df5$Coverage_End_Date == 2008 & df5$Cand_Party_Affiliation == "DEM", "winner", df5$result)
df5$result = ifelse (df5$Coverage_End_Date == 2008 & df5$Cand_Party_Affiliation == "REP", "loser", df5$result)
df5$result = ifelse (df5$Coverage_End_Date == 2012 & df5$Cand_Party_Affiliation == "REP", "loser", df5$result)
df5$result = ifelse (df5$Coverage_End_Date == 2012 & df5$Cand_Party_Affiliation == "DEM", "winner", df5$result)
df5$result = ifelse (df5$Coverage_End_Date == 2016 & df5$Cand_Party_Affiliation == "REP", "winner", df5$result)
df5$result = ifelse (df5$Coverage_End_Date == 2016 & df5$Cand_Party_Affiliation == "DEM", "loser", df5$result)
df5$result = ifelse (df5$Coverage_End_Date == 2020 & df5$Cand_Party_Affiliation == "REP", "loser", df5$result)
df5$result = ifelse (df5$Coverage_End_Date == 2020 & df5$Cand_Party_Affiliation == "DEM", "winner", df5$result)

## plot grouped bar based on result
ggplot(df5, aes(factor(Coverage_End_Date), Total_Receipt, fill = result)) + 
  geom_bar(stat="identity", position = "dodge") + 
  geom_text(aes(label=Cand_Party_Affiliation), vjust=1.6, color="white", position=position_dodge(width=1), size=3.5)+
  scale_fill_brewer(palette = "Set1") + 
  ggtitle("Total_Receipt of final Presidential Candidates form 2008 to 2020") +
  xlab("Year") + ylab("Total_Receipt")
```

![](Hw2_part1_HH_1_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

##### Findings in information 5: DEM final candidate usually has the larger amounts of total\_receipt than REP final candidate in the year of running President. This cannot be a standard to guarantee the winner, because in the year 2016, the winner is REP candidate, while DEM candidate still had a larger total\_receipt.

## \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*

## Information 6: Donations and contributions from DEM and REP Parties from 2008 to 2020

## \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*

``` r
# create new dataset with 4 columns from data
df_donate1 = data[,c('Cand_Name', 'Coverage_End_Date', 'Cand_Party_Affiliation', 'Other_Disbursements')]

# Extract Year from date
df_donate1$Coverage_End_Date = year(as.Date(df_donate1$Coverage_End_Date, format = "%m/%d/%Y"))

# remove missing values in coverage end date
df_donate1 = na.omit(df_donate1)

# replace all Parties which are not Dem or Rep to Others
df_donate1$Cand_Party_Affiliation = ifelse (df_donate1$Cand_Party_Affiliation != "REP" & df_donate1$Cand_Party_Affiliation != "DEM", "OTHERS", df_donate1$Cand_Party_Affiliation)

# remove years > 2020 and <= 2007
df_donate1 = df_donate1[df_donate1$Coverage_End_Date <= 2020,]
df_donate1 = df_donate1[df_donate1$Coverage_End_Date > 2007,]

# drop the Others Parties, it's because the other Parties are just a very small part compare to DEM and REP 
df_donate1 = df_donate1[df_donate1$Cand_Party_Affiliation != "OTHERS",]

## Plot for donations from Parties
ggplot(df_donate1, aes(factor(Coverage_End_Date), Other_Disbursements, fill = Cand_Party_Affiliation)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1") + 
  ggtitle("Donations and contributions from DEM and REP Parties from 2008 to 2020") +
  xlab("Year") + ylab("Other_disburshments")
```

![](Hw2_part1_HH_1_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

##### Findings in information 6: 1. In general, both Parties contributed the significant amounts in the years of running President. 2. In Information 5, I saw the amount of total\_receipt of DEM final President candidates are larger than REP final President Candidates. COmpare with the plot in Information 6, DEM Party usually pays more in donations and contributions.

## \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*

## Information 7: Parties control House of Representative from 2007 to 2020

## \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*

``` r
# create new dataset with 4 columns from data
df_House = data[,c('Cand_Name', 'Coverage_End_Date', 'Cand_Party_Affiliation', 'Cand_Office')]

# Extract Year from date
df_House$Coverage_End_Date = year(as.Date(df_House$Coverage_End_Date, format = "%m/%d/%Y"))

# remove missing values in coverage end date
df_House = na.omit(df_House)

# remove years > 2020 and <= 2007
df_House = df_House[df_House$Coverage_End_Date <= 2020,]
df_House = df_House[df_House$Coverage_End_Date >2007,]
df_House = df_House[df_House$Cand_Office == "H",] # Pick the House Ofice

# remove cand_office not Presidential > 2020
df_House = df_House[df_House$Cand_Party_Affiliation == "REP" | df_House$Cand_Party_Affiliation == "DEM",]

# Count the frequency ò Parties in the matching year
counts <- ddply(df_House, .(df_House$Cand_Party_Affiliation, df_House$Coverage_End_Date), nrow)
names(counts) <- c("Parties", "Year", "Freq")


# plot
ggplot(data=counts, aes(x=Year, y=Freq, group = Parties, colour = Parties)) +
     geom_line() +
     geom_point() + 
    ggtitle("Parties control House of Representative from 2007 to 2020") +
    xlab("Year") + ylab("Freq")
```

![](Hw2_part1_HH_1_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

##### Findings in information 7: In 2018, it had a general elections, the DEM gain a majority in the U.S. House. Before that, REP controled the House from 2011 to 2018. In range of analyzing, DEM last controled the House before 2018 election was before 2011. There is no certain that DEM or REP will always be the one controls the House. The plot, I think, reflects the fair elections. The result of who gains the majority in the House based on the campaign, the situation, etc. at that time.

## \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*

## Information 8: Compare both kinds of Individual contribution of DEM within 2020

## \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*

``` r
# Create data
df_dem1 = data[,c('Coverage_End_Date', 'Cand_Party_Affiliation', 'Individual_Unitemized_Contribution', 'Individual_Itemized_Contribution')]
df_dem1 <- df_dem1[df_dem1$Cand_Party_Affiliation %in% c("DEM"),]
df_dem1 <- df_dem1[,c('Coverage_End_Date', 'Individual_Unitemized_Contribution', 'Individual_Itemized_Contribution')]
df_dem1$Coverage_End_Date <- as.Date(df_dem1$Coverage_End_Date, "%m/%d/%Y")

# select year 2020
df_dem1 <- df_dem1[df_dem1$Coverage_End_Date >= "2020-01-01" & df_dem1$Coverage_End_Date <= "2020-12-31",]
df_dem1$Coverage_End_Date <- format(as.Date(df_dem1$Coverage_End_Date), "%m")

df_dem1 = na.omit(df_dem1)

# Sum amounts by month
df_dem1 <- aggregate(select(df_dem1, "Individual_Unitemized_Contribution", "Individual_Itemized_Contribution"), by=df_dem1["Coverage_End_Date"], sum)

# change dataframe to melt
df_dem_Melted <- melt(df_dem1, "Coverage_End_Date")

# Plot
ggplot(df_dem_Melted, aes(factor(Coverage_End_Date), value, fill = variable)) + 
  geom_bar(stat="identity", position = "dodge") + 
  ggtitle("Two kinds of Individual contribution of DEM within 2020") +
  xlab("Year") + ylab("Amount")
```

![](Hw2_part1_HH_1_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

##### Findings in information 8: The plot shows the contributions of individuals for DEM in 2020. I notice that the amount becomes significant on September, larger in November and decrease but still large on December. This can be explained that on September, the running for President position was pushed heavier than months before. Then on November, the amount became huge when election happened. The amount decrase more than half in December but still large because the remain campaign.

## \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*

## \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*

## Part 2

## \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*

## \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*

#### Hypothesis: the amount collection is significant in the circle of 2 years, and larger in the year of running President

``` r
# Create data
df_part2 = data[,c('Coverage_End_Date', 'Cand_Party_Affiliation', 'Total_Receipt')]

# Extract Year from date
df_part2$Coverage_End_Date = year(as.Date(df_part2$Coverage_End_Date, format = "%m/%d/%Y"))

# remove missing values in coverage end date
df_part2 = na.omit(df_part2)

# remove years > 2020 and <= 2007
df_part2 = df_part2[df_part2$Coverage_End_Date <= 2020,]
df_part2 = df_part2[df_part2$Coverage_End_Date >2007,]

df_part2 <- df_part2[df_part2$Cand_Party_Affiliation %in% c("DEM", "REP"),]
df_part2 = na.omit(df_part2)

# Sum amounts by year
df_part2 <- aggregate(select(df_part2, "Total_Receipt"), by= list(df_part2$Coverage_End_Date, df_part2$Cand_Party_Affiliation), sum)

# Plot
ggplot(df_part2, aes(factor(Group.1), Total_Receipt, fill = Group.2)) + 
  geom_bar(stat="identity", position = "dodge") + 
  ggtitle("Total_receipt of DEM and REP from 2008 to 2020") +
  xlab("Year") + ylab("Amount") + 
  guides(fill=guide_legend(title="Parties"))
```

![](Hw2_part1_HH_1_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

#### We can hardly see the very small amounts of total\_receipt on years 2009, 2011, 2013, 2015, 2017, and 2019 (they are mostly invisible when coparing with the amounts come from other years in range). In 2008, we had the Presidential election and House of Representative elections. In 2010, we had Senate elections and House of Representative elections. In 2012, we had the Presidential election and House of Representative elections. In 2014, we have some seats in Senate were up for election and House of Representative elections. In 2016, we had Presidential election, House of Representative elections and Senate elections. In 2018, we had Federal elecions in House of Representative and some seats in Senate were up for eletions. And in 2020, we have the Presidential election, House of Representative elections and some seats in Senate were up for elections.

#### There was no important elections in 2009, 2011, 2013, 2015, 2017 and 2019.

#### The plot shows that the Parties (here uses DEM and REP) usually collect and use money the most in the circle of 2 years, which occurs the elections in Presidential position, Senate and House.
