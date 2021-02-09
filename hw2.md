hw2
================

``` r
library(ggplot2)
library(dplyr)
library(tidyverse)
library("gapminder")
library(reshape2)
library(maps)
library(ggmap)
```

``` r
data <- read.csv(file = './data/fec_2008-2022.csv')

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

## Correlation

``` r
# Get correlation matrix
corr <- cor(num_data)

# Fill upper triangle to NaN values
corr[upper.tri(corr)] <- NA

# Melt to one to one dataframe
corr <- melt(corr)

# Part for sorting unused
# group_corr <- corr %>% 
#   drop_na() %>% 
#   group_by(Var2) %>% 
#   summarize(med_value= median(value))
# 
# group_corr
# 
# corr <- corr %>% 
#   left_join(group_corr, by="Var2")
# 
# corr <- corr[order(corr$med_val, corr$Var2),]
```

``` r
ggplot(data=corr, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile(color="white") +
  theme(text = element_text(size=12),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_gradient2(low="blue", mid="white", high="red", na.value = "grey90",
                       limit = c(-1,1), name="Pearson Correlation")
```

![](hw2_files/figure-gfm/fig1-1.png)<!-- -->

## Candidate VS Total\_Receipt

``` r
# Sort by Total_Receipt and get top 50 candidates
receipt_data = data[order(-data$Total_Receipt),]
receipt_data = receipt_data[0:50,]

ggplot(receipt_data, aes(x=reorder(Cand_Name,Total_Receipt), y=Total_Receipt)) +
  geom_point() +
  coord_flip() +
  theme(text = element_text(size=13, color="black"), axis.text.y=element_text(colour="black"))
```

![](hw2_files/figure-gfm/fig2-1.png)<!-- -->

## Party Affiliation Ratio by states

``` r
# Get party_ratio on each states
party_ratio_states <- data %>%
  group_by(Cand_State) %>%
  summarize(party_ratio = mean(Cand_Party_Affiliation_num)) %>% 
  select(c(Cand_State, party_ratio))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
head(party_ratio_states)
```

    ## # A tibble: 6 x 2
    ##   Cand_State party_ratio
    ##   <chr>            <dbl>
    ## 1 alabama        -0.230 
    ## 2 alaska         -0.104 
    ## 3 arizona        -0.124 
    ## 4 arkansas       -0.0739
    ## 5 california      0.0470
    ## 6 colorado       -0.0267

``` r
# Get map of united states consisting of longitude and latitude
states_map <- map_data("state")

head(states_map)
```

    ##        long      lat group order  region subregion
    ## 1 -87.46201 30.38968     1     1 alabama      <NA>
    ## 2 -87.48493 30.37249     1     2 alabama      <NA>
    ## 3 -87.52503 30.37249     1     3 alabama      <NA>
    ## 4 -87.53076 30.33239     1     4 alabama      <NA>
    ## 5 -87.57087 30.32665     1     5 alabama      <NA>
    ## 6 -87.58806 30.32665     1     6 alabama      <NA>

``` r
map_data <- left_join(party_ratio_states , states_map, by=c("Cand_State" = "region"))

head(map_data)
```

    ## # A tibble: 6 x 7
    ##   Cand_State party_ratio  long   lat group order subregion
    ##   <chr>            <dbl> <dbl> <dbl> <dbl> <int> <chr>    
    ## 1 alabama         -0.230 -87.5  30.4     1     1 <NA>     
    ## 2 alabama         -0.230 -87.5  30.4     1     2 <NA>     
    ## 3 alabama         -0.230 -87.5  30.4     1     3 <NA>     
    ## 4 alabama         -0.230 -87.5  30.3     1     4 <NA>     
    ## 5 alabama         -0.230 -87.6  30.3     1     5 <NA>     
    ## 6 alabama         -0.230 -87.6  30.3     1     6 <NA>

``` r
ggplot(map_data, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = party_ratio), color = "white") +
    scale_fill_gradient2(limit = c(-1,1),low="blue", mid="white", high="red", 
                         na.value = "grey90", name="Party Ratio")
```

![](hw2_files/figure-gfm/fig2.5-1.png)<!-- -->

## Party Affiliations and their numbers

``` r
data2 <- data %>% 
  group_by(Cand_Party_Affiliation) %>% 
  mutate(count=n())

ggplot(data2, aes(x=reorder(Cand_Party_Affiliation, count), y=count)) +
  geom_bar(stat="identity", fill="skyblue") +
  coord_flip()
```

![](hw2_files/figure-gfm/fig3-1.png)<!-- -->
