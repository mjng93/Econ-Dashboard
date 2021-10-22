rm(list=ls())
library(shiny)
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(zoo)
library(quantmod)
library(rmarkdown)
# library(TTR)
# library(gridExtra)
# library(grid)
library(DT)
#library(kableExtra)
#library(tables)
#library(knitr)
library(rsconnect)
library(plotly)
#library(ggthemes)
#library(stargazer)
library(fredr)
library(devtools)
library(tidyquant)
library(tidyverse)
library(Quandl)
library(Riex)
library(dplyr)
library(cryptowatchR)



#test2

#Read in FRED Data

fredr_set_key("aec101831dfbd18b0d4bb0470ca4d74b")

unemp=fredr_series_observations(
  series_id = "UNRATE",
  observation_start = as.Date("1970-01-01"),
  frequency = "m",
  units = "chg"
)

weekly=c("ICSA","CCSA","WEI","STLFSI2",
         "TASACBW027SBOG",
         "OSEACBW027SBOG",
         "TOTCI",
         "RELACBW027SBOG",
         "CLSACBW027SBOG",
         "AOLACBW027SBOG",
         "CASACBW027SBOG",
         "H8B3092NCBA",
         "LCBACBW027SBOG",
         "H8B3053NCBA")

monthly=c("UNRATE","U6RATE","LNS11300060","LNS12300060","RSXFS","INDPRO","DGORDER","AMDMVS","NEWORDER","ANXAVS",
          "PCEC96","PCEDGC96","PCENDC96","PCESC96",
          "ALTSALES","WHLSLRIRSA","UMCSENT","HOUST","COMPUTSA","PERMIT","TTLCONS","TLNRESCONS","TLRESCONS","TLPRVCONS","PNRESCONS","PRRESCONS","HSN1F","EXHOSLUSM495S","CSUSHPISA","AHETPI","CPIAUCSL","CPILFESL","PCEPI","PCEPILFE","GEPUCURRENT","RPI","DSPIC96","PSAVERT","W875RX1","JTSJOL",
          "RSAOMV",
          "MRTSSM4413USS",
          "RSFHFS",
          "RSEAS",
          "RSBMGESD",
          "RSDBS",
          "RSGCS",
          "MRTSSM4453USS",
          "RSHPCS",
          "RSGASS",
          "RSCCAS",
          "MRTSSM44811USS",
          "MRTSSM44812USS",
          "MRTSSM44814USN",
          "MRTSSM4482USS",
          "RSSGHBMS",
          "RSDSELD",
          "RSGMS",
          "RSMSR",
          "RSNSR",
          "RSFSDP")
quarterly=c("ECOMSA","ECOMPCTSA","TDSP","GDPC1","A939RX0Q048SBEA","PCEC","GPDIC1","PNFIC1","NETEXC","GCEC1","A825RX1Q020SBEA","OPHNFB","OPHMFG","OPHPBS","RRVRUSQ156N","RHVRUSQ156N","RSAHORUSQ156S","DRSFRMACBS","DRCCLACBS","DRCLACBS","DRBLACBS")

params <- list(
  series_id = c(weekly,monthly,quarterly),
  frequency = c(rep("w",length(weekly)),rep("m",length(monthly)),rep("q",length(quarterly)))
  )


fred=as.data.frame(pmap_dfr(
  .l = params,
  .f = ~ fredr(series_id = .x, frequency = .y)
))

fred=as.data.frame(dcast(fred,date~series_id))
fred=rename(fred,c("Unemployment Insurance - Initial Claims"="ICSA",
                   "Unemployment Insurance - Continued Claims"="CCSA",
                   "NY Fed Weekly Economic Indicator"="WEI",
                   "STL Fed Financial Stress Index"="STLFSI2",
                   "All Commercial Banks - Treasury and Agency Securities"="TASACBW027SBOG",
                   "All Commercial Banks - Other Securities"="OSEACBW027SBOG",
                   "All Commercial Banks - C&I Loans"="TOTCI",
                   "All Commercial Banks - Real Estate Loans"="RELACBW027SBOG",
                   "All Commercial Banks - Consumer Loans"="CLSACBW027SBOG",
                   "All Commercial Banks - Allother Loans and Leases"="AOLACBW027SBOG",
                   "All Commercial Banks - Cash Assets"="CASACBW027SBOG",
                   "All Commercial Banks - Loans to Commercial Banks"="H8B3092NCBA",
                   "All Commercial Banks - Fed Funds Sold and reverse RPs"="LCBACBW027SBOG",
                   "All Commercial Banks - Other Assets (including trading)"="H8B3053NCBA",
                   "Real Personal Income (Ex. Transfers)"="W875RX1",
                   "Retail Sales - E-commerce"="ECOMSA",
                   "E-commerce Share of Retail"="ECOMPCTSA",
                   "Household Debt Service Ratio"="TDSP",
                   "Real GDP"="GDPC1",
                   "Real GDP Per Capita"="A939RX0Q048SBEA",
                   "PCE"="PCEC",
                   "Real Gross Investment"="GPDIC1",
                   "Nonresidential Fixed Investment"="PNFIC1",
                   "Net Exports"="NETEXC",
                   "Government Expenditures and Gross Investment"="GCEC1",
                   "Government Expenditures and Gross Investment - Federal Ex. Defense"="A825RX1Q020SBEA",
                   "Real Labor Productivity"="OPHNFB",
                   "Real Labor Productivity - Manufacturing"="OPHMFG",
                   "Real Labor Productivity - Business"="OPHPBS",
                   "Job Openings"="JTSJOL",
                   "Rental Vacancy Rate"="RRVRUSQ156N",
                   "Home Vacancy Rate"="RHVRUSQ156N",
                   "Home Ownership Rate"="RSAHORUSQ156S",
                   "Mortgage Delinquency"="DRSFRMACBS",
                   "Credit Card Delinquency"="DRCCLACBS",
                   "Consumer Loans Delinquency"="DRCLACBS",
                   "C&I Loans Delinquency"="DRBLACBS",
                   "Savings Rate"="PSAVERT",
                   "Real Disposable Income"="DSPIC96",
                   "Real Personal Income"="RPI",
                   "Global Policy Uncertainty Index"="GEPUCURRENT",
                   "Core PCE Price Index"="PCEPILFE",
                   "PCE Price Index"="PCEPI",
                   "Core CPI Price Index"="CPILFESL",
                   "CPI Price Index"="CPIAUCSL",
                   "Average Hourly Earnings (Production and Non-Supervisory Workers)"="AHETPI",
                   "S&P500/Case-Shiller Home Price Index"="CSUSHPISA",
                   "Existing Home Sales"="EXHOSLUSM495S",
                   "New Home Sales"="HSN1F",
                   "Private Construction Spending (Residential)"="PRRESCONS",
                   "Private Construction Spending (Non-Residential)"="PNRESCONS",
                   "Private Construction Spending"="TLPRVCONS",
                   "Construction Spending (Residential)"="TLRESCONS",
                   "Construction Spending (Non-Residential)"="TLNRESCONS",
                   "Construction Spending"="TTLCONS",
                   "Building Permits"="PERMIT",
                   "Housing Completions"="COMPUTSA",
                   "Housing Starts"="HOUST",
                   "Consumer Sentiment (UM)"="UMCSENT",
                   "Wholesale Inventory/Sales Ratio"="WHLSLRIRSA",
                   "Lightweight Vehicle Sales"="ALTSALES",
                   "Real PCE (Services)"="PCESC96",
                   "Real PCE (Nondurables)"="PCENDC96",
                   "Real PCE (Durables)"="PCEDGC96",
                   "Real PCE (Overall)"="PCEC96",
                   "Capital Goods - Shipments (Nondefense, ex. aircraft)"="ANXAVS",
                   "Capital Goods - New Orders (Nondefense, ex. aircraft)"="NEWORDER",
                   "Durable Goods - Shipments"="AMDMVS",
                   "Durable Goods - New Orders"="DGORDER",
                   "Industrial Production"="INDPRO",
                   "Retail Sales (ex. Food Services)"="RSXFS",
                   "Employment Population Ratio (Age 25-54)"="LNS12300060",
                   "Labor Force Participation Rate (Age 25-54)"="LNS11300060",
                   "Unemployment Rate (U-6)"="U6RATE",
                   "Unemployment Rate (U-3)"="UNRATE",
                   "Retail Spending - Auto"="RSAOMV",
                   "Retail Spending - Auto Parts"="MRTSSM4413USS",
                   "Retail Spending - Furniture and Home Furnishings"="RSFHFS",
                   "Retail Spending - Electronics & Appliances"="RSEAS",
                   "Retail Spending - Building Materials & Garden"="RSBMGESD",
                   "Retail Spending - Food and Beverage Stores"="RSDBS",
                   "Retail Spending - Groceries"="RSGCS",
                   "Retail Spending - Alcohol"="MRTSSM4453USS",
                   "Retail Spending - Health & Personal Care"="RSHPCS",
                   "Retail Spending - Gasoline"="RSGASS",
                   "Retail Spending - Clothing"="RSCCAS",
                   "Retail Spending - Men's Clothing"="MRTSSM44811USS",
                   "Retail Spending - Women's Clothing"="MRTSSM44812USS",
                   "Retail Spending - Family Clothing"="MRTSSM44814USN",
                   "Retail Spending - Shoes"="MRTSSM4482USS",
                   "Retail Spending -  Sporting goods, hobby, musical
 instrument, & book stores"="RSSGHBMS",
                   "Retail Spending - Department Stores"="RSDSELD",
                   "Retail Spending - General Merchanise"="RSGMS",
                   "Retail Spending - Misc. Retailers"="RSMSR",
                   "Retail Spending - Nonstore Retailers"="RSNSR",
                   "Retail Spending - Food Services & Drinking Places"="RSFSDP"
                   ))
#colnames(fred)=c("date","real_personal_income_ex_transfers","savings_rate","real_disposable_income","real_personal_income,"global_policy_uncertainty","core_pce","pce,"core_cpi,"cpi,"ahe_prod_nonsup,"case_shiller_home_price_index,"existing_home_sales,"new_home_sales,"private_construction_spending_res","private_construction_spending_non_res","private_construction_spending","construction_spending_res","construction_spending_non_res","construction_spending","building_permits","housing_completions","housing_starts","um_c_sentiment","wholesale_inventory_sales_ratio","lw_vehicle_sales","real_pce_services","real_pce_nondurables","real_pce_durables","real_pce","capital_goods_shipments_nd_exair","capital_goods_new_orders_nd_exair","durable_goods_shipments","durable_goods_new_orders","industrial_production","retail_sales_exfood","prime_epop","prime_lfpr","u6","unemp")

fred$date=as.Date(fred$date)

#breakdown IP and PCE by sector

#Read in Asset Prices

# tickers = c("AAPL","NFLX","AMZN","MSFT","GOOG","FB","Z","TSLA","WMT","NKE", "W","CRM","DOCU","DIS","GRUB")
# 
# getSymbols(tickers,
#            from = "2000-01-01",
#            to = Sys.Date())
# prices <- map(tickers,function(x) Ad(get(x)))
# prices <- reduce(prices,merge)
# colnames(prices) <- tickers
# prices$date=row.names(prices)
# prices=as.data.frame(prices)

#Zillow Housing Prices : https://www.zillow.com/research/data/; download from this site and save as zvhi_monthyear.csv

zillow=read.csv("zhvi_sept2021.csv",stringsAsFactors = F)
zillow=subset(zillow,SizeRank<=35)
zillow=subset(zillow,select=-c(RegionID,RegionType,StateName,SizeRank))
zillow.long=reshape2::melt(zillow,id.vars=c("RegionName"))
zillow.long$variable=as.Date(gsub("X","",zillow.long$variable,ignore.case = T),format="%Y.%m.%d")
zillow.main=reshape2::dcast(zillow.long,variable~RegionName,value.var="value")
colnames(zillow.main)[1]="date"
zillow.main$date=as.Date(as.yearmon(zillow.main$date))

fred=merge(fred,zillow.main,all.x = TRUE,by="date")

#financial data 

Quandl.api_key("gR-_BnyE26mNqNkNAWvy")

t_yields = Quandl("USTREASURY/YIELD", type="raw",collapse="daily")

#stocks = Quandl.datatable('SHARADAR/SEP', date='2018-12-31,2018-12-28,2018-12-27', ticker='XOM,WMT')

library(Riex)
sk <- "sk_6aba1419eea9404a8049da7e5de57134"
x = c("TSLA","GOOGL","FB","AMZN","AI","QS","SFIX","BBWI","F","DG","NVDA")
r = "2y"

stocks = list()

for (i in 1:length(x)){

stocks[[i]] = as.data.frame(iex.chart(x[i], "1y", sk))
stocks[[i]][,"ticker"] = x[i]
stocks[[i]][,"date"] = row.names(stocks[[i]])
  
}

stocks_all = bind_rows(stocks, .id = "column_label")

cryptos = tolower(c("BTCUSD","ETHUSD","usdtusd","ADAUSD","XRPUSD","BCHUSD"))
crypto_names = c("Bitcoin","Ethereum","Tether","Cardano","XRP","Bitcoin Cash")

crypto = list()

for (i in 1:length(cryptos)){
  
  # crypto[[i]] = as.data.frame(crypto(cryptos[i], sk))
  # crypto[[i]][,"currency"] = crypto_names[i]
  # crypto[[i]][,"date"] = row.names(crypto[[i]])
  
  crypto[[i]] = get_ohlc(cryptos[i], periods = 86400, before = "2021-10-09",
                                       after = "2021-01-01", datetime = TRUE)
  crypto[[i]][,"currency"] = crypto_names[i]
  crypto[[i]][,"date"] = crypto[[i]][,"CloseTime"]
  
}

crypto_all = bind_rows(crypto, .id = "column_label")




