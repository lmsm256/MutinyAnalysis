rm(list = ls())
source("https://raw.githubusercontent.com/lmsm256/MutinyAnalysis/refs/heads/main/building_coup_data.R")

#------------------------------------------------------------------------------------------------#
#Pulling in Vars
#------------------------------------------------------------------------------------------------#  
#vdem polyarchy
#vdem polyarchy squared
#(we might play around with the REIGN regime categories, but I'll need to clean that up a bit, too)
#GDP per capita
#change in gdp per capita
#ongoing civil conflict
#ongoing international conflict (or time since MID or whatever we had)
#trade (probably the trade_glob version)
#military regime
#military size/personnel
#military expenditures (per soldier is fine, but we can talk about option on this)
#time since last coup attempt (and probably squared and cubed and/or splines)
#time since last mutiny (and probably squared and cubed and/or splines)
#leader time in power
#cold war

#------------------------------------------------------------------------------------------------#
#Vdem; polyarchy, polyarchy squared, regime
#------------------------------------------------------------------------------------------------#  
vdem_og <- vdem
vdem <- vdem %>%
  subset(select = c(country_name, # Country. 
                    e_regionpol_6C, # Region. 
                    year, # Year. 
                    v2x_regime, # Regime type. 
                    v2x_polyarchy, # Regime type. 
                    v2x_execorr, # Executive corruption index.
                    v2x_jucon, # Judicial constraints on the executive index ordinal
                    v2x_corr, # Political corruption. 
                    v2cacamps, # Political polarization.
                    v2x_civlib, # Civil liberties. 
                    v2x_rule, # Rule of law. 
                    v2xcs_ccsi, # Core civil society index. 
                    v2x_genpp, # Women political participation index. 
                    v2x_gender, # Women political empowerment index. 
                    v2x_gencl, # Women civil liberties.
                    e_pelifeex, # Life expectancy. 
                    e_gdp, # GDP. 
                    e_gdppc, # GDPPC. 
                    e_miinflat, # Inflation rate. 
                    e_cow_exports, # Exports. 
                    e_cow_imports, # Imports. 
                    v2smgovshut, # Government Internet shut down in practice.
                    v2smgovfilprc, # Government  Internet filtering in practice, 
                    v2smfordom)) # Foreign governments dissemination of false information. 
'Assume these columns are clear of NAs unless stated otherwise.'

vdem <- vdem %>%
  rename(country = country_name,
         region = e_regionpol_6C,
         year = year, 
         regime = v2x_regime, 
         regime2 = v2x_polyarchy,
         exec_corr = v2x_execorr, 
         jud_const = v2x_jucon, 
         pol_corr = v2x_corr, 
         civ_lib = v2x_civlib, 
         pol_polar = v2cacamps, 
         law_rule = v2x_rule, # Ditto Timor-Leste. 
         civ_soc = v2xcs_ccsi, 
         wom_polpart = v2x_genpp, # Lots of issues... run: na_df <- vdem[is.na(vdem$wom_polpart), , drop = FALSE]
         women_polemp = v2x_gender, # Run: na_df <- vdem[is.na(vdem$v2x_gender), , drop = FALSE]
         wom_civlib = v2x_gencl, # No issues--could replace wom_polpart and women_polemp 
         life_exp = e_pelifeex, # Between 1800 to 2022; missing South Yemen, Republic of Vietnam (1950-75), Kosovo, German Democratic Republic, Palestine/Gaza, Somaliland, Hong Kong, Zanzibar 
         gdp = e_gdp, # Between 1789 to 2019... good for past data, might need something else for current data. 
         gdppc = e_gdppc, # Ditto GDP. 
         infla_rate = e_miinflat, # Ditto GDP. 
         exports = e_cow_exports, # Between 1870 to 2014.
         imports = e_cow_imports, # Between 1870 to 2014.
         int_shutdown = v2smgovshut, # Between 2000 to 2023... can likely code this to be 0 for previous years. 
         int_censor = v2smgovfilprc, # Between 2000 to 2023.
         forgov_misinfo = v2smfordom) %>% # Between 2000 to 2023.
  filter(year >= 1949)
vdem <- vdem %>% # Merging in ccodes. 
  left_join(ccodes, by = c("year", "country")) %>%
  filter(!is.na(ccode)) # Non-state actors. 

#-------------------polyarchy; polyarchy2; from Vdem------------------------#

vdem_regime2 <- vdem %>% 
  subset(select = c(country, ccode, year, regime2)) %>% 
  rename(polyarchy=regime2) %>%
  filter(!(country == "Kazakhstan" & year == 1990)) %>% # Kazakhstan became independent this year.
  filter(!(country == "Turkmenistan" & year == 1990)) %>% # Turkmenistan became independent this year. 
  subset(select = -c(country)) %>%
  mutate(year=year+1) %>% #just lagged
  mutate(polyarchy2=polyarchy*polyarchy) %>%
  set_variable_labels(
    polyarchy="v2x_polyarchy, vdem, t-1",
    polyarchy2="v2x_polyarchy^2, vdem, t-1") %>%
  mutate(month=12)

#merge to baseline
base_data <- base_data %>% 
  left_join(vdem_regime2, by = c("ccode", "year", "month")) %>%
  arrange(ccode, year, month) %>%
  group_by(ccode) %>%
  arrange(ccode, year, month) %>%
  fill(polyarchy, .direction="updown") %>%
  fill(polyarchy2, .direction="updown") %>%
  ungroup()


#---------------------------Regime type (v2x_regime); from Vdem-----------------------------#  

#Reading in data. Cleaning it up.
regime_type <- vdem %>%
  subset(select = c(country, year, regime)) %>%
  rename(regime_type = regime) %>%
  mutate(year=year+1) %>% #just lagged
  filter(year >= 1950,
         year < 2026)
regime_type <- regime_type %>% 
  left_join(ccodes, by = c("country", "year")) %>% # NAs resulting from state-like actors, not full states.  
  subset(select = -c(country))   %>% # To prevent future duplicated columns. 
  drop_na() %>%
  distinct() # No duplicates
label(regime_type$regime_type) <- "0 = Closed autocracy, 1 = Electoral autocracy, 2 = Electoral democracy, 3 = Liberal Democracy"

#Organizing variables for regression 
regime_type <- regime_type %>%
  mutate(
    closed_autocracy = ifelse(regime_type == 0, 1, 0),
    electoral_autocracy = ifelse(regime_type == 1, 1, 0),
    electoral_democracy = ifelse(regime_type == 2, 1, 0),
    liberal_democracy = ifelse(regime_type == 3, 1, 0)
  ) %>%
  set_variable_labels(
    closed_autocracy="from Vdem, t-1",
    electoral_autocracy="from Vdem, t-1",
    electoral_democracy="from Vdem, t-1",
    liberal_democracy="fromvdem, t-1"
  )
table(regime_type$regime_type, regime_type$closed_autocracy)
table(regime_type$regime_type, regime_type$electoral_autocracy)
table(regime_type$regime_type, regime_type$electoral_democracy)
table(regime_type$regime_type, regime_type$liberal_democracy)
#all above looks good
regime_type <- regime_type %>%
  select(-regime_type) %>%
  mutate(month=12)

#Merging into data set. 
base_data <- base_data %>% 
  left_join(regime_type, by = c("ccode", "year", "month")) # Missing data simply is not updated by V-Dem, so I will not be dropping them. 
rm(regime_type)

#best option with these regime vars is linear interpolation for missing months (non-Decembers), so do that...

base_data <- base_data %>%
  arrange(ccode, year, month) %>%
  group_by(ccode, year) %>%
  fill(closed_autocracy, .direction = "updown") %>%
  fill(electoral_autocracy, .direction = "updown") %>%
  fill(electoral_democracy, .direction = "updown") %>%
  fill(liberal_democracy, .direction = "updown") %>%
  ungroup()
rm(vdem_regime2)

#------------------------------------------------------------------------------------------------#
# Leader age; regime age data; emailed from Powell to Thyne on 04/18/25
#------------------------------------------------------------------------------------------------#
tenure <- read_csv("https://github.com/lmsm256/MutinyAnalysis/raw/refs/heads/main/leader_tenure_csv.csv")
tenure <- tenure %>%
  mutate(drop = ifelse(ccode == lag(ccode) & year == lag(year) & month == lag(month), 1, 0)) %>%
  filter(drop!=1) #basically just lagged so outgoing leaders is counted in months where there was a transition; see Powell's email for a better way
tenure <- tenure %>%
  mutate(birthyear=ifelse(birthyear<0, NA, birthyear)) %>%
  select(ccode, month, year, Leader_duration=month_counter, birthyear) %>%
  mutate(Leader_age=year-birthyear+1) %>%
  select(-birthyear)
base_data <- base_data %>%
  left_join(tenure, by=c("ccode", "year", "month"))
rm(tenure)


#------------------------------------------------------------------------------------------------#
# Economic; gdp, gdppc, trade
#------------------------------------------------------------------------------------------------#

#-------------------------building gdp/cap measure------------------------------------------#    

#start with Vdem; already lagged from above
vdem_gdppc <- vdem %>%
  subset(select = c(country, ccode, year, gdppc)) %>%
  rename(vdem_gdppc = gdppc,
         c_merge = country) %>%
  mutate(month=12)
base_data <- base_data %>%
  left_join(vdem_gdppc, by = c("ccode", "year", "month"))
base_data <- base_data %>%
  subset(select = -c(c_merge))
rm(vdem_gdppc)

#Penn World Table GDP Data
pwt <- read_stata("https://dataverse.nl/api/access/datafile/554030")

pwt <- pwt %>%
  select(country, year, pop, hc, rgdpna) %>%
  set_variable_labels(
    pop="pop, Penn, t-1, ln",
    hc="human cap index, Penn, t-1"
  ) %>%
  mutate(year=year+1) %>%
  mutate(gdppc=log10(rgdpna/pop)) %>%
  select(-rgdpna, -pop) %>% 
  left_join(ccodes, by = c("country" = "country", "year" = "year"))
check <- pwt %>%
  filter(is.na(ccode)) %>%
  select(country) %>%
  distinct()
rm(check)
#need to add:
#490 = D.R. of the Congo
#812 = Lao People's DR
#510 = U.R. of Tanzania: Mainland
pwt <- pwt %>%
  mutate(ccode=ifelse(country=="D.R. of the Congo", 490, ccode)) %>%
  mutate(ccode=ifelse(country=="Lao People's DR", 812, ccode)) %>%
  mutate(ccode=ifelse(country=="U.R. of Tanzania: Mainland", 510, ccode)) %>%
  select(-country) %>%
  mutate(month=12)
base_data <- base_data %>%
  left_join(pwt, by = c("ccode", "year", "month"))
rm(pwt)

#WDI
wdi_gdppc <- WDI(country = "all",
                 indicator = "NY.GDP.PCAP.CD", 
                 start = 1960, 
                 end = 2026,
                 extra = TRUE)
wdi_gdppc <- wdi_gdppc %>%
  subset(select = c(country, year, NY.GDP.PCAP.CD)) %>%
  rename(wdi_gdppc = NY.GDP.PCAP.CD) %>%
  mutate(year=year+1) %>% #just lagged
  mutate(month=12) %>%
  left_join(ccodes, by = c("year", "country")) %>%
  rename(c_merge = country) %>%
  mutate(ccode=ifelse(c_merge=="Turkiye", 640, ccode)) %>%
  mutate(ccode=ifelse(c_merge=="Somalia, Fed. Rep.", 520, ccode)) %>%
  select(-c_merge)
base_data <- base_data %>%
  left_join(wdi_gdppc, by = c("ccode", "year", "month")) 
rm(wdi_gdppc)
cor.test(base_data$gdppc, base_data$vdem_gdppc) #looks reasonable
cor.test(base_data$gdppc, base_data$wdi_gdppc) #looks reasonable
cor.test(base_data$vdem_gdppc, base_data$wdi_gdppc) #looks reasonable

#year, state coverage...
penn <- base_data %>%
  select(year, gdppc) %>%
  filter(!is.na(gdppc))
summary(penn) #1951-2024
rm(penn)
vdem <- base_data %>%
  select(year, vdem_gdppc) %>%
  filter(!is.na(vdem_gdppc))
summary(vdem) #1950-2020
rm(vdem)
wdi <- base_data %>%
  select(year, wdi_gdppc) %>%
  filter(!is.na(wdi_gdppc))
summary(wdi) #1961-2025
rm(wdi)

#not seeing Penn add anything, so going to drop that. It also has the lowest correlation.
#linear interpolate both WDI and Vdem
base_data <- base_data %>%
  select(-gdppc) %>%
  arrange(ccode, year, month) %>%
  group_by(ccode) %>%
  mutate(vdem_gdppc_OG=vdem_gdppc) %>%
  set_variable_labels(vdem_gdppc_OG = "vdem, original, gdppc, t-1") %>%
  mutate(wdi_gdppc_OG=wdi_gdppc) %>%
  set_variable_labels(wdi_gdppc_OG = "wdi, original, gdppc, t-1") %>%
  mutate(
    time_id = year + (month - 1) / 12,
    vdem = na.approx(vdem_gdppc, x = time_id, na.rm = FALSE)
  ) %>%
  mutate(wdi=na.approx(wdi_gdppc, x=time_id, na.rm=FALSE)) %>%
  ungroup() %>%
  select(-vdem_gdppc, -wdi_gdppc)
cor.test(base_data$vdem, base_data$wdi) #looks reasonable
base_data <- base_data %>%
  group_by(ccode) %>%
  arrange(ccode, year, month) %>%
  mutate(per = (wdi - lag(wdi)) / lag(wdi)) %>%
  mutate(
    gdppc = accumulate(
      seq_along(vdem),
      .init = vdem[1],
      ~ if (!is.na(vdem[.y])) {
        vdem[.y]
      } else {
        .x * (1 + per[.y])
      }
    )[-1]
  ) %>%
  ungroup()

lin_extrap <- function(x, y) {
  keep <- !is.na(y)
  
  if (sum(keep) == 0) return(y)
  if (sum(keep) == 1) return(rep(y[keep][1], length(y)))
  
  x_obs <- x[keep]
  y_obs <- y[keep]
  
  y_new <- approx(x_obs, y_obs, xout = x, method = "linear", rule = 1)$y
  
  left_slope <- (y_obs[2] - y_obs[1]) / (x_obs[2] - x_obs[1])
  left_idx <- x < min(x_obs)
  y_new[left_idx] <- y_obs[1] + left_slope * (x[left_idx] - x_obs[1])
  
  n <- length(x_obs)
  right_slope <- (y_obs[n] - y_obs[n - 1]) / (x_obs[n] - x_obs[n - 1])
  right_idx <- x > max(x_obs)
  y_new[right_idx] <- y_obs[n] + right_slope * (x[right_idx] - x_obs[n])
  
  y_new
}
base_data <- base_data %>%
  arrange(ccode, year, month) %>%
  group_by(ccode) %>%
  mutate(
    gdppc = lin_extrap(time_id, gdppc)
  ) %>%
  ungroup() %>%
  mutate(gdppc=log10(gdppc+1))
base_data <- base_data %>%
  select(-vdem, -wdi, -per) %>%
  set_variable_labels(gdppc = "GDP/cap, WDI+vdem splice, interpolated")

#deal with human cap index from Penn
summary(base_data$hc) #range: 1.007, 3.986
base_data <- base_data %>%
  arrange(ccode, year, month) %>%
  mutate(hc_OG = hc) %>%
  group_by(ccode) %>%
  mutate(hc = lin_extrap(time_id, hc_OG)) %>%
  mutate(hc = ifelse(hc<1.007, 1.007, hc)) %>%
  mutate(hc = ifelse(hc>3.986, 3.986, hc)) %>%
  ungroup() %>%
  set_variable_labels(hc_OG = "human cap index, original, Penn, t-1") %>%
  set_variable_labels(hc = "human cap index, interpolated")

#create % change in GDP/cap
base_data <- base_data %>%
  group_by(ccode) %>%
  mutate(ch_gdppc=((gdppc-lag(gdppc))/lag(gdppc))) %>%
  set_variable_labels(ch_gdppc = "% ch in ln GDP/cap")
base_data <- base_data %>%
  select(-time_id, -vdem_gdppc_OG, -wdi_gdppc_OG, -hc_OG)

#----------------------------------add trade-----------------------------------------------#  

#Start with COW; monadic
url <- "https://correlatesofwar.org/wp-content/uploads/COW_Trade_4.0.zip"
download.file(url, "data.zip")
unzip("data.zip", exdir="data")
cow <- read_csv("data/COW_Trade_4.0/National_COW_4.0.csv")
unlink("data.zip")
unlink("data", recursive=TRUE)
rm(url)

cow <- cow %>%
  mutate(year=year+1) %>% #just lagged
  mutate(trade=(imports+exports)) %>%
  mutate(ltrade=log(trade+1)) %>%
  select(ccode, statename, year, trade, ltrade)

#merge to base_yearly for all to make splicing make sense; then put into base_data (monthly)
142344/12 #yearly DF should have around 11,862 obs
yearly <- base_data %>%
  select(country, ccode, year) %>%
  distinct() %>%
  arrange(ccode, year)
yearly <- yearly %>%
  left_join(cow, by=c("ccode", "year"))
rm(cow)
yearly <- yearly %>%
  select(-statename)
ch <- yearly %>%
  select(ccode, year) %>%
  distinct() #no duplicates, we're good
rm(ch)

#add trade w/ US only from COW
url <- "https://correlatesofwar.org/wp-content/uploads/COW_Trade_4.0.zip"
download.file(url, "data.zip")
unzip("data.zip", exdir="data")
cow <- read_csv("data/COW_Trade_4.0/Dyadic_COW_4.0.csv")
unlink("data.zip")
unlink("data", recursive=TRUE)
rm(url)
#clean; note that these are not directed dyads
cow <- cow %>%
  filter(ccode1==2) %>%
  mutate(year=year+1) %>%
  select(ccode=ccode2, flow1, flow2, year) %>%
  mutate(flow1=ifelse(flow1<0, 0, flow1)) %>%
  mutate(flow2=ifelse(flow2<0, 0, flow2)) %>%
  mutate(dtrade=flow1+flow2) %>%
  mutate(ldtrade=log(dtrade+1)) %>%
  select(-flow1, -flow2)
ch <- cow %>%
  select(ccode, year) %>%
  distinct() #no duplicates, we're good
rm(ch)
yearly <- yearly %>%
  left_join(cow, by=c("ccode", "year"))
rm(cow)

#now do WDI, monadic

#Add WDI; monadic
indicators <- c("NE.EXP.GNFS.CD", "NE.IMP.GNFS.CD")
wdi <- WDI(indicator = indicators, start = 1960, end = 2026, extra = TRUE)
rm(indicators)
wdi <- wdi %>%
  select(country, year, NE.EXP.GNFS.CD, NE.IMP.GNFS.CD) %>%
  rename(exports = NE.EXP.GNFS.CD) %>%
  rename(imports = NE.IMP.GNFS.CD) %>%
  mutate(imports=ifelse(is.na(imports), 0, imports)) %>%
  mutate(exports=ifelse(is.na(exports), 0, exports)) %>%
  mutate(year=year+1) %>%
  mutate(wdi_trade=(imports+exports)) %>%
  mutate(wdi_ltrade=log(wdi_trade+1))
ch <- wdi %>%
  select(country, year) %>%
  distinct() #no duplicates, we're good
rm(ch)
wdi <- wdi %>%
  left_join(ccodes, by=c("country", "year"))
wdi <- wdi %>%
  mutate(ccode=ifelse(country=="Turkiye", 640, ccode)) %>%
  mutate(ccode=ifelse(country=="Somalia, Fed. Rep.", 520, ccode)) %>%
  rename(wdi_country=country) %>%
  filter(wdi_trade>0) %>%
  filter(!is.na(ccode)) %>%
  select(-exports, -imports)
ch <- wdi %>%
  select(ccode, year) %>%
  distinct() #no duplicates, we're good
rm(ch)
yearly <- yearly %>%
  left_join(wdi, by=c("ccode", "year"))
yearly <- yearly %>%
  select(-wdi_country)
rm(wdi)
ch <- yearly %>%
  select(ccode, year) %>%
  distinct() #looks good
rm(ch)

#add trade w/ US only; from https://dataweb.usitc.gov/; couldn't pull these directly from web so putting them on github
#grabbed updated data on 03/27/26
url <- "https://github.com/lmsm256/MutinyAnalysis/raw/refs/heads/main/updated-DataWeb-Query-Export%20(2).xlsx"
destfile <- "DataWeb_Query_Export_20_1_.xlsx"
curl::curl_download(url, destfile)
exports <- read_excel(destfile, skip = 2, sheet = "FAS Value")
rm(destfile, url)
exports <- exports %>%
  select(-"Data Type") %>%
  pivot_longer(cols=-Country,
               names_to="year",
               values_to="exports") %>%
  rename(country=Country) %>% 
  mutate(year=as.numeric(year)) %>%
  mutate(exports=as.numeric(exports)) %>%
  mutate(year=year+1) 
exports <- exports %>%
  left_join(ccodes, by=c("country", "year")) %>%
  arrange(ccode, year, -exports) %>%
  mutate(problem=ifelse(ccode==lag(ccode) & year==lag(year), 1, 0)) %>%
  filter(problem!=1) %>%
  select(-problem, -country) 

url <- "https://github.com/lmsm256/MutinyAnalysis/raw/refs/heads/main/updated_imports_DataWeb-Query-Export%20(2).xlsx"
destfile <- "DataWeb_Query_Export_20_1_.xlsx"
curl::curl_download(url, destfile)
imports <- read_excel(destfile, skip = 2, sheet = "General Customs Value")
rm(destfile, url)
imports <- imports %>%
  select(-"Data Type") %>%
  pivot_longer(cols=-Country,
               names_to="year",
               values_to="imports") %>%
  rename(country=Country) %>% 
  mutate(year=as.numeric(year)) %>%
  mutate(imports=as.numeric(imports)) %>%
  mutate(year=year+1) 
imports <- imports %>%
  left_join(ccodes, by=c("country", "year")) %>%
  arrange(ccode, year, -imports) %>%
  mutate(problem=ifelse(ccode==lag(ccode) & year==lag(year), 1, 0)) %>%
  filter(problem!=1) %>%
  select(-problem)

usitc <- exports %>% 
  full_join(imports, by=c("ccode", "year"))
usitc <- usitc %>%
  mutate(exports=ifelse(is.na(exports), 0, exports)) %>%
  mutate(imports=ifelse(is.na(imports), 0, imports)) %>%
  filter(year<=2026) %>%  mutate(usitc_dtrade=imports+exports) %>%
  mutate(usitc_ldtrade=log(imports+exports+1)) %>%
  select(-imports, -exports) 
usitc <- usitc %>%
  mutate(ccode=ifelse(country=="Côte d`Ivoire", 437, ccode)) %>%
  mutate(ccode=ifelse(country=="São Tomé and Príncipe", 403, ccode)) %>%
  mutate(ccode=ifelse(country=="Czechia (Czech Republic)", 316, ccode)) %>%
  mutate(ccode=ifelse(country=="Eswatini (Swaziland)", 572, ccode))
usitc <- usitc %>%
  filter(!is.na(ccode))
usitc <- usitc %>%
  select(-country)

yearly <- yearly %>%
  left_join(usitc, by=c("ccode", "year"))
rm(usitc, exports, imports)

cor(yearly$ldtrade, yearly$usitc_ldtrade, use="complete.obs")
cor(yearly$dtrade, yearly$usitc_dtrade, use="complete.obs")
#above very high correlations so getting at the same thing; okay to splice

#splice monadic
mon <- yearly %>%
  group_by(ccode) %>%
  select(country, ccode, year, ltrade, wdi_ltrade) %>%
  mutate(ch=(wdi_ltrade-lag(wdi_ltrade))/lag(wdi_ltrade)) %>%
  mutate(splice=ltrade) %>%
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice))
mon <- mon %>%
  select(ccode, year, ltrade=splice)

#splice dyadic
dy <- yearly %>%
  group_by(ccode) %>%
  select(country, ccode, year, ldtrade, usitc_ldtrade) %>%
  filter(ccode!=2) %>%
  mutate(splice=ldtrade) %>%
  mutate(ch=(usitc_ldtrade-lag(usitc_ldtrade))/lag(usitc_ldtrade)) %>%
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice)) %>%
  select(ccode, year, ldtrade=splice)
yearly <- yearly %>%
  select(ccode, year) %>%
  left_join(mon, by=c("ccode", "year")) %>%
  left_join(dy, by=c("ccode", "year")) %>% 
  mutate(month=12)
base_data <- base_data %>%
  left_join(yearly, by=c("ccode", "year", "month"))
rm(dy, mon, yearly)

#interpolate
base_data <- base_data %>%
  mutate(ltrade_OG=ltrade) %>%
  set_variable_labels(ltrade_OG = "total trade, log10, t-1") %>%
  mutate(ldtrade_OG=ldtrade) %>%
  set_variable_labels(ldtrade_OG = "trade w/ US, log10, t-1")
base_data <- base_data %>%
  arrange(ccode, year, month) %>%
  group_by(ccode) %>%
  mutate(
    time_id = year + (month - 1)/12,
    ltrade = na.approx(
      ltrade,
      x = time_id,
      na.rm = FALSE,
      rule = 2
    )
  ) %>%
  mutate(
    ldtrade = na.approx(
      ldtrade,
      x = time_id,
      na.rm = FALSE,
      rule = 2
    )
  ) %>%
  ungroup() %>%
  select(-time_id)

base_data <- base_data %>%
  select(-ldtrade_OG, -ltrade_OG)

#------------------------------------------------------------------------------------------------#
#mass mobilization; civil wars; civil conflict severity (battle related deaths), Interstate conflicts (MID)
#------------------------------------------------------------------------------------------------#  

# -------------------------- Mass Mobilization (V-Dem) ----------------------------- #

# Bringing in data. 
vdem_data <- vdem

# Cleaning up data. 
vdem_data <- vdem %>%
  subset(select = c(country_name, # Country. 
                    year, # Year. 
                    v2cagenmob, # Mass mobilization (ordinal, converted to interval; Z-score). 
                    v2caconmob)) %>% # Mass mobilization concentration (ordinal, converted to interval; Z-score). 
  rename(country = country_name,
         year = year, 
         mobilization = v2cagenmob,
         mobil_conc = v2caconmob) %>% 
  mutate(year=year+1) %>% # Just lagged. 
  filter(year >= 1950) %>%
  mutate(country = case_when(
    country == "Republic of Vietnam"              ~ "Republic of Vietnam",   
    country == "German Federal Republic"          ~ "German Federal Republic",
    country == "German Democratic Republic"       ~ "German Democratic Republic",
    country == "Yemen Arab Republic"              ~ "Yemen Arab Republic",
    country == "Yemen People's Republic"          ~ "Yemen People's Republic",
    country == "Czechoslovakia"                   ~ "Czechoslovakia",
    TRUE                                          ~ country
  )) 
vdem_data <- vdem_data %>% # Merging in ccodes. 
  left_join(ccodes, by = c("year", "country")) 
check <- vdem_data %>%
  filter(is.na(ccode)) %>%
  select(country) %>%
  distinct() #all good
rm(check)
vdem_data <- vdem_data %>%
  filter(!is.na(ccode)) %>%
  mutate(month=12) %>%
  select(-country)
base_data <- base_data %>%
  left_join(vdem_data, by=c("ccode", "year", "month"))
rm(vdem_data)

#interpolate
mobil <- base_data %>%
  filter(!is.na(mobilization))
summary(mobil) #1950-2025; #mobilization range: -3.5680, 4.0130; mobil_conc range: -3.0710, 3.7370
rm(mobil)
base_data <- base_data %>%
  arrange(ccode, year, month) %>%
  group_by(ccode) %>%
  mutate(mobilization_OG=mobilization) %>%
  mutate(mobil_conc_OG=mobil_conc) %>%
  set_variable_labels(mobilization_OG="mobilization, vdem, t-1, OG") %>%
  set_variable_labels(mobil_conc_OG="mobil_conc, vdem, t-1, OG") %>%
  mutate(mobilization = na.approx(mobilization, x=row_number(), na.rm=FALSE, rule=2)) %>%
  mutate(mobilization=ifelse(mobilization < -3.5680, -3.5680, mobilization)) %>%
  mutate(mobilization=ifelse(mobilization > 4.0130, 4.0130, mobilization)) %>%
  mutate(mobil_conc = na.approx(mobil_conc, x=row_number(), na.rm=FALSE, rule=2)) %>%
  mutate(mobil_conc=ifelse(mobil_conc < -3.0710, -3.0710, mobil_conc)) %>%
  mutate(mobil_conc=ifelse(mobil_conc > 3.737, 3.737, mobil_conc))
base_data <- base_data %>%
  select(-mobil_conc, -mobilization_OG, -mobil_conc_OG)

# -------------------------- civil wars from UCDP/PRIO ACD----------------------------- #

#bring in data
url <- "https://ucdp.uu.se/downloads/ucdpprio/ucdp-prio-acd-251-xlsx.zip"
download.file(url, "data.zip")
unzip("data.zip", exdir="data")
unlink("data.zip")
cw <- read_excel("data/UcdpPrioConflict_v25_1.xlsx")
unlink("data", recursive=TRUE)
rm(url)  

#clean data; only type 3 and 4 CWs
cw <- cw %>%
  filter(type_of_conflict=="3" | type_of_conflict=="4") %>%
  mutate(loc = as.numeric(gwno_loc)) %>%
  mutate(year = as.numeric(year)) %>%
  filter(loc!=2) %>% #US in civil war is miscoded
  select(location, year, loc, start_date2, ep_end_date) %>%
  mutate(ep_end_date = coalesce(ep_end_date, as.Date("2026-04-30"))) %>%
  rename(country=location) %>%
  left_join(ccodes, by=c("country", "year"))
check <- cw %>%
  filter(loc!=ccode | is.na(ccode)) #use 'loc'
rm(check)
cw <- cw %>%
  select(-ccode) %>%
  rename(ccode=loc) %>%
  select(-year) %>%
  distinct()

#now make it ccode/year/month
cw <- cw %>%
  mutate(start_date2 = as.Date(start_date2)) %>%
  mutate(ep_end_date = as.Date(ep_end_date)) %>%
  mutate(start=floor_date(start_date2, unit="month")) %>%
  mutate(end=floor_date(ep_end_date, unit="month")) %>%
  rowwise() %>%
  mutate(month_seq=list(seq.Date(start, end, by="month"))) %>%
  ungroup() %>%
  select(ccode, month_seq) %>%
  unnest(month_seq) %>%
  mutate(month_seq = month_seq %m+% months(1)) %>% #just lagged by 1 month
  mutate(year=year(month_seq)) %>%
  mutate(month=month(month_seq)) %>%
  mutate(cw=1) %>%
  set_variable_labels(cw = "ACD CW, t-1") %>%
  select(-month_seq) %>%
  distinct() %>%
  rename(cw_lag=cw)

#merge into base
base_data <- base_data %>%
  left_join(cw, by=c("ccode", "year", "month")) %>%
  mutate(cw_lag = ifelse(is.na(cw_lag), 0, cw_lag))
rm(cw)

#---------------------civil conflict severity (battle-related deaths) from UCDP------------------------#  

#leah- updated through 2026; every month is showing the same value for the year, not sure if thats ideal but could be fixed later

#bring in data
url <- "https://ucdp.uu.se/downloads/brd/ucdp-brd-conf-251-xlsx.zip"
download.file(url, "data.zip")
unzip("data.zip", exdir="data")
unlink("data.zip")
brd <- read_excel(list.files("data", pattern="\\.xlsx$", full.names=TRUE)[1])
unlink("data", recursive=TRUE)
rm(url)

#clean data; only type 3 and 4 conflicts; aggregate deaths to country-year
brd <- brd %>%
  filter(type_of_conflict=="3" | type_of_conflict=="4") %>%
  mutate(year=as.numeric(year)) %>%
  rename(country=location_inc) %>%
  group_by(country, year) %>%
  summarise(brd=sum(bd_best, na.rm=TRUE), .groups="drop")
brd <- brd %>%
  left_join(ccodes, by=c("country", "year"))
#expand for 2025; assume that conflicts in 2024 continue to 2025
brd_2025 <- brd %>%
  filter(!is.na(ccode)) %>%
  distinct() %>%
  filter(year == 2024) %>%
  mutate(year = 2025)
brd <- brd %>%
  bind_rows(brd_2025) %>%
  arrange(country, ccode, year)
brd <- brd %>%
  group_by(ccode) %>%
  mutate(year=year+1) %>% #just lagged
  distinct() %>%
  select(ccode, year, brd)
#merge
base_data <- base_data %>%
  left_join(brd, by=c("ccode", "year"))
rm(brd)
base_data <- base_data %>%
  mutate(brd=ifelse(is.na(brd) & year>=1989 & year<=2025, 0, brd)) %>%
  set_variable_labels(brd="battle-related deaths, types 3-4, UCDP, t-1")

#----------------------------------MID--------------------------------#
#bring in data
url <- "https://ucdp.uu.se/downloads/dyadic/ucdp-dyadic-251-xlsx.zip"
download.file(url, "data.zip")
unzip("data.zip", exdir="data")
unlink("data.zip")
mid <- read_excel("data/Dyadic_v25_1.xlsx")
unlink("data", recursive=TRUE)
rm(url)  

primary <- mid %>%
  filter(type_of_conflict==2) %>%
  select(year, gwno_a, gwno_b)

primary <- primary %>%
  select(year, gwno_a, gwno_b) %>%
  mutate(
    gwno_a = str_split(as.character(gwno_a), ",\\s*"),
    gwno_b = str_split(as.character(gwno_b), ",\\s*"),
    ccode = map2(gwno_a, gwno_b, ~ c(.x, .y))
  ) %>%
  select(year, ccode) %>%
  unnest(ccode) %>%
  mutate(ccode = as.numeric(str_trim(ccode)),
         mid = 1) %>%
  filter(!is.na(ccode)) %>%
  distinct(ccode, year, .keep_all = TRUE) %>%
  arrange(ccode, year) %>%
  mutate(year=year+1) %>%
  rename(mid_primary=mid) %>%
  distinct()

base_data <- base_data %>%
  left_join(primary, by = c("ccode", "year")) %>%
  group_by(ccode) %>%
  mutate(
    mid25 = dplyr::coalesce(
      dplyr::first(mid_primary[year == 2025]),
      NA_real_
    ),
    mid_primary = if_else(year == 2026 & is.na(mid_primary), mid25, mid_primary),
    mid_primary = coalesce(mid_primary, 0)
  ) %>%
  ungroup() %>%
  select(-mid25) %>%
  set_variable_labels(mid_primary="interstate dispute, primary only, ucdp, t-1")
rm(primary)

second <- mid %>%
  filter(type_of_conflict==2) %>%
  select(year, gwno_a_2nd, gwno_b_2nd) %>%
  mutate(drop=ifelse(is.na(gwno_a_2nd) & is.na(gwno_b_2nd), 1, 0)) %>%
  filter(drop==0) %>%
  select(-drop) %>%
  mutate(
    gwno_a = str_split(as.character(gwno_a_2nd), ",\\s*"),
    gwno_b = str_split(as.character(gwno_b_2nd), ",\\s*"),
    ccode = map2(gwno_a, gwno_b, ~ c(.x, .y))
  ) %>%
  select(year, ccode) %>%
  unnest(ccode) %>%
  mutate(ccode = as.numeric(str_trim(ccode)),
         mid = 1) %>%
  filter(!is.na(ccode)) %>%
  distinct(ccode, year, .keep_all = TRUE) %>%
  arrange(ccode, year) %>%
  mutate(year=year+1) %>%
  rename(mid_secondary=mid) %>%
  distinct()

base_data <- base_data %>%
  left_join(second, by = c("ccode", "year")) %>%
  group_by(ccode) %>%
  mutate(
    mid25 = dplyr::coalesce(
      dplyr::first(mid_secondary[year == 2025]),
      NA_real_
    ),
    mid_secondary = if_else(year == 2026 & is.na(mid_secondary), mid25, mid_secondary),
    mid_secondary = coalesce(mid_secondary, 0)
  ) %>%
  ungroup() %>%
  select(-mid25) %>%
  set_variable_labels(mid_secondary="interstate dispute, secondary only, ucdp, t-1")
rm(second, mid)

base_data <- base_data %>%
  mutate(mid=ifelse(mid_primary==1 | mid_secondary==1, 1, 0)) %>%
  select(-mid_secondary) %>%
  set_variable_labels(mid = "mid primary or secondary, ucdp, t-1")

#------------------------------------------------------------------------------------------------#
#military regime, military personnel, military expenditures
#------------------------------------------------------------------------------------------------#  

#---------------------------------military regime------------------------------------#
milreg <- read_csv("https://raw.githubusercontent.com/lmsm256/MutinyAnalysis/refs/heads/main/coupcats_military_regime.csv")
milreg <- milreg %>%
  rename(country_milreg=country) 

milreg <- milreg %>% 
  select(-reign_type) %>%
  mutate(year=year+1) %>% #just lagged
  distinct()
#originally filled all n/as as 0; now making assuption that milreg stays if 1 in march 2026 and then filling all remaining n/a with 0
base_data <- base_data %>%
  left_join(milreg, by = c("ccode", "year", "month"))
still_active <- milreg %>%
  filter(year == 2026, month == 3, milreg == 1) %>%
  pull(ccode)
base_data <- base_data %>%
  mutate(milreg = ifelse(ccode %in% still_active & year == 2026 & month == 4, 1, milreg)) 
base_data <- base_data %>%
  mutate(milreg = replace_na(milreg, 0))
rm(milreg)
base_data <- base_data %>%
  set_variable_labels(milreg="milreg from reign; powell updates") %>%
  select(-country_milreg)

#--------------------------------Milex from SIPRI--------------------------------------#  

url <- "https://www.sipri.org/sites/default/files/SIPRI-Milex-data-1949-2024_2.xlsx"
download.file(url, destfile = "~/SIPRI-Milex-data-1949-2024.xlsx", mode = "wb")
sipri <- read_excel("~/SIPRI-Milex-data-1949-2024.xlsx", sheet = "Constant (2023) US$", skip=5)
rm(url)

#clean; turn
sipri <- sipri %>%
  rename(country=Country) %>%
  select(-...2, -Notes) %>%
  pivot_longer(
    cols=-c(country),
    names_to="year",
    values_to="milex"
  ) %>%
  mutate(year = as.integer(year)) %>%
  arrange(country, year) %>%
  mutate(milex=as.numeric(milex)) %>%
  arrange(country, year) %>%
  mutate(month=12) %>%
  rename(sipri_milex=milex) %>%
  mutate(year=year+1) #just lagged

sipri <- sipri %>%
  left_join(ccodes, by=c("country", "year"))
#need Congo, DR=490; Congo, Rep=484; Yemen, North=678
sipri <- sipri %>%
  mutate(ccode=ifelse(country=="Congo, DR", 490, ccode)) %>%
  mutate(ccode=ifelse(country=="Congo, Republic", 484, ccode)) %>%
  mutate(ccode=ifelse(country=="Yemen, North", 678, ccode)) %>%
  rename(sipri_country=country) 
sipri <- sipri %>%
  filter(!is.na(sipri_country)) %>%
  filter(!is.na(ccode)) %>%
  arrange(ccode, year) %>%
  mutate(problem=ifelse(ccode==lag(ccode) & year==lag(year), 1, NA)) %>% #We're good
  select(-problem, -sipri_country)
check <- sipri %>%
  arrange(ccode, year) %>%
  mutate(problem=ifelse(ccode==lag(ccode) & year==lag(year), 1, 0)) #345 and 365 an issue with duplicate years
sipri <- sipri %>%
  filter(!is.na(sipri_milex)) 
rm(check)
base_data <- base_data %>%
  left_join(sipri, by=c("ccode", "year", "month"))
rm(sipri)

#--------------------------------Milex and milper from COW---------------------------------------#  

#Bring in the NMC v6 data; available: https://correlatesofwar.org/data-sets/national-material-capabilities/
url <- "https://correlatesofwar.org/wp-content/uploads/NMC_Documentation-6.0.zip"
download.file(url, "data.zip")
unzip("data.zip", exdir="data")
unlink("data.zip")
unzip("data/NMC-60-wsupplementary.zip", exdir="NMC_data")
nmc <- read_dta("NMC_data/NMC-60-wsupplementary.dta")
unlink("data", recursive=TRUE)
unlink("NMC_data", recursive=TRUE)
rm(url)
nmc <- nmc %>%
  select(statenme, ccode, year, milex, milper) %>%
  rename(cow_milex=milex) %>%
  rename(cow_milper=milper) %>%
  rename(cow_country=statenme) %>%
  mutate(year=year+1) %>% #just lagged
  mutate(cow_milex=ifelse(cow_milex<0, NA, cow_milex)) %>%
  mutate(cow_milper=ifelse(cow_milper<0, NA, cow_milper)) %>%
  mutate(month=12)
#feels like linear interpolation (but not extrapolation) should be fine for milex
#feels like linear interpolation (but not extrapolation) should be fine for milper
#merge into base
base_data <- base_data %>%
  left_join(nmc, by=c("ccode", "year", "month"))
rm(nmc)
cor.test(base_data$sipri_milex, base_data$cow_milex) #r=.867 so matching up about what we'd expect
check <- base_data %>%
  filter(country!=cow_country) %>%
  select(country, cow_country) %>%
  distinct() #we're good
rm(check) 
base_data <- base_data %>%
  select(-cow_country)

#----------------------------------Splice SIPRI/COW milex-------------------------------------------#  

df <- base_data %>%
  select(country, ccode, year, month, sipri_milex, cow_milex) %>%
  filter(month==12) %>%
  distinct() %>%
  select(-month)
df <- df %>%
  arrange(ccode, year) %>%
  group_by(ccode) %>%
  mutate(ch=((sipri_milex-lag(sipri_milex))/lag(sipri_milex)))
df <- df %>%
  group_by(ccode) %>%
  mutate(splice=cow_milex) %>%
  mutate(ch=ifelse(ccode==101 & year==2018, 1, ch)) %>% #Ven 1018 weird because went from 0 to 1, so ch divided by 0; fixed it with this
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice))%>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice))%>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice))%>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice))%>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice))%>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice))%>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice))%>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice))%>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice))%>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice))
df <- df %>%
  select(ccode, year, splice) %>%
  rename(milex_spliced=splice) %>%
  mutate(milex_spliced=ifelse(is.nan(milex_spliced), 0, milex_spliced)) %>%
  mutate(month=12)
base_data <- base_data %>%
  ungroup() %>%
  left_join(df, by=c("ccode", "year", "month")) %>%
  select(-sipri_milex,  -cow_milex)
rm(df)

#create linear interpolation/extrapolation
base_data <- base_data %>%
  arrange(ccode, year, month) %>%
  mutate(milex_spliced_OG=milex_spliced) %>%
  group_by(ccode) %>%
  mutate(
    time_id = year + (month - 1)/12,
    milex_spliced = na.approx(
      milex_spliced,
      x = time_id,
      na.rm = FALSE,
      rule = 2
    )
  ) %>%
  ungroup() %>%
  select(-time_id)
#log values for milex
base_data <- base_data %>%
  mutate(milex_spliced=log(milex_spliced+1)) %>%
  set_variable_labels(milex_spliced="SIPRI+COW, t-1, log, linear interpolation")

#-------------------------------Milper from WDI---------------------------------------#  

wdi <- WDI(country = "all", indicator = "MS.MIL.TOTL.P1", start = 1960, end = 2026)
wdi <- wdi %>%
  select(country, year, MS.MIL.TOTL.P1) %>%
  rename(wdi_milper = MS.MIL.TOTL.P1) %>%
  mutate(year=year+1) %>% #just lagged
  left_join(ccodes, by=c("country", "year"))
check <- wdi %>%
  filter(is.na(ccode)) %>%
  select(country) %>%
  distinct()
rm(check)
wdi <- wdi %>%
  mutate(ccode=ifelse(country=="Turkiye", 640, ccode)) %>%
  mutate(ccode=ifelse(country=="Somalia, Fed. Rep.", 520, ccode)) %>%
  rename(wdi_country=country) %>%
  filter(!is.na(ccode)) %>%
  select(-wdi_country) %>%
  mutate(month=12)
base_data <- base_data %>%
  left_join(wdi, by=c("ccode", "year", "month"))
rm(wdi)

#bounce it back to country year, deal with missing data in wdi, then splice
df <- base_data %>%
  select(ccode, year, month, cow_milper, wdi_milper) %>%
  filter(month==12) %>%
  distinct()
df <- df %>%
  group_by(ccode) %>%
  mutate(ch=(wdi_milper-lag(wdi_milper))/lag(wdi_milper))
df <- df %>%
  group_by(ccode) %>%
  mutate(splice=cow_milper) %>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice)) %>%
  ungroup()
df <- df %>%
  select(ccode, year, splice) %>%
  rename(milper_spliced=splice) %>%
  mutate(month=12)
base_data <- base_data %>%
  ungroup() %>%
  left_join(df, by=c("ccode", "year", "month")) %>%
  select(-cow_milper, -wdi_milper)
rm(df)

base_data <- base_data %>%
  mutate(milper_spliced_OG=milper_spliced) %>%
  arrange(ccode, year, month) %>%
  group_by(ccode) %>%
  mutate(
    time_id = year + (month - 1)/12,
    milper_spliced = na.approx(
      milper_spliced,
      x = time_id,
      na.rm = FALSE,
      rule = 2
    )
  ) %>%
  ungroup() %>%
  select(-time_id) %>%
  mutate(milper_spliced=log10(milper_spliced+1))
base_data <- base_data %>%
  mutate(solqual=(milex_spliced/(milper_spliced+1)))
base_data <- base_data %>%
  select(-milex_spliced_OG, -milper_spliced_OG) %>%
  set_variable_labels(solqual="(military expenditures) / (military personnel)")

#------------------------------------------------------------------------------------------------#
#add cold war dummy
#------------------------------------------------------------------------------------------------#  

base_data <- base_data %>%
  mutate(cold=ifelse(year<=1989, 1, 0))

#------------------------------------------------------------------------------------------------#
#Mutiny
#------------------------------------------------------------------------------------------------#  
mutiny <- read_csv("https://github.com/lmsm256/MutinyAnalysis/raw/refs/heads/main/MIA3.20260611%20-%20Sheet1.csv")
mutiny <- mutiny %>%
  filter(confidence==1) %>%
  rename(mutiny_date = edate) %>%
  rename(mutiny_resulted_coup = coup) %>%
  rename(mutiny = confidence) %>%
  select(ccode, year, month, mutiny_date, mutiny, mutiny_resulted_coup)
base_data <- base_data %>%
  left_join(mutiny, by=c("ccode", "year", "month")) %>%
  group_by(ccode) %>%
  mutate(across(c(mutiny_date, mutiny, mutiny_resulted_coup), lag)) %>%
  ungroup()
base_data <- base_data %>%
mutate(mutiny = replace_na(mutiny, 0)) 

#add time since last mutiny
df <- base_data %>%
  select(ccode, year, month, mutiny) %>%
  arrange(ccode, year, month, mutiny) %>%
  group_by(ccode) %>%
  mutate(
    mutiny_row = if_else(mutiny == 1, row_number(), NA_integer_),
    last_mutiny_row = cummax(replace_na(mutiny_row, 0)),
    months_since_mutiny = if_else(
      last_mutiny_row == 0,
      row_number(),                    # before first mutiny
      row_number() - last_mutiny_row   # after a mutiny
    )
  ) %>%
  ungroup() %>%
  mutate(months_since_mutiny2 = months_since_mutiny^2) %>%
  mutate(months_since_mutiny3 = months_since_mutiny^3) %>%
  select(-mutiny_row, -last_mutiny_row, -mutiny)
base_data <- base_data %>%
  left_join(df, by=c("ccode", "year", "month"))

#duplicates formed somewhere in the code. removing them here for now and will find source of problem later
base_data <- base_data %>%
  distinct(country, ccode, year, month, .keep_all = TRUE)

