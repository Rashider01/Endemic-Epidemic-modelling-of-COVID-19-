# Packages used throughout the project

library(dplyr)
library(geodata)
library(ggplot2)
library(lattice)
library(lubridate)
library(patchwork)
library(readr)
library(reshape2)
library(rgdal)
library(rmarkdown)
library(scales)
library(scoringRules)
library(sf)
library(spdep)
library(surveillance)
library(tibble)
library(tidyr)
library(tidyverse)
library(xtable)

### LOADING DATASETS AND DATA PREPROCESSING
covid19za <- read_csv("covid19za_provincial_cumulative_timeline_confirmed (1) (1).csv")
str(covid19za)
Stringency<-read_csv("C:/Users/raalo/Desktop/filter_index.csv")
str(Stringency)
covid_vaccine <- read_csv("C:/Users/raalo/Desktop/share-people-vaccinated-covid.csv")
str(daily.covid_19.vaccine)

data.index <- daily.ovid_19.vaccine %>% 
  filter(Entity == "South Africa") %>% 
  select(Day,'People vaccinated (cumulative, per hundred)' )
data.index$date <- as.Date(data.index$Day)
start <- as.Date('2021-02-17')
end <- as.Date('2022-07-25')
filter.index <- subset(data.index, Day >= start & Day <= end)

#### Filter for South Africa and select relevant columns
data.index <- daily_covid_19_vaccine %>% 
  filter(Entity == "South Africa") %>% 
  select(Day, `People vaccinated (cumulative, per hundred)` )

data.index$date <- as.Date(data.index$Day)
start <- as.Date('2021-02-17')
end <- as.Date('2022-07-25')
filter.index <- subset(data.index, Day >= start & Day <= end)

# Creating new variable 'vaccine' that equals 1 - coverage 
filter.index <- filter.index %>%
  mutate(vaccine = (1 - (`People vaccinated (cumulative, per hundred)` / 100)) * 100)

filter.index <- filter.index %>%
  mutate(vaccine = 
           
           round((1 - (`People vaccinated (cumulative, per hundred)` / 100)) * 100, digits = 2))
View(filter.index)
str(filter.index)covid19za <- covid19za %>%
  mutate(date = dmy(date)) 
Stringency <- Stringency %>%
  mutate(date = as.Date(date))  

# Merging the datasets by date
merged.data <- covid19za %>%
  left_join(Stringency, by = "date")
head(merged.data)
write.csv(merged.data, "Merged_COVID_Stringency.csv", row.names = FALSE)

covid.new <- merged.data%>% select(-source)
covid.new<- na.omit(covid.new)
dim(covid.new)
colSums(is.na(covid.new))
head(covid.new)
covid.new$date <- as.Date(covid.new$date, format = "%d-%m-%Y")
covid.new <- covid.new %>%
  arrange(date)
covid.new <- covid.new %>%
  mutate(new.cases = total - lag(total, default = first(total)))
head(covid.new)
str(covid.new)
covid.new$date <- as.Date(covid.new$date)
filter.index$date <- as.Date(filter.index$date)
full.dates <- tibble(date = seq(from = as.Date("2020-03-05"), 
                                to   = as.Date("2025-07-25"), 
                                by   = "day"))

# Preparing vaccination data:
vacc.full <- full.dates %>%
  left_join(filter.index %>% 
              select(date, `vaccine`),
            by = "date") %>%
  mutate(`vacine` =
           replace_na(`vaccine`, 0)) %>%
  
  rename(vaccination = `vaccine`)

if("vaccination" %in% names(covid.new)) {
  covid.new <- covid_new %>% select(-vaccination)}

covid.new <- left_join(covid.new, vacc_full, by = "date")
str(covid.new)

covid.new$date <- as.Date(covid.new$date)
filter.index$date <- as.Date(filter.index$date)
full.dates <- tibble(date = seq(from = as.Date("2020-03-05"), 
                                to   = as.Date("2025-07-25"), 
                                by   = "day"))
vacc.full <- full.dates %>%
  left_join(
    filter.index %>% 
      select(date, vaccine),
    by = "date"
  ) %>%
  mutate(vaccine = replace_na(vaccine, 0)  
  )

covid.new <- left_join(covid_new, vacc_full, by = "date")
covid.new <- covid.new %>%
  mutate(vaccination = tidyr::replace_na(vaccination, 0))
covid.new <- covid.new %>%
  select(-ends_with(".x"), -ends_with(".y"))
str(covid.new)


covid.new$date <- as.Date(covid.new$date)
filter.index$date <- as.Date(filter.index$date)

full_dates <- tibble(date = seq(from = as.Date("2020-03-05"), 
                                to   = as.Date("2025-07-25"), 
                                by   = "day"))

names(covid.new) <- str_trim(names(covid.new))
names(covid.new) <- str_trim(names(covid.new))
covid_new <- covid_new %>%
  mutate(`People vaccinated (cumulative, per hundred)` = 
           round(`People vaccinated (cumulative, per hundred)`, 2)) 
str(covid.new)
covid.new <- covid.new %>%
  mutate(wave = case_when(
    date >= as.Date("2020-03-01") & date <= as.Date("2020-09-30") ~ "First Wave",
    date >= as.Date("2020-10-01") & date <= as.Date("2021-02-28") ~ "Second Wave",
    date >= as.Date("2021-05-01") & date <= as.Date("2021-10-31") ~ "Third Wave",
    date >= as.Date("2021-11-01") & date <= as.Date("2022-02-28") ~ "Fourth Wave",
    TRUE ~ "Outside Waves"
  ))

### Converting  data to an " sts" format for hhh4
covid.sts <- sts(
  observed = covid.new$new.cases,  
  epoch = as.numeric(as.Date(covid.new$date)),  
  start = c(2020, 3), 
  frequency = 365 )

#### DATA EXPLORATION STARTS
custom_breaks <- seq(as.Date("2020-03-01"), as.Date("2022-07-01"), by = "6 months")

ggplot(covid.new, aes(x = date, y = new.cases)) +
  geom_bar(stat = "identity", fill = "red", color = "red") +  
  scale_x_date(breaks = custom_breaks, labels = format(custom_breaks, "%b-%Y"), 
               limits = c(as.Date("2020-03-01"), as.Date("2022-07-31"))) + 
  scale_y_continuous(labels = scales::comma, limits = c(0, 40000)) + 
  labs(title = "Daily COVID-19 New Cases Over Time",
       x = "Date",
       y = "No. of New Cases") +
  theme_bw() +  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 12),y
    axis.text.y = element_text(color = "black", size = 12), 
    axis.title = element_text(face = "bold", size = 14), 
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5) 
  )



##### Vaccination plot
ggplot(covid.new, aes(x = as.Date(date), y =`People vaccinated (cumulative, per hundred)`))
+ geom_line(color = "red") +
  
  labs(title = "Vaccination Coverage Over Time",
       x = "Date", y ="People vaccinated (cumulative, per hundred)" ) +
  theme_bw()

ggplot(covid.new, aes(x = as.Date(date), y = vaccination)) +
  geom_line(color = "red") +
  labs(title = "Vaccination Coverage Over Time",
       x = "Date", y ="People vaccinated (cumulative, per hundred)" ) +
  theme_bw()

scale_factor <- max(covid.new$new.cases,
                    
                    na.rm = TRUE) / max(covid.new$'People vaccinated (cumulative, per hundred)', 
                                        na.rm = TRUE)

ggplot(covid.new, aes(x = as.Date(date))) +
  geom_line(aes(y = new_cases, color = "Cases")) +
  geom_line(aes(y = `People vaccinated (cumulative, per hundred)`* scale_factor,
                
                color = "Vaccination")) +
  scale_y_continuous(
    name = "Number of Cases",
    sec.axis = sec_axis(~./scale_factor, name = "Vaccination")
  ) +
  labs(title = "COVID-19 Cases and Vaccination Over Time", x = "Date") +
  scale_color_manual(values = c("Cases" = "blue", "Vaccination" = "pink")) +
  theme_bw()

scale.factor<- max(covid.new$new.cases, 
                   
                   na.rm = TRUE) / max(covid.new$stringency.index, na.rm = TRUE)

ggplot(covid.new, aes(x = as.Date(date))) +
  geom_line(aes(y = new.cases, color = "Cases")) +
  geom_line(aes(y = stringency.index * scale.factor, color = "Stringency")) +
  scale_y_continuous(
    name = "Number of Cases",
    sec.axis = sec_axis(~./scale.factor, name = "Stringency Index")
  ) +
  labs(title = "COVID-19 Cases and Stringency Index Over Time", x = "Date") +
  scale_color_manual(values = c("Cases" = "blue", "Stringency" = "red")) +
  theme_bw()

####Variants plot

variant_dates <- seq(as.Date("2020-12-01"), as.Date("2022-07-01"), by = "1 month")

variant.data <- tibble(
  date = variant.dates,
  Original = c(seq(0.8, 0.0, length.out = 7), rep(0, 13)),
  Beta = c(rep(0, 3), seq(0.0, 0.6, length.out = 4), seq(0.6, 0.0, length.out = 4), 
           rep(0, 9)),
  Delta = c(rep(0, 8), seq(0.1, 0.9, length.out = 4), seq(0.9, 0.1, length.out = 4),rep(0, 4)),
  Omicron = c(rep(0, 13), seq(0.1, 1, length.out = 4), rep(1, 3))
)
variant.long <- variant.data %>%
  pivot_longer(cols = -date, names_to = "Variant", values_to = "Prevalence")

ggplot(variant.long, aes(x = date, y = Prevalence, fill = Variant)) +
  geom_area(alpha = 0.9, color = "white") +
  scale_y_continuous(labels = percent_format(), expand = c(0, 0)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b\n%Y", expand = c(0, 0)) +
  labs(
    title = "Estimated Prevalence of SARS-CoV-2 Variants in South Africa",
    y = "Estimated Prevalence (%)",
    x = "Date",
    fill = "Variant"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.4,size = 12),
    axis.text.x = element_text(angle = 30, hjust = 1),
    panel.grid = element_blank()
  )

#### Summary Statistics
province.columns <- c("EC", "FS", "GP", "KZN", "LP", "MP", "NC", "NW", "WC")
covid.new.daily <- covid.new %>%
  arrange(date) %>%
  mutate(across(all_of(province.columns), ~ c(NA, diff(.)), .names = "new_{col}"))

covid.new.daily <- covid_new_daily %>%
  mutate(
    Period = case_when(
      date >= ymd("2020-03-01") & date <= ymd("2020-08-31") ~ "2020(Mar–Aug)",
      date >= ymd("2020-09-01") & date <= ymd("2021-02-28") ~ "2020(Sep–Feb)",
      date >= ymd("2021-03-01") & date <= ymd("2021-08-31") ~ "2021(Mar–Aug)",
      date >= ymd("2021-09-01") & date <= ymd("2022-02-28") ~ "2021(Sep–Feb)",
      date >= ymd("2022-03-01") & date <= ymd("2022-07-25") ~ "2022(Mar–Jul)"
    )
  )

new.case.columns <- paste0("new_", province.columns)

max.new.cases<- covid.new.daily %>%
  group_by(Period) %>%
  summarise(across(all_of(new.case.columns), ~ max(., na.rm = TRUE))) %>%
  ungroup()

colnames(max.new.cases) <- c("Period", province.columns)

print(max.new.cases)


covid.new <- covid.new %>%
  mutate(
    Period = case_when(
      date >= as.Date("2020-03-01") & date <= as.Date("2020-08-31") ~ "2020-H1",
      date >= as.Date("2020-09-01") & date <= as.Date("2021-02-28") ~ "2020-H2",
      date >= as.Date("2021-03-01") & date <= as.Date("2021-08-31") ~ "2021-H1",
      date >= as.Date("2021-09-01") & date <= as.Date("2022-02-28") ~ "2021-H2",
      date >= as.Date("2022-03-01") & date <= as.Date("2022-07-31") ~ "2022-H1",
      TRUE ~ NA_character_
    )
  )


province_columns <- c("EC", "FS", "GP", "KZN", "LP", "MP", "NC", "NW", "WC")

summary.by.period <- covid.new %>%
  filter(!is.na(Period)) %>%  
  group_by(Period) %>%
  summarise(across(
    all_of(province_columns),
    list(
      Min = ~min(., na.rm = TRUE),
      Max = ~max(., na.rm = TRUE),
      Mean = ~mean(., na.rm = TRUE),
      SD = ~sd(., na.rm = TRUE),
      Total = ~sum(., na.rm = TRUE)
    ),
    .names = "{.col}_{.fn}"
  )) %>%
  pivot_longer(
    cols = -Period,
    names_to = c("Province", ".value"),
    names_sep = "_"
  ) %>%
  relocate(Period, Province, Min, Max, Mean, SD, Total) %>%
  arrange(Period, Province)
summary.by.period <- summary.by.period %>%
  mutate(across(where(is.numeric), round, digits = 1))
print(summary.by.period)
province_columns <- c("EC", "FS", "GP", "KZN", "LP", "MP", "NC", "NW", "WC")

### Summarising max per province and period
max.cases.table <- covid.new %>%
  filter(!is.na(Period)) %>%
  group_by(Period) %>%
  summarise(across(all_of(province_columns), ~ max(.x, na.rm = TRUE))) %>%
  arrange(Period)
max.cases.table <- max.cases.table %>%
  mutate(across(where(is.numeric), round, digits = 1))
print(max.cases.table)


####Preparing Data for fitting Models
covid.new.incidence <- covid.new %>%
  mutate(
    EC = c(0, diff(EC)),
    FS = c(0, diff(FS)),
    GP = c(0, diff(GP)),
    KZN = c(0, diff(KZN)),
    LP = c(0, diff(LP)),
    MP = c(0, diff(MP)),
    NC = c(0, diff(NC)),
    NW = c(0, diff(NW)),
    WC = c(0, diff(WC))
  )
str(covid.new.incidence)

#### Creating Power-law weights matrix
set.seed(123)
province.names <- c("EC", "FS", "GP", "KZN", "LP", "MP", "NC", "NW", "WC")

adj.matrix <- matrix(0, nrow = 9, ncol = 9)
colnames(adj.matrix) <- rownames(adj.matrix) <- province_names

### Filling in the adjacencies manually 
adj.matrix["EC", c("FS", "KZN")] <- 1
adj.matrix["FS", c("EC", "GP", "KZN", "MP", "NC", "NW")] <- 1
adj.matrix["GP", c("FS", "MP", "NW")] <- 1
adj.matrix["KZN", c("EC", "FS", "MP")] <- 1
adj.matrix["LP", c("MP", "NW")] <- 1
adj.matrix["MP", c("LP", "GP", "FS", "KZN")] <- 1
adj.matrix["NC", c("FS", "NW", "WC")] <- 1
adj.matrix["NW", c("GP", "FS", "NC", "LP")] <- 1
adj.matrix["WC", c("NC")] <- 1

### Symmetrize
adj.matrix <- (adj.matrix + t(adj.matrix)) > 0
adj.matrix <- 1 * adj.matrix 
nb.obj <- mat2listw(adj.matrix, style = "B")$neighbours

####Creating neighborhood order matrix
order.matrix <- nbOrder(adj.matrix)
weight.fun <- W_powerlaw(maxlag = max(order.matrix, na.rm = TRUE))
spatial.weights <- weight_fun$w(d = 1.6, nbmat = order.matrix)
spatial.weights <- spatial.weights / rowSums(spatial.weights)
round(spatial.weights, 3)
class(spatial.weights)
dim(spatial.weights)  
head(spatial.weights)
rowSums(spatial.weights)

#### Creating  sts data for model analysis
set.seed(123)
covid.new.incidence <- covid.new.incidence %>%
  mutate(across(c(EC, FS, GP, KZN, LP, MP, NC, NW, WC), ~ pmax(0, .)))
apply(covid.new.incidence %>% select(EC, FS, GP, KZN, LP, MP, NC, NW, WC), 2, function(x) 
  
  sum(x < 0))
sts.data.incidence <- sts(
  observed = covid.new.incidence %>% 
    select(EC, FS, GP, KZN, LP, MP, NC, NW, WC) %>% 
    as.matrix(),
  frequency = 365,  # daily data
  start = c(2020, 3)
)
#####Creating Population Offset
province.population <- c(
  EC = 6734001,
  FS = 2928903,
  GP = 15488137,
  KZN = 11531628,
  LP = 5852553,
  MP = 4679786,
  NC = 1292786,
  NW = 4108816,
  WC = 7005741
)

n.time <- nrow(sts.data.incidence)
population.mat <- matrix(rep(province.population, each = n_time), nrow = n.time,
                         
                         byrow = FALSE)
population.frac <- population.mat / 1000000
population(sts.data.incidence) <- population.frac
log.offset <- log(population(sts.data.incidence))



####MODEL 1 Baseline Model
fit.incidence <- hhh4(
  sts.data.incidence,
  control = list(
    end = list(f = ~ 1,offset = population(sts.data.incidence)),
    ar = list(f = ~ 1),
    ne = list(f = ~ 1, weights = spatial.weights, start = 0.01),
    family = "NegBin1"
  )
)

summary(fit.incidence, idx2Exp = TRUE, amplitudeShift = TRUE, maxEV = TRUE)
AIC(fit.incidence)
confint(fit.incidence,parm="overdisp")
plot(fit_incidence, type = "fitted", units = 1:9, hide0s = TRUE, legend = TRUE)

####MODEL2 SEASONALITY +TIME INDEX
set.seed(123)
time_index <- seq_len(nrow(sts.data.incidence@observed))  
season_sin <- sin(2 * pi * time_index / 365)
season_cos <- cos(2 * pi * time_index / 365)
f.end <- addSeason2formula(
  f = ~ 1 + time_index,  
  S = 3, period = 365    
)
f.ar <- addSeason2formula(
  f = ~ 1 + time_index,  
  S = 3, period = 365     
)
f.ne <- addSeason2formula(
  f = ~ 1 + time_index,  
  S = 3, period = 365  )

fit.seasonal.trend <- hhh4(
  sts_data_incidence,
  control = list(
    end = list(f = f.end,offset = log.offset),
    ar = list(f = f.ar),  
    ne = list(f = f.ne, weights = spatial.weights),  
    family = "NegBin1",
    optimizer = list(variance = list(method = "Nelder-Mead")),
    covariates = list(
      time_index = time_index, 
      season_sin = season_sin,
      season_cos = season_cos
    )  
  )
)

summary(fit.seasonal.trend, idx2Exp = TRUE, amplitudeShift = TRUE, maxEV = TRUE)
AIC(fit.seasonal.trend)

covid.new.incidence <- covid.new.incidence %>%
  mutate(variant = case_when(
    wave == 1 ~ "Wuhan",
    wave == 2 ~ "Beta",
    wave == 3 ~ "Delta",
    wave == 4 ~ "Omicron",
    TRUE ~ "Unknown"  
  ))

covid.new.incidence <- covid.new.incidence %>%
  mutate(
    variant_Wuhan = as.numeric(variant == "Wuhan"),
    variant_Beta = as.numeric(variant == "Beta"),
    variant_Delta = as.numeric(variant == "Delta"),
    variant_Omicron = as.numeric(variant == "Omicron")
  )
colnames(covid.new.incidence)
covid.new.incidence <- covid.new.incidence %>%
  mutate(
    variant_Beta = ifelse(wave == "Second Wave", 1, 0),
    variant_Delta = ifelse(wave == "Third Wave", 1, 0),
    variant_Omicron = ifelse(wave == "Fourth Wave", 1, 0)
  )

table(covid.new.incidence$variant_Beta)
table(covid.new.incidence$variant_Delta)
table(covid.new.incidence$variant_Omicron)

#####MODEL3 SEASONALITY + VARIANTS
set.seed(123)
f.end <- addSeason2formula(
  f = ~ 1 + time.index + variant_Beta + variant_Delta + variant_Omicron, 
  S = 3, period = 365
)
f.ar <- addSeason2formula(
  f = ~ 1 + time_index + variant_Beta + variant_Delta + variant_Omicron, 
  S = 3, period = 365
)
f.ne <- addSeason2formula(
  f = ~ 1 + time_index + variant_Beta + variant_Delta + variant_Omicron, 
  S = 3, period = 365
)

fit.seasonal.trend.variant<- hhh4(
  sts_data_incidence,
  control = list(
    end = list(f = f.end,offset = log_offset),
    ar = list(f = f.ar),  
    ne = list(f = f.ne, weights = spatial_weights),  
    family = "NegBin1",
    optimizer = list(variance = list(method = "Nelder-Mead")),
    covariates = list(
      time_index = time_index,  
      season_sin = season_sin,
      season_cos = season_cos
    )  
  )
)

summary(fit.seasonal.trend.variant, idx2Exp = TRUE, amplitudeShift = TRUE, maxEV = TRUE)
AIC(fit.seasonal.trend.variant)

####MODEL4
set.seed(123)

time_index <- seq_len(nrow(sts_data_incidence@observed))
f.end <- addSeason2formula(
  f = ~ -1 + time_index +variant_Beta + variant_Omicron+ variant_Delta + 
    
    ri(type="iid", corr="none"),
  S = 3, period = 365)

f.ar <- ~ -1 + time_index +variant_Beta + variant_Omicron+ variant_Delta + 
  ri(type="iid", corr="none")

f.ne <- ~ -1 +  ri(type="iid", corr="none")
fit.optimized1 <- hhh4(
  sts_data_incidence,
  control = list(
    end = list(f = f.end, offset = log.offset),
    ar = list(f = f.ar),  
    ne = list(f = f.ne, weights = spatial.weights), family = "NegBinM",
    optimizer = list(variance = list(method = "Nelder-Mead")),
    covariates = list(
      time_index = time_index,  
      season_sin = season_sin,
      season_cos = season_cos)))

summary(fit.optimized1, amplitudeShift = TRUE, maxEV = TRUE)

####Calculating AIC and BIC
set.seed(123)
logLik.value <- logLik(fit.optimized1)
num.parameters <- length(coef(fit.optimized1))
AIC.manual <- -2 * as.numeric(logLik.value) + 2 * num.parameters
num.obs <- nrow(sts.data.incidence@observed)  
BIC.manual <- -2 * as.numeric(logLik.value) +log(num.obs) * num.parameters
cat("AIC:", AIC.manual, "\n")
cat("BIC:", BIC.manual, "\n")

####Sorting stringency index
stringency_raw <- covid.new$stringency_index
stringency_numeric <- as.numeric(as.character(stringency_raw))
print(class(stringency_numeric))  
n_time <- nrow(sts.data.incidence)
stringency_trimmed <- stringency_numeric[1:n_time]
print(length(stringency_trimmed)) 

####MODEL5  WITH STRINGENCY
set.seed(123)
time_index <- seq_len(nrow(sts.data.incidence@observed))
covid.new$stringency_index <- as.numeric(as.character(covid.new$stringency_index))
n_time <- nrow(sts.data.incidence) 
stringency_trimmed <- covid.new$stringency_index[1:n_time]
print(length(stringency_trimmed))  
stringency <- stringency_trimmed
f.end <- addSeason2formula(
  f = ~ -1 + time_index + variant_Beta + variant_Omicron + variant_Delta +
    I(stringency) + ri(type = "iid", corr = "none"),
  S = 3, period = 365)

f.ar <- ~ -1 + time_index + variant_Beta + variant_Omicron + variant_Delta  +
  ri(type = "iid", corr = "none")
f.ne <- ~ -1 + ri(type = "iid", corr = "none")
fit.optimized2 <- hhh4(
  sts.data.incidence,
  control = list(
    end = list(f = f.end, offset = log.offset),
    ar  = list(f = f.ar),
    ne  = list(f = f.ne, weights = spatial.weights),
    family = "NegBinM",
    optimizer = list(variance = list(method = "Nelder-Mead")),
    covariates = list(
      time_index = time_index,
      season_sin = season_sin,
      season_cos = season_cos,
      stringency = stringency  
    )
  )
)
summary(fit.optimized2, idx2Exp = TRUE,  amplitudeShift = TRUE, maxEV = TRUE)

set.seed(123)
logLik.value <- logLik(fit.optimized2)
num.parameters <- length(coef(fit.optimized2))
AIC.manual <- -2 * as.numeric(logLik.value) + 2 * num.parameters
num.obs <- nrow(sts.data.incidence@observed) 
BIC.manual <- -2 * as.numeric(logLik.value) + log(num.obs) * num.parameters
cat("AIC:", AIC.manual, "\n")
cat("BIC:", BIC.manual, "\n")


#### MODEL 6 WITH WEEKEND EFFECT
n_time <- nrow(sts.data.incidence) 
t <- 1:n_time                 
covid.new$stringency_index <- as.numeric(as.character(covid.new$stringency_index))
stringency <- covid.new$stringency_index[1:n_time]  
dates <- as.Date(covi._new$date[1:n_time])
weekend <- ifelse(weekdays(dates) %in% c("Saturday", "Sunday"), 1, 0)
print(class(weekend))  
print(table(weekend))  

variant_Beta <- as.numeric(as.character(variant_Beta))
variant_Omicron <- as.numeric(as.character(variant_Omicron))
variant_Delta <- as.numeric(as.character(variant_Delta))
if(length(variant_Beta) < n_time) {
  variant_Beta <- rep(variant_Beta[1], n_time)
} else {
  variant_Beta <- variant_Beta[1:n_time]
}
if(length(variant_Omicron) < n_time) {
  variant_Omicron <- rep(variant_Omicron[1], n_time)
} else {
  variant_Omicron <- variant_Omicron[1:n_time]
}
if(length(variant_Delta) < n_time) {
  variant_Delta <- rep(variant_Delta[1], n_time)
} else {
  variant_Delta <- variant_Delta[1:n_time]
}

sts.data.incidence.sub <- sts.data.incidence  
t_sub            <- t
season_sin_sub   <- season_sin
season_cos_sub   <- season_cos
variant_Beta_sub <- variant_Beta
variant_Omicron_sub <- variant_Omicron
variant_Delta_sub   <- variant_Delta
stringency_sub <- stringency
weekend_sub    <- weekend

f.end <- addSeason2formula(
  f = ~ -1 + t + variant_Beta + variant_Omicron + variant_Delta +
    I(stringency) + weekend + ri(type = "iid", corr = "none"),
  S = 3, period = 365)
f.ar <- ~ -1 + t + variant_Beta + variant_Omicron + variant_Delta +
  I(stringency) + weekend +
  ri(type = "iid", corr = "none")
f.ne <- ~ -1 + weekend + ri(type = "iid", corr = "none")

fit.optimized3 <- hhh4(
  sts.data.incidence.sub,
  control = list(
    end = list(f = f.end,offset = log.offset),
    ar  = list(f = f.ar),
    ne  = list(f = f.ne, weights = spatial.weights),
    family = "NegBinM",
    optimizer = list(variance = list(method = "Nelder-Mead")),
    covariates = list(
      t = t_sub,
      season_sin = season_sin_sub,
      season_cos = season_cos_sub,
      variant_Beta = variant_Beta_sub,
      variant_Omicron = variant_Omicron_sub,
      variant_Delta = variant_Delta_sub,
      stringency = stringency_sub,
      weekend = weekend_sub
    )
  )
)
summary(fit.optimized3,idx2Exp = TRUE,  amplitudeShift = TRUE, maxEV = TRUE)

set.seed(123)
logLik.value <- logLik(fit.optimized3)
num.parameters <- length(coef(fit.optimized3))
AIC.manual <- -2 * as.numeric(logLik.value) + 2 * num.parameters
num.obs <- nrow(sts.data.incidence@observed) 
BIC.manual <- -2 * as.numeric(logLik.value) + log(num.obs) * num.parameters
cat("AIC:", AIC_manual, "\n")
cat("BIC:", BIC_manual, "\n")

##### FINAL MODEL 7

set.seed(123)
n_time <- nrow(sts.data.incidence)   
t_sub <- 1:n_time                    
covid.new$stringency_index <- as.numeric(as.character(covid.new$stringency_index))
stringency_sub <-covid.new$stringency_index[1:n_time]  
dates <- as.Date(covid.new$date[1:n_time])
weekend_sub <- ifelse(weekdays(dates) %in% c("Saturday", "Sunday"), 1, 0)

variant_Beta_sub <- as.numeric(as.character(variant_Beta))
variant_Omicron_sub <- as.numeric(as.character(variant_Omicron))
variant_Delta_sub <- as.numeric(as.character(variant_Delta))

if(length(variant_Beta_sub) < n_time) {
  variant_Beta_sub <- rep(variant_Beta_sub[1], n_time)
} else {
  variant_Beta_sub <- variant_Beta_sub[1:n_time]
}
if(length(variant_Omicron_sub) < n_time) {
  variant_Omicron_sub <- rep(variant_Omicron_sub[1], n_time)
} else {
  variant_Omicron_sub <- variant_Omicron_sub[1:n_time]
}
if(length(variant_Delta_sub) < n_time) {
  variant_Delta_sub <- rep(variant_Delta_sub[1], n_time)
} else {
  variant_Delta_sub <- variant_Delta_sub[1:n_time]
}

sts.data.incidence.sub <- sts.data.incidence
season_sin_sub <- season_sin
season_cos_sub <- season_cos

covid.new$vaccination <- as.numeric(as.character(covid.new$vaccination))
vaccination_sub <- round(covid_new$vaccination[1:n_time], 2)

f.end <- addSeason2formula(
  f = ~ -1 + t_sub + variant_Beta + variant_Omicron + variant_Delta +
    I(stringency) + weekend  +I(lag(vaccination, 7)) + 
    ri(type = "iid", corr = "none"),
  S = 3, period = 365)
f.ar <- ~ -1 + t_sub  + variant_Omicron + variant_Delta + 
  I(stringency) + weekend +  I(lag(vaccination, 7)) +
  ri(type = "iid", corr = "none")

f.ne <- ~ -1 +variant_Beta + weekend +  I(lag(vaccination, 7)) + 
  ri(type = "iid", corr = "none")

covariates_list <- list(
  t_sub = t_sub,
  season_sin = season_sin_sub,
  season_cos = season_cos_sub,
  variant_Beta = variant_Beta_sub,
  variant_Omicron = variant_Omicron_sub,
  variant_Delta = variant_Delta_sub,
  stringency = stringency_sub,
  weekend = weekend_sub,
  vaccination = vaccination_sub
)


set.seed(123)
fit.optimized.final <- hhh4(
  sts_data_incidence_sub,
  control = list(
    end = list(f = f.end, offset = log.offset),
    ar  = list(f = f.ar),
    ne  = list(f = f.ne, weights = spatial.weights),
    family = "NegBinM",
    optimizer = list(variance = list(method = "Nelder-Mead")),
    covariates = covariates_list
  )
)

summary(fit.optimized.final,idx2Exp = TRUE,  amplitudeShift = TRUE, maxEV = TRUE)
exp(confint(fit.optimized.final))

tp_last <- n_time - 1  
preds <- oneStepAhead(fit.optimized.final, tp = tp_last, type = "rolling")
summary(preds)
print(preds$pred)


####Calculationg AIC and BIC
set.seed(123)
logLik.value <- logLik(fit.optimized.final)
num.parameters <- length(coef(fit.optimized.final))
AIC.manual <- -2 * as.numeric(logLik.value) + 2 * num_parameters
num.obs <- nrow(sts.data.incidence@observed)  
BIC.manual <- -2 * as.numeric(logLik.value) + log(num.obs) * num.parameters
cat("AIC:", AIC.manual, "\n")
cat("BIC:", BIC.manual, "\n")

#### GOODNESS OF FIT PLOT for final model
set.seed(123)
plot(fit.optimized.final, type = "fitted", units = 1:9, hide0s = TRUE )
lattice.options(default.theme = modifyList(standard.theme("pdf"), 
                                           list(fontsize = list(text = 12))))

png("goodness_of_fit_model7.png", width = 1500, height = 1200, res = 200)
plot(fit_optimized_final,
     type = "fitted",
     units = 1:9,
     hide0s = TRUE,
     col = c("black", "blue", "orange"),
     legend = TRUE,
     ylab = "No. Infected",
     xlab = "Year")
dev.off()

#####SEASONALITY EFFECT PLOT
plot(fit.optimized.final, type = "season", components = "end", 
     main = "Estimated Seasonality in the Endemic Component",
     xlab = "Day of the Year", ylab = "Seasonal Multiplier")
abline(v = c(87, 166, 349), col = c("red", "blue", "darkgreen"), lty = 2)
legend("topright", legend = c("Lockdown Start", "School Break", "Festive Period"),
       col = c("red", "blue", "darkgreen"), lty = 2, bty = "n")




####INTERNAL VALIDATION FOR 770-784

observed_cases <- rowSums(observed(sts.data.incidence)[770:784, ])
set.seed(123)
pred_sim <- simulate(fit.optimized.final, nsim = 10000, subset = 770:784)
sim_totals <- apply(pred_sim, c(1, 3), sum)  # dimensions: [days, simulations]
predicted_cases <- rowMeans(sim_totals)
lower_bound <- apply(sim_totals, 1, quantile, probs = 0.025)
upper_bound <- apply(sim_totals, 1, quantile, probs = 0.975)
validation_df <- data.frame(
  Day = 770:784,
  Observed = observed_cases,
  Predicted = predicted_cases,
  Lower = lower_bound,
  Upper = upper_bound
)

province_names <- colnames(observed(sts.data.incidence))
day_range <- 770:784
observed_matrix <- observed(sts_data_incidence)[day_range, ]
set.seed(123)
sim_data <- simulate(fit.optimized.final, nsim = 10000, subset = day_range)  
n_days <- length(day_range)
n_prov <- length(province_names)
validation.df <- expand.grid(
  Day = day_range,
  Province = province_names
)

validation.df$Observed <- as.vector(observed_matrix)
validation.df$Predicted <- apply(sim_data, c(1, 2), mean) %>% as.vector()
validation.df$Lower <- apply(sim_data, c(1, 2), quantile, probs = 0.025) %>% as.vector()
validation.df$Upper <- apply(sim_data, c(1, 2), quantile, probs = 0.975) %>% as.vector()

ggplot(validation.df, aes(x = Day)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "pink", alpha = 0.4) +
  geom_line(aes(y = Predicted, color = "Predicted"), size = 1.1) +
  geom_point(aes(y = Observed, color = "Observed"), size = 0.8) +
  facet_wrap(~ Province, scales = "free_y", ncol = 3) +
  labs(
    title = "Internal Validation: Observed vs Predicted COVID-19 Cases(Days:770-784)",
    x = "Day Index",
    y = "Reported Cases",
    color = ""
  ) +
  theme(
    plot.title = element_text(size = 15)
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("Observed" = "blue", "Predicted" = "red"))


####INTERNAL VALIDATION FOR 849-862
province_names <- colnames(observed(sts.data.incidence))
day_range <- 849:862
observed_matrix <- observed(sts.data.incidence)[day_range, ]
set.seed(123)
sim_data <- simulate(fit.optimized.final, nsim = 10000, subset = day_range)  
n_days <- length(day_range)
n_prov <- length(province_names)

validation.df <- expand.grid(
  Day = day_range,
  Province = province_names
)

validation.df$Observed <- as.vector(observed_matrix)
validation_df$Predicted <- apply(sim_data, c(1, 2), mean) %>% as.vector()
validation.df$Lower <- apply(sim_data, c(1, 2), quantile, probs = 0.025) %>% as.vector()
validation.df$Upper <- apply(sim_data, c(1, 2), quantile, probs = 0.975) %>% as.vector()

ggplot(validation.df, aes(x = Day)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "pink", alpha = 0.4) +
  geom_line(aes(y = Predicted, color = "Predicted"), size = 1.1) +
  geom_point(aes(y = Observed, color = "Observed"), size = 0.8) +
  facet_wrap(~ Province, scales = "free_y", ncol = 3) +
  labs(
    title = "Internal Validation:Observed vs Predicted COVID-19 CasesDays(Days:849-862)",
    x = "Day Index",
    y = "Reported Cases",
    color = ""
  ) +
  theme(
    plot.title = element_text(size = 15)
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("Observed" = "blue", "Predicted" = "red"))


#### MEAN LOG SCORE
set.seed(123)
num.time.points <- nrow(sts_data_incidence)
tp_last <- num.time.points - 14 
preds <- oneStepAhead(fit.optimized.final, tp = tp_last, type = "rolling")
summary(preds)
print(preds$pred)
observed_cases <- preds$observed
library(scoringRules)
log_scores <- logs_norm(y = preds$observed, mean = preds$pred, sd = sqrt(preds$psi))
mean_log_score <- mean(log_scores, na.rm = TRUE)
cat("Mean Log Score (LS):", mean_log_score, "\n")
observed_cases


# Calculate Mean Absolute Error and RMSE
set.seed(123)
mae <- mean(abs(predicted_cases - observed_cases))
print(mae)
rmse <- sqrt(mean((observed_cases - predicted_cases)^2))
print(rmse)

###Extracting RandomEffects for each Province
re_vector <- ranef(fit.optimized.final)
re_df <- data.frame(
  Component = gsub("\\..*$", "", names(re_vector)),  
  Province = gsub("^.*\\.", "", names(re_vector)),   
  Effect = as.numeric(re_vector))
re_wide <- pivot_wider(re_df, names_from = Component, values_from = Effect)
sa_map <- sa_map %>%
  left_join(re_wide, by = c("province_code" = "Province"))  

###Random Effects Map
sa_map <- geodata::gadm(country = "ZAF", level = 1, path = tempdir())
plot(sa_map)
head(sa_map)
names(sa_map)
sa_map$Province <- dplyr::recode(sa_map$NAME_1,
                                 "Gauteng" = "GP",
                                 "Western Cape" = "WC",
                                 "KwaZulu-Natal" = "KZN",
                                 "Limpopo" = "LP",
                                 "Mpumalanga" = "MP",
                                 "North West" = "NW",
                                 "Northern Cape" = "NC",
                                 "Eastern Cape" = "EC",
                                 "Free State" = "FS"
)
sa_map <- sf::st_as_sf(sa_map)
sa_map <- dplyr::left_join(sa_map, re_wide, by = "Province")

label_size <- 3.5

###AR map
map_ar <- ggplot(sa_map) +
  geom_sf(aes(fill = ar), color = "white") +
  geom_text(data = province_labels,
            aes(X, Y, label = Province),
            size = label_size, color = "black") +
  scale_fill_viridis_c(name = "AR Effect", direction = -1) +
  labs(title = "Autoregressive ") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(), axis.title.y = element_blank())

###NE map
map_ne <- ggplot(sa_map) +
  geom_sf(aes(fill = ne), color = "white") +
  geom_text(data = province.labels,
            aes(X, Y, label = Province),
            size = label_size, color = "black") +
  scale_fill_viridis_c(name = "NE Effect", direction = -1) +
  labs(title = "Neighbourhood ") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(), axis.title.y = element_blank())

# Endemic map
map_end <- ggplot(sa_map) +
  geom_sf(aes(fill = end), color = "white") +
  geom_text(data = province.labels,
            aes(X, Y, label = Province),
            size = label_size, color = "black") +
  scale_fill_viridis_c(name = "Endemic Effect", direction = -1) +
  labs(title = "Endemic") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(), axis.title.y = element_blank())

combined_plot <- (map_ar | map_ne | map_end) +
  plot_layout(guides = "collect") &
  theme(legend.position = 'bottom')
combined_plot

ggsave("RandomEffects_Labeled.png", combined_plot, width = 14, height = 5, dpi = 300)

#####Component Contribution Map

endemic.matrix <- predict(fit.optimized.final, type = "endemic")
ar.matrix      <- predict(fit.optimized.final, type = "epi.own")
ne.matrix      <- predict(fit.optimized.final, type = "epi.neighbours")

component.totals <- data.frame(
  Province = colnames(endemic.matrix),
  Endemic = round(colSums(endemic.matrix)),
  AR = round(colSums(ar.matrix)),
  NE = round(colSums(ne.matrix))
)
component.percent <- component.totals %>%
  mutate(
    Total = Endemic + AR + NE,
    Endemic_pct = round(100 * Endemic / Total, 1),
    AR_pct = round(100 * AR / Total, 1),
    NE_pct = round(100 * NE / Total, 1)
  ) %>%
  select(Province, Endemic_pct, AR_pct, NE_pct)

total_row <- data.frame(
  Province = "South Africa",
  Endemic_pct = round(100 * sum(component.totals$Endemic) / sum(component.totals$Endemic + 
                                                                  
                                                                  component.totals$AR + component.totals$NE), 1),
  AR_pct = round(100 * sum(component.totals$AR) / sum(component_totals$Endemic + 
                                                        
                                                        component.totals$AR + component.totals$NE), 1),
  NE_pct = round(100 * sum(component.totals$NE) / sum(component.totals$Endemic + 
                                                        
                                                        component.totals$AR + component.totals$NE), 1))

component.percent <- bind_rows(component.percent, total_row)
library(xtable)
print(xtable(component.percent, 
             caption = "Proportional Contribution of each 
component to total predicted COVID-19 cases by province."),
      include.rownames = FALSE)

sa.map <- merge(sa.map, component.percent, by = "Province", all.x = TRUE)
label.size <- 3.2

map.end <- ggplot(sa.map) +
  geom_sf(aes(fill = Endemic_pct), color = "white") +
  geom_text(data = province.labels, aes(X, Y, label = Province),
            size = label.size, color = "black") +
  scale_fill_viridis_c(name = "Endemic (%)", direction = -1) +
  labs(title = "Endemic ") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

map.ar <- ggplot(sa.map) +
  geom_sf(aes(fill = AR_pct), color = "white") +
  geom_text(data = province.labels, aes(X, Y, label = Province),
            size = label.size, color = "black") +
  scale_fill_viridis_c(name = "AR (%)", direction = -1) +
  labs(title = "Autoregressive") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

map.ne <- ggplot(sa.map) +
  geom_sf(aes(fill = NE_pct), color = "white") +
  geom_text(data = province.labels, aes(X, Y, label = Province),
            size = label.size, color = "black") +
  scale_fill_viridis_c(name = "NE (%)", direction = -1) +
  labs(title = "Neighbourhood") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

combined_plot <- (map.end | map.ar | map.ne) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")
combined_plot

ggsave("Component.Contribution.Maps.png", combined_plot, width = 14, height = 6, dpi = 300)

####FORECASTING AFTER
set.seed(123)
horizon.1 <- 1
nsim <- 10000

y.start <- observed(sts.data.incidence)[nrow(sts.data.incidence), ]

sim1 <- simulate(fit.optimized.final, nsim = nsim,
                 t0 = nrow(sts.data.incidence),
                 T  = nrow(sts.data.incidence) + horizon.1,
                 y.start = y.start)

sim1_mean <- apply(sim1, c(1, 2), mean)

sim1_ower <- apply(sim1, c(1, 2), quantile, probs = 0.025)
sim1_upper <- apply(sim1, c(1, 2), quantile, probs = 0.975)

day1.summary <- data.frame(
  Region = colnames(observed(sts.data.incidence)),
  Mean = sim1_mean[1, ],
  Lower = sim1_lower[1, ],
  Upper = sim1_upper[1, ]
)

print(day1.summary)

set.seed(123)

horizon.7 <- 7
nsim <- 10000

y.start <- observed(sts.data.incidence)[nrow(sts.data.incidence), ]

sim7 <- simulate(fit.optimized.final, nsim = nsim,
                 t0 = nrow(sts.data.incidence),
                 T  = nrow(sts.data.incidence) + horizon.7,
                 y.start = y.start)

sim7_mean <- apply(sim7, c(1, 2), mean)

sim7_lower <- apply(sim7, c(1, 2), quantile, probs = 0.025)
sim7_upper <- apply(sim7, c(1, 2), quantile, probs = 0.975)

day7.summary <- data.frame(
  Region = colnames(observed(sts.data.incidence)),
  Mean = sim7_mean[7, ],
  Lower = sim7_lower[7, ],
  Upper = sim7_upper[7, ]
)

print(day7.summary)


set.seed(123)

horizon.14 <- 14
nsim <- 10000 


y.start <- observed(sts.data.incidence)[nrow(sts.data.incidence), ]


sim14 <- simulate(fit.optimized.final, nsim = nsim,
                  t0 = nrow(sts.data.incidence),
                  T  = nrow(sts.data.incidence) + horizon.14,
                  y.start = y.start)


sim14_mean <- apply(sim14, c(1, 2), mean)
sim14_lower <- apply(sim14, c(1, 2), quantile, probs = 0.025)
sim14_upper <- apply(sim14, c(1, 2), quantile, probs = 0.975)

forecast.day14 <- data.frame(
  Region = colnames(observed(sts_data_incidence)),
  Mean   = sim14_mean[horizon.14, ],
  Lower  = sim14_lower[horizon.14, ],
  Upper  = sim14_upper[horizon.14, ]
)

print(forecast.day14)

####FORECAST AFTER PLOT
day1.summary$Day <- 1
day7.summary$Day <- 7
forecast.day14$Day <- 14
forecast.plot <- bind_rows(day1.summary, day7.summary, forecast.day14)
ggplot(forecast_all_days, aes(x = factor(Day), y = Mean)) +
  geom_point(size = 2, color = "black") +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, color = "darkred") +
  facet_wrap(~ Region, scales = "free_y") +
  labs(
    title = "Predicted Case Forecasts with 95% Intervals",
    x = "Day Ahead",
    y = "Predicted Cases"
  ) +
  theme_bw(base_size = 14)
forecast.plot

