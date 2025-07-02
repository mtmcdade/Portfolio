rm(list=ls())

library(tidyverse)
library(fpp3)
library(ggthemes)
library(gt)

data <- read_csv("https://lwhite01.github.io/Data/HersheyData.csv")

data %>% 
  mutate(FullDate = paste(Year, Quarter, sep='Q')) %>% 
  select(Year, Quarter, FullDate, everything()) %>% 
  arrange(FullDate) -> data


# See all the data

ggplot(data, aes(x = FullDate)) +
  geom_line(aes(y = `Net Sales (in Millions)`, color = "Net Sales", group = 1), linewidth = 1) +
  geom_line(aes(y = `Cost of Sales (In Millions)`, color = "Cost of Sales", group = 1), linewidth = 1) +
  geom_line(aes(y = `Income before Income Taxes`, 
                color = "Income Before Taxes", group = 1), linewidth = 1) +
  labs(title = "Net Sales, Cost of Sales, and Income Before Taxes Over Time",
       x = "Time (Year and Quarter)",
       y = "Amount (in Millions)",
       color = "Legend") -> fullPlot

Q1pos <- grep('Q1',data$FullDate)

fullPlot + geom_vline(xintercept = Q1pos, linetype = "dashed", color = "red")

# Make time series tsibble

data %>% 
  mutate(FullDate=yearquarter(FullDate)) %>%
  as_tsibble(index=FullDate) %>% 
  mutate(Quarter = factor(quarter(FullDate)))->data_ts


#### NET SALES ####

# Decomposition

data_ts %>%
  model(STL=STL(`Net Sales (in Millions)`~trend()+
                  season(), robust=TRUE)) %>%
  components() %>% autoplot()+ theme_gray()

data_ts %>% filter_index(.~"2022 Q2") -> train
data_ts %>% filter_index("2022 Q3"~"2024 Q1") -> test

train %>% model(TSLM=TSLM(`Net Sales (in Millions)`~trend()+season()),
                TSLM2=TSLM(`Net Sales (in Millions)`~trend()+ I(trend()^2) + season()),
                ETS=ETS(`Net Sales (in Millions)`),
                ARIMA=ARIMA(`Net Sales (in Millions)`)) -> fit

fit %>% forecast(test) %>%
  accuracy(data_ts, list(winkler = winkler_score))

data_ts %>% stretch_tsibble(.init = 20, .step=7) %>%
  model(TSLM=TSLM(`Net Sales (in Millions)`~trend()+season()),
        TSLM2=TSLM(`Net Sales (in Millions)`~trend()+ I(trend()^2) + season()),
        ARIMA=ARIMA(`Net Sales (in Millions)`),
        ETS=ETS(`Net Sales (in Millions)`)) %>% 
  mutate(Combo=0.2*ARIMA+0.8*TSLM2) %>%                 
  forecast(h=7) %>%
  accuracy(data_ts)  

# Combo of ARIMA and ETS, maybe just ETS because of Winkler

# Create fit including the combo

data_ts %>% model(TSLM2=TSLM(`Net Sales (in Millions)`~trend()+ I(trend()^2) + season()),
                  ARIMA=ARIMA(`Net Sales (in Millions)`),
                  ETS=ETS(`Net Sales (in Millions)`)) %>% 
  mutate(Combo=0.2*ARIMA+0.8*TSLM2) -> fit2

fit2 %>% augment() -> resids2

resids2 %>% autoplot(.resid) + theme_clean()

# Plot Combo Model - best here

data_ts %>% autoplot(`Net Sales (in Millions)`) +
  autolayer(resids2 %>% filter(.model=="Combo"),.fitted, 
            lty=2,col="red") +
  theme_clean() +
  labs(title = 'Combo Model vs Actual Data')

# Plot TSLM2 Model

data_ts %>% autoplot(`Net Sales (in Millions)`) +
  autolayer(resids2 %>% filter(.model=="TSLM2"),.fitted, 
            lty=2,col="red") +
  theme_clean() +
  labs(title = 'TSLM2 Model vs Actual Data')

# Plot ARIMA Model

data_ts %>% autoplot(`Net Sales (in Millions)`) +
  autolayer(resids2 %>% filter(.model=="ARIMA"),.fitted, 
            lty=2,col="red") +
  theme_clean() +
  labs(title = 'ARIMA Model vs Actual Data')



fit2 %>% forecast(h=7) %>% hilo(level=95)

fit2 %>% select(Combo) %>% forecast(h=7) %>% autoplot(level=95) + 
  autolayer(data_ts, `Net Sales (in Millions)`) +
  theme_clean() +
  labs(title = "Hershey's Projected Net Sales for 2024/2025")
  



#### COST OF SALES ####

data_ts %>%
  model(STL=STL(`Cost of Sales (In Millions)`~trend()+
                  season(), robust=TRUE)) %>%
  components() %>% autoplot()+ theme_classic()

#train,test same as from above

train %>% model(TSLM=TSLM(`Cost of Sales (In Millions)`~trend()+season()),
                TSLM2=TSLM(`Cost of Sales (In Millions)`~trend()+ I(trend()^2) + season()),
                ETS=ETS(`Cost of Sales (In Millions)`),
                ARIMA=ARIMA(`Cost of Sales (In Millions)`)) -> fitC

# Accuracy of Models

fitC %>% forecast(test) %>%
  accuracy(data_ts, list(winkler = winkler_score))

data_ts %>% stretch_tsibble(.init = 20, .step=7) %>%
  model(TSLM=TSLM(`Cost of Sales (In Millions)`~trend()+season()),
        TSLM2=TSLM(`Cost of Sales (In Millions)`~trend()+ I(trend()^2) + season()),
        ARIMA=ARIMA(`Cost of Sales (In Millions)`),
        ETS=ETS(`Cost of Sales (In Millions)`)) %>% 
  mutate(Combo=0.1*ETS+0.9*TSLM2) %>%                 
  forecast(h=7) %>%
  accuracy(data_ts)  

# TSLM2 appears the best

data_ts %>% model(TSLM2=TSLM(`Cost of Sales (In Millions)`~trend()+ I(trend()^2) + season()),
                  ARIMA=ARIMA(`Cost of Sales (In Millions)`),
                  ETS=ETS(`Cost of Sales (In Millions)`)) %>% 
  mutate(Combo=0.1*ETS+0.9*TSLM2) -> fitC2

fitC2 %>% augment() -> residsC2

residsC2 %>% autoplot(.resid) + theme_clean()

# Plot TSLM2 Model - best model here

data_ts %>% autoplot(`Cost of Sales (In Millions)`) +
  autolayer(residsC2 %>% filter(.model=="TSLM2"),.fitted, 
            lty=2,col="red") +
  theme_clean() +
  labs(title = 'TSLM2 Model vs Actual Data')


# Plot ETS Model

data_ts %>% autoplot(`Cost of Sales (In Millions)`) +
  autolayer(residsC2 %>% filter(.model=="ETS"),.fitted, 
            lty=2,col="red") +
  theme_clean() +
  labs(title = 'ETS Model vs Actual Data')


# Plot Combo Model

data_ts %>% autoplot(`Cost of Sales (In Millions)`) +
  autolayer(residsC2 %>% filter(.model=="Combo"),.fitted, 
            lty=2,col="red") +
  theme_clean() +
  labs(title = 'Combo Model vs Actual Data')


fitC2 %>% forecast(h=7) %>% hilo(level=95)

fitC2 %>% select(TSLM2) %>% forecast(h=7) %>% autoplot(level=95) + 
  autolayer(data_ts, `Cost of Sales (In Millions)`) +
  theme_clean() +
  labs(title = "Hershey's Projected Cost of Sales for 2024/2025")












