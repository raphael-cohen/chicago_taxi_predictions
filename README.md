<div align="center">

**CSP571 Project - Fall 2018**
# 🚕 Analysis of Chicago Taxi Trips

</div>

## Introduction
Our main objective is to analyse the taxi data for the city of Chicago for the time period of 2013 - 2018. Our goals are:

0. [Data Cleaning](/0_data_preparation)
1. [Initial Exploratory Analysis](/1_exploratory_analysis)
2. [Price analysis and prediction](/2_price_analysis)
3. [Correlate with Weather/CTA Data](/4_correlations)
4. [**TaxiPool**](/4_taxipool): Cluster people by 5 dimensions (Pickup LAT & LON, Dropoff LAT & LON, Start time), to prove we can put a lot of people with matching trips inside the same taxi.
5. [**ProfitMaximiser**](/5_profit_maximiser): Predict where to place your taxi to maximize likelihood of getting a client, based on time of day (and weather?).

## Results

### 0. Data Cleaning

- Removed unnecessary or redundant data, Convert features to the right type
- Partition the data by year and week
- PostgreSQL (see [schema.sql](0_data_preparation/schema.sql))

### 1. Initial Exploratory Analysis

![](/results/Taxi_Companies_By_Earnings.jpg?raw=true)
![](/results/Taxi_Companies_By_Trips.jpg?raw=true)
![](/results/avg_count_perdayow.png?raw=true)
![](/results/Time_Day.png?raw=true)
![](/results/Payments_Methods.png?raw=true)
