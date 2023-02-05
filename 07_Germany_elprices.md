# Levelized cost of electric vehicles charging options
## Abstract dell'articolo
With rapidly decreasing purchase prices of electric vehicles, charging costs are becoming ever more important for the diffusion of electric vehicles as 
required to decarbonize transport. However, the costs of charging electric vehicles in Europe are largely unknown. Here we develop a systematic 
classification of charging options, gather extensive market data on equipment cost, and employ a levelized cost approach to model charging costs in 
30 European countries (European Union 27, Great Britain, Norway, Switzerland) and for 13 different charging options for private passenger transport.
The findings demonstrate a large variance of charging costs across countries and charging options, suggesting different policy options to reduce 
charging costs. A specific analysis on the impacts and relevance of publicly accessible charging station utilization is performed. 
The results reveal charging costs at these stations to be competitive with fuel costs at typical utilization rates exhibited already today.

## Introduction
The true cost of charging electric vehicles (EV) goes beyond uniform electricity price assumptions and must include additional factors such as charging 
infrastructure cost, infrastructure utilization rates, and a more accurate representation of electricity prices. 
EV charging costs to consumers are not as straightforward, as they depend on a variety of factors including charging location, charging speed, 
time of charging, or even other pricing mechanisms such as charging subscription packages.
Here we model the levelized cost of charging electric vehicles in 30 European countries and for all charging options that are relevant for EV passenger 
transport to address this research gap. We compile a cost component database and develop a systematic modeling framework to estimate country-specific 
levelized cost of charging (LCOC) of different power levels and charging sites. 
Importantly, detailed charging cost components are disaggregated and estimated to better understand potential cost differences and derive actionable policy
implications. Taking into account cost of capital and sales margins of commercial operators, the LCOC represents the long-term average cost to 
the consumer and is thus compared to average costs of conventional fuels.

## Results
$$LCOC = \left( \frac{ C_{Equipment} + C_{Installation} + \sum_{t=1}^{T} \frac{C_{O \\& Amp ; M_t}}{(1+i)^t}  } { \sum_{t=1}^{T} \frac{E_{Charging, g_t}}{(1+i)^t} } + \frac{C_{Electricity}}{\eta} \right) * (1 + C_{Transaction})$$
(See the arcticle to get knowledge about each term)
where 
 - $C_{Equipment}$ is the cost of the charging equipment hardware (€ plug−1),
 - $C_{installation}$ is the cost of installing the charging equipment, including all project costs except the equipment hardware (€ plug−1), 
 - $C_{O \\& Amp ; M_t}$ is the cost of operation and maintenance of the charging infrastructure in year $t$ of the project’s lifetime (€ year−1), 
 - $E_{charging_t}$ is the yearly amount of energy that is charged at the plug in year $t$ of the project’s lifetime (kWh year−1), 
 - $C_{electricity}$ is the electricity cost (€ kWh−1), 
 - $\eta$ is the charging station efficiency (%), 
 - $C_{transaction} is the transaction cost for settling the payment for the charging energy where applicable (%),
 - $T$ is the project lifetime over which the LCOC is calculated (years), 
 - $i$ denotes the interest rate, used to discount future costs and energy to a net present value (reflecting the financing cost of charging station operators where applicable).

To analyze cost heterogeneity across different charging infrastructure, we differentiate a range of power levels and charging sites. 
Reflecting distinct types of charging technology and typical operating conditions in Europe, the power levels are grouped as follows:
 - Low AC (<2.3 kW): socket charging without designated charging equipment (230 V, max. 10 A).
 - Medium AC (3.7–7.4 kW): single-phase AC charging equipment (230 V, 16–32 A).
 - High AC (11–22 kW): three-phase AC charging equipment (230 V, 3 × 16–3 × 32 A).
 - DC (50 kW): DC fast charging equipment.

In addition, the charging sites represent specific locations where charging infrastructure can be installed. We consider four different charging sites:
 - Residential (grid): home charging, drawing electricity from the distribution grid.
 - Residential (PV): home charging, replacing a limited share of the grid electricity during the day with electricity from a rooftop solar PV system, based on realistic hour-by-hour load profiles.
 - Commercial (privately accessible): workplace or fleet charging (e.g. taxi fleet).
 - Commercial (publicly accessible): publicly accessible, fee-based charging (e.g. on roads).

We define a charging option as the combination of a specific power level and charging site, as can be seen in the axes of the matrix of charging options 
displayed in Fig. 1. To reflect real-world charging behavior, we introduce user profiles, which specify the share of energy charged through different 
options in the total energy charged by typical EV owners. Four different characteristic user profiles are defined in Fig. 1. In addition to the four 
characteristic users, we define an Average user which represents the weighted average charging behavior of all defined users.

https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9458728/bin/41467_2022_32835_Fig1_HTML.jpg![image](https://user-images.githubusercontent.com/91499293/216764249-3b873e26-71f2-4130-8580-6aa091b49a25.png)

**REFERENCE** https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9458728/#CR10
