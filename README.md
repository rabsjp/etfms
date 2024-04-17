# etfms
This repository supports the paper *The impact of ETF index inclusion on stock prices* by JP Rabanal, J. Duffy, D. Friedman, and O. Rud

Raw data: 
The app in https://github.com/Leeps-Lab/otree_etf_cda generates 3 datasets using the session name: 
- *csv all trades
- *json the limit/market orders
- *csv otree standard output 

The folder (a2c, a2c_ss, abc) contains the sessions per treatment (3 treatments) for the first 2 files, the last file is the otree output which is important for the holding analysis. It  stores in the holdings folder. The subfolders 2 and 3 in that folder include the sessions for treatment 2 (A2C) and treatment 3 (A2C_SS). 

The file clean_raw.R reads the otree_markets files output for all 3 treatments. 
For the json file generates the file data_session.Rda, and for the traders datatrades_session.Rda. These data is in the folder /clean. This is the first file that needs to be run. After running clean_raw for each treatment, then run get_together.R r

The file get_together.R reads all data_session and datarades_session and merges them in a single data set. It creates the dataset: alldata.Rda, and alltrades.Rda. 

Dictionary: 
ALLDATA.RDA

- round: market round
- time_entered: timestamp order entered the book
- asset: 1 for A, 2 for B, 3 for C, and 4 for ETF
- pcode: player code
- price: order price
- bid: true or false (ask)
- id: order id
- time_inactive: timestamp for order inactive
- status: current status of the order (e.g. Accepted_taker if order was accepted and the order is a taker)
- tiempo: time variable create for each round
- ex_price: execute price if there is a transaction. 
- tre: treatment, 1 for ABC, 2 for A2C and 3 for A2C_ss

HOLDINGS_treatment.RDA
- a,b,c,etf which denotes the number of unit held by player and treatment (tre). 

ALLTRADES.RDA
- make_pcode maker id (player)
- take_pcode taker id (player)
- make_isbid (TRUE if the make order is a bid)

THe file all_analysis runs measures.R, holdings.R, and graphs.R. 

The file measures.R creates order book measures and stores them in detf_new.csv

DICTIONARY for the new variables: 
- bb: best bid
- bo: best offer
- ordenes_compra: number of offers in the book
- ordenes_venta: number of bids in the book
- z measure defined in the paper for excess demand, 
- activo: asset 1 for A, 2 for B, 3 for C, and 4 for ETF
- precio: order value
- i: tick number
- r: round
- sess: session name

The file holdings.R creates the dataset holdings_treatment.Rda.

The file graphs.R creates Figures 2-4 and session summary graphs included in Appendix C. It uses the data from detf_new file. 

The file holdings_analysis.R analyzes holding data and creates Figure 5. 

The file spreads.R analyzes the spreads and order imbalance. It creates the dataset spreads_session.Rda to use in measures_ag.R. It also provides Table 5, Table D.2.

The file measures_ag.R analyzes prices and turnover. It creates Table 4, Table D.1, Table D.4

The file indv.R creates Table D.3. 


