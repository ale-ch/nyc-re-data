# nyc_re_data
The scripts collect and clean data from various sources to create a dataset with sale price estimates for over 700 thousand properties in New York City. 

The `scripts` folder contains the following scripts:
- `rolling_sales.R`: cleans and joins NYC real estate sales data of the past 12 months. 
- `prices_scraping.R`: scrapes real estate market trends estimates for each NYC borough from Property Shark. For residential properties, the estimates are taken from the table at the bottom of the page. For the other property categories, the estimates are taken from the orange exagons at the beginning of the page (Edit: 4/3/2022). 

The above scripts are used to calculate the variable `est_sale_pr_sqft`. The final sale price estimates are calculated simply by multiplying `est_sale_pr_sqft` by `bldgarea`. 

- Link to NYC land use data (at tax lot level): https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_21v4_csv.zip
- Link to NYC sales data: https://data.cityofnewyork.us/dataset/NYC-Citywide-Rolling-Calendar-Sales/usep-8jbt
- Links to Property Shark pages:
    - https://www.propertyshark.com/mason/market-trends/residential/nyc/ + borough name
    - https://www.propertyshark.com/mason/market-trends/commercial/nyc/ + borough name
    - https://www.propertyshark.com/mason/market-trends/multifamily/nyc + borough name
    - https://www.propertyshark.com/mason/market-trends/retail/nyc/ + borough name
    - https://www.propertyshark.com/mason/market-trends/office/nyc/ + borough name
    - https://www.propertyshark.com/mason/market-trends/industrial/nyc/ + borough name
